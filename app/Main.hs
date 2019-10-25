{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GitLab
import Network.HTTP.Types.Status
import Data.Semigroup ((<>))
import Options.Applicative

main :: IO ()
main = processOptions =<< execParser opts
  where
    opts = info (parser <**> helper)
           ( fullDesc
             <> progDesc "Executes actions against a GitLab server"
             <> header "Program to execute bulk GitLab actions" )

data Options =
  Options
  { gitlabUrl :: String
  , gitlabToken :: String
  , usersFilename :: String
  , group :: String
  , project :: String
  , addReportersToGroup :: Bool
  , addGroupToProject :: Bool
  , isRegistered :: Bool
  }
  deriving (Show)

parser :: Parser Options
parser = Options
  <$> strOption
  ( long "host"
    <> metavar "host"
    <> help "URL of the GitLab server" )
  <*> strOption
  ( long "token"
    <> metavar "token"
    <> value ""
    <> help "GitLab access token" )
  <*> strOption
  ( long "filename"
    <> metavar "filename"
    <> value ""
    <> help "name of a CSV file" )
  <*> strOption
  ( long "group"
    <> metavar "group"
    <> value ""
    <> help "name of a group" )
  <*> strOption
  ( long "project"
    <> metavar "project"
    <> value ""
    <> help "name of a project" )
  <*> switch
  ( long "add-reporters-to-group"
    <> help "Add users as reporters to a group" )
  <*> switch
  ( long "share-projects-with-group"
    <> help "Share all projects with a given name to the specific group" )
  <*> switch
  ( long "registered"
    <> help "prints 'yes' or 'no' depending on whether a user exists on the GitLab server" )

processOptions :: Options -> IO ()
processOptions opts = go
  where
    go | addReportersToGroup opts && usersFilename opts == "" =
           return ()

       | addReportersToGroup opts && group opts == "" =
         putStrLn "you must specify a group name too"

       | addReportersToGroup opts && addGroupToProject opts =
         putStrLn "you can only choose one of adding users to group or adding group to projects"

         -- add reporters to a group
       | addReportersToGroup opts && usersFilename opts /= "" = do
           text <- T.readFile (usersFilename opts)
           let usernames = T.splitOn "," text
           runGitLab
             (defaultGitLabServer
              { url = T.pack (gitlabUrl opts)
              , token = T.pack (gitlabToken opts)
              })
             (addUsersToGroupDbg (T.pack (group opts)) Reporter usernames)

       | not (addReportersToGroup opts) && addGroupToProject opts && null (group opts) && null (project opts) =
           error "for --share-project you must specify a group name and a project name"

       -- add group as a member to all projects with a given name, as Reporter
       | not (addReportersToGroup opts) && addGroupToProject opts =
           runGitLab
             (defaultGitLabServer
              { url = T.pack (gitlabUrl opts)
              , token = T.pack (gitlabToken opts)
              })
             (do groups <- groupsWithName (T.pack (group opts))
                 void $ liftIO $ when (null groups)
                   (error ("group not found: " ++ group opts))
                 void $ liftIO $ when (length groups > 1)
                   (error ("multiple groups found for: " ++ group opts))
               -- should only be 1 group with this head
                 let grp = head groups
                 projects <- projectsWithName (T.pack (project opts))
                 mapM_
                   (\prj -> do
                       result <- shareProjectWithGroup (group_id grp) (project_id prj) Reporter
                       case result of
                         Left st -> liftIO (putStrLn ("unable to share project " ++ show (project_id prj) ++ " with group " ++ show (group_id grp) ++ ". Reason: " ++ show st))
                         Right _details -> liftIO (putStrLn ("Added group " ++ show (group_id grp) ++ " to project " ++ show (project_id prj) ++ " as a Reporter"))
                   )
                   projects
             )

       -- ask if users are registered for a given GitLab server
       | not (addReportersToGroup opts) && not (addGroupToProject opts) && isRegistered opts =
           putStrLn "--registered needs a filename with --filename"
  
       | not (addReportersToGroup opts) && not (addGroupToProject opts) && isRegistered opts && usersFilename opts == "" =
           putStrLn "--registered needs a filename with --filename"

       | not (addReportersToGroup opts) && not (addGroupToProject opts) && isRegistered opts = do
           text <- T.readFile (usersFilename opts)
           let usernames = T.splitOn "," text
           runGitLab
             (defaultGitLabServer
              { url = T.pack (gitlabUrl opts)
              , token = T.pack (gitlabToken opts)
              })
             (mapM_
               (\usrName ->
                   do res <- searchUser usrName
                      -- empty list of returned users
                      if null res
                        then liftIO $ putStrLn (T.unpack usrName ++ ": no")
                        else liftIO $ putStrLn (T.unpack usrName ++ ": yes")
               )
               usernames
             )

       | otherwise =
            error "combination of flags not recognised"
  

addUsersToGroupDbg ::
  (MonadIO m) => Text -> AccessLevel -> [Text] -> GitLab m ()
addUsersToGroupDbg groupName access usernames = do
  (results :: [Either Status Member]) <-
    addUsersToGroup groupName access usernames
  liftIO $ mapM_
    (\(result :: Either Status Member) ->
       case result of
         Left errorStatus -> print errorStatus
         Right member ->
           putStrLn
             ("Added to " ++
              show groupName ++ ": " ++ show (member_username member)))
    results
