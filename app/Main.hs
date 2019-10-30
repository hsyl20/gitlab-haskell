{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GitLab
import Network.HTTP.Types.Status
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Data.ByteString.Char8 as C
import Stanford.Moss
import Data.Maybe
import System.FilePath.Posix
import System.IO.Temp
import System.Directory

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
  , plagiarism :: Bool
  , mossUserId :: String
  , mossLang :: String
  , plagiarismProject :: String
  , plagiarismFilePaths :: String
  , parentUser :: String
  , projectLinks :: Bool
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
  ( long "userFilename"
    <> metavar "userFilename"
    <> value ""
    <> help "name of a CSV file with user IDs" )
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
  <*> switch
  ( long "plagiarism"
    <> help "use MOSS to detect plagiarism" )
  <*> strOption
  ( long "mossUser"
    <> metavar "mossUser"
    <> value ""
    <> help "MOSS user ID" )
  <*> strOption
  ( long "mossLang"
    <> metavar "mossLang"
    <> value ""
    <> help "languge to pass to MOSS" )
  <*> strOption
  ( long "plagiarismProject"
    <> metavar "plagiarismProject"
    <> value ""
    <> help "project to detect plagiarism in" )
  <*> strOption
  ( long "plagiarismFiles"
    <> metavar "plagiarismFiles"
    <> value ""
    <> help "comma separated list of plagiarism files" )
  <*> strOption
  ( long "parent"
    <> metavar "parent"
    <> value ""
    <> help "parent username of skeleton code" )
  <*> switch
  ( long "projectLinks"
    <> help "get the URL links for searched for projects" )

processOptions :: Options -> IO ()
processOptions opts = go
  where
    go
      | addReportersToGroup opts && usersFilename opts == "" = return ()
      | addReportersToGroup opts && group opts == "" =
        putStrLn "you must specify a group name too"
      | addReportersToGroup opts && addGroupToProject opts =
        putStrLn
          "you can only choose one of adding users to group or adding group to projects"
         -- add reporters to a group
      | addReportersToGroup opts && usersFilename opts /= "" = do
        text <- T.readFile (usersFilename opts)
        let usernames = T.splitOn "," text
        runGitLab
          (defaultGitLabServer
           {url = T.pack (gitlabUrl opts), token = T.pack (gitlabToken opts)})
          (addUsersToGroupDbg (T.pack (group opts)) Reporter usernames)
      | not (addReportersToGroup opts) &&
          addGroupToProject opts && null (group opts) && null (project opts) =
        error
          "for --share-project you must specify a group name and a project name"
       -- add group as a member to all projects with a given name, as Reporter
      | not (addReportersToGroup opts) && addGroupToProject opts =
        runGitLab
          (defaultGitLabServer
           {url = T.pack (gitlabUrl opts), token = T.pack (gitlabToken opts)})
          (do groups <- groupsWithNameOrPath (T.pack (group opts))
              void $
                liftIO $
                when (null groups) (error ("group not found: " ++ group opts))
              void $
                liftIO $
                when
                  (length groups > 1)
                  (error ("multiple groups found for: " ++ group opts))
               -- should only be 1 group with this head
              let grp = head groups
              projects <- projectsWithName (T.pack (project opts))
              mapM_
                (\prj -> do
                   result <-
                     shareProjectWithGroup
                       (group_id grp)
                       (project_id prj)
                       Reporter
                   case result of
                     Left st ->
                       liftIO
                         (putStrLn
                            ("unable to share project " ++
                             show (project_id prj) ++
                             " with group " ++
                             show (group_id grp) ++ ". Reason: " ++ show st))
                     Right _details ->
                       liftIO
                         (putStrLn
                            ("Added group " ++
                             show (group_id grp) ++
                             " to project " ++
                             show (project_id prj) ++ " as a Reporter")))
                projects)
       -- ask if users are registered for a given GitLab server
      | not (addReportersToGroup opts) &&
          not (addGroupToProject opts) && isRegistered opts =
        putStrLn "--registered needs a filename with --filename"
      | not (addReportersToGroup opts) &&
          not (addGroupToProject opts) &&
          isRegistered opts && usersFilename opts == "" =
        putStrLn "--registered needs a filename with --filename"
      | not (addReportersToGroup opts) &&
          not (addGroupToProject opts) && isRegistered opts = do
        text <- T.readFile (usersFilename opts)
        let usernames = T.splitOn "," text
        runGitLab
          (defaultGitLabServer
           {url = T.pack (gitlabUrl opts), token = T.pack (gitlabToken opts)})
          (mapM_
             (\usrName -> do
                res <- searchUser usrName
                      -- empty list of returned users
                if null res
                  then liftIO $ putStrLn (T.unpack usrName ++ ": no")
                  else liftIO $ putStrLn (T.unpack usrName ++ ": yes"))
             usernames)
      | plagiarism opts &&
          ((mossUserId opts == "") ||
           (mossLang opts == "") ||
           (plagiarismFilePaths opts == "") ||
           (plagiarismProject opts == "") ||
           (parentUser opts == "") || (usersFilename opts == "")) =
        putStrLn
          "--plagiarism needs a MOSS user, MOSS language, filepath list and a parent user"
      | plagiarism opts = do
        usersData <- readFile (usersFilename opts)
        let usernames = T.splitOn "," (T.pack usersData)
        let files = T.splitOn "," (T.pack (plagiarismFilePaths opts))
        runGitLab
          (defaultGitLabServer
           {url = T.pack (gitlabUrl opts), token = T.pack (gitlabToken opts)})
          (do reports <-
                runMoss
                  files
                  usernames
                  (T.pack (parentUser opts))
                  (T.pack (plagiarismProject opts))
                  (T.pack (mossUserId opts))
                  (T.pack (mossLang opts))
              liftIO (putStrLn "MOSS reports:")
              liftIO $ mapM_ T.putStr reports)
      | projectLinks opts && null (project opts) =
        error "--projectLinks must also be used with --project"
      | projectLinks opts = do
        usersData <- readFile (usersFilename opts)
        let usernames = T.splitOn "," (T.pack usersData)
        runGitLab
          (defaultGitLabServer
           {url = T.pack (gitlabUrl opts), token = T.pack (gitlabToken opts)})
          (do mapM_
                (\username -> do
                   projects <-
                     projectsWithNameAndUser username (T.pack (project opts))
                   case projects of
                     Nothing -> return ()
                     Just proj -> do
                       isPassing <- projectCISuccess proj
                       numCommits <- projectCommits proj
                       liftIO $
                         putStrLn
                           (T.unpack (project_web_url proj) <> " (" <>
                            (if isPassing
                              then "passing, "
                              else "failing, ")
                             <> show (length numCommits) <> " commits)"))
                usernames)
      | otherwise = error "combination of flags not recognised"

{- add users -}
  
addUsersToGroupDbg ::
  (MonadIO m, MonadUnliftIO m) => Text -> AccessLevel -> [Text] -> GitLab m ()
addUsersToGroupDbg groupName access usernames = do
  (users :: [User]) <-
    catMaybes <$>
    mapM (\username ->
            do maybeUser <- searchUser username
               case maybeUser of
                 Nothing -> do
                   liftIO $ putStrLn $ T.unpack username <> " not found on server."
                   return Nothing
                 u -> return u
         ) usernames
  liftIO (mapM_ print users)
  (results :: [Either Status Member]) <- do
    liftIO (putStrLn ("group: " <> show groupName))
    addUsersToGroup groupName access users
  liftIO $ mapM_
    (\(result :: Either Status Member) ->
       case result of
         Left errorStatus -> print errorStatus
         Right member ->
           putStrLn
             ("Added to " ++
              show groupName ++ ": " ++ show (member_username member)))
    results

{- plagiarism detection -}

runMoss :: (MonadIO m, MonadUnliftIO m) => [Text] -> [Text] -> Text -> Text -> Text -> Text -> GitLab m [Text]
runMoss filepaths usernames skeletonUser projectName mossUserKey lang =
  mapM
    (\fpath -> do
       liftIO $ putStrLn ("Processing file " ++ T.unpack fpath ++ "\n")
       liftIO $
         putStrLn
           ("getting handed out code " ++
            T.unpack skeletonUser ++ "/" ++ T.unpack projectName ++ "\n")
       skeletonCode <-
         fromJust <$> getCode fpath "master" skeletonUser projectName
       userSubmissions <-
         mapM
           (\usr -> do
              liftIO
                (putStrLn
                   ("getting " ++ T.unpack fpath ++ " from " ++
                    T.unpack usr ++ "/" ++ T.unpack projectName))
              maybe_userCode <- getCode fpath "master" usr projectName
              case maybe_userCode of
                Nothing -> return Nothing
                Just userCode -> return (Just (usr, userCode)))
           usernames
       liftIO (putStrLn "sending job to moss")
       result <- moss fpath skeletonCode (catMaybes userSubmissions) mossUserKey lang
       liftIO $ putStrLn ("File " ++ T.unpack fpath ++ " processed.\n")
       return result
    )
    filepaths

moss :: (MonadIO m, MonadUnliftIO m) => Text -> Text -> [(Text,Text)] -> Text -> Text -> GitLab m Text
moss skeletonFilename skeletonData studentCode mossUserKey lang = do
  let language =
        case T.unpack lang of
          "C" -> C
          "CPP" -> CPP
          "Java" -> Java
          "CSharp" -> CSharp
          "Python" -> Python
          "VisualBasic" -> VisualBasic
          "Javascript" -> Javascript
          "FORTRAN" -> FORTRAN
          "ML" -> ML
          "Haskell" -> Haskell
          "Lisp" -> Lisp
          "Scheme" -> Scheme
          "Pascal" -> Pascal
          "Modula2" -> Modula2
          "Ada" -> Ada
          "Perl" -> Perl
          "TCL" -> TCL
          "Matlab" -> Matlab
          "VHDL" -> VHDL
          "Verilog" -> Verilog
          _ -> error "unrecognised MOSS language name"
  let ext =
        case T.unpack lang of
          "C" -> ".c"
          "CPP" -> ".cpp"
          "Java" -> ".java"
          "CSharp" -> ".cs"
          "Python" -> ".py"
          "VisualBasic" -> ".vb"
          "Javascript" -> ".js"
          "FORTRAN" -> ".f"
          "ML" -> ".ml"
          "Haskell" -> ".hs"
          "Lisp" -> ".lisp"
          "Scheme" -> ".scm"
          "Pascal" -> ".pas"
          "Modula2" -> ".mod"
          "Ada" -> ".ada"
          "Perl" -> ".pl"
          "TCL" -> ".tcl"
          "Matlab" -> ".m"
          "VHDL" -> ".vhdl"
          "Verilog" -> ".v"
          _ -> error "unrecognised MOSS language name"        
  let cfg :: MossCfg
      cfg = defaultMossCfg {
        mossLanguage = language
        , mossUser = C.pack (T.unpack mossUserKey)
        }
  tmpSkelFile <- liftIO $ writeSystemTempFile (takeFileName (T.unpack skeletonFilename)) (T.unpack skeletonData)
  userFiles <- mapM (\(userId,code) -> do
                        userFile <- liftIO $ writeSystemTempFile (T.unpack userId <> ext) (T.unpack code)
                        return (userFile,userId)
                    )
               studentCode
  liftIO $ withMoss cfg $ do
    addBaseFile "Skeleton" tmpSkelFile
    mapM_ (\(userFile,userId) ->
           addFile (T.unpack userId) userFile
          ) userFiles
    res <- fromJust <$> query (C.pack ("moss-" <> (takeFileName (T.unpack skeletonFilename))))

    -- remove temporary files
    liftIO $ removeFile tmpSkelFile
    liftIO $ mapM_ (\(fileName,_) -> removeFile fileName) userFiles
    
    return (T.pack (C.unpack res))

getCode :: (MonadIO m, MonadUnliftIO m) => Text -> Text -> Text -> Text -> GitLab m (Maybe Text)
getCode filepath branch username projectName = do
    maybe_proj <- projectsWithNameAndUser username projectName
    case maybe_proj of
      Nothing -> return Nothing
      Just proj -> do
        maybe_file <- repositoryFiles proj filepath branch
        case maybe_file of
          Nothing -> error "repo file not found"
          Just repoFile -> do
            let blobHash = blob_id repoFile
            contents <- repositoryFileBlob (project_id proj) blobHash
            return (Just (T.pack contents))
