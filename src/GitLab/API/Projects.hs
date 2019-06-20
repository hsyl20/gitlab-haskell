{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Projects
Description : Queries about projects
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Projects where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.URI
import UnliftIO.Async

import GitLab.API.Commits
import GitLab.API.Issues
import GitLab.API.Pipelines
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | gets all projects.
allProjects :: (MonadIO m) => GitLab m [Project]
allProjects =
  gitlabWithAttrs "/projects" "&statistics=true"

-- | gets all forks of a project. Supports use of namespaces.
--
-- > projectForks "project1"
-- > projectForks "group1/project1"
projectForks :: (MonadUnliftIO m, MonadIO m)
  => Text -- ^ name or namespace of the project
  -> GitLab m [Project]
projectForks projectName = do
  let urlPath =
        "/projects/" <>
        T.decodeUtf8 (urlEncode False (T.encodeUtf8 projectName)) <>
        "/forks"
  gitlab urlPath

-- | searches for a 'Project' with the given project ID, returns
-- 'Nothing' if a project with the given ID is not found.
searchProjectId :: (MonadIO m)
  => Int -- ^ project ID
  -> GitLab m (Maybe Project)
searchProjectId projectId = do
  let urlPath = T.pack ("/projects/" <> show projectId)
  gitlabWithAttrsOne urlPath  "&statistics=true"

-- | gets all projects with the given project name.
--
-- > projectsWithName "project1"
projectsWithName :: (MonadUnliftIO m, MonadIO m)
  => Text -- ^ project name being searched for.
  -> GitLab m [Project]
projectsWithName projectName =
  filter (\project -> projectName == project_path project) <$>
  gitlabWithAttrs "/projects" ("&search=" <> projectName)

-- | gets a project with the given name for the given username. E.g. 
--
-- > projectsNamespaceName "user1" "project1"
--
-- looks for "user1/project1"
projectsWithNameAndUser ::
     (MonadUnliftIO m, MonadIO m) => Text -> Text -> GitLab m (Maybe Project)
projectsWithNameAndUser username projectName =
  -- gitlabOne
  -- ("/projects/" <>
  --  T.decodeUtf8
  --     (urlEncode False (T.encodeUtf8 (username <> "/" <> projectName))))
  gitlabWithAttrsOne
  ("/projects/" <>
   T.decodeUtf8
      (urlEncode False (T.encodeUtf8 (username <> "/" <> projectName))))
  "&statistics=true"

-- | returns 'True' if a project has multiple committers, according to
-- the email addresses of the commits.
multipleCommitters :: (MonadUnliftIO m, MonadIO m) => Project -> GitLab m Bool
multipleCommitters project = do
  emailAddresses <- commitsEmailAddresses project
  return (length (nub emailAddresses) > 1)

-- | gets the email addresses in the author information in all commit
-- for a project. 
commitsEmailAddresses ::
     (MonadUnliftIO m, MonadIO m) => Project -> GitLab m [Text]
commitsEmailAddresses = commitsEmailAddresses' . project_id

-- | gets the email addresses in the author information in all commit
-- for a project defined by the project's ID. 
commitsEmailAddresses' :: (MonadUnliftIO m, MonadIO m) => Int -> GitLab m [Text]
commitsEmailAddresses' projectId = do
  (commits :: [Commit]) <- projectCommits' projectId
  return (map author_email commits)

-- | gets all projects for a user's username.
--
-- > userProjects "harry"
userProjects' ::
     (MonadUnliftIO m, MonadIO m) => Text -> GitLab m (Maybe [Project])
userProjects' username = do
  userMaybe <- searchUser username
  case userMaybe of
    Nothing -> return Nothing
    Just usr -> Just <$> gitlab (urlPath (user_id usr))
  where
    urlPath userId = "/users/" <> T.pack (show userId) <> "/projects"

userProjects ::
     (MonadUnliftIO m, MonadIO m) => User -> GitLab m (Maybe [Project])
userProjects theUser = userProjects' (user_username theUser)

-- | gets the 'GitLab.Types.Project' against which the given 'Issue'
-- was created.
projectOfIssue :: (MonadIO m) => Issue -> GitLab m Project
projectOfIssue issue =
  fromJust <$> searchProjectId (issue_project_id issue)

-- | finds all issues created by a user.
--
-- > issuesCreatedByUser "user1"
--
-- returns a (user,projects) tuple, where 'user' is the 'User' found
-- for the given searched username, and a list of 'Project's that the
-- user has created issues for.
issuesCreatedByUser :: (MonadUnliftIO m, MonadIO m) => Text -> GitLab m (Maybe (User,[Project]))
issuesCreatedByUser username = do
  user_maybe <- searchUser username
  case user_maybe of
    Nothing -> return Nothing
    Just usr -> do
      usersIssues <- userIssues usr
      projects <- mapConcurrently projectOfIssue usersIssues
      return (Just (usr, projects))

-- | searches for all projects with the given name, and returns a list
-- of triples of: 1) the found project, 2) the list of issues for the
-- found projects, and 3) a list of users who've created issues.
issuesOnForks ::
     (MonadUnliftIO m, MonadIO m)
  => Text -- ^ name or namespace of the project
  -> GitLab m [(Project, [Issue], [User])]
issuesOnForks projectName = do
  projects <- projectsWithName projectName
  mapM processProject projects
  where
    processProject ::
         (MonadUnliftIO m, MonadIO m)
      => Project
      -> GitLab m (Project, [Issue], [User])
    processProject proj = do
      (openIssues :: [Issue]) <- projectOpenedIssues proj
      let authors = map issue_author openIssues
      return (proj, openIssues, authors)

-- | returns a (namespace,members) tuple for the given 'Project',
-- where namespace is the namespace of the project
-- e.g. "user1/project1", and members is a list of (username,name)
-- tuples about all members of the project.
projectMemebersCount :: (MonadIO m) => Project -> GitLab m (Text,[(Text,Text)])
projectMemebersCount project = do
  friends <- count
  return (namespace_name (namespace project), friends)
  where
    count = do
      let addr =
            "/projects/" <> T.pack (show (project_id project)) <> "/members/all"
      (res :: [Member]) <- gitlab addr
      return (map (\x -> (member_username x, member_name x)) res)

-- | returns 'True' is the last commit for a project passes all
-- continuous integration tests.
projectCISuccess ::
     (MonadIO m)
  => Project -- ^ the name or namespace of the project
  -> GitLab m Bool
projectCISuccess project = do
  pipes <- pipelines project
  case pipes of
    [] -> return False
    (x:_) -> return (pipeline_status x == "success")

-- | searches for a username, and returns a user ID for that user, or
-- 'Nothing' if a user cannot be found. 
namespacePathToUserId :: (MonadIO m)
  => Text -- ^ name or namespace of project 
  -> GitLab m (Maybe Int)
namespacePathToUserId namespacePath = do
  user_maybe <- searchUser namespacePath
  case user_maybe of
    Nothing -> return Nothing
    Just usr -> return (Just (user_id usr))
