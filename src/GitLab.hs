
{-|
Module      : GitLab
Description : Contains the 'runGitLab' function to run GitLab actions
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab
  ( runGitLab
  , module GitLab.Types
  , module GitLab.API.Pipelines
  , module GitLab.API.Groups
  , module GitLab.API.Members
  , module GitLab.API.Commits
  , module GitLab.API.Projects
  , module GitLab.API.Users
  , module GitLab.API.Issues
  , module GitLab.API.Branches
  , module GitLab.API.Jobs
  , module GitLab.API.MergeRequests
  , module GitLab.API.Repositories
  , module GitLab.API.RepositoryFiles
  , module GitLab.API.Todos
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import System.IO

import GitLab.API.Branches
import GitLab.API.Commits
import GitLab.API.Groups
import GitLab.API.Issues
import GitLab.API.Jobs
import GitLab.API.Members
import GitLab.API.MergeRequests
import GitLab.API.Pipelines
import GitLab.API.Projects
import GitLab.API.Repositories
import GitLab.API.RepositoryFiles
import GitLab.API.Todos
import GitLab.API.Users
import GitLab.Types

-- | runs a GitLab action.
--
-- Internally, this creates a single 'Manager', whichs keeps track of
-- open connections for keep-alive and which is shared between
-- multiple threads and requests.
--
-- An example of its use is:
--
-- > projectsWithIssuesEnabled :: IO [Project]
-- > projectsWithIssuesEnabled =
-- >   runGitLab myConfig $ filter (issueEnabled . issues_enabled) <$> allProjects
-- >   where
-- >     myConfig = defaultGitLabServer
-- >         { url = "https://gitlab.example.com"
-- >         , token = "my_access_token" }
-- >     issueEnabled Nothing = False
-- >     issueEnabled (Just b) = b
runGitLab ::
  (MonadUnliftIO m, MonadIO m) => GitLabServerConfig -> GitLab m a -> m a
runGitLab cfg action = do
  liftIO $ hSetBuffering stdout LineBuffering
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- liftIO $ newManager settings
  runReaderT action (GitLabState cfg manager)
