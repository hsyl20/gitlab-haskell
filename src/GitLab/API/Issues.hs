{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Issues
Description : Queries about issues created against projects
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Issues where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Data.Array.IO
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Network.HTTP.Types.Status
import System.IO
import System.Random
import UnliftIO.Async

import GitLab.WebRequests.GitLabWebCalls
import GitLab.Types

-- | returns all issues against a project.
projectOpenedIssues ::
     (MonadIO m)
  => Project -- ^ the project
  -> GitLab m [Issue]
projectOpenedIssues = projectOpenedIssues' . project_id

-- | returns all issues against a project given its project ID.
projectOpenedIssues' ::
     (MonadIO m)
  => Int -- ^ the project ID
  -> GitLab m [Issue]
projectOpenedIssues' projectId = do
    let path = "/projects/" <> T.pack (show projectId) <> "/issues"
    gitlab path
    -- gitlabReq path "&state=opened"

-- | gets all issues create by a user.
userIssues ::
     (MonadIO m)
  => User -- ^ the user
  -> GitLab m [Issue]
userIssues user =
  gitlabWithAttrs addr attrs
  where
    addr = "/issues"
    attrs = T.pack $
      "&author_id="
      <> show (user_id user)
      <> "&scope=all"
