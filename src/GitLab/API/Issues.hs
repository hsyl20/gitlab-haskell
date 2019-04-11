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

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.Text as T

import GitLab.WebRequests.GitLabWebCalls
import GitLab.Types

-- | returns all issues against a project.
projectOpenedIssues ::
     (MonadIO m, MonadUnliftIO m)
  => Project -- ^ the project
  -> GitLab m [Issue]
projectOpenedIssues = projectOpenedIssues' . project_id

-- | returns all issues against a project given its project ID.
projectOpenedIssues' ::
     (MonadIO m, MonadUnliftIO m)
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
userIssues usr =
  gitlabWithAttrs addr attrs
  where
    addr = "/issues"
    attrs = T.pack $
      "&author_id="
      <> show (user_id usr)
      <> "&scope=all"
