{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Issues
-- Description : Queries about issues created against projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Issues where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Either
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | returns all issues against a project.
projectOpenedIssues ::
  (MonadIO m, MonadUnliftIO m) =>
  -- | the project
  Project ->
  GitLab m [Issue]
projectOpenedIssues p = do
  result <- projectOpenedIssues' (project_id p)
  return (fromRight (error "projectOpenedIssues error") result)

-- | returns all issues against a project given its project ID.
projectOpenedIssues' ::
  (MonadIO m, MonadUnliftIO m) =>
  -- | the project ID
  Int ->
  GitLab m (Either Status [Issue])
projectOpenedIssues' projectId = do
  let path = "/projects/" <> T.pack (show projectId) <> "/issues"
  gitlab path

-- gitlabReq path "&state=opened"

-- | gets all issues create by a user.
userIssues ::
  (MonadIO m) =>
  -- | the user
  User ->
  GitLab m [Issue]
userIssues usr =
  gitlabWithAttrsUnsafe addr attrs
  where
    addr = "/issues"
    attrs =
      T.pack $
        "&author_id="
          <> show (user_id usr)
          <> "&scope=all"
