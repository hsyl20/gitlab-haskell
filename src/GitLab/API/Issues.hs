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
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy

-- | returns all issues against a project.
projectOpenedIssues ::
  -- | the project
  Project ->
  GitLab [Issue]
projectOpenedIssues p = do
  result <- projectOpenedIssues' (project_id p)
  return (fromRight (error "projectOpenedIssues error") result)

-- | returns all issues against a project given its project ID.
projectOpenedIssues' ::
  -- | the project ID
  Int ->
  GitLab (Either Status [Issue])
projectOpenedIssues' projectId = do
  let path = "/projects/" <> T.pack (show projectId) <> "/issues"
  gitlab path

-- gitlabReq path "&state=opened"

-- | gets all issues create by a user.
userIssues ::
  -- | the user
  User ->
  GitLab [Issue]
userIssues usr =
  gitlabWithAttrsUnsafe addr attrs
  where
    addr = "/issues"
    attrs =
      T.pack $
        "&author_id="
          <> show (user_id usr)
          <> "&scope=all"

-- | edits an issue. see <https://docs.gitlab.com/ee/api/issues.html#edit-issue>
editIssue ::
  (MonadIO m) =>
  ProjectId ->
  IssueId ->
  EditIssueReq ->
  GitLab m (Either Status Issue)
editIssue projId issueId editIssueReq = do
  let path = "/projects/" <> T.pack (show projId)
             <> "/issues/" <> T.pack (show issueId)
  gitlabPut path
    (Data.Text.Lazy.toStrict
       (Data.Text.Lazy.Encoding.decodeUtf8
          (J.encode editIssueReq)))
