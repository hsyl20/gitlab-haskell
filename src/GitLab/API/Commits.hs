{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Commits
Description : Queries about commits in repositories
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Commits where

import Control.Monad.IO.Unlift
import Data.Text (Text)
import qualified Data.Text as T

import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | returns all commits for a project.
projectCommits :: (MonadIO m)
  => Project -- ^ the project
  -> GitLab m [Commit]
projectCommits project = projectCommits' (project_id project)

-- | returns all commits for a project given its project ID.
projectCommits' :: (MonadIO m)
  => Int -- ^ project ID
  -> GitLab m [Commit]
projectCommits' projectId =
  gitlab (commitsAddr projectId)
  where
    commitsAddr :: Int -> Text
    commitsAddr projectId =
      "/projects/" <> T.pack (show projectId) <> "/repository" <> "/commits"

-- | returns a commit for the given project and commit hash, if such
-- a commit exists.
commitDetails :: (MonadIO m)
  => Project -- ^ the project
  -> Text    -- ^ the commit hash
  -> GitLab m (Maybe Commit)
commitDetails project = commitDetails' (project_id project)

-- | returns a commit for the given project ID and commit hash, if
-- such a commit exists.
commitDetails' :: (MonadIO m)
  => Int  -- ^ project ID
  -> Text -- ^ the commit hash
  -> GitLab m (Maybe Commit)
commitDetails' projectId hash =
  gitlabOne (commitsAddr projectId)
  where
    commitsAddr :: Int -> Text
    commitsAddr projectId =
      "/projects/"
      <> T.pack (show projectId)
      <> "/repository"
      <> "/commits"
      <> "/" <> hash
