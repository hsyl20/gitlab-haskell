{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jobs
-- Description : Queries about jobs ran on projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Jobs where

import Control.Monad.IO.Unlift
import Data.Either
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | returns all jobs ran on a project.
jobs ::
  (MonadIO m) =>
  -- | the project
  Project ->
  GitLab m [Job]
jobs project = do
  result <- jobs' (project_id project)
  return (fromRight (error "jobs error") result)

-- | returns all jobs ran on a project given its project ID.
jobs' ::
  (MonadIO m) =>
  -- | the project ID
  Int ->
  GitLab m (Either Status [Job])
jobs' projectId =
  gitlab addr
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/jobs"
