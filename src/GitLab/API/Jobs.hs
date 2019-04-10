{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Jobs
Description : Queries about jobs ran on projects
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Jobs where

import Control.Monad.IO.Unlift
import qualified Data.Text as T

import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | returns all jobs ran on a project.
jobs ::
     (MonadIO m)
  => Project -- ^ the project
  -> GitLab m [Job]
jobs project = jobs' (project_id project)

-- | returns all jobs ran on a project given its project ID.
jobs' ::
     (MonadIO m)
  => Int -- ^ the project ID
  -> GitLab m [Job]
jobs' projectId =
  gitlab addr
  where
    addr =
      "/projects/"
      <> T.pack (show projectId)
      <> "/jobs"
