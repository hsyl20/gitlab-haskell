{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Repositories
Description : Queries about project repositories
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Repositories where

import Control.Monad.IO.Unlift
import qualified Data.Text as T

import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | returns a list of repository files and directories in a project.
repositories :: (MonadIO m)
  => Project -- ^ the project
  -> GitLab m [Repository]
repositories project = repositories' (project_id project)

-- | returns a list of repository files and directories in a project
-- given its project ID.
repositories' :: (MonadIO m)
  => Int -- ^ the project ID
  -> GitLab m [Repository]
repositories' projectId =
  gitlab addr
  where
    addr =
      "/projects/"
      <> T.pack (show projectId)
      <> "/repository"
      <> "/tree"
