{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : RepositoryFiles
Description : Queries about project repository files
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.RepositoryFiles where

import Control.Monad.IO.Unlift
import Data.Text (Text)
import qualified Data.Text as T

import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | Get a list of repository files and directories in a project.
repositoryFiles :: (MonadIO m)
  => Project -- ^ the project
  -> Text -- ^ the file path
  -> Text -- ^ name of the branch, tag or commit
  -> GitLab m (Maybe RepositoryFile)
repositoryFiles project = repositoryFiles' (project_id project)

-- | Get a list of repository files and directories in a project given
-- the project's ID.
repositoryFiles' :: (MonadIO m)
  => Int -- ^ project ID
  -> Text -- ^ the file path
  -> Text -- ^ name of the branch, tag or commit
  -> GitLab m (Maybe RepositoryFile)
repositoryFiles' projectId filePath reference =
  gitlabWithAttrsOne addr ("&ref=" <> reference)
  where
    addr =
      "/projects/"
      <> T.pack (show projectId)
      <> "/repository"
      <> "/files"
      <> "/"
      <> filePath
