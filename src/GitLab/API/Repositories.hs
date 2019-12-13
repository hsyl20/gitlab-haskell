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
import qualified Data.ByteString.Lazy as BSL
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

-- | get a file archive of the repository files. For example:
--
-- > getFileArchive myProject TarGz "/tmp/myProject.tar.gz"
getFileArchive :: (MonadIO m)
  => Project -- ^ project
  -> ArchiveFormat -- ^ file format
  -> FilePath -- ^ file path to store the archive
  -> GitLab m ()
getFileArchive project = getFileArchive' (project_id project)

-- | get a file archive of the repository files using the project's
--   ID. For example:
--
-- > getFileArchive' 3453 Zip "/tmp/myProject.zip"
getFileArchive' :: (MonadIO m)
  => Int -- ^ project ID
  -> ArchiveFormat -- ^ file format
  -> FilePath -- ^ file path to store the archive
  -> GitLab m ()
getFileArchive' projectId format path = do
  archiveData <- gitlabReqByteString addr
  liftIO $ BSL.writeFile path archiveData
  where
    addr =
      "/projects/"
      <> T.pack (show projectId)
      <> "/repository"
      <> "/archive"
      <> T.pack (show format)
  
