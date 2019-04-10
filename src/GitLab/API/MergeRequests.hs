{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : MergeRequests
Description : Queries about merge requests against projects
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.MergeRequests where

import Control.Monad.IO.Unlift
import qualified Data.Text as T

import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | returns the merge requests for a project.
mergeRequests :: (MonadIO m)
  => Project -- ^ the project
  -> GitLab m [MergeRequest]
mergeRequests = mergeRequests' . project_id

-- | returns the merge requests for a project given its project ID.
mergeRequests' :: (MonadIO m)
  => Int -- ^ project ID
  -> GitLab m [MergeRequest]
mergeRequests' projectId =
  gitlabWithAttrs addr "&scope=all"
  where
    addr =
      "/projects/"
      <> T.pack (show projectId)
      <> "/merge_requests"
