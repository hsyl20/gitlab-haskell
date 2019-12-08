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
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Status

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

-- | Creates a merge request.
createMergeRequest
  :: (MonadIO m)
  => Project -- ^ project
  -> Text -- ^ source branch
  -> Text -- ^ target branch
  -> Int -- ^ target project ID
  -> Text -- ^ merge request title
  -> Text -- ^ merge request description
  -> GitLab m  (Either Status MergeRequest)
createMergeRequest project =
  createMergeRequest' (project_id project)
  
  -- | Creates a merge request.
createMergeRequest'
  :: (MonadIO m)
  => Int -- ^ project ID
  -> Text -- ^ source branch
  -> Text -- ^ target branch
  -> Int -- ^ target project ID
  -> Text -- ^ merge request title
  -> Text -- ^ merge request description
  -> GitLab m  (Either Status MergeRequest)
createMergeRequest' projectId sourceBranch targetBranch targetProjectId mrTitle mrDescription =
  gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
      "source_branch=" <> sourceBranch <> "&target_branch=" <> targetBranch <>
      "&target_project_id=" <>
      T.pack (show targetProjectId) <>
      "&title=" <>
      mrTitle <>
      "&description=" <>
      mrDescription
    addr = T.pack $ "/projects/" <> show projectId <> "/merge_requests"
