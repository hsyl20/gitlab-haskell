{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Pipelines
Description : Queries about project pipelines
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Pipelines where

import Control.Monad.IO.Unlift
import qualified Data.Text as T

import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | returns the pipelines for a project.
pipelines :: (MonadIO m)
  => Project -- ^ the project
  -> GitLab m [Pipeline]
pipelines = pipelines' . project_id

-- | returns the pipelines for a project given its project ID.
pipelines' :: (MonadIO m)
  => Int -- ^ the project ID
  -> GitLab m [Pipeline]
pipelines' projectId =
  gitlabWithAttrs
  addr
  "&sort=desc" -- most recent first
  where
    addr =
      "/projects/"
      <> T.pack (show projectId)
      <> "/pipelines"
