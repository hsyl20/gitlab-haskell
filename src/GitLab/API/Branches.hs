{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Branches
Description : Queries about repository branches
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Branches where

import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Control.Monad.IO.Unlift


-- | Get a list of repository branches from a project, sorted by name
-- alphabetically.
branches :: (MonadIO m) => Project -> GitLab m [Branch]
branches project = branches' (project_id project)

-- | Get a list of repository branches from a project given its
-- project ID, sorted by name alphabetically.
branches' :: (MonadIO m) => Int -> GitLab m [Branch]
branches' projectId =
  gitlab addr
  where
    addr =
      "/projects/"
      <> T.pack (show projectId)
      <> "/repository"
      <> "/branches"

