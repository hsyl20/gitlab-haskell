{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Commits
-- Description : Queries about commits in repositories
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Commits where

import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | returns all commits for a project.
projectCommits ::
  -- | the project
  Project ->
  GitLab [Commit]
projectCommits project = do
  result <- projectCommits' (project_id project)
  return (fromRight (error "projectCommits error") result)

-- | returns all commits for a project given its project ID.
projectCommits' ::
  -- | project ID
  Int ->
  GitLab (Either Status [Commit])
projectCommits' projectId =
  gitlabWithAttrs (commitsAddr projectId) "&with_stats=true"
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/" <> T.pack (show projId) <> "/repository" <> "/commits"

-- | returns a commit for the given project and commit hash, if such
-- a commit exists.
commitDetails ::
  -- | the project
  Project ->
  -- | the commit hash
  Text ->
  GitLab (Maybe Commit)
commitDetails project theHash = do
  result <- commitDetails' (project_id project) theHash
  return (fromRight (error "commitDetails error") result)

-- | returns a commit for the given project ID and commit hash, if
-- such a commit exists.
commitDetails' ::
  -- | project ID
  Int ->
  -- | the commit hash
  Text ->
  GitLab (Either Status (Maybe Commit))
commitDetails' projectId hash =
  gitlabOne (commitsAddr projectId)
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits"
        <> "/"
        <> hash
