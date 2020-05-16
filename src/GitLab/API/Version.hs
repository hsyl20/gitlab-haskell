{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Version
-- Description : Queries about GitLab instance version
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Version where

import Control.Monad.IO.Class
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

gitlabVersion :: GitLab (Either Status (Maybe Version))
gitlabVersion = do
  let path = "/version"
  gitlabOne path
