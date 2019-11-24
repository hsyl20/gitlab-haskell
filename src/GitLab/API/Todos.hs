{-# LANGUAGE OverloadedStrings #-}

module GitLab.API.Todos where

import Control.Monad.IO.Class
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

todos :: (MonadIO m) => GitLab m [Todo]
todos = gitlab "/todos"
