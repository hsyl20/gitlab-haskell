{-# LANGUAGE OverloadedStrings #-}

module GitLab.API.Todos where

import Control.Monad.IO.Class
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | returns all pending todos for the user, as defined by the access token.
todos :: (MonadIO m) => GitLab m [Todo]
todos = gitlab "/todos"
