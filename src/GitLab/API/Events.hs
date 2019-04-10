
{-
 This module is not currently being used.
-}

module GitLab.API.Events () where

import Control.Monad.IO.Unlift
import qualified Data.Text as T

import GitLab.Types

-- | Get a list of events.
issueEvents :: (MonadIO m) => GitLab m [Issue]
issueEvents = undefined
  -- gitlabWithAttrs addr attrs
  -- where
  --   addr =
  --     "/events"
  --   attrs =
  --     "&target_type=issue"
