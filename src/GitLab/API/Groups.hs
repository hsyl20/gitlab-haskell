{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Groups
Description : Queries about and updates to groups
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Groups where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI

import GitLab.API.Members
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | adds all registered users to a group.
addAllUsersToGroup ::
     (MonadIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> GitLab m [Either Status Member]
addAllUsersToGroup groupName access = do
  allRegisteredUsers <- allUsers
  let allUserIds = map user_username allRegisteredUsers
  addUsersToGroup groupName access allUserIds

-- | adds a user to a group.
addUserToGroup ::
     (MonadIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> User -- ^ the user
  -> GitLab m (Either Status Member)
addUserToGroup groupName access user = addUserToGroup' groupName access (user_id user)

-- | adds a list of users to a group.
addUsersToGroup ::
     (MonadIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> [Text] -- ^ list of usernames to be added to the group
  -> GitLab m [Either Status Member]
addUsersToGroup groupName access usernames = do
  users <- catMaybes <$> mapM searchUser usernames
  mapM (addUserToGroup' groupName access . user_id) users

-- | adds a user with a given user ID to a group.
addUserToGroup' ::
     (MonadIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> Int -- ^ user ID
  -> GitLab m (Either Status Member)
addUserToGroup' groupName access userId = gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
      "user_id=" <> T.pack (show userId) <> "&access_level=" <>
      T.pack (show access)
    addr = "/groups/" <> groupName <> "/members"

