{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Groups
-- Description : Queries about and updates to groups
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Groups where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.API.Members
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI

-- | gets groups with the given group name or path.
--
-- > projectsWithNameOrPath "group1"
groupsWithNameOrPath ::
  (MonadUnliftIO m, MonadIO m) =>
  -- | group name being searched for.
  Text ->
  GitLab m (Either Status [Group])
groupsWithNameOrPath groupName = do
  result <- gitlabWithAttrs "/groups" ("&search=" <> groupName)
  case result of
    Left {} -> return result
    Right groups ->
      return
        ( Right
            $filter
            ( \group ->
                groupName == group_name group
                  || groupName == group_path group
            )
            groups
        )

-- | adds all registered users to a group.
addAllUsersToGroup ::
  (MonadIO m, MonadUnliftIO m) =>
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  GitLab m [Either Status Member]
addAllUsersToGroup groupName access = do
  allRegisteredUsers <- allUsers
  let allUserIds = map user_username allRegisteredUsers
  addUsersToGroup' groupName access allUserIds

-- | adds a user to a group.
addUserToGroup ::
  (MonadIO m, MonadUnliftIO m) =>
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | the user
  User ->
  GitLab m (Either Status Member)
addUserToGroup groupName access usr =
  addUserToGroup' groupName access (user_id usr)

-- | adds a user with a given user ID to a group.
addUserToGroup' ::
  (MonadIO m, MonadUnliftIO m) =>
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | user ID
  Int ->
  GitLab m (Either Status Member)
addUserToGroup' groupName access userId = do
  attempt <- groupsWithNameOrPath groupName
  case attempt of
    Left httpStatus -> return (Left httpStatus)
    Right [] -> return (Left (mkStatus 404 (C.pack "cannot find group")))
    Right [grp] ->
      gitlabPost addr dataBody
      where
        dataBody :: Text
        dataBody =
          "user_id=" <> T.pack (show userId) <> "&access_level="
            <> T.pack (show access)
        addr =
          "/groups/"
            <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (group_id grp)))))
            <> "/members"
    Right (_ : _) ->
      return (Left (mkStatus 404 (C.pack "too many groups found")))

-- | adds a list of users to a group.
addUsersToGroup ::
  (MonadIO m, MonadUnliftIO m) =>
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | list of usernames to be added to the group
  [User] ->
  GitLab m [Either Status Member]
addUsersToGroup groupName access =
  mapM (addUserToGroup groupName access)

-- | adds a list of users to a group.
addUsersToGroup' ::
  (MonadIO m, MonadUnliftIO m) =>
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | list of usernames to be added to the group
  [Text] ->
  GitLab m [Either Status Member]
addUsersToGroup' groupName access usernames = do
  users <- catMaybes <$> mapM searchUser usernames
  mapM (addUserToGroup' groupName access . user_id) users
