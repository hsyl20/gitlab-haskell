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

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import qualified Data.ByteString.Char8 as C

import GitLab.API.Members
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | gets groups with the given group name or path.
--
-- > projectsWithNameOrPath "group1"
groupsWithNameOrPath :: (MonadUnliftIO m, MonadIO m)
  => Text -- ^ group name being searched for.
  -> GitLab m [Group]
groupsWithNameOrPath groupName =
  filter (\group ->
            groupName == group_name group
            || groupName == group_path group
         ) <$>
  gitlabWithAttrs "/groups" ("&search=" <> groupName)

-- | adds all registered users to a group.
addAllUsersToGroup ::
     (MonadIO m, MonadUnliftIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> GitLab m [Either Status Member]
addAllUsersToGroup groupName access = do
  allRegisteredUsers <- allUsers
  let allUserIds = map user_username allRegisteredUsers
  addUsersToGroup' groupName access allUserIds

-- | adds a user to a group.
addUserToGroup ::
     (MonadIO m, MonadUnliftIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> User -- ^ the user
  -> GitLab m (Either Status Member)
addUserToGroup groupName access usr =
  addUserToGroup' groupName access (user_id usr)

-- | adds a user with a given user ID to a group.
addUserToGroup' ::
     (MonadIO m, MonadUnliftIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> Int -- ^ user ID
  -> GitLab m (Either Status Member)
addUserToGroup' groupName access userId = do
  groups <- groupsWithNameOrPath groupName
  case groups of
    [] -> return (Left (mkStatus 404 (C.pack "cannot find group")))
    [grp] ->
      gitlabPost addr dataBody
      where
        dataBody :: Text
        dataBody =
          "user_id=" <> T.pack (show userId) <> "&access_level=" <>
          T.pack (show access)
        addr = "/groups/"
               <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (group_id grp)))))
               <> "/members"
    (_:_) ->
      return (Left (mkStatus 404 (C.pack "too many groups found")))

-- | adds a list of users to a group.
addUsersToGroup ::
     (MonadIO m, MonadUnliftIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> [User] -- ^ list of usernames to be added to the group
  -> GitLab m [Either Status Member]
addUsersToGroup groupName access =
  mapM (addUserToGroup groupName access)

-- | adds a list of users to a group.
addUsersToGroup' ::
     (MonadIO m, MonadUnliftIO m)
  => Text -- ^ group name
  -> AccessLevel -- ^ level of access granted
  -> [Text] -- ^ list of usernames to be added to the group
  -> GitLab m [Either Status Member]
addUsersToGroup' groupName access usernames = do
  users <- catMaybes <$> mapM searchUser usernames
  mapM (addUserToGroup' groupName access . user_id) users
