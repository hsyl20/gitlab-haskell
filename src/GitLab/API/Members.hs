{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Members
Description : Queries about and updates to members of projects
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Members where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Status

import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | the access levels for project members. See <https://docs.gitlab.com/ee/user/permissions.html#project-members-permissions>
data AccessLevel =
  Guest
  | Reporter
  | Developer
  | Maintainer
  | Owner

instance Show AccessLevel where
  show Guest = "10"
  show Reporter = "20"
  show Developer = "30"
  show Maintainer = "40"
  show Owner = "50"

-- | the members of a project.
membersOfProject :: (MonadIO m) => Project -> GitLab m [Member]
membersOfProject  = membersOfProject' . project_id

-- | the members of a project given its ID.
membersOfProject' :: (MonadIO m) => Int -> GitLab m [Member]
membersOfProject' projectId =
  gitlab addr
  where
    addr =
      "/projects/" <> T.pack (show projectId) <> "/members"

-- | adds a user to a project with the given access level. Returns
-- 'Right Member' for each successful action, otherwise it returns
-- 'Left Status'.
addMemberToProject ::
     (MonadIO m)
     => Project -- ^ the project
     -> AccessLevel -- ^ level of access
     -> User -- ^ the user
     -> GitLab m (Either Status Member)
addMemberToProject project access usr =
  addMemberToProject' (project_id project) access (user_id usr)

-- | adds a user to a project with the given access level, given the
-- project's ID and the user's ID. Returns @Right Member@ for each
-- successful action, otherwise it returns @Left Status@.
addMemberToProject' ::
     (MonadIO m)
     => Int -- ^ project ID
     -> AccessLevel -- ^ level of access
     -> Int -- ^ user ID
     -> GitLab m (Either Status Member)
addMemberToProject' projectId access userId =
  gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
        "user_id=" <> T.pack (show userId) <> "&access_level=" <> T.pack (show access)
    addr =
      "/projects/" <> T.pack (show projectId) <> "/members"


-- | adds a list of users to a project with the given access
-- level. Returns 'Right Member' for each successful action, otherwise
-- it returns 'Left Status'.
addMembersToProject ::
     (MonadIO m)
  => Project -- ^ the project
  -> AccessLevel -- ^ level of access
  -> [User] -- ^ users to add to the project
  -> GitLab m [Either Status Member]
addMembersToProject project access =
  mapM (addMemberToProject project access)

-- | adds a list of users to a project with the given access level,
-- given the project's ID and the user IDs. Returns @Right Member@ for
-- each successful action, otherwise it returns @Left Status@.
addMembersToProject' ::
     (MonadIO m)
     => Int -- ^ project ID
     -> AccessLevel -- ^ level of acces
     -> [Int] -- ^ IDs of users to add to the project
     -> GitLab m [Either Status Member]
addMembersToProject' projectId access =
  mapM (addMemberToProject' projectId access)

