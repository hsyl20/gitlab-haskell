{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : GitLab.SystemHooks.Types
-- Description : Haskell records corresponding to JSON data from GitLab system hook events
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.SystemHooks.Types
  ( Rule (..),
    SystemHook (..),
    ProjectCreate (..),
    ProjectDestroy (..),
    ProjectRename (..),
    ProjectTransfer (..),
    ProjectUpdate (..),
    ProjectChanges (..),
    UserAddToTeam (..),
    UserUpdateForTeam (..),
    UserRemoveFromTeam (..),
    UserCreate (..),
    UserRemove (..),
    UserFailedLogin (..),
    UserRename (..),
    KeyCreate (..),
    KeyRemove (..),
    GroupCreate (..),
    GroupRemove (..),
    GroupRename (..),
    NewGroupMember (..),
    GroupMemberRemove (..),
    GroupMemberUpdate (..),
    Push (..),
    TagPush (..),
    ProjectEvent (..),
    RepositoryEvent (..),
    RepositoryUpdate (..),
    CommitEvent (..),
    CommitAuthorEvent (..),
    Visibility (..),
    MergeRequestEvent (..),
    MergeRequestChanges (..),
    MergeRequestChange (..),
    ObjectAttributes (..),
    MergeParams (..),
    UserEvent (..),
    parseEvent,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import GitLab.Types

-- | Pattern matching rules on GitLab hook events.
data Rule where
  Match :: (Typeable a, SystemHook a) => String -> (a -> GitLab ()) -> Rule
  MatchIf :: (Typeable a, SystemHook a) => String -> (a -> GitLab Bool) -> (a -> GitLab ()) -> Rule

-- | A typeclass for GitLab hook events.
class (FromJSON a) => SystemHook a where
  match :: String -> (a -> GitLab ()) -> Rule
  matchIf :: String -> (a -> GitLab Bool) -> (a -> GitLab ()) -> Rule

-- | Parse JSON data into GitLab events.
parseEvent :: (FromJSON a) => String -> Maybe a
parseEvent string =
  case eitherDecode (BSL.pack string) of
    Left _error -> Nothing
    Right event -> Just event

instance SystemHook ProjectCreate where
  match = Match
  matchIf = MatchIf

-- | GitLab project creation.
data ProjectCreate = ProjectCreate
  { projectCreate_created_at :: Text,
    projectCreate_updated_at :: Text,
    projectCreate_action :: Text,
    projectCreate_name :: Text,
    projectCreate_owner_email :: Text,
    projectCreate_owner_name :: Text,
    projectCreate_path :: Text,
    projectCreate_path_with_namespace :: Text,
    projectCreate_project_id :: Int,
    projectCreate_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook ProjectDestroy where
  match = Match
  matchIf = MatchIf

-- | Removal of a GitLab removal.
data ProjectDestroy = ProjectDestroy
  { projectDestroy_created_at :: Text,
    projectDestroy_updated_at :: Text,
    projectDestroy_action :: Text,
    projectDestroy_name :: Text,
    projectDestroy_owner_email :: Text,
    projectDestroy_owner_name :: Text,
    projectDestroy_path :: Text,
    projectDestroy_path_with_namespace :: Text,
    projectDestroy_project_id :: Int,
    projectDestroy_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook ProjectRename where
  match = Match
  matchIf = MatchIf

-- | Renaming of a GitLab project.
data ProjectRename = ProjectRename
  { projectRename_created_at :: Text,
    projectRename_updated_at :: Text,
    projectRename_event_name :: Text,
    projectRename_name :: Text,
    projectRename_path :: Text,
    projectRename_path_with_namespace :: Text,
    projectRename_project_id :: Int,
    projectRename_owner_name :: Text,
    projectRename_owner_email :: Text,
    projectRename_project_visibility :: Visibility,
    projectRename_old_path_with_namespace :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook ProjectTransfer where
  match = Match
  matchIf = MatchIf

-- | A project has been transferred.
data ProjectTransfer = ProjectTransfer
  { projectTransfer_created_at :: Text,
    projectTransfer_updated_at :: Text,
    projectTransfer_event_name :: Text,
    projectTransfer_name :: Text,
    projectTransfer_path :: Text,
    projectTransfer_path_with_namespace :: Text,
    projectTransfer_project_id :: Int,
    projectTransfer_owner_name :: Text,
    projectTransfer_owner_email :: Text,
    projectTransfer_project_visibility :: Visibility,
    projectTransfer_old_path_with_namespace :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook ProjectUpdate where
  match = Match
  matchIf = MatchIf

-- | A project has been updated.
data ProjectUpdate = ProjectUpdate
  { projectUpdate_created_at :: Text,
    projectUpdate_updated_at :: Text,
    projectUpdate_event_name :: Text,
    projectUpdate_name :: Text,
    projectUpdate_owner_email :: Text,
    projectUpdate_owner_name :: Text,
    projectUpdate_path :: Text,
    projectUpdate_path_with_namespace :: Text,
    projectUpdate_project_id :: Int,
    projectUpdate_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserAddToTeam where
  match = Match
  matchIf = MatchIf

-- | A user has been added to a team.
data UserAddToTeam = UserAddToTeam
  { userAddTeam_created_at :: Text, -- todo improve: date
    userAddTeam_updated_at :: Text, -- todo improve: date
    userAddTeam_event_name :: Text,
    userAddTeam_access_level :: Text, -- todo improve: Maintainer/...
    userAddTeam_project_id :: Int,
    userAddTeam_project_name :: Text,
    userAddTeam_project_path :: Text,
    userAddTeam_project_path_with_namespace :: Text,
    userAddTeam_user_email :: Text,
    userAddTeam_user_name :: Text,
    userAddTeam_user_username :: Text,
    userAddTeam_user_id :: Int,
    userAddTeam_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserUpdateForTeam where
  match = Match
  matchIf = MatchIf

-- | A user in a team has been updated.
data UserUpdateForTeam = UserUpdateForTeam
  { userUpdateTeam_created_at :: Text, -- todo improve: date
    userUpdateTeam_updated_at :: Text, -- todo improve: date
    userUpdateTeam_event_name :: Text,
    userUpdateTeam_access_level :: Text, -- todo improve: Maintainer/...
    userUpdateTeam_project_id :: Int,
    userUpdateTeam_project_name :: Text,
    userUpdateTeam_project_path :: Text,
    userUpdateTeam_project_path_with_namespace :: Text,
    userUpdateTeam_user_email :: Text,
    userUpdateTeam_user_name :: Text,
    userUpdateTeam_user_username :: Text,
    userUpdateTeam_user_id :: Int,
    userUpdateTeam_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserRemoveFromTeam where
  match = Match
  matchIf = MatchIf

-- | A user has been removed from a team.
data UserRemoveFromTeam = UserRemoveFromTeam
  { userRemoveTeam_created_at :: Text, -- todo improve: date
    userRemoveTeam_updated_at :: Text, -- todo improve: date
    userRemoveTeam_event_name :: Text,
    userRemoveTeam_access_level :: Text, -- todo improve: Maintainer/...
    userRemoveTeam_project_id :: Int,
    userRemoveTeam_project_name :: Text,
    userRemoveTeam_project_path :: Text,
    userRemoveTeam_project_path_with_namespace :: Text,
    userRemoveTeam_user_email :: Text,
    userRemoveTeam_user_name :: Text,
    userRemoveTeam_user_username :: Text,
    userRemoveTeam_user_id :: Int,
    userRemoveTeam_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserCreate where
  match = Match
  matchIf = MatchIf

-- | A user has been created.
data UserCreate = UserCreate
  { userCreate_created_at :: Text, -- todo improve: date
    userCreate_updated_at :: Text, -- todo improve: date
    userCreate_email :: Text,
    userCreate_event_name :: Text,
    userCreate_name :: Text,
    userCreate_username :: Text,
    userCreate_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserRemove where
  match = Match
  matchIf = MatchIf

-- | A user has been removed.
data UserRemove = UserRemove
  { userRemove_created_at :: Text, -- todo improve: date
    userRemove_updated_at :: Text, -- todo improve: date
    userRemove_email :: Text,
    userRemove_event_name :: Text,
    userRemove_name :: Text,
    userRemove_username :: Text,
    userRemove_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserFailedLogin where
  match = Match
  matchIf = MatchIf

-- | A user has failed to log in.
data UserFailedLogin = UserFailedLogin
  { userFailedLogin_event_name :: Text,
    userFailedLogin_created_at :: Text, -- todo improve: date
    userFailedLogin_updated_at :: Text, -- todo improve: date
    userFailedLogin_name :: Text,
    userFailedLogin_email :: Text,
    userFailedLogin_user_id :: Int,
    userFailedLogin_username :: Text,
    -- create Haskell sum type for this
    userFailedLogin_state :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserRename where
  match = Match
  matchIf = MatchIf

-- | A user has been renamed.
data UserRename = UserRename
  { userRename_event_name :: Text,
    userRename_created_at :: Text, -- todo improve: date
    userRename_updated_at :: Text, -- todo improve: date
    userRename_name :: Text,
    userRename_email :: Text,
    userRename_user_id :: Int,
    userRename_username :: Text,
    userRename_old_username :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook KeyCreate where
  match = Match
  matchIf = MatchIf

-- | A key has been created.
data KeyCreate = KeyCreate
  { keyCreate_event_name :: Text,
    keyCreate_created_at :: Text, -- todo improve: date
    keyCreate_updated_at :: Text, -- todo improve: date
    keyCreate_username :: Text,
    keyCreate_key :: Text,
    keyCreate_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook KeyRemove where
  match = Match
  matchIf = MatchIf

-- | A key has been removed.
data KeyRemove = KeyRemove
  { keyRemove_event_name :: Text,
    keyRemove_created_at :: Text, -- todo improve: date
    keyRemove_updated_at :: Text, -- todo improve: date
    keyRemove_username :: Text,
    keyRemove_key :: Text,
    keyRemove_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupCreate where
  match = Match
  matchIf = MatchIf

-- | A group has been created.
data GroupCreate = GroupCreate
  { groupCreate_created_at :: Text, -- todo improve: date
    groupCreate_updated_at :: Text, -- todo improve: date
    groupCreate_event_name :: Text,
    groupCreate_name :: Text,
    groupCreate_owner_email :: Maybe Text,
    groupCreate_owner_name :: Maybe Text,
    groupCreate_path :: Text,
    groupCreate_group_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupRemove where
  match = Match
  matchIf = MatchIf

-- | A group has been removed.
data GroupRemove = GroupRemove
  { groupRemove_created_at :: Text, -- todo improve: date
    groupRemove_updated_at :: Text, -- todo improve: date
    groupRemove_event_name :: Text,
    groupRemove_name :: Text,
    groupRemove_owner_email :: Maybe Text,
    groupRemove_owner_name :: Maybe Text,
    groupRemove_path :: Text,
    groupRemove_group_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupRename where
  match = Match
  matchIf = MatchIf

-- | A group has been renamed.
data GroupRename = GroupRename
  { groupRename_event_name :: Text,
    groupRename_created_at :: Text, -- todo improve: date
    groupRename_updated_at :: Text, -- todo improve: date
    groupRename_name :: Text,
    groupRename_path :: Text,
    groupRename_full_path :: Text,
    groupRename_group_id :: Int,
    groupRename_owner_name :: Maybe Text,
    groupRename_owner_email :: Maybe Text,
    groupRename_old_path :: Text,
    groupRename_old_full_path :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook NewGroupMember where
  match = Match
  matchIf = MatchIf

-- | A user has been added to a group.
data NewGroupMember = NewGroupMember
  { newGroupMember_created_at :: Text, -- todo improve: date
    newGroupMember_updated_at :: Text, -- todo improve: date
    newGroupMember_event_name :: Text,
    newGroupMember_group_access :: Text, -- todo Haskell type for this
    newGroupMember_group_id :: Int,
    newGroupMember_group_name :: Text,
    newGroupMember_group_path :: Text,
    newGroupMember_user_email :: Text,
    newGroupMember_user_name :: Text,
    newGroupMember_user_username :: Text,
    newGroupMember_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupMemberRemove where
  match = Match
  matchIf = MatchIf

-- | A user has been removed from a group.
data GroupMemberRemove = GroupMemberRemove
  { groupMemberRemove_created_at :: Text, -- todo improve: date
    groupMemberRemove_updated_at :: Text, -- todo improve: date
    groupMemberRemove_event_name :: Text,
    groupMemberRemove_group_access :: Text, -- todo Haskell type for this
    groupMemberRemove_group_id :: Int,
    groupMemberRemove_group_name :: Text,
    groupMemberRemove_group_path :: Text,
    groupMemberRemove_user_email :: Text,
    groupMemberRemove_user_name :: Text,
    groupMemberRemove_user_username :: Text,
    groupMemberRemove_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupMemberUpdate where
  match = Match
  matchIf = MatchIf

-- | A group member has been updated.
data GroupMemberUpdate = GroupMemberUpdate
  { groupMemberUpdate_created_at :: Text, -- todo improve: date
    groupMemberUpdate_updated_at :: Text, -- todo improve: date
    groupMemberUpdate_event_name :: Text,
    groupMemberUpdate_group_access :: Text, -- todo Haskell type for this
    groupMemberUpdate_group_id :: Int,
    groupMemberUpdate_group_name :: Text,
    groupMemberUpdate_group_path :: Text,
    groupMemberUpdate_user_email :: Text,
    groupMemberUpdate_user_name :: Text,
    groupMemberUpdate_user_username :: Text,
    groupMemberUpdate_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook Push where
  match = Match
  matchIf = MatchIf

-- | Commits have been pushed to the server.
data Push = Push
  { push_event_name :: Text,
    push_before :: Text,
    push_after :: Text,
    push_ref :: Text,
    push_checkout_sha :: Text,
    push_user_id :: Int,
    push_user_name :: Text,
    push_user_email :: Text,
    push_user_avatar :: Text,
    push_project_id :: Int,
    push_project :: ProjectEvent,
    push_repository :: RepositoryEvent,
    push_commits :: [CommitEvent],
    push_total_commits_count :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook TagPush where
  match = Match
  matchIf = MatchIf

-- | Tags have been pushed to the server.
data TagPush = TagPush
  { tagPush_event_name :: Text,
    tagPush_before :: Text,
    tagPush_after :: Text,
    tagPush_ref :: Text,
    tagPush_checkout_sha :: Text,
    tagPush_user_id :: Int,
    tagPush_user_name :: Text,
    tagPush_user_avatar :: Text,
    tagPush_project_id :: Int,
    tagPush_project :: ProjectEvent,
    tagPush_repository :: RepositoryEvent,
    tagPush_commits :: [CommitEvent],
    tagPush_total_commits_count :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook RepositoryUpdate where
  match = Match
  matchIf = MatchIf

-- | Tags have been pushed to the server.
data RepositoryUpdate = RepositoryUpdate
  { repositoryUpdate_event_name :: Text,
    repositoryUpdate_user_id :: Int,
    repositoryUpdate_user_name :: Text,
    repositoryUpdate_user_email :: Text,
    repositoryUpdate_user_avatar :: Text,
    repositoryUpdate_project_id :: Int,
    repositoryUpdate_project :: ProjectEvent,
    repositoryUpdate_changes :: [ProjectChanges],
    repositoryUpdate_refs :: [Text]
  }
  deriving (Typeable, Show, Eq)

-- | A project event.
data ProjectEvent = ProjectEvent
  { projectEvent_name :: Text,
    projectEvent_description :: Text,
    projectEvent_web_url :: Text,
    projectEvent_avatar_url :: Maybe Text,
    projectEvent_git_ssh_url :: Text,
    projectEvent_git_http_url :: Text,
    projectEvent_namespace :: Text,
    projectEvent_visibility_level :: Visibility,
    projectEvent_path_with_namespace :: Text,
    projectEvent_default_branch :: Text,
    -- projectEvent_ci_config_path :: Maybe Text,
    projectEvent_homepage :: Maybe Text,
    projectEvent_url :: Text,
    projectEvent_ssh_url :: Text,
    projectEvent_http_url :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | A project event.
data ProjectChanges = ProjectChanges
  { projectChanges_before :: Text,
    projectChanges_after :: Text,
    projectChanges_ref :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | A repository event.
data RepositoryEvent = RepositoryEvent
  { repositoryEvent_name :: Text,
    repositoryEvent_url :: Text,
    repositoryEvent_description :: Text,
    repositoryEvent_homepage :: Maybe Text,
    -- these three not in the merge_request event example
    -- in the GitLab documentation. Is the merge_request documentation
    -- out dated?
    repositoryEvent_git_http_url :: Maybe Text,
    repositoryEvent_git_ssh_url :: Maybe Text,
    repositoryEvent_visibility_level :: Maybe Visibility
  }
  deriving (Typeable, Show, Eq, Generic)

-- | A commit event.
data CommitEvent = CommitEvent
  { commitEvent_id :: Text,
    commitEvent_message :: Text,
    commitEvent_timestamp :: Text, -- TODO improve.
    commitEvent_url :: Text,
    commitEvent_author :: CommitAuthorEvent
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Commit author information.
data CommitAuthorEvent = CommitAuthorEvent
  { commitAuthorEvent_name :: Text,
    commitAuthorEvent_email :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

instance SystemHook MergeRequestEvent where
  match = Match
  matchIf = MatchIf

-- | Merge request (named so, since 'MergeRequest' type already used
-- in GitLab.Types.
data MergeRequestEvent = MergeRequestEvent
  { mergeRequest_object_kind :: Text,
    mergeRequest_event_type :: Text,
    mergeRequest_user :: UserEvent,
    mergeRequest_project :: ProjectEvent,
    mergeRequest_object_attributes :: ObjectAttributes,
    mergeRequest_labels :: Maybe [Text],
    mergeRequest_changes :: MergeRequestChanges,
    mergeRequest_repository :: RepositoryEvent
  }
  deriving (Typeable, Show, Eq, Generic)

data MergeRequestChanges = MergeRequestChanges
  { mergeRequestChanges_author_id :: MergeRequestChange Int,
    mergeRequestChanges_created_at :: MergeRequestChange Text,
    mergeRequestChanges_description :: MergeRequestChange Text,
    mergeRequestChanges_id :: MergeRequestChange Int,
    mergeRequestChanges_iid :: MergeRequestChange Int,
    mergeRequestChanges_source_branch :: MergeRequestChange Text,
    mergeRequestChanges_source_project_id :: MergeRequestChange Int,
    mergeRequestChanges_target_branch :: MergeRequestChange Text,
    mergeRequestChanges_target_project_id :: MergeRequestChange Int,
    mergeRequestChanges_title :: MergeRequestChange Text,
    mergeRequestChanges_updated_at :: MergeRequestChange Text
  }
  deriving (Typeable, Show, Eq, Generic)

data MergeRequestChange a = MergeRequestChange
  { mergeRequestChange_previous :: Maybe a,
    mergeRequestChange_current :: Maybe a
  }
  deriving (Typeable, Show, Eq, Generic)

data ObjectAttributes = ObjectAttributes
  { objectAttributes_id :: Int,
    objectAttributes_target_branch :: Text,
    objectAttributes_source_branch :: Text,
    objectAttributes_source_project_id :: Int,
    objectAttributes_author_id :: Int,
    objectAttributes_assignee_id :: Maybe Int,
    objectAttributes_assignee_ids :: Maybe [Int],
    objectAttributes_title :: Text,
    objectAttributes_created_at :: Text,
    objectAttributes_updated_at :: Text,
    objectAttributes_milestone_id :: Maybe Int,
    objectAttributes_state :: Text,
    objectAttributes_state_id :: Maybe Int,
    objectAttributes_merge_status :: Text,
    objectAttributes_target_project_id :: Int,
    objectAttributes_iid :: Int,
    objectAttributes_description :: Text,
    objectAttributes_updated_by_id :: Maybe Int,
    objectAttributes_merge_error :: Maybe Text,
    objectAttributes_merge_params :: MergeParams,
    objectAttributes_merge_when_pipeline_succeeds :: Bool,
    objectAttributes_merge_user_id :: Maybe Int,
    objectAttributes_merge_commit_sha :: Maybe Text,
    objectAttributes_deleted_at :: Maybe Text,
    objectAttributes_in_progress_merge_commit_sha :: Maybe Text,
    objectAttributes_lock_version :: Maybe Int,
    objectAttributes_time_estimate :: Int,
    objectAttributes_last_edited_at :: Maybe Text,
    objectAttributes_last_edited_by_id :: Maybe Int,
    objectAttributes_head_pipeline_id :: Maybe Int,
    objectAttributes_ref_fetched :: Maybe Bool,
    objectAttributes_merge_jid :: Maybe Int,
    objectAttributes_source :: ProjectEvent,
    objectAttributes_target :: ProjectEvent,
    objectAttributes_last_commit :: CommitEvent,
    objectAttributes_work_in_progress :: Bool,
    objectAttributes_total_time_spent :: Int,
    objectAttributes_human_total_time_spent :: Maybe Int,
    objectAttributes_human_time_estimate :: Maybe Int,
    objectAttributes_action :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

data MergeParams = MergeParams
  { mergeParams_force_remove_source_branch :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

data UserEvent = UserEvent
  { userEvent_name :: Text,
    userEvent_username :: Text,
    userEvent_avatar_url :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

data ProjectAction
  = ProjectCreated
  | ProjectDestroyed
  | ProjectRenamed
  | ProjectTransferred
  | ProjectUpdated
  | UserAddedToTeam
  | UserUpdatedForTeam
  | UserRemovedFromTeam
  | UserCreated
  | UserRemoved
  | UserFailedToLogin
  | UserRenamed
  | KeyCreated
  | KeyRemoved
  | GroupCreated
  | GroupRemoved
  | GroupRenamed
  | GroupMemberAdded
  | GroupMemberRemoved
  | GroupMemberUpdated
  | Pushed
  | TagPushed
  | RepositoryUpdated
  | MergeRequested
  deriving (Show, Eq)

-- |  Project visibility.
data Visibility
  = Public
  | Private
  | Internal
  deriving (Show, Eq)

instance FromJSON ProjectCreate where
  parseJSON =
    withObject "ProjectCreate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectCreated ->
              ProjectCreate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "project_create parsing failed"
        _unexpected -> fail "project_create parsing failed"

instance FromJSON ProjectDestroy where
  parseJSON =
    withObject "ProjectDestroy" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectDestroyed ->
              ProjectDestroy
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "project_destroy parsing failed"
        _unexpected -> fail "project_destroy parsing failed"

instance FromJSON ProjectRename where
  parseJSON =
    withObject "ProjectRename" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectRenamed ->
              ProjectRename
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "owner_name"
                <*> v .: "owner_email"
                <*> v .: "project_visibility"
                <*> v .: "old_path_with_namespace"
            _unexpected -> fail "project_rename parsing failed"
        _unexpected -> fail "project_rename parsing failed"

instance FromJSON ProjectTransfer where
  parseJSON =
    withObject "ProjectTransfer" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectTransferred ->
              ProjectTransfer
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "owner_name"
                <*> v .: "owner_email"
                <*> v .: "project_visibility"
                <*> v .: "old_path_with_namespace"
            _unexpected -> fail "project_transfer parsing failed"
        _unexpected -> fail "project_transfer parsing failed"

instance FromJSON ProjectUpdate where
  parseJSON =
    withObject "ProjectUpdate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectUpdated ->
              ProjectUpdate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "project_update parsing failed"
        _unexpected -> fail "project_update parsing failed"

instance FromJSON UserAddToTeam where
  parseJSON =
    withObject "UserAddToTeam" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserAddedToTeam ->
              UserAddToTeam
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "access_level"
                <*> v .: "project_id"
                <*> v .: "project_name"
                <*> v .: "project_path"
                <*> v .: "project_path_with_namespace"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "user_add_to_team parsing failed"
        _unexpected -> fail "user_add_to_team parsing failed"

instance FromJSON UserUpdateForTeam where
  parseJSON =
    withObject "UserUpdateForTeam" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserUpdatedForTeam ->
              UserUpdateForTeam
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "access_level"
                <*> v .: "project_id"
                <*> v .: "project_name"
                <*> v .: "project_path"
                <*> v .: "project_path_with_namespace"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "user_update_for_team parsing failed"
        _unexpected -> fail "user_update_for_team parsing failed"

instance FromJSON UserRemoveFromTeam where
  parseJSON =
    withObject "UserRemoveFromTeam" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserRemovedFromTeam ->
              UserRemoveFromTeam
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "access_level"
                <*> v .: "project_id"
                <*> v .: "project_name"
                <*> v .: "project_path"
                <*> v .: "project_path_with_namespace"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "user_remove_from_team parsing failed"
        _unexpected -> fail "user_remove_from_team parsing failed"

instance FromJSON UserCreate where
  parseJSON =
    withObject "UserCreate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserCreated ->
              UserCreate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "email"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "username"
                <*> v .: "user_id"
            _unexpected -> fail "user_create parsing failed"
        _unexpected -> fail "user_create parsing failed"

instance FromJSON UserRemove where
  parseJSON =
    withObject "UserRemove" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserRemoved ->
              UserRemove
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "email"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "username"
                <*> v .: "user_id"
            _unexpected -> fail "user_destroy parsing failed"
        _unexpected -> fail "user_destroy parsing failed"

instance FromJSON UserFailedLogin where
  parseJSON =
    withObject "UserFailedLogin" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserFailedToLogin ->
              UserFailedLogin
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "name"
                <*> v .: "email"
                <*> v .: "user_id"
                <*> v .: "username"
                <*> v .: "state"
            _unexpected -> fail "user_failed_login parsing failed"
        _unexpected -> fail "user_failed_login parsing failed"

instance FromJSON UserRename where
  parseJSON =
    withObject "UserRename" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserRenamed ->
              UserRename
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "name"
                <*> v .: "email"
                <*> v .: "user_id"
                <*> v .: "username"
                <*> v .: "old_username"
            _unexpected -> fail "user_rename parsing failed"
        _unexpected -> fail "user_rename parsing failed"

instance FromJSON KeyCreate where
  parseJSON =
    withObject "KeyCreate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            KeyCreated ->
              KeyCreate
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "username"
                <*> v .: "key"
                <*> v .: "id"
            _unexpected -> fail "key_create parsing failed"
        _unexpected -> fail "key_create parsing failed"

instance FromJSON KeyRemove where
  parseJSON =
    withObject "KeyRemove" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            KeyRemoved ->
              KeyRemove
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "username"
                <*> v .: "key"
                <*> v .: "id"
            _unexpected -> fail "key_destroy parsing failed"
        _unexpected -> fail "key_destroy parsing failed"

instance FromJSON GroupCreate where
  parseJSON =
    withObject "GroupCreate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupCreated ->
              GroupCreate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "group_id"
            _unexpected -> fail "group_create parsing failed"
        _unexpected -> fail "group_create parsing failed"

instance FromJSON GroupRemove where
  parseJSON =
    withObject "GroupRemove" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupRemoved ->
              GroupRemove
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "group_id"
            _unexpected -> fail "group_remove parsing failed"
        _unexpected -> fail "group_remove parsing failed"

instance FromJSON GroupRename where
  parseJSON =
    withObject "GroupRename" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupRenamed ->
              GroupRename
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "name"
                <*> v .: "path"
                <*> v .: "full_path"
                <*> v .: "group_id"
                <*> v .: "owner_name"
                <*> v .: "owner_email"
                <*> v .: "old_path"
                <*> v .: "old_full_path"
            _unexpected -> fail "group_rename parsing failed"
        _unexpected -> fail "group_rename parsing failed"

instance FromJSON NewGroupMember where
  parseJSON =
    withObject "NewGroupMember" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupMemberAdded ->
              NewGroupMember
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "group_access"
                <*> v .: "group_id"
                <*> v .: "group_name"
                <*> v .: "group_path"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
            _unexpected -> fail "user_add_to_group parsing failed"
        _unexpected -> fail "user_add_to_group parsing failed"

instance FromJSON GroupMemberRemove where
  parseJSON =
    withObject "GroupMemberRemove" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupMemberRemoved ->
              GroupMemberRemove
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "group_access"
                <*> v .: "group_id"
                <*> v .: "group_name"
                <*> v .: "group_path"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
            _unexpected -> fail "user_remove_from_group parsing failed"
        _unexpected -> fail "user_remove_from_group parsing failed"

instance FromJSON GroupMemberUpdate where
  parseJSON =
    withObject "GroupMemberUpdate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupMemberUpdated ->
              GroupMemberUpdate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "group_access"
                <*> v .: "group_id"
                <*> v .: "group_name"
                <*> v .: "group_path"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
            _unexpected -> fail "user_update_for_group parsing failed"
        _unexpected -> fail "user_update_for_group parsing failed"

instance FromJSON Push where
  parseJSON =
    withObject "Push" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            Pushed ->
              Push
                <$> v .: "event_name"
                <*> v .: "before"
                <*> v .: "after"
                <*> v .: "ref"
                <*> v .: "checkout_sha"
                <*> v .: "user_id"
                <*> v .: "user_name"
                <*> v .: "user_email"
                <*> v .: "user_avatar"
                <*> v .: "project_id"
                <*> v .: "project"
                <*> v .: "repository"
                <*> v .: "commits"
                <*> v .: "total_commits_count"
            _unexpected -> fail "push parsing failed"
        _unexpected -> fail "push parsing failed"

instance FromJSON TagPush where
  parseJSON =
    withObject "TagPush" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            TagPushed ->
              TagPush
                <$> v .: "event_name"
                <*> v .: "before"
                <*> v .: "after"
                <*> v .: "ref"
                <*> v .: "checkout_sha"
                <*> v .: "user_id"
                <*> v .: "user_name"
                <*> v .: "user_avatar"
                <*> v .: "project_id"
                <*> v .: "project"
                <*> v .: "repository"
                <*> v .: "commits"
                <*> v .: "total_commits_count"
            _unexpected -> fail "tag_push parsing failed"
        _unexpected -> fail "tag_push parsing failed"

instance FromJSON RepositoryUpdate where
  parseJSON =
    withObject "RepositoryUpdate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            RepositoryUpdated ->
              RepositoryUpdate
                <$> v .: "event_name"
                <*> v .: "user_id"
                <*> v .: "user_name"
                <*> v .: "user_email"
                <*> v .: "user_avatar"
                <*> v .: "project_id"
                <*> v .: "project"
                <*> v .: "changes"
                <*> v .: "refs"
            _unexpected -> fail "repository_update parsing failed"
        _unexpected -> fail "repository_update parsing failed"

instance FromJSON MergeRequestEvent where
  parseJSON =
    withObject "MergeRequestEvent" $ \v -> do
      -- Note: it's `event_name` in all other examples, but the GitLab
      -- documentation for MergeRequests says `object_kind`.
      --
      -- `object_kind` has been tried.
      --
      -- Bug in GitLab system hooks documentation?
      isProjectEvent <- v .:? "object_kind"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            MergeRequested ->
              MergeRequestEvent
                <$> v .: "object_kind"
                <*> v .: "event_type"
                <*> v .: "user"
                <*> v .: "project"
                <*> v .: "object_attributes"
                <*> v .: "labels"
                <*> v .: "changes"
                <*> v .: "repository"
            _unexpected -> fail "merge_request parsing failed"
        _unexpected -> fail "merge_request parsing failed"

bodyNoPrefix :: String -> String
bodyNoPrefix "projectEvent_name" = "name"
bodyNoPrefix "projectEvent_description" = "description"
bodyNoPrefix "projectEvent_web_url" = "web_url"
bodyNoPrefix "projectEvent_avatar_url" = "avatar_url"
bodyNoPrefix "projectEvent_git_ssh_url" = "git_ssh_url"
bodyNoPrefix "projectEvent_git_http_url" = "git_http_url"
bodyNoPrefix "projectEvent_namespace" = "namespace"
bodyNoPrefix "projectEvent_visibility_level" = "visibility_level"
bodyNoPrefix "projectEvent_path_with_namespace" = "path_with_namespace"
bodyNoPrefix "projectEvent_default_branch" = "default_branch"
-- bodyNoPrefix "projectEvent_ci_config_path" = "ci_config_path"
bodyNoPrefix "projectEvent_homepage" = "homepage"
bodyNoPrefix "projectEvent_url" = "url"
bodyNoPrefix "projectEvent_ssh_url" = "ssh_url"
bodyNoPrefix "projectEvent_http_url" = "http_url"
bodyNoPrefix "projectChanges_before" = "before"
bodyNoPrefix "projectChanges_after" = "after"
bodyNoPrefix "projectChanges_ref" = "ref"
bodyNoPrefix "repositoryEvent_name" = "name"
bodyNoPrefix "repositoryEvent_url" = "url"
bodyNoPrefix "repositoryEvent_description" = "description"
bodyNoPrefix "repositoryEvent_homepage" = "homepage"
bodyNoPrefix "repositoryEvent_git_http_url" = "git_http_url"
bodyNoPrefix "repositoryEvent_git_ssh_url" = "git_ssh_url"
bodyNoPrefix "repositoryEvent_visibility_level" = "visibility_level"
bodyNoPrefix "commitEvent_id" = "id"
bodyNoPrefix "commitEvent_message" = "message"
bodyNoPrefix "commitEvent_timestamp" = "timestamp"
bodyNoPrefix "commitEvent_url" = "url"
bodyNoPrefix "commitEvent_author" = "author"
bodyNoPrefix "commitAuthorEvent_name" = "name"
bodyNoPrefix "commitAuthorEvent_email" = "email"
bodyNoPrefix "mergeParams_force_remove_source_branch" = "force_remove_source_branch"
bodyNoPrefix "userEvent_name" = "name"
bodyNoPrefix "userEvent_username" = "username"
bodyNoPrefix "userEvent_avatar_url" = "avatar_url"
bodyNoPrefix "objectAttributes_id" = "id"
bodyNoPrefix "objectAttributes_target_branch" = "target_branch"
bodyNoPrefix "objectAttributes_source_branch" = "source_branch"
bodyNoPrefix "objectAttributes_source_project_id" = "source_project_id"
bodyNoPrefix "objectAttributes_author_id" = "author_id"
bodyNoPrefix "objectAttributes_assignee_id" = "assignee_id"
bodyNoPrefix "objectAttributes_assignee_ids" = "assignee_ids"
bodyNoPrefix "objectAttributes_title" = "title"
bodyNoPrefix "objectAttributes_created_at" = "created_at"
bodyNoPrefix "objectAttributes_updated_at" = "updated_at"
bodyNoPrefix "objectAttributes_milestone_id" = "milestone_id"
bodyNoPrefix "objectAttributes_state" = "state"
bodyNoPrefix "objectAttributes_state_id" = "state_id"
bodyNoPrefix "objectAttributes_merge_status" = "merge_status"
bodyNoPrefix "objectAttributes_target_project_id" = "target_project_id"
bodyNoPrefix "objectAttributes_iid" = "iid"
bodyNoPrefix "objectAttributes_description" = "description"
bodyNoPrefix "objectAttributes_updated_by_id" = "updated_by_id"
bodyNoPrefix "objectAttributes_merge_error" = "merge_error"
bodyNoPrefix "objectAttributes_merge_params" = "merge_params"
bodyNoPrefix "objectAttributes_merge_when_pipeline_succeeds" = "merge_when_pipeline_succeeds"
bodyNoPrefix "objectAttributes_merge_user_id" = "merge_user_id"
bodyNoPrefix "objectAttributes_merge_commit_sha" = "merge_commit_sha"
bodyNoPrefix "objectAttributes_deleted_at" = "deleted_at"
bodyNoPrefix "objectAttributes_in_progress_merge_commit_sha" = "in_progress_merge_commit_sha"
bodyNoPrefix "objectAttributes_lock_version" = "lock_version"
bodyNoPrefix "objectAttributes_time_estimate" = "time_estimate"
bodyNoPrefix "objectAttributes_last_edited_at" = "last_edited_at"
bodyNoPrefix "objectAttributes_last_edited_by_id" = "last_edited_by_id"
bodyNoPrefix "objectAttributes_head_pipeline_id" = "head_pipeline_id"
bodyNoPrefix "objectAttributes_ref_fetched" = "ref_fetched"
bodyNoPrefix "objectAttributes_merge_jid" = "merge_jid"
bodyNoPrefix "objectAttributes_source" = "source"
bodyNoPrefix "objectAttributes_target" = "target"
bodyNoPrefix "objectAttributes_last_commit" = "last_commit"
bodyNoPrefix "objectAttributes_work_in_progress" = "work_in_progress"
bodyNoPrefix "objectAttributes_total_time_spent" = "total_time_spent"
bodyNoPrefix "objectAttributes_human_total_time_spent" = "human_total_time_spent"
bodyNoPrefix "objectAttributes_human_time_estimate" = "human_time_estimate"
bodyNoPrefix "objectAttributes_action" = "action"
bodyNoPrefix "mergeRequestChanges_author_id" = "author_id"
bodyNoPrefix "mergeRequestChanges_created_at" = "created_at"
bodyNoPrefix "mergeRequestChanges_description" = "description"
bodyNoPrefix "mergeRequestChanges_id" = "id"
bodyNoPrefix "mergeRequestChanges_iid" = "iid"
bodyNoPrefix "mergeRequestChanges_source_branch" = "source_branch"
bodyNoPrefix "mergeRequestChanges_source_project_id" = "source_project_id"
bodyNoPrefix "mergeRequestChanges_target_branch" = "target_branch"
bodyNoPrefix "mergeRequestChanges_target_project_id" = "target_project_id"
bodyNoPrefix "mergeRequestChanges_title" = "title"
bodyNoPrefix "mergeRequestChanges_updated_at" = "updated_at"
bodyNoPrefix "mergeRequestChange_previous" = "previous"
bodyNoPrefix "mergeRequestChange_current" = "current"
bodyNoPrefix s = error ("uexpected JSON field prefix: " <> s)

instance FromJSON ProjectEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON ProjectChanges where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON CommitEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON RepositoryEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON CommitAuthorEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON ObjectAttributes where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON MergeParams where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON UserEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON MergeRequestChanges where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance (FromJSON a) => FromJSON (MergeRequestChange a) where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON ProjectAction where
  parseJSON (String "project_create") = return ProjectCreated
  parseJSON (String "project_destroy") = return ProjectDestroyed
  parseJSON (String "project_rename") = return ProjectRenamed
  parseJSON (String "project_transfer") = return ProjectTransferred
  parseJSON (String "project_update") = return ProjectUpdated
  parseJSON (String "user_add_to_team") = return UserAddedToTeam
  parseJSON (String "user_update_for_team") = return UserUpdatedForTeam
  parseJSON (String "user_remove_from_team") = return UserRemovedFromTeam
  parseJSON (String "user_create") = return UserCreated
  parseJSON (String "user_destroy") = return UserRemoved
  parseJSON (String "user_failed_login") = return UserFailedToLogin
  parseJSON (String "user_rename") = return UserRenamed
  parseJSON (String "key_create") = return KeyCreated
  parseJSON (String "key_destroy") = return KeyRemoved
  parseJSON (String "group_create") = return GroupCreated
  parseJSON (String "group_destroy") = return GroupRemoved
  parseJSON (String "group_rename") = return GroupRenamed
  parseJSON (String "user_add_to_group") = return GroupMemberAdded
  parseJSON (String "user_remove_from_group") = return GroupMemberRemoved
  parseJSON (String "user_update_for_group") = return GroupMemberUpdated
  parseJSON (String "push") = return Pushed
  parseJSON (String "tag_push") = return TagPushed
  parseJSON (String "repository_update") = return RepositoryUpdated
  parseJSON (String "merge_request") = return MergeRequested
  parseJSON s = error ("unexpected system hook event: " <> show s)

instance FromJSON Visibility where
  parseJSON (String "public") = return Public
  parseJSON (String "private") = return Private
  parseJSON (String "internal") = return Internal
  parseJSON (Number 0) = return Private
  parseJSON (Number 10) = return Internal
  parseJSON (Number 20) = return Public
  parseJSON n = error (show n)
