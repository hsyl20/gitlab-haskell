{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GitLab.Types
Description : Haskell records corresponding to JSON data from GitLab API calls
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.Types
  (
    GitLab
  , GitLabState(..)
  , GitLabServerConfig(..)
  , defaultGitLabServer
  , Member(..)
  , Namespace(..)
  , Links(..)
  , Owner(..)
  , Permissions(..)
  , Project(..)
  , ProjectStats(..)
  , User(..)
  , Milestone(..)
  , TimeStats(..)
  , Issue(..)
  , Pipeline(..)
  , Commit(..)
  , CommitStats(..)
  , Repository(..)
  , Job(..)
  , Artifact(..)
  , Branch(..)
  , RepositoryFile(..)
  , MergeRequest(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Conduit
import Control.Monad.Trans.Reader

-- | type synonym for all GitLab actions.
type GitLab m a = ReaderT GitLabState m a

-- | state used by GitLab actions, used internally. 
data GitLabState =
  GitLabState
  { serverCfg :: GitLabServerConfig
  , httpManager :: Manager
  }

-- | configuration data specific to a GitLab server.
data GitLabServerConfig =
  GitLabServerConfig
  { url :: Text
  , token :: Text -- ^ personal access token, see <https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html>
  , timeout :: Int -- ^ milliseconds
  , retries :: Int -- ^ how many times to retry a HTTP request before giving up and returning an error.
  }

-- | default settings, the 'url' and 'token' values will need to be overwritten.
defaultGitLabServer :: GitLabServerConfig
defaultGitLabServer =
  GitLabServerConfig
  { url = "https://gitlab.com"
  , token = ""
  , timeout = 15000000 -- 15 seconds
  , retries = 5
  }

-- | member of a project.
data Member =
  Member
  { member_id :: Int
  , member_name :: Text
  , member_username :: Text
  , member_state :: Text
  , member_avatar_uri :: Maybe Text
  , member_web_url :: Maybe Text
  , access_level :: Int
  , expires_at :: Maybe Text
  } deriving (Generic, Show)

-- | namespaces.
data Namespace =
  Namespace
  { namespace_id :: Int
  , namespace_name :: Text
  , namespace_path :: Text
  , kind :: Text
  , full_path :: Text
  , parent_id :: Maybe Text
  } deriving (Generic, Show, Eq)

-- | links.
data Links =
  Links
  { self :: Text
  , issues :: Text
  , merge_requests :: Text
  , repo_branches :: Text
  , link_labels :: Text
  , link_events :: Text
  , members :: Text
  } deriving (Generic, Show, Eq)

-- | owners.
data Owner =
  Ownwer
  { owner_id :: Int
  , owner_name :: Text
  , owner_username :: Text
  , state :: Text
  , owner_avatar_url :: Maybe Text
  , owner_web_url :: Text
  } deriving (Generic, Show, Eq)

-- | permissions.
data Permissions =
  Permissions
  { project_access :: Maybe Object
  , group_access :: Maybe Object
  } deriving (Generic, Show, Eq)

-- | projects.
data Project =
  Project
  { project_id :: Int
  , description :: Maybe Text
  , project_name :: Text
  , name_with_namespace :: Text
  , project_path :: Text
  , path_with_namespace :: Text
  , project_created_at :: Text
  , default_branch :: Maybe Text
  , tag_list:: [Text] -- check
  , ssh_url_to_repo:: Text
  , http_url_to_repo:: Text
  , project_web_url:: Text
  , readme_url :: Maybe Text -- check
  , project_avatar_url:: Maybe Text -- check
  , star_count:: Int
  , forks_count:: Int
  , last_activity_at:: Text
  , namespace :: Namespace
  , _links :: Maybe Links
  , archived :: Maybe Bool
  , visibility :: Maybe Text
  , owner :: Maybe Owner
  , resolve_outdated_diff_discussions :: Maybe Bool
  , container_registry_enabled :: Maybe Bool
  , issues_enabled :: Maybe Bool
  , merge_requests_enabled:: Maybe Bool
  , wiki_enabled :: Maybe Bool
  , jobs_enabled :: Maybe Bool
  , snippets_enabled :: Maybe Bool
  , shared_runners_enabled :: Maybe Bool
  , lfs_enabled :: Maybe Bool
  , creator_id :: Maybe Int
  , forked_from_project :: Maybe Project
  , import_status :: Maybe String
  , open_issues_count :: Maybe Int
  , public_jobs :: Maybe Bool
  , ci_config_path :: Maybe Text -- check null
  , shared_with_groups :: Maybe [Object]
  , only_allow_merge_if_pipeline_succeeds :: Maybe Bool
  , request_access_enabled :: Maybe Bool
  , only_allow_merge_if_all_discussions_are_resolved :: Maybe Bool
  , printing_merge_request_link_enabled :: Maybe Bool
  , merge_method:: Maybe Text
  , permissions :: Maybe Permissions
  , project_stats :: Maybe ProjectStats
  } deriving (Generic, Show, Eq)

data ProjectStats =
  ProjectStats
  { commit_count :: Int
  , storage_size :: Int
  , repository_size :: Int
  , wiki_size :: Maybe Int
  , lfs_objects_size :: Maybe Int
  , job_artifacts_size :: Maybe Int
  , packages_size :: Maybe Int
  } deriving (Generic, Show, Eq)

-- | registered users.
data User =
  User
  { user_id :: Int
  , user_username :: Text
  , user_name :: Text
  , user_state :: Text
  , user_avatar_uri :: Maybe Text
  , user_web_url :: Maybe Text
  } deriving (Generic, Show, Eq)

-- | project milestones.
data Milestone =
  Milestone
  { milestone_project_id :: Int
  , milestone_description :: Text
  , milestone_state :: Text
  , due_date :: Maybe Text
  , milestone_iid :: Int
  , milestone_created_at :: Maybe Text
  , milestone_title :: Text
  , milestone_id :: Int
  , milestone_updated_at :: Text
  } deriving (Generic, Show, Eq)

-- | time stats.
data TimeStats =
  TimeStats
  { time_estimate :: Int
  , total_time_spent :: Int
  , humane_time_estimate :: Maybe Int
  , human_total_time_spent :: Maybe Int
  } deriving (Generic, Show, Eq)

-- | project issues.
data Issue =
  Issue
  { issue_state :: Text
  , issue_description :: Text
  , issue_author :: User
  , milestone :: Maybe Milestone
  , issue_project_id :: Int
  , assignees :: Maybe [User]
  , assignee :: Maybe User
  , updated_at :: Text
  , closed_at :: Maybe Text
  , closed_by :: Maybe User
  , issue_id :: Int
  , issue_title :: Text
  , issue_created_at :: Text
  , iid :: Int
  , issue_labels :: [Text]
  , upvotes :: Int
  , downvotes :: Int
  , user_notes_count :: Int
  , issue_due_date :: Maybe Text
  , issue_web_url :: Text
  , confidential :: Bool
  , weight :: Maybe Text -- Int?
  , discussion_locked :: Maybe Bool
  , time_stats :: TimeStats
  } deriving (Generic, Show, Eq)

-- | project pipelines
data Pipeline =
  Pipeline
  { pipeline_id :: Int
  , sha :: Text
  , pipeline_ref :: Text
  , pipeline_status :: Text
  , pipeline_web_url :: Maybe Text
  } deriving (Generic, Show)

-- | code commits.
data Commit =
  Commit
  { commit_id :: Text
  , short_id :: Text
  , title :: Text
  , author_name :: Text
  , author_email :: Text
  , authored_date :: Text
  , committer_name :: Text
  , committer_email :: Text
  , committed_date :: Text
  , commit_created_at :: Text
  , message :: Text
  , parent_ids :: Maybe [Text]
  , last_pipeline :: Maybe Pipeline
  , commit_stats :: Maybe CommitStats
  , commit_status :: Maybe Text
  } deriving (Generic, Show)

-- | commit stats.
data CommitStats =
  Stats
  { additions :: Int
  , deletions :: Int
  , total :: Int
  } deriving (Generic, Show)

-- | repositories.
data Repository =
  Repository
  { repository_id :: Text
  , repository_name :: Text
  , repository_type :: Text
  , repository_path :: Text
  , mode :: Text
  } deriving (Generic, Show)

-- | jobs.
data Job =
  Job
  {
    commit :: Commit
  , coverage :: Maybe Text -- ?
  , created_at :: Text
  , started_at :: Text
  , finished_at :: Text
  , duration :: Double
  , artifacts_expire_at :: Maybe Text
  , id :: Int
  , name :: Text
  , pipeline :: Pipeline
  , job_ref :: Text
  , artifacts :: [Artifact]
  -- , runner :: Maybe Text
  , stage :: Text
  , status :: Text
  , tag :: Bool
  , job_web_url :: Text
  , user :: User
  } deriving (Generic, Show)

-- | artifacts.
data Artifact =
  Artifact
  { file_type :: Text
  , size :: Int
  , filename :: Text
  , file_format :: Maybe Text
  } deriving (Generic, Show)

-- | code branches.
data Branch =
  Branch
  { branch_name :: Text
  , merged :: Bool
  , protected :: Bool
  , branch_default :: Bool
  , developers_can_push :: Bool
  , developers_can_merge :: Bool
  , can_push :: Bool
  , branch_commit :: Commit
  } deriving (Generic, Show)

-- | files in a repository.
data RepositoryFile =
  RepositoryFile
  { repository_file_file_name :: Text
  , repository_file_file_path :: Text
  , repository_file_size :: Int
  , encoding :: Text
  , content :: Text
  , content_sha256 :: Text
  , ref :: Text
  , blob_id :: Text
  , repository_file_commit_id :: Text
  , last_commit_id :: Text
  } deriving (Generic, Show)

-- | project merge requests.
data MergeRequest =
  MergeRequest
  { merge_request_id :: Int
  , merge_request_iid :: Int
  , merge_request_project_id :: Int
  , merge_request_title :: Text
  , merge_request_description :: Text
  , merge_request_state :: Text
  , merge_request_merged_by :: Maybe User
  , merge_request_merged_at :: Maybe Text
  , merge_request_closed_by :: Maybe User
  , merge_request_closed_at :: Maybe Text
  , merge_request_created_at :: Text
  , merge_request_updated_at :: Text
  , merge_request_target_branch :: Text
  , merge_request_source_branch :: Text
  , merge_request_upvotes :: Int
  , merge_request_downvotes :: Int
  , merge_request_author :: User
  , merge_request_assignee :: Maybe User
  , merge_request_source_project_id :: Int
  , merge_request_target_project_id :: Int
  , merge_request_labels :: [Text]
  , merge_request_work_in_progress :: Bool
  , merge_request_milestone :: Maybe Milestone
  , merge_request_merge_when_pipeline_succeeds :: Bool
  , merge_request_merge_status :: Text
  , merge_request_sha :: Text
  , merge_request_merge_commit_sha :: Maybe Text
  , merge_request_user_notes_count ::Int
  , merge_request_discussion_locked :: Maybe Bool
  , merge_request_should_remove_source_branch :: Maybe Bool
  , merge_request_force_remove_source_branch :: Maybe Bool
  -- , merge_request_allow_collaboration :: Bool
  -- , merge_request_allow_maintainer_to_push :: Bool
  , merge_request_web_url :: Text
  , merge_request_time_stats :: TimeStats
  , merge_request_squash :: Bool
  , merge_request_approvals_before_merge :: Maybe Bool -- ?
  } deriving (Generic, Show)

-----------------------------
-- JSON GitLab parsers below
-----------------------------

bodyNoPrefix :: String -> String
bodyNoPrefix "project_id" = "id"
bodyNoPrefix "namespace_id" = "id"
bodyNoPrefix "owner_id" = "id"
bodyNoPrefix "user_id" = "id"
bodyNoPrefix "repository_id" = "id"
bodyNoPrefix "project_name" = "name"
bodyNoPrefix "repository_name" = "name"
bodyNoPrefix "project_path" = "path"
bodyNoPrefix "namespace_name" = "name"
bodyNoPrefix "user_name" = "name"
bodyNoPrefix "namespace_path" = "path"
bodyNoPrefix "owner_name" = "name"
bodyNoPrefix "owner_avatar_url" = "avatar_url"
bodyNoPrefix "user_avatar_url" = "avatar_url"
bodyNoPrefix "owner_web_url" = "web_url"
bodyNoPrefix "job_web_url" = "web_url"
bodyNoPrefix "project_avatar_url" = "avatar_url"
bodyNoPrefix "project_web_url" = "web_url"
bodyNoPrefix "member_id" = "id"
bodyNoPrefix "member_name" = "name"
bodyNoPrefix "member_avatar_url" = "avatar_url"
bodyNoPrefix "member_web_url" = "we_url"
bodyNoPrefix "user_web_url" = "we_url"
bodyNoPrefix "member_username" = "username"
bodyNoPrefix "owner_username" = "username"
bodyNoPrefix "user_username" = "username"
bodyNoPrefix "pipeline_id" = "id"
bodyNoPrefix "pipeline_web_url" = "web_url"
bodyNoPrefix "commit_id" = "id"
bodyNoPrefix "member_state" = "state"
bodyNoPrefix "user_state" = "state"
bodyNoPrefix "issue_state" = "state"
bodyNoPrefix "issue_description" = "description"
bodyNoPrefix "issue_author" = "author"
bodyNoPrefix "issue_project_id" = "project_id"
bodyNoPrefix "issue_id" = "id"
bodyNoPrefix "issue_title" = "title"
bodyNoPrefix "issue_due_date" = "due_date"
bodyNoPrefix "issue_web_url" = "web_url"
bodyNoPrefix "commit_status" = "status"
bodyNoPrefix "pipeline_status" = "status"
bodyNoPrefix "issue_labels" = "labels"
bodyNoPrefix "link_labels" = "labels"
bodyNoPrefix "issue_created_at" = "created_at"
bodyNoPrefix "commit_created_at" = "created_at"
bodyNoPrefix "project_created_at" = "created_at"
bodyNoPrefix "milestone_created_at" = "created_at"
bodyNoPrefix "link_events" = "events"
bodyNoPrefix "repository_type" = "type"
bodyNoPrefix "repository_path" = "path"

bodyNoPrefix "event_title" = "title"
bodyNoPrefix "event_project_id" = "project_id"

bodyNoPrefix "job_ref" = "ref"
bodyNoPrefix "pipeline_ref" = "ref"

bodyNoPrefix "branch_name" = "name"
bodyNoPrefix "branch_default" = "default"
bodyNoPrefix "branch_commit" = "commit"

bodyNoPrefix "repository_file_file_name" = "file_name"
bodyNoPrefix "repository_file_file_path" = "file_path"
bodyNoPrefix "repository_file_size" = "size"
bodyNoPrefix "repository_file_commit_id" = "commit_id"

bodyNoPrefix "merge_request_id" = "id"
bodyNoPrefix "merge_request_iid" = "iid"
bodyNoPrefix "merge_request_project_id" = "project_id"
bodyNoPrefix "merge_request_title" = "title"
bodyNoPrefix "merge_request_description" = "description"
bodyNoPrefix "merge_request_state" = "state"
bodyNoPrefix "merge_request_merged_by" = "merged_by"
bodyNoPrefix "merge_request_merged_at" = "merged_at"
bodyNoPrefix "merge_request_closed_by" = "closed_by"
bodyNoPrefix "merge_request_closed_at" = "closed_at"
bodyNoPrefix "merge_request_created_at" = "created_at"
bodyNoPrefix "merge_request_updated_at" = "updated_at"
bodyNoPrefix "merge_request_target_branch" = "target_branch"
bodyNoPrefix "merge_request_source_branch" = "source_branch"
bodyNoPrefix "merge_request_upvotes" = "upvotes"
bodyNoPrefix "merge_request_downvotes" = "downvotes"
bodyNoPrefix "merge_request_author" = "author"
bodyNoPrefix "merge_request_assignee" = "assignee"
bodyNoPrefix "merge_request_source_project_id" = "source_project_id"
bodyNoPrefix "merge_request_target_project_id" = "target_project_id"
bodyNoPrefix "merge_request_labels" = "labels"
bodyNoPrefix "merge_request_work_in_progress" = "work_in_progress"
bodyNoPrefix "merge_request_milestone" = "milestone"
bodyNoPrefix "merge_request_merge_when_pipeline_succeeds" = "merge_when_pipeline_succeeds"
bodyNoPrefix "merge_request_merge_status" = "merge_status"
bodyNoPrefix "merge_request_sha" = "sha"
bodyNoPrefix "merge_request_merge_commit_sha" = "merge_commit_sha"
bodyNoPrefix "merge_request_user_notes_count" = "user_notes_count"
bodyNoPrefix "merge_request_discussion_locked" = "discussion_locked"
bodyNoPrefix "merge_request_should_remove_source_branch" = "should_remove_source_branch"
bodyNoPrefix "merge_request_force_remove_source_branch" = "force_remove_source_branch"
bodyNoPrefix "merge_request_allow_collaboration" = "allow_collaboration"
bodyNoPrefix "merge_request_allow_maintainer_to_push" = "allow_maintainer_to_push"
bodyNoPrefix "merge_request_web_url" = "web_url"
bodyNoPrefix "merge_request_time_stats" = "time_stats"
bodyNoPrefix "merge_request_squash" = "squash"
bodyNoPrefix "merge_request_approvals_before_merge" = "approvals_before_merge"
bodyNoPrefix "project_stats" = "statistics"
bodyNoPrefix "commit_stats" = "stats"



-- TODO field names for Issues data type
bodyNoPrefix s = s

instance FromJSON TimeStats where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Milestone where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Issue where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON User where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Commit where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON CommitStats where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Pipeline where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Member where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Permissions where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Owner where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Links where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Namespace where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Project where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON ProjectStats where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Repository where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Job where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Artifact where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON Branch where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON RepositoryFile where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })

instance FromJSON MergeRequest where
  parseJSON = genericParseJSON
              (defaultOptions
               { fieldLabelModifier = bodyNoPrefix })
