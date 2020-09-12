{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SystemHookTests (systemHookTests) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

systemHookTests :: [TestTree]
systemHookTests =
  [parserTests, matchTests, matchIfTests, receiveTests]

parserTests :: TestTree
parserTests =
  testGroup
    "GitLab system hook rules"
    [ testCase
        "project-create-event"
        ( readFile "data/system-hooks/project-created.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectCreatedHaskell
        ),
      testCase
        "project-destroy-event"
        ( readFile "data/system-hooks/project-destroyed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectDestroyedHaskell
        ),
      testCase
        "project-rename-event"
        ( readFile "data/system-hooks/project-renamed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectRenamedHaskell
        ),
      testCase
        "project-transfer-event"
        ( readFile "data/system-hooks/project-transferred.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectTransferredHaskell
        ),
      testCase
        "project-update-event"
        ( readFile "data/system-hooks/project-updated.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectUpdatedHaskell
        ),
      testCase
        "new-team-member-event"
        ( readFile "data/system-hooks/new-team-member.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userAddedToTeamHaskell
        ),
      testCase
        "team-member-removed-event"
        ( readFile "data/system-hooks/team-member-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userRemovedFromTeamHaskell
        ),
      testCase
        "team-member-updated-event"
        ( readFile "data/system-hooks/team-member-updated.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userUpdatedForTeamHaskell
        ),
      testCase
        "user-created-event"
        ( readFile "data/system-hooks/user-created.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userCreatedHaskell
        ),
      testCase
        "user-removed-event"
        ( readFile "data/system-hooks/user-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userRemovedHaskell
        ),
      testCase
        "user-failed-login-event"
        ( readFile "data/system-hooks/user-failed-login.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userFailedLoginHaskell
        ),
      testCase
        "user-renamed-event"
        ( readFile "data/system-hooks/user-renamed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userRenamedHaskell
        ),
      testCase
        "key-added-event"
        ( readFile "data/system-hooks/key-added.json"
            >>= \eventJson -> parseEvent eventJson @?= Just keyCreatedHaskell
        ),
      testCase
        "key-removed-event"
        ( readFile "data/system-hooks/key-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just keyRemovedHaskell
        ),
      testCase
        "group-created-event"
        ( readFile "data/system-hooks/group-created.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupCreatedHaskell
        ),
      testCase
        "group-removed-event"
        ( readFile "data/system-hooks/group-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupRemovedHaskell
        ),
      testCase
        "group-renamed-event"
        ( readFile "data/system-hooks/group-renamed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupRenamedHaskell
        ),
      testCase
        "new-group-member-event"
        ( readFile "data/system-hooks/new-group-member.json"
            >>= \eventJson -> parseEvent eventJson @?= Just newGroupMemberHaskell
        ),
      testCase
        "group-member-removed-event"
        ( readFile "data/system-hooks/group-member-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupMemberRemovedHaskell
        ),
      testCase
        "group-member-updated-event"
        ( readFile "data/system-hooks/group-member-updated.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupMemberUpdatedHaskell
        ),
      testCase
        "push-event"
        ( readFile "data/system-hooks/push.json"
            >>= \eventJson -> parseEvent eventJson @?= Just pushHaskell
        ),
      testCase
        "tag-push-event"
        ( readFile "data/system-hooks/tag-push.json"
            >>= \eventJson -> parseEvent eventJson @?= Just tagPushHaskell
        ),
      testCase
        "merge-request-event"
        ( readFile "data/system-hooks/merge-request.json"
            >>= \eventJson -> parseEvent eventJson @?= Just mergeRequestHaskell
        ),
      testCase
        "repository-update-event"
        ( readFile "data/system-hooks/repository-update.json"
            >>= \eventJson -> parseEvent eventJson @?= Just repositoryUpdateHaskell
        )
    ]

matchTest :: String -> String -> Rule -> String -> Rule -> [TestTree]
matchTest lbl jsonFilename rule wrongJson wrongRule =
  [ testCase lbl $
      runGitLabDbg
        ( liftIO (readFile ("data/system-hooks/" <> jsonFilename))
            >>= \eventJson -> tryFire eventJson rule
        )
        @? (lbl <> " failed"),
    testCase (lbl <> "-wrong-json") $
      not
        <$> runGitLabDbg
          ( liftIO (readFile ("data/system-hooks/" <> wrongJson))
              >>= \eventJson -> tryFire eventJson rule
          )
        @? (lbl <> "-wrong-json failed"),
    testCase (lbl <> "-wrong-rule") $
      not
        <$> runGitLabDbg
          ( liftIO (readFile ("data/system-hooks/" <> jsonFilename))
              >>= \eventJson -> tryFire eventJson wrongRule
          )
        @? (lbl <> "-wrong-rule failed")
  ]

matchIfTest :: String -> String -> Rule -> Rule -> [TestTree]
matchIfTest lbl jsonFilename yesFire noFire =
  [ testCase (lbl <> "-yes") $
      runGitLabDbg
        ( liftIO (readFile ("data/system-hooks/" <> jsonFilename))
            >>= \eventJson -> tryFire eventJson yesFire
        )
        @? (lbl <> "-fireIf-yes failed"),
    testCase (lbl <> "-no") $
      not
        <$> runGitLabDbg
          ( liftIO (readFile ("data/system-hooks/" <> jsonFilename))
              >>= \eventJson -> tryFire eventJson noFire
          )
        @? (lbl <> "-fireIf-no failed")
  ]

-- [ testCase lbl $
--     runGitLabDbg
--       ( liftIO (readFile ("data/system-hooks/" <> jsonFilename))
--           >>= \eventJson -> fire eventJson rule
--       )
--       @? (lbl <> " failed"),
--   testCase (lbl <> "-wrong-json") $
--     not
--       <$> runGitLabDbg
--         ( liftIO (readFile ("data/system-hooks/" <> wrongJson))
--             >>= \eventJson -> fire eventJson rule
--         )
--       @? (lbl <> "-wrong-json failed"),
--   testCase (lbl <> "-wrong-rule") $
--     not
--       <$> runGitLabDbg
--         ( liftIO (readFile ("data/system-hooks/" <> jsonFilename))
--             >>= \eventJson -> fire eventJson wrongRule
--         )
--       @? (lbl <> "-wrong-rule failed")
-- ]

matchTests :: TestTree
matchTests =
  testGroup "GitLab system hook match" $
    matchTest "project-create" "project-created.json" projectCreateRule "project-destroyed.json" projectDestroyRule
      <> matchTest "project-destroy" "project-destroyed.json" projectDestroyRule "project-created.json" projectCreateRule
      <> matchTest "project-rename" "project-renamed.json" projectRenameRule "project-created.json" projectCreateRule
      <> matchTest "project-transfer" "project-transferred.json" projectTransferRule "project-created.json" projectCreateRule
      <> matchTest "project-update" "project-updated.json" projectUpdateRule "project-created.json" projectCreateRule
      <> matchTest "user-add-to-team" "new-team-member.json" userAddToTeamRule "project-created.json" projectCreateRule
      <> matchTest "user-update-for-team" "team-member-updated.json" userUpdateForTeamRule "project-created.json" projectCreateRule
      <> matchTest "user-remove-from-team" "team-member-removed.json" userRemoveFromTeamRule "project-created.json" projectCreateRule
      <> matchTest "user-create" "user-created.json" userCreateRule "project-created.json" projectCreateRule
      <> matchTest "user-remove" "user-removed.json" userRemoveRule "project-created.json" projectCreateRule
      <> matchTest "user-failed-login" "user-failed-login.json" userFailedLoginRule "project-created.json" projectCreateRule
      <> matchTest "user-rename" "user-renamed.json" userRenameRule "project-created.json" projectCreateRule
      <> matchTest "key-create" "key-added.json" keyCreateRule "project-created.json" projectCreateRule
      <> matchTest "key-remove" "key-removed.json" keyRemoveRule "project-created.json" projectCreateRule
      <> matchTest "group-create" "group-created.json" groupCreateRule "project-created.json" projectCreateRule
      <> matchTest "group-remove" "group-removed.json" groupRemoveRule "project-created.json" projectCreateRule
      <> matchTest "group-rename" "group-renamed.json" groupRenameRule "project-created.json" projectCreateRule
      <> matchTest "new-group-member" "new-group-member.json" newGroupMemberRule "project-created.json" projectCreateRule
      <> matchTest "group-member-remove" "group-member-removed.json" groupMemberRemoveRule "project-created.json" projectCreateRule
      <> matchTest "group-member-update" "group-member-updated.json" groupMemberUpdateRule "project-created.json" projectCreateRule
      <> matchTest "push" "push.json" pushRule "project-created.json" projectCreateRule
      <> matchTest "tag-push" "tag-push.json" tagPushRule "project-created.json" projectCreateRule
      <> matchTest "repository-update" "repository-update.json" repositoryUpdateRule "project-created.json" projectCreateRule
      <> matchTest "merge-request" "merge-request.json" mergeRequestRule "project-created.json" projectCreateRule

matchIfTests :: TestTree
matchIfTests =
  testGroup "GitLab system hook matchIf" $
    matchIfTest "project-create" "project-created.json" projectCreateIfRuleYes projectCreateIfRuleNo
      <> matchIfTest "project-destroy" "project-destroyed.json" projectDestroyIfRuleYes projectDestroyIfRuleNo
      <> matchIfTest "project-rename" "project-renamed.json" projectRenameIfRuleYes projectRenameIfRuleNo
      <> matchIfTest "project-transfer" "project-transferred.json" projectTransferIfRuleYes projectTransferIfRuleNo
      <> matchIfTest "project-update" "project-updated.json" projectUpdateIfRuleYes projectUpdateIfRuleNo
      <> matchIfTest "user-add-to-team" "new-team-member.json" userAddToTeamIfRuleYes userAddToTeamIfRuleNo
      <> matchIfTest "user-update-for-team" "team-member-updated.json" userUpdateForTeamIfRuleYes userUpdateForTeamIfRuleNo
      <> matchIfTest "user-remove-from-team" "team-member-removed.json" userRemoveFromTeamIfRuleYes userRemoveFromTeamIfRuleNo
      <> matchIfTest "user-create" "user-created.json" userCreateIfRuleYes userCreateIfRuleNo
      <> matchIfTest "user-remove" "user-removed.json" userRemoveIfRuleYes userRemoveIfRuleNo
      <> matchIfTest "user-failed-login" "user-failed-login.json" userFailedLoginIfRuleYes userFailedLoginIfRuleNo
      <> matchIfTest "user-rename" "user-renamed.json" userRenameIfRuleYes userRenameIfRuleNo
      <> matchIfTest "key-create" "key-added.json" keyCreateIfRuleYes keyCreateIfRuleNo
      <> matchIfTest "key-remove" "key-removed.json" keyRemoveIfRuleYes keyRemoveIfRuleNo
      <> matchIfTest "group-create" "group-created.json" groupCreateIfRuleYes groupCreateIfRuleNo
      <> matchIfTest "group-remove" "group-removed.json" groupRemoveIfRuleYes groupRemoveIfRuleNo
      <> matchIfTest "group-rename" "group-renamed.json" groupRenameIfRuleYes groupRenameIfRuleNo
      <> matchIfTest "new-group-member" "new-group-member.json" newGroupMemberIfRuleYes newGroupMemberIfRuleNo
      <> matchIfTest "group-member-remove" "group-member-removed.json" groupMemberRemoveIfRuleYes groupMemberRemoveIfRuleNo
      <> matchIfTest "group-member-update" "group-member-updated.json" groupMemberUpdateIfRuleYes groupMemberUpdateIfRuleNo
      <> matchIfTest "push" "push.json" pushIfRuleYes pushIfRuleNo
      <> matchIfTest "tag-push" "tag-push.json" tagPushIfRuleYes tagPushIfRuleNo
      <> matchIfTest "repository-update" "repository-update.json" repositoryUpdateIfRuleYes repositoryUpdateIfRuleNo
      <> matchIfTest "merge-request" "merge-request.json" mergeRequestIfRuleYes mergeRequestIfRuleNo

receiveTests :: TestTree
receiveTests =
  testGroup
    "GitLab system hooks receive"
    [ testCase "1-rule-match" $
        runGitLabDbg $
          liftIO (readFile "data/system-hooks/project-created.json")
            >>= \eventJson ->
              receiveString
                eventJson
                [projectCreateRule],
      testCase "1-rule-no-match" $
        runGitLabDbg $
          liftIO (readFile "data/system-hooks/project-created.json")
            >>= \eventJson ->
              receiveString
                eventJson
                [projectRenameRule],
      testCase
        "2-rules"
        $ runGitLabDbg $
          liftIO (readFile "data/system-hooks/project-created.json")
            >>= \eventJson ->
              receiveString
                eventJson
                [ projectCreateRule,
                  projectDestroyRule
                ]
    ]

projectCreateRule :: Rule
projectCreateRule =
  match
    "projectCreate rule"
    ( \ProjectCreate {} -> do
        return ()
    )

projectCreateIfRuleYes :: Rule
projectCreateIfRuleYes =
  matchIf
    "projectCreate-if rule"
    ( \proj@ProjectCreate {} -> do
        return (projectCreate_owner_email proj == "johnsmith@gmail.com")
    )
    ( \ProjectCreate {} -> do
        return ()
    )

projectCreateIfRuleNo :: Rule
projectCreateIfRuleNo =
  matchIf
    "projectCreate-if rule"
    ( \proj@ProjectCreate {} -> do
        return (projectCreate_owner_email proj == "johnsmith@hotmail.com")
    )
    ( \ProjectCreate {} -> do
        return ()
    )

projectDestroyRule :: Rule
projectDestroyRule =
  match
    "projectDestroy rule"
    ( \ProjectDestroy {} -> do
        return ()
    )

projectDestroyIfRuleYes :: Rule
projectDestroyIfRuleYes =
  matchIf
    "projectDestroy rule-if yes"
    ( \event@ProjectDestroy {} -> do
        return
          ( projectDestroy_owner_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \ProjectDestroy {} -> do
        return ()
    )

projectDestroyIfRuleNo :: Rule
projectDestroyIfRuleNo =
  matchIf
    "projectDestroy rule-if no"
    ( \event@ProjectDestroy {} -> do
        return
          ( projectDestroy_owner_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \ProjectDestroy {} -> do
        return ()
    )

projectRenameRule :: Rule
projectRenameRule =
  match
    "projectRename rule"
    ( \ProjectRename {} -> do
        return ()
    )

projectRenameIfRuleYes :: Rule
projectRenameIfRuleYes =
  matchIf
    "projectRename rule-if yes"
    ( \event@ProjectRename {} -> do
        return
          ( projectRename_created_at event
              == "2012-07-21T07:30:58Z"
          )
    )
    ( \ProjectRename {} -> do
        return ()
    )

projectRenameIfRuleNo :: Rule
projectRenameIfRuleNo =
  matchIf
    "projectRename rule-if no"
    ( \event@ProjectRename {} -> do
        return
          ( projectRename_created_at event
              == ""
          )
    )
    ( \ProjectRename {} -> do
        return ()
    )

projectTransferRule :: Rule
projectTransferRule =
  match
    "projectTransfer rule"
    ( \ProjectTransfer {} -> do
        return ()
    )

projectTransferIfRuleYes :: Rule
projectTransferIfRuleYes =
  matchIf
    "projectTransfer rule-if yes"
    ( \event@ProjectTransfer {} -> do
        return
          ( projectTransfer_owner_name event
              == "John Smith"
          )
    )
    ( \ProjectTransfer {} -> do
        return ()
    )

projectTransferIfRuleNo :: Rule
projectTransferIfRuleNo =
  matchIf
    "projectTransfer rule-if no"
    ( \event@ProjectTransfer {} -> do
        return
          ( projectTransfer_owner_name event
              == ""
          )
    )
    ( \ProjectTransfer {} -> do
        return ()
    )

projectUpdateRule :: Rule
projectUpdateRule =
  match
    "projectTransfer rule"
    ( \ProjectUpdate {} -> do
        return ()
    )

projectUpdateIfRuleYes :: Rule
projectUpdateIfRuleYes =
  matchIf
    "projectTransfer rule-if yes"
    ( \event@ProjectUpdate {} -> do
        return
          ( projectUpdate_path event
              == "storecloud"
          )
    )
    ( \ProjectUpdate {} -> do
        return ()
    )

projectUpdateIfRuleNo :: Rule
projectUpdateIfRuleNo =
  matchIf
    "projectTransfer rule-if no"
    ( \event@ProjectUpdate {} -> do
        return
          ( projectUpdate_path event
              == ""
          )
    )
    ( \ProjectUpdate {} -> do
        return ()
    )

groupMemberUpdateRule :: Rule
groupMemberUpdateRule =
  match
    "groupMemberUpdate rule"
    ( \GroupMemberUpdate {} -> do
        return ()
    )

groupMemberUpdateIfRuleYes :: Rule
groupMemberUpdateIfRuleYes =
  matchIf
    "groupMemberUpdate rule-if yes"
    ( \event@GroupMemberUpdate {} -> do
        return
          ( groupMemberUpdate_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \GroupMemberUpdate {} -> do
        return ()
    )

groupMemberUpdateIfRuleNo :: Rule
groupMemberUpdateIfRuleNo =
  matchIf
    "groupMemberUpdate rule-if no"
    ( \event@GroupMemberUpdate {} -> do
        return
          ( groupMemberUpdate_user_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \GroupMemberUpdate {} -> do
        return ()
    )

userAddToTeamRule :: Rule
userAddToTeamRule =
  match
    "userAddToTeam rule"
    ( \UserAddToTeam {} -> do
        return ()
    )

userAddToTeamIfRuleYes :: Rule
userAddToTeamIfRuleYes =
  matchIf
    "userAddToTeam rule-if yes"
    ( \event@UserAddToTeam {} -> do
        return
          ( userAddTeam_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \UserAddToTeam {} -> do
        return ()
    )

userAddToTeamIfRuleNo :: Rule
userAddToTeamIfRuleNo =
  matchIf
    "userAddToTeam rule-if no"
    ( \event@UserAddToTeam {} ->
        return
          ( userAddTeam_user_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \UserAddToTeam {} -> do
        return ()
    )

userUpdateForTeamRule :: Rule
userUpdateForTeamRule =
  match
    "userUpdateForTeam rule"
    ( \UserUpdateForTeam {} -> do
        return ()
    )

userUpdateForTeamIfRuleYes :: Rule
userUpdateForTeamIfRuleYes =
  matchIf
    "userUpdateForTeam rule-if yes"
    ( \event@UserUpdateForTeam {} -> do
        return
          ( userUpdateTeam_project_path_with_namespace event
              == "jsmith/storecloud"
          )
    )
    ( \UserUpdateForTeam {} -> do
        return ()
    )

userUpdateForTeamIfRuleNo :: Rule
userUpdateForTeamIfRuleNo =
  matchIf
    "userUpdateForTeam rule-if no"
    ( \event@UserUpdateForTeam {} -> do
        return
          ( userUpdateTeam_project_path_with_namespace event
              == ""
          )
    )
    ( \UserUpdateForTeam {} -> do
        return ()
    )

userRemoveFromTeamRule :: Rule
userRemoveFromTeamRule =
  match
    "userRemoveFromTeam rule"
    ( \UserRemoveFromTeam {} -> do
        return ()
    )

userRemoveFromTeamIfRuleYes :: Rule
userRemoveFromTeamIfRuleYes =
  matchIf
    "userRemoveFromTeam rule-if yes"
    ( \event@UserRemoveFromTeam {} -> do
        return
          ( userRemoveTeam_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \UserRemoveFromTeam {} -> do
        return ()
    )

userRemoveFromTeamIfRuleNo :: Rule
userRemoveFromTeamIfRuleNo =
  matchIf
    "userRemoveFromTeam rule-if no"
    ( \event@UserRemoveFromTeam {} -> do
        return
          ( userRemoveTeam_user_email event
              == "js@gmail.com"
          )
    )
    ( \UserRemoveFromTeam {} -> do
        return ()
    )

userCreateRule :: Rule
userCreateRule =
  match
    "userCreate rule"
    ( \UserCreate {} -> do
        return ()
    )

userCreateIfRuleYes :: Rule
userCreateIfRuleYes =
  matchIf
    "userCreate rule-if yes"
    ( \event@UserCreate {} -> do
        return
          ( userCreate_email event
              == "js@gitlabhq.com"
          )
    )
    ( \UserCreate {} -> do
        return ()
    )

userCreateIfRuleNo :: Rule
userCreateIfRuleNo =
  matchIf
    "userCreate rule-if no"
    ( \event@UserCreate {} -> do
        return
          ( userCreate_email event
              == "js@gmail.com"
          )
    )
    ( \UserCreate {} -> do
        return ()
    )

userRemoveRule :: Rule
userRemoveRule =
  match
    "userRemove rule"
    ( \UserRemove {} -> do
        return ()
    )

userRemoveIfRuleYes :: Rule
userRemoveIfRuleYes =
  matchIf
    "userRemove rule-if yes"
    ( \event@UserRemove {} -> do
        return
          ( userRemove_email event
              == "js@gitlabhq.com"
          )
    )
    ( \UserRemove {} -> do
        return ()
    )

userRemoveIfRuleNo :: Rule
userRemoveIfRuleNo =
  matchIf
    "userRemove rule-if no"
    ( \event@UserRemove {} -> do
        return
          ( userRemove_email event
              == "js@gmail.com"
          )
    )
    ( \UserRemove {} -> do
        return ()
    )

userFailedLoginRule :: Rule
userFailedLoginRule =
  match
    "userFailedLogin rule"
    ( \UserFailedLogin {} -> do
        return ()
    )

userFailedLoginIfRuleYes :: Rule
userFailedLoginIfRuleYes =
  matchIf
    "userFailedLogin rule"
    ( \event@UserFailedLogin {} -> do
        return
          ( userFailedLogin_email event
              == "user4@example.com"
          )
    )
    ( \UserFailedLogin {} -> do
        return ()
    )

userFailedLoginIfRuleNo :: Rule
userFailedLoginIfRuleNo =
  matchIf
    "userFailedLogin rule"
    ( \event@UserFailedLogin {} -> do
        return
          ( userFailedLogin_email event
              == "user4@gmail.com"
          )
    )
    ( \UserFailedLogin {} -> do
        return ()
    )

userRenameRule :: Rule
userRenameRule =
  match
    "userRename rule"
    ( \UserRename {} -> do
        return ()
    )

userRenameIfRuleYes :: Rule
userRenameIfRuleYes =
  matchIf
    "userRename rule-if yes"
    ( \event@UserRename {} -> do
        return
          ( userRename_username event
              == "new-exciting-name"
          )
    )
    ( \UserRename {} -> do
        return ()
    )

userRenameIfRuleNo :: Rule
userRenameIfRuleNo =
  matchIf
    "userRename rule-if no"
    ( \event@UserRename {} -> do
        return
          ( userRename_username event
              == "old-boring-name"
          )
    )
    ( \UserRename {} -> do
        return ()
    )

keyCreateRule :: Rule
keyCreateRule =
  match
    "keyCreate rule"
    ( \KeyCreate {} -> do
        return ()
    )

keyCreateIfRuleYes :: Rule
keyCreateIfRuleYes =
  matchIf
    "keyCreate rule-if yes"
    ( \event@KeyCreate {} -> do
        return
          ( keyCreate_updated_at event
              == "2012-07-21T07:38:22Z"
          )
    )
    ( \KeyCreate {} -> do
        return ()
    )

keyCreateIfRuleNo :: Rule
keyCreateIfRuleNo =
  matchIf
    "keyCreate rule-if no"
    ( \event@KeyCreate {} -> do
        return
          ( keyCreate_updated_at event
              == ""
          )
    )
    ( \KeyCreate {} -> do
        return ()
    )

keyRemoveRule :: Rule
keyRemoveRule =
  match
    "keyRemove rule"
    ( \KeyRemove {} -> do
        return ()
    )

keyRemoveIfRuleYes :: Rule
keyRemoveIfRuleYes =
  matchIf
    "keyRemove rule-if yes"
    ( \event@KeyRemove {} -> do
        return
          ( keyRemove_updated_at event
              == "2012-07-21T07:38:22Z"
          )
    )
    ( \KeyRemove {} -> do
        return ()
    )

keyRemoveIfRuleNo :: Rule
keyRemoveIfRuleNo =
  matchIf
    "keyRemove rule-if no"
    ( \event@KeyRemove {} -> do
        return
          ( keyRemove_updated_at event
              == ""
          )
    )
    ( \KeyRemove {} -> do
        return ()
    )

groupCreateRule :: Rule
groupCreateRule =
  match
    "groupCreate rule"
    ( \GroupCreate {} -> do
        return ()
    )

groupCreateIfRuleYes :: Rule
groupCreateIfRuleYes =
  matchIf
    "groupCreate rule-if yes"
    ( \event@GroupCreate {} -> do
        return
          ( groupCreate_name event
              == "StoreCloud"
          )
    )
    ( \GroupCreate {} -> do
        return ()
    )

groupCreateIfRuleNo :: Rule
groupCreateIfRuleNo =
  matchIf
    "groupCreate rule-if no"
    ( \event@GroupCreate {} -> do
        return
          ( groupCreate_name event
              == ""
          )
    )
    ( \GroupCreate {} -> do
        return ()
    )

groupRemoveRule :: Rule
groupRemoveRule =
  match
    "groupRemove rule"
    ( \GroupRemove {} -> do
        return ()
    )

groupRemoveIfRuleYes :: Rule
groupRemoveIfRuleYes =
  matchIf
    "groupRemove rule-if yes"
    ( \event@GroupRemove {} -> do
        return
          ( groupRemove_name event
              == "StoreCloud"
          )
    )
    ( \GroupRemove {} -> do
        return ()
    )

groupRemoveIfRuleNo :: Rule
groupRemoveIfRuleNo =
  matchIf
    "groupRemove rule-if no"
    ( \event@GroupRemove {} -> do
        return
          ( groupRemove_name event
              == ""
          )
    )
    ( \GroupRemove {} -> do
        return ()
    )

groupRenameRule :: Rule
groupRenameRule =
  match
    "groupRename rule"
    ( \GroupRename {} -> do
        return ()
    )

groupRenameIfRuleYes :: Rule
groupRenameIfRuleYes =
  matchIf
    "groupRename rule-if yes"
    ( \event@GroupRename {} -> do
        return
          ( groupRename_full_path event
              == "parent-group/better-name"
          )
    )
    ( \GroupRename {} -> do
        return ()
    )

groupRenameIfRuleNo :: Rule
groupRenameIfRuleNo =
  matchIf
    "groupRename rule-if no"
    ( \event@GroupRename {} -> do
        return
          ( groupRename_full_path event
              == "parent-group/wrong-name"
          )
    )
    ( \GroupRename {} -> do
        return ()
    )

newGroupMemberRule :: Rule
newGroupMemberRule =
  match
    "newGroupMember rule"
    ( \NewGroupMember {} -> do
        return ()
    )

newGroupMemberIfRuleYes :: Rule
newGroupMemberIfRuleYes =
  matchIf
    "newGroupMember rule-if yes"
    ( \event@NewGroupMember {} -> do
        return
          ( newGroupMember_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \NewGroupMember {} -> do
        return ()
    )

newGroupMemberIfRuleNo :: Rule
newGroupMemberIfRuleNo =
  matchIf
    "newGroupMember rule-if no"
    ( \event@NewGroupMember {} -> do
        return
          ( newGroupMember_user_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \NewGroupMember {} -> do
        return ()
    )

groupMemberRemoveRule :: Rule
groupMemberRemoveRule =
  match
    "groupMemberRemove rule"
    ( \GroupMemberRemove {} -> do
        return ()
    )

groupMemberRemoveIfRuleYes :: Rule
groupMemberRemoveIfRuleYes =
  matchIf
    "groupMemberRemove rule-if yes"
    ( \event@GroupMemberRemove {} -> do
        return
          ( groupMemberRemove_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \GroupMemberRemove {} -> do
        return ()
    )

groupMemberRemoveIfRuleNo :: Rule
groupMemberRemoveIfRuleNo =
  matchIf
    "groupMemberRemove rule-if no"
    ( \event@GroupMemberRemove {} -> do
        return
          ( groupMemberRemove_user_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \GroupMemberRemove {} -> do
        return ()
    )

pushRule :: Rule
pushRule =
  match
    "push rule"
    ( \Push {} -> do
        return ()
    )

pushIfRuleYes :: Rule
pushIfRuleYes =
  matchIf
    "push rule-if yes"
    ( \event@Push {} -> do
        return (push_checkout_sha event == "da1560886d4f094c3e6c9ef40349f7d38b5d27d7")
    )
    ( \Push {} -> do
        return ()
    )

pushIfRuleNo :: Rule
pushIfRuleNo =
  matchIf
    "push rule-if no"
    ( \event@Push {} -> do
        return (push_checkout_sha event == "")
    )
    ( \Push {} -> do
        return ()
    )

tagPushRule :: Rule
tagPushRule =
  match
    "tagPush rule"
    ( \TagPush {} -> do
        return ()
    )

tagPushIfRuleYes :: Rule
tagPushIfRuleYes =
  matchIf
    "tagPush rule-if yes"
    ( \event@TagPush {} -> do
        return (tagPush_ref event == "refs/tags/v1.0.0")
    )
    ( \TagPush {} -> do
        return ()
    )

tagPushIfRuleNo :: Rule
tagPushIfRuleNo =
  matchIf
    "tagPush rule-if no"
    ( \event@TagPush {} -> do
        return (tagPush_ref event == "refs/tags/v2.0.0")
    )
    ( \TagPush {} -> do
        return ()
    )

repositoryUpdateRule :: Rule
repositoryUpdateRule =
  match
    "repositoryUpdate rule"
    ( \RepositoryUpdate {} -> do
        return ()
    )

repositoryUpdateIfRuleYes :: Rule
repositoryUpdateIfRuleYes =
  matchIf
    "repositoryUpdate rule-if yes"
    ( \event@RepositoryUpdate {} -> do
        return (repositoryUpdate_refs event == ["refs/heads/master"])
    )
    ( \RepositoryUpdate {} -> do
        return ()
    )

repositoryUpdateIfRuleNo :: Rule
repositoryUpdateIfRuleNo =
  matchIf
    "repositoryUpdate rule-if no"
    ( \event@RepositoryUpdate {} -> do
        return (repositoryUpdate_refs event == ["refs/heads/branch-1"])
    )
    ( \RepositoryUpdate {} -> do
        return ()
    )

mergeRequestRule :: Rule
mergeRequestRule =
  match
    "mergeRequest rule"
    ( \MergeRequestEvent {} -> do
        return ()
    )

mergeRequestIfRuleYes :: Rule
mergeRequestIfRuleYes =
  matchIf
    "mergeRequest rule-if yes"
    ( \event@MergeRequestEvent {} -> do
        return (userEvent_name (mergeRequest_user event) == "Administrator")
    )
    ( \MergeRequestEvent {} -> do
        return ()
    )

mergeRequestIfRuleNo :: Rule
mergeRequestIfRuleNo =
  matchIf
    "mergeRequest rule-if no"
    ( \event@MergeRequestEvent {} -> do
        return (userEvent_name (mergeRequest_user event) == "joe")
    )
    ( \MergeRequestEvent {} -> do
        return ()
    )

projectCreatedHaskell :: ProjectCreate
projectCreatedHaskell =
  ProjectCreate
    { projectCreate_created_at = "2012-07-21T07:30:54Z",
      projectCreate_updated_at = "2012-07-21T07:38:22Z",
      projectCreate_action = "project_create",
      projectCreate_name = "StoreCloud",
      projectCreate_owner_email = "johnsmith@gmail.com",
      projectCreate_owner_name = "John Smith",
      projectCreate_path = "storecloud",
      projectCreate_path_with_namespace = "jsmith/storecloud",
      projectCreate_project_id = 74,
      projectCreate_project_visibility = Private
    }

projectDestroyedHaskell :: ProjectDestroy
projectDestroyedHaskell =
  ProjectDestroy
    { projectDestroy_created_at = "2012-07-21T07:30:58Z",
      projectDestroy_updated_at = "2012-07-21T07:38:22Z",
      projectDestroy_action = "project_destroy",
      projectDestroy_name = "Underscore",
      projectDestroy_owner_email = "johnsmith@gmail.com",
      projectDestroy_owner_name = "John Smith",
      projectDestroy_path = "underscore",
      projectDestroy_path_with_namespace = "jsmith/underscore",
      projectDestroy_project_id = 73,
      projectDestroy_project_visibility = Internal
    }

projectRenamedHaskell :: ProjectRename
projectRenamedHaskell =
  ProjectRename
    { projectRename_created_at = "2012-07-21T07:30:58Z",
      projectRename_updated_at = "2012-07-21T07:38:22Z",
      projectRename_event_name = "project_rename",
      projectRename_name = "Underscore",
      projectRename_path = "underscore",
      projectRename_path_with_namespace = "jsmith/underscore",
      projectRename_project_id = 73,
      projectRename_owner_name = "John Smith",
      projectRename_owner_email = "johnsmith@gmail.com",
      projectRename_project_visibility = Internal,
      projectRename_old_path_with_namespace = "jsmith/overscore"
    }

projectTransferredHaskell :: ProjectTransfer
projectTransferredHaskell =
  ProjectTransfer
    { projectTransfer_created_at = "2012-07-21T07:30:58Z",
      projectTransfer_updated_at = "2012-07-21T07:38:22Z",
      projectTransfer_event_name = "project_transfer",
      projectTransfer_name = "Underscore",
      projectTransfer_path = "underscore",
      projectTransfer_path_with_namespace = "scores/underscore",
      projectTransfer_project_id = 73,
      projectTransfer_owner_name = "John Smith",
      projectTransfer_owner_email = "johnsmith@gmail.com",
      projectTransfer_project_visibility = Internal,
      projectTransfer_old_path_with_namespace = "jsmith/overscore"
    }

projectUpdatedHaskell :: ProjectUpdate
projectUpdatedHaskell =
  ProjectUpdate
    { projectUpdate_created_at = "2012-07-21T07:30:54Z",
      projectUpdate_updated_at = "2012-07-21T07:38:22Z",
      projectUpdate_event_name = "project_update",
      projectUpdate_name = "StoreCloud",
      projectUpdate_owner_email = "johnsmith@gmail.com",
      projectUpdate_owner_name = "John Smith",
      projectUpdate_path = "storecloud",
      projectUpdate_path_with_namespace = "jsmith/storecloud",
      projectUpdate_project_id = 74,
      projectUpdate_project_visibility = Private
    }

userAddedToTeamHaskell :: UserAddToTeam
userAddedToTeamHaskell =
  UserAddToTeam
    { userAddTeam_created_at = "2012-07-21T07:30:56Z",
      userAddTeam_updated_at = "2012-07-21T07:38:22Z",
      userAddTeam_event_name = "user_add_to_team",
      userAddTeam_access_level = "Maintainer",
      userAddTeam_project_id = 74,
      userAddTeam_project_name = "StoreCloud",
      userAddTeam_project_path = "storecloud",
      userAddTeam_project_path_with_namespace = "jsmith/storecloud",
      userAddTeam_user_email = "johnsmith@gmail.com",
      userAddTeam_user_name = "John Smith",
      userAddTeam_user_username = "johnsmith",
      userAddTeam_user_id = 41,
      userAddTeam_project_visibility = Private
    }

userUpdatedForTeamHaskell :: UserUpdateForTeam
userUpdatedForTeamHaskell =
  UserUpdateForTeam
    { userUpdateTeam_created_at = "2012-07-21T07:30:56Z",
      userUpdateTeam_updated_at = "2012-07-21T07:38:22Z",
      userUpdateTeam_event_name = "user_update_for_team",
      userUpdateTeam_access_level = "Maintainer",
      userUpdateTeam_project_id = 74,
      userUpdateTeam_project_name = "StoreCloud",
      userUpdateTeam_project_path = "storecloud",
      userUpdateTeam_project_path_with_namespace = "jsmith/storecloud",
      userUpdateTeam_user_email = "johnsmith@gmail.com",
      userUpdateTeam_user_name = "John Smith",
      userUpdateTeam_user_username = "johnsmith",
      userUpdateTeam_user_id = 41,
      userUpdateTeam_project_visibility = Private
    }

userRemovedFromTeamHaskell :: UserRemoveFromTeam
userRemovedFromTeamHaskell =
  UserRemoveFromTeam
    { userRemoveTeam_created_at = "2012-07-21T07:30:56Z",
      userRemoveTeam_updated_at = "2012-07-21T07:38:22Z",
      userRemoveTeam_event_name = "user_remove_from_team",
      userRemoveTeam_access_level = "Maintainer",
      userRemoveTeam_project_id = 74,
      userRemoveTeam_project_name = "StoreCloud",
      userRemoveTeam_project_path = "storecloud",
      userRemoveTeam_project_path_with_namespace = "jsmith/storecloud",
      userRemoveTeam_user_email = "johnsmith@gmail.com",
      userRemoveTeam_user_name = "John Smith",
      userRemoveTeam_user_username = "johnsmith",
      userRemoveTeam_user_id = 41,
      userRemoveTeam_project_visibility = Private
    }

userCreatedHaskell :: UserCreate
userCreatedHaskell =
  UserCreate
    { userCreate_created_at = "2012-07-21T07:44:07Z",
      userCreate_updated_at = "2012-07-21T07:38:22Z",
      userCreate_email = "js@gitlabhq.com",
      userCreate_event_name = "user_create",
      userCreate_name = "John Smith",
      userCreate_username = "js",
      userCreate_user_id = 41
    }

userRemovedHaskell :: UserRemove
userRemovedHaskell =
  UserRemove
    { userRemove_created_at = "2012-07-21T07:44:07Z",
      userRemove_updated_at = "2012-07-21T07:38:22Z",
      userRemove_email = "js@gitlabhq.com",
      userRemove_event_name = "user_destroy",
      userRemove_name = "John Smith",
      userRemove_username = "js",
      userRemove_user_id = 41
    }

userFailedLoginHaskell :: UserFailedLogin
userFailedLoginHaskell =
  UserFailedLogin
    { userFailedLogin_event_name = "user_failed_login",
      userFailedLogin_created_at = "2017-10-03T06:08:48Z",
      userFailedLogin_updated_at = "2018-01-15T04:52:06Z",
      userFailedLogin_name = "John Smith",
      userFailedLogin_email = "user4@example.com",
      userFailedLogin_user_id = 26,
      userFailedLogin_username = "user4",
      userFailedLogin_state = "blocked"
    }

userRenamedHaskell :: UserRename
userRenamedHaskell =
  UserRename
    { userRename_event_name = "user_rename",
      userRename_created_at = "2017-11-01T11:21:04Z",
      userRename_updated_at = "2017-11-01T14:04:47Z",
      userRename_name = "new-name",
      userRename_email = "best-email@example.tld",
      userRename_user_id = 58,
      userRename_username = "new-exciting-name",
      userRename_old_username = "old-boring-name"
    }

keyCreatedHaskell :: KeyCreate
keyCreatedHaskell =
  KeyCreate
    { keyCreate_event_name = "key_create",
      keyCreate_created_at = "2014-08-18 18:45:16 UTC",
      keyCreate_updated_at = "2012-07-21T07:38:22Z",
      keyCreate_username = "root",
      keyCreate_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC58FwqHUbebw2SdT7SP4FxZ0w+lAO/erhy2ylhlcW/tZ3GY3mBu9VeeiSGoGz8hCx80Zrz+aQv28xfFfKlC8XQFpCWwsnWnQqO2Lv9bS8V1fIHgMxOHIt5Vs+9CAWGCCvUOAurjsUDoE2ALIXLDMKnJxcxD13XjWdK54j6ZXDB4syLF0C2PnAQSVY9X7MfCYwtuFmhQhKaBussAXpaVMRHltie3UYSBUUuZaB3J4cg/7TxlmxcNd+ppPRIpSZAB0NI6aOnqoBCpimscO/VpQRJMVLr3XiSYeT6HBiDXWHnIVPfQc03OGcaFqOit6p8lYKMaP/iUQLm+pgpZqrXZ9vB john@localhost",
      keyCreate_id = 4
    }

keyRemovedHaskell :: KeyRemove
keyRemovedHaskell =
  KeyRemove
    { keyRemove_event_name = "key_destroy",
      keyRemove_created_at = "2014-08-18 18:45:16 UTC",
      keyRemove_updated_at = "2012-07-21T07:38:22Z",
      keyRemove_username = "root",
      keyRemove_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC58FwqHUbebw2SdT7SP4FxZ0w+lAO/erhy2ylhlcW/tZ3GY3mBu9VeeiSGoGz8hCx80Zrz+aQv28xfFfKlC8XQFpCWwsnWnQqO2Lv9bS8V1fIHgMxOHIt5Vs+9CAWGCCvUOAurjsUDoE2ALIXLDMKnJxcxD13XjWdK54j6ZXDB4syLF0C2PnAQSVY9X7MfCYwtuFmhQhKaBussAXpaVMRHltie3UYSBUUuZaB3J4cg/7TxlmxcNd+ppPRIpSZAB0NI6aOnqoBCpimscO/VpQRJMVLr3XiSYeT6HBiDXWHnIVPfQc03OGcaFqOit6p8lYKMaP/iUQLm+pgpZqrXZ9vB john@localhost",
      keyRemove_id = 4
    }

groupCreatedHaskell :: GroupCreate
groupCreatedHaskell =
  GroupCreate
    { groupCreate_created_at = "2012-07-21T07:30:54Z",
      groupCreate_updated_at = "2012-07-21T07:38:22Z",
      groupCreate_event_name = "group_create",
      groupCreate_name = "StoreCloud",
      groupCreate_owner_email = Nothing,
      groupCreate_owner_name = Nothing,
      groupCreate_path = "storecloud",
      groupCreate_group_id = 78
    }

groupRemovedHaskell :: GroupRemove
groupRemovedHaskell =
  GroupRemove
    { groupRemove_created_at = "2012-07-21T07:30:54Z",
      groupRemove_updated_at = "2012-07-21T07:38:22Z",
      groupRemove_event_name = "group_destroy",
      groupRemove_name = "StoreCloud",
      groupRemove_owner_email = Nothing,
      groupRemove_owner_name = Nothing,
      groupRemove_path = "storecloud",
      groupRemove_group_id = 78
    }

groupRenamedHaskell :: GroupRename
groupRenamedHaskell =
  GroupRename
    { groupRename_event_name = "group_rename",
      groupRename_created_at = "2017-10-30T15:09:00Z",
      groupRename_updated_at = "2017-11-01T10:23:52Z",
      groupRename_name = "Better Name",
      groupRename_path = "better-name",
      groupRename_full_path = "parent-group/better-name",
      groupRename_group_id = 64,
      groupRename_owner_name = Nothing,
      groupRename_owner_email = Nothing,
      groupRename_old_path = "old-name",
      groupRename_old_full_path = "parent-group/old-name"
    }

newGroupMemberHaskell :: NewGroupMember
newGroupMemberHaskell =
  NewGroupMember
    { newGroupMember_created_at = "2012-07-21T07:30:56Z",
      newGroupMember_updated_at = "2012-07-21T07:38:22Z",
      newGroupMember_event_name = "user_add_to_group",
      newGroupMember_group_access = "Maintainer",
      newGroupMember_group_id = 78,
      newGroupMember_group_name = "StoreCloud",
      newGroupMember_group_path = "storecloud",
      newGroupMember_user_email = "johnsmith@gmail.com",
      newGroupMember_user_name = "John Smith",
      newGroupMember_user_username = "johnsmith",
      newGroupMember_user_id = 41
    }

groupMemberRemovedHaskell :: GroupMemberRemove
groupMemberRemovedHaskell =
  GroupMemberRemove
    { groupMemberRemove_created_at = "2012-07-21T07:30:56Z",
      groupMemberRemove_updated_at = "2012-07-21T07:38:22Z",
      groupMemberRemove_event_name = "user_remove_from_group",
      groupMemberRemove_group_access = "Maintainer",
      groupMemberRemove_group_id = 78,
      groupMemberRemove_group_name = "StoreCloud",
      groupMemberRemove_group_path = "storecloud",
      groupMemberRemove_user_email = "johnsmith@gmail.com",
      groupMemberRemove_user_name = "John Smith",
      groupMemberRemove_user_username = "johnsmith",
      groupMemberRemove_user_id = 41
    }

groupMemberUpdatedHaskell :: GroupMemberUpdate
groupMemberUpdatedHaskell =
  GroupMemberUpdate
    { groupMemberUpdate_created_at = "2012-07-21T07:30:56Z",
      groupMemberUpdate_updated_at = "2012-07-21T07:38:22Z",
      groupMemberUpdate_event_name = "user_update_for_group",
      groupMemberUpdate_group_access = "Maintainer",
      groupMemberUpdate_group_id = 78,
      groupMemberUpdate_group_name = "StoreCloud",
      groupMemberUpdate_group_path = "storecloud",
      groupMemberUpdate_user_email = "johnsmith@gmail.com",
      groupMemberUpdate_user_name = "John Smith",
      groupMemberUpdate_user_username = "johnsmith",
      groupMemberUpdate_user_id = 41
    }

pushHaskell :: Push
pushHaskell =
  Push
    { push_event_name = "push",
      push_before = "95790bf891e76fee5e1747ab589903a6a1f80f22",
      push_after = "da1560886d4f094c3e6c9ef40349f7d38b5d27d7",
      push_ref = "refs/heads/master",
      push_checkout_sha = "da1560886d4f094c3e6c9ef40349f7d38b5d27d7",
      push_user_id = 4,
      push_user_name = "John Smith",
      push_user_email = "john@example.com",
      push_user_avatar = "https://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=8://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=80",
      push_project_id = 15,
      push_project = ProjectEvent {projectEvent_name = "Diaspora", projectEvent_description = "", projectEvent_web_url = "http://example.com/mike/diaspora", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:mike/diaspora.git", projectEvent_git_http_url = "http://example.com/mike/diaspora.git", projectEvent_namespace = "Mike", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "mike/diaspora", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/mike/diaspora", projectEvent_url = "git@example.com:mike/diaspora.git", projectEvent_ssh_url = "git@example.com:mike/diaspora.git", projectEvent_http_url = "http://example.com/mike/diaspora.git"},
      push_repository = RepositoryEvent {repositoryEvent_name = "Diaspora", repositoryEvent_url = "git@example.com:mike/diaspora.git", repositoryEvent_description = "", repositoryEvent_homepage = Just "http://example.com/mike/diaspora", repositoryEvent_git_http_url = Just "http://example.com/mike/diaspora.git", repositoryEvent_git_ssh_url = Just "git@example.com:mike/diaspora.git", repositoryEvent_visibility_level = Just Private},
      push_commits = [CommitEvent {commitEvent_id = "c5feabde2d8cd023215af4d2ceeb7a64839fc428", commitEvent_message = "Add simple search to projects in public area", commitEvent_timestamp = "2013-05-13T18:18:08+00:00", commitEvent_url = "https://dev.gitlab.org/gitlab/gitlabhq/commit/c5feabde2d8cd023215af4d2ceeb7a64839fc428", commitEvent_author = CommitAuthorEvent {commitAuthorEvent_name = "Example User", commitAuthorEvent_email = "user@example.com"}}],
      push_total_commits_count = 1
    }

tagPushHaskell :: TagPush
tagPushHaskell =
  TagPush
    { tagPush_event_name = "tag_push",
      tagPush_before = "0000000000000000000000000000000000000000",
      tagPush_after = "82b3d5ae55f7080f1e6022629cdb57bfae7cccc7",
      tagPush_ref = "refs/tags/v1.0.0",
      tagPush_checkout_sha = "5937ac0a7beb003549fc5fd26fc247adbce4a52e",
      tagPush_user_id = 1,
      tagPush_user_name = "John Smith",
      tagPush_user_avatar = "https://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=8://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=80",
      tagPush_project_id = 1,
      tagPush_project = ProjectEvent {projectEvent_name = "Example", projectEvent_description = "", projectEvent_web_url = "http://example.com/jsmith/example", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:jsmith/example.git", projectEvent_git_http_url = "http://example.com/jsmith/example.git", projectEvent_namespace = "Jsmith", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "jsmith/example", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/jsmith/example", projectEvent_url = "git@example.com:jsmith/example.git", projectEvent_ssh_url = "git@example.com:jsmith/example.git", projectEvent_http_url = "http://example.com/jsmith/example.git"},
      tagPush_repository = RepositoryEvent {repositoryEvent_name = "Example", repositoryEvent_url = "ssh://git@example.com/jsmith/example.git", repositoryEvent_description = "", repositoryEvent_homepage = Just "http://example.com/jsmith/example", repositoryEvent_git_http_url = Just "http://example.com/jsmith/example.git", repositoryEvent_git_ssh_url = Just "git@example.com:jsmith/example.git", repositoryEvent_visibility_level = Just Private},
      tagPush_commits = [],
      tagPush_total_commits_count = 0
    }

repositoryUpdateHaskell :: RepositoryUpdate
repositoryUpdateHaskell =
  RepositoryUpdate
    { repositoryUpdate_event_name = "repository_update",
      repositoryUpdate_user_id = 1,
      repositoryUpdate_user_name = "John Smith",
      repositoryUpdate_user_email = "admin@example.com",
      repositoryUpdate_user_avatar = "https://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=8://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=80",
      repositoryUpdate_project_id = 1,
      repositoryUpdate_project = ProjectEvent {projectEvent_name = "Example", projectEvent_description = "", projectEvent_web_url = "http://example.com/jsmith/example", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:jsmith/example.git", projectEvent_git_http_url = "http://example.com/jsmith/example.git", projectEvent_namespace = "Jsmith", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "jsmith/example", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/jsmith/example", projectEvent_url = "git@example.com:jsmith/example.git", projectEvent_ssh_url = "git@example.com:jsmith/example.git", projectEvent_http_url = "http://example.com/jsmith/example.git"},
      repositoryUpdate_changes = [ProjectChanges {projectChanges_before = "8205ea8d81ce0c6b90fbe8280d118cc9fdad6130", projectChanges_after = "4045ea7a3df38697b3730a20fb73c8bed8a3e69e", projectChanges_ref = "refs/heads/master"}],
      repositoryUpdate_refs = ["refs/heads/master"]
    }

mergeRequestHaskell :: MergeRequestEvent
mergeRequestHaskell =
  MergeRequestEvent
    { mergeRequest_object_kind = "merge_request",
      mergeRequest_user = UserEvent {userEvent_name = "Administrator", userEvent_username = "root", userEvent_avatar_url = "http://www.gravatar.com/avatar/e64c7d89f26bd1972efa854d13d7dd61?s=80&d=identicon"},
      mergeRequest_project = ProjectEvent {projectEvent_name = "Example", projectEvent_description = "", projectEvent_web_url = "http://example.com/jsmith/example", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:jsmith/example.git", projectEvent_git_http_url = "http://example.com/jsmith/example.git", projectEvent_namespace = "Jsmith", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "jsmith/example", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/jsmith/example", projectEvent_url = "git@example.com:jsmith/example.git", projectEvent_ssh_url = "git@example.com:jsmith/example.git", projectEvent_http_url = "http://example.com/jsmith/example.git"},
      mergeRequest_object_attributes = ObjectAttributes {objectAttributes_id = 90, objectAttributes_target_branch = "master", objectAttributes_source_branch = "ms-viewport", objectAttributes_source_project_id = 14, objectAttributes_author_id = 51, objectAttributes_assignee_id = 6, objectAttributes_title = "MS-Viewport", objectAttributes_created_at = "2017-09-20T08:31:45.944Z", objectAttributes_updated_at = "2017-09-28T12:23:42.365Z", objectAttributes_milestone_id = Nothing, objectAttributes_state = "opened", objectAttributes_merge_status = "unchecked", objectAttributes_target_project_id = 14, objectAttributes_iid = 1, objectAttributes_description = "", objectAttributes_updated_by_id = Just 1, objectAttributes_merge_error = Nothing, objectAttributes_merge_params = MergeParams {mergeParams_force_remove_source_branch = "0"}, objectAttributes_merge_when_pipeline_succeeds = False, objectAttributes_merge_user_id = Nothing, objectAttributes_merge_commit_sha = Nothing, objectAttributes_deleted_at = Nothing, objectAttributes_in_progress_merge_commit_sha = Nothing, objectAttributes_lock_version = Just 5, objectAttributes_time_estimate = 0, objectAttributes_last_edited_at = "2017-09-27T12:43:37.558Z", objectAttributes_last_edited_by_id = 1, objectAttributes_head_pipeline_id = 61, objectAttributes_ref_fetched = True, objectAttributes_merge_jid = Nothing, objectAttributes_source = ProjectEvent {projectEvent_name = "Awesome Project", projectEvent_description = "", projectEvent_web_url = "http://example.com/awesome_space/awesome_project", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:awesome_space/awesome_project.git", projectEvent_git_http_url = "http://example.com/awesome_space/awesome_project.git", projectEvent_namespace = "root", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "awesome_space/awesome_project", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/awesome_space/awesome_project", projectEvent_url = "http://example.com/awesome_space/awesome_project.git", projectEvent_ssh_url = "git@example.com:awesome_space/awesome_project.git", projectEvent_http_url = "http://example.com/awesome_space/awesome_project.git"}, objectAttributes_target = ProjectEvent {projectEvent_name = "Awesome Project", projectEvent_description = "Aut reprehenderit ut est.", projectEvent_web_url = "http://example.com/awesome_space/awesome_project", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:awesome_space/awesome_project.git", projectEvent_git_http_url = "http://example.com/awesome_space/awesome_project.git", projectEvent_namespace = "Awesome Space", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "awesome_space/awesome_project", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/awesome_space/awesome_project", projectEvent_url = "http://example.com/awesome_space/awesome_project.git", projectEvent_ssh_url = "git@example.com:awesome_space/awesome_project.git", projectEvent_http_url = "http://example.com/awesome_space/awesome_project.git"}, objectAttributes_last_commit = CommitEvent {commitEvent_id = "ba3e0d8ff79c80d5b0bbb4f3e2e343e0aaa662b7", commitEvent_message = "fixed readme", commitEvent_timestamp = "2017-09-26T16:12:57Z", commitEvent_url = "http://example.com/awesome_space/awesome_project/commits/da1560886d4f094c3e6c9ef40349f7d38b5d27d7", commitEvent_author = CommitAuthorEvent {commitAuthorEvent_name = "GitLab dev user", commitAuthorEvent_email = "gitlabdev@dv6700.(none)"}}, objectAttributes_work_in_progress = False, objectAttributes_total_time_spent = 0, objectAttributes_human_total_time_spent = Nothing, objectAttributes_human_time_estimate = Nothing},
      mergeRequest_labels = Nothing,
      mergeRequest_repository = RepositoryEvent {repositoryEvent_name = "git-gpg-test", repositoryEvent_url = "git@example.com:awesome_space/awesome_project.git", repositoryEvent_description = "", repositoryEvent_homepage = Just "http://example.com/awesome_space/awesome_project", repositoryEvent_git_http_url = Nothing, repositoryEvent_git_ssh_url = Nothing, repositoryEvent_visibility_level = Nothing}
    }
