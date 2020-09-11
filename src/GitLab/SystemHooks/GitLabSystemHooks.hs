{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : GitLab.SystemHooks.GitLabSystemHooks
-- Description : Haskell records corresponding to JSON data from GitLab system hooks
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.SystemHooks.GitLabSystemHooks
  ( receive,
    receiveString,
    tryFire,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import GitLab.SystemHooks.Types
import GitLab.Types
import System.IO

-- | Attempts to fire each rule in sequence. Reads the JSON data
-- received from the GitLab server from standard input.
receive :: [Rule] -> GitLab ()
receive rules = do
  eventContent <- liftIO getContents
  receiveString eventContent rules

-- | Attempts to fire each rule in sequence. Reads the JSON data
-- received from a function argument.
receiveString :: String -> [Rule] -> GitLab ()
receiveString eventContent rules = do
  mapM_ (fire eventContent) rules

orElse :: GitLab Bool -> GitLab Bool -> GitLab Bool
orElse f g = do
  x <- f
  if x
    then return True
    else g

fire :: String -> Rule -> GitLab ()
fire contents rule = do
  result <- tryFire contents rule
  when result $
    -- liftIO (putStrLn ("fired: " <> labelOf rule))
    --
    -- so that it prints these to the GitLab server log file:
    -- /var/log/gitlab/gitlab-rails/plugin.log
    liftIO (hPutStrLn stderr ("fired: " <> labelOf rule))
  where
    labelOf :: Rule -> String
    labelOf (Match lbl _) = lbl
    labelOf (MatchIf lbl _ _) = lbl

-- | Try to fire a GitLab rule, returns 'True' if the rule fired and
-- 'False' if it did not fire.
tryFire :: String -> Rule -> GitLab Bool
tryFire contents (Match _ f) = do
  fireIf'
    (Just (\_ -> return True))
    (cast f :: Maybe (ProjectCreate -> GitLab ()))
    (parseEvent contents :: Maybe ProjectCreate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (ProjectDestroy -> GitLab ()))
      (parseEvent contents :: Maybe ProjectDestroy)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (ProjectRename -> GitLab ()))
      (parseEvent contents :: Maybe ProjectRename)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (ProjectTransfer -> GitLab ()))
      (parseEvent contents :: Maybe ProjectTransfer)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (ProjectUpdate -> GitLab ()))
      (parseEvent contents :: Maybe ProjectUpdate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupMemberUpdate -> GitLab ()))
      (parseEvent contents :: Maybe GroupMemberUpdate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserAddToTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserAddToTeam)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserUpdateForTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserUpdateForTeam)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserRemoveFromTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserRemoveFromTeam)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserCreate -> GitLab ()))
      (parseEvent contents :: Maybe UserCreate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserRemove -> GitLab ()))
      (parseEvent contents :: Maybe UserRemove)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserFailedLogin -> GitLab ()))
      (parseEvent contents :: Maybe UserFailedLogin)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserRename -> GitLab ()))
      (parseEvent contents :: Maybe UserRename)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (KeyCreate -> GitLab ()))
      (parseEvent contents :: Maybe KeyCreate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (KeyRemove -> GitLab ()))
      (parseEvent contents :: Maybe KeyRemove)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupCreate -> GitLab ()))
      (parseEvent contents :: Maybe GroupCreate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupRemove -> GitLab ()))
      (parseEvent contents :: Maybe GroupRemove)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupRename -> GitLab ()))
      (parseEvent contents :: Maybe GroupRename)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (NewGroupMember -> GitLab ()))
      (parseEvent contents :: Maybe NewGroupMember)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupMemberRemove -> GitLab ()))
      (parseEvent contents :: Maybe GroupMemberRemove)
    -- `orElse` fireIf'
    --   (Just (\_ -> return True))
    --   (cast f :: Maybe (ProjectEvent -> GitLab ()))
    --   (parseEvent contents :: Maybe ProjectEvent)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (Push -> GitLab ()))
      (parseEvent contents :: Maybe Push)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (TagPush -> GitLab ()))
      (parseEvent contents :: Maybe TagPush)
tryFire contents (MatchIf _ predF f) = do
  fireIf'
    (cast predF :: Maybe (ProjectCreate -> GitLab Bool))
    (cast f :: Maybe (ProjectCreate -> GitLab ()))
    (parseEvent contents :: Maybe ProjectCreate)
    `orElse` fireIf'
      (cast predF :: Maybe (ProjectDestroy -> GitLab Bool))
      (cast f :: Maybe (ProjectDestroy -> GitLab ()))
      (parseEvent contents :: Maybe ProjectDestroy)
    `orElse` fireIf'
      (cast predF :: Maybe (ProjectRename -> GitLab Bool))
      (cast f :: Maybe (ProjectRename -> GitLab ()))
      (parseEvent contents :: Maybe ProjectRename)
    `orElse` fireIf'
      (cast predF :: Maybe (ProjectTransfer -> GitLab Bool))
      (cast f :: Maybe (ProjectTransfer -> GitLab ()))
      (parseEvent contents :: Maybe ProjectTransfer)
    `orElse` fireIf'
      (cast predF :: Maybe (ProjectUpdate -> GitLab Bool))
      (cast f :: Maybe (ProjectUpdate -> GitLab ()))
      (parseEvent contents :: Maybe ProjectUpdate)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupMemberUpdate -> GitLab Bool))
      (cast f :: Maybe (GroupMemberUpdate -> GitLab ()))
      (parseEvent contents :: Maybe GroupMemberUpdate)
    `orElse` fireIf'
      (cast predF :: Maybe (UserAddToTeam -> GitLab Bool))
      (cast f :: Maybe (UserAddToTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserAddToTeam)
    `orElse` fireIf'
      (cast predF :: Maybe (UserUpdateForTeam -> GitLab Bool))
      (cast f :: Maybe (UserUpdateForTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserUpdateForTeam)
    `orElse` fireIf'
      (cast predF :: Maybe (UserRemoveFromTeam -> GitLab Bool))
      (cast f :: Maybe (UserRemoveFromTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserRemoveFromTeam)
    `orElse` fireIf'
      (cast predF :: Maybe (UserCreate -> GitLab Bool))
      (cast f :: Maybe (UserCreate -> GitLab ()))
      (parseEvent contents :: Maybe UserCreate)
    `orElse` fireIf'
      (cast predF :: Maybe (UserRemove -> GitLab Bool))
      (cast f :: Maybe (UserRemove -> GitLab ()))
      (parseEvent contents :: Maybe UserRemove)
    `orElse` fireIf'
      (cast predF :: Maybe (UserFailedLogin -> GitLab Bool))
      (cast f :: Maybe (UserFailedLogin -> GitLab ()))
      (parseEvent contents :: Maybe UserFailedLogin)
    `orElse` fireIf'
      (cast predF :: Maybe (UserRename -> GitLab Bool))
      (cast f :: Maybe (UserRename -> GitLab ()))
      (parseEvent contents :: Maybe UserRename)
    `orElse` fireIf'
      (cast predF :: Maybe (KeyCreate -> GitLab Bool))
      (cast f :: Maybe (KeyCreate -> GitLab ()))
      (parseEvent contents :: Maybe KeyCreate)
    `orElse` fireIf'
      (cast predF :: Maybe (KeyRemove -> GitLab Bool))
      (cast f :: Maybe (KeyRemove -> GitLab ()))
      (parseEvent contents :: Maybe KeyRemove)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupCreate -> GitLab Bool))
      (cast f :: Maybe (GroupCreate -> GitLab ()))
      (parseEvent contents :: Maybe GroupCreate)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupRemove -> GitLab Bool))
      (cast f :: Maybe (GroupRemove -> GitLab ()))
      (parseEvent contents :: Maybe GroupRemove)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupRename -> GitLab Bool))
      (cast f :: Maybe (GroupRename -> GitLab ()))
      (parseEvent contents :: Maybe GroupRename)
    `orElse` fireIf'
      (cast predF :: Maybe (NewGroupMember -> GitLab Bool))
      (cast f :: Maybe (NewGroupMember -> GitLab ()))
      (parseEvent contents :: Maybe NewGroupMember)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupMemberRemove -> GitLab Bool))
      (cast f :: Maybe (GroupMemberRemove -> GitLab ()))
      (parseEvent contents :: Maybe GroupMemberRemove)
    -- `orElse` fireIf'
    --   (cast predF :: Maybe (ProjectEvent -> GitLab Bool))
    --   (cast f :: Maybe (ProjectEvent -> GitLab ()))
    --   (parseEvent contents :: Maybe ProjectEvent)
    `orElse` fireIf'
      (cast predF :: Maybe (Push -> GitLab Bool))
      (cast f :: Maybe (Push -> GitLab ()))
      (parseEvent contents :: Maybe Push)
    `orElse` fireIf'
      (cast predF :: Maybe (TagPush -> GitLab Bool))
      (cast f :: Maybe (TagPush -> GitLab ()))
      (parseEvent contents :: Maybe TagPush)

fireIf' :: (Typeable a, Show a) => Maybe (a -> GitLab Bool) -> Maybe (a -> GitLab ()) -> Maybe a -> GitLab Bool
fireIf' castPred castF parsed = do
  case castPred of
    Nothing -> return False
    Just pred' ->
      case castF of
        Nothing -> return False
        Just f' ->
          case parsed of
            Nothing -> return False
            Just parsed' -> do
              testPred <- pred' parsed'
              if testPred
                then do
                  f' parsed'
                  return True
                else return False
