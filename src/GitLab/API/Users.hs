{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Users
Description : Queries about registered users
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Users where

import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Text (Text)

import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | all registered users.
allUsers :: (MonadIO m) => GitLab m [User]
allUsers = do
    let path = "/users"
    gitlab path

-- | searches for a user given a username. Returns @Just User@ if the
-- user is found, otherwise @Nothing@.
searchUser :: (MonadIO m)
  => Text -- ^ username to search for
  -> GitLab m (Maybe User)
searchUser username = do
  let path = "/users"
      attrs = "&username=" <> username
  res <- gitlabWithAttrs path attrs
  if null res
  then return Nothing
  else return (Just (head res))

-- | searches for users given a list of usernames, returns them in
-- alphabetical order of their usernames.
orderedUsers :: (MonadIO m)
  => [Text] -- ^ usernames to search for
  -> GitLab m [User]
orderedUsers usernames = do
  users <- catMaybes <$> mapM searchUser usernames
  return (orderUsersByName users)
  where
    orderUsersByName :: [User] -> [User]
    orderUsersByName =
      sortBy (\u1 u2 -> compare (user_name u1) (user_name u2))
