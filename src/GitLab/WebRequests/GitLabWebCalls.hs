{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | internal module to support modules in GitLab.API
module GitLab.WebRequests.GitLabWebCalls
  (
    gitlab
  , gitlabWithAttrs
  , gitlabOne
  , gitlabWithAttrsOne
  , gitlabPost
  , gitlabReqText
  ) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import qualified Control.Exception as E
import Network.HTTP.Types.Status
import GitLab.Types
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 as C

gitlabPost ::
  (MonadIO m, FromJSON b) =>
  Text -- ^ the URL to post to
  -> Text -- ^ the data to post
  -> GitLab m (Either Status b)
gitlabPost urlPath dataBody = do
  cfg <- serverCfg <$> ask
  manager <- httpManager <$> ask
  let url' = url cfg <> "/api/v4" <> urlPath
  let request' = parseRequest_ (T.unpack url')
      request = request'
                { method = "POST"
                , requestHeaders =
                  [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg))]
                , requestBody = RequestBodyBS (T.encodeUtf8 dataBody) }
  res <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
  case responseStatus res of
    resp@(Status 404 _msg) -> return (Left resp)
    resp@(Status 409 _msg) -> return (Left resp)
    _ -> return (case parseBSOne (responseBody res) of
                   Just x -> Right x
                   Nothing -> Left $
                     mkStatus 409 "unable to parse POST response")

tryGitLab ::
     Int -- ^ the current retry count
  -> Request -- ^ the GitLab request
  -> Int -- ^ maximum number of retries permitted
  -> Manager -- ^ HTTP manager
  -> Maybe HttpException -- ^ the exception to report if maximum retries met
  -> IO (Response BSL.ByteString)
tryGitLab i request maxRetries manager lastException
  | i == maxRetries = error (show lastException)
  | otherwise =
      httpLbs request manager
      `E.catch`
        \ex -> tryGitLab (i+1) request maxRetries manager (Just ex)

parseBSOne :: FromJSON a => BSL.ByteString -> Maybe a
parseBSOne bs =
  case eitherDecode bs of
    Left _err -> Nothing
      -- useful when debugging
      -- error (show _err)
    Right xs -> Just xs

parseBSMany :: FromJSON a => BSL.ByteString -> [a]
parseBSMany bs =
  case eitherDecode bs of
    Left s -> error s
    Right xs -> xs

gitlabReq :: (MonadIO m, FromJSON a) => Text -> Text -> GitLab m [a]
gitlabReq urlPath attrs =
  go 1 []
  where
    go i accum = do
      cfg <- serverCfg <$> ask
      manager <- httpManager <$> ask
      let url' =
               url cfg
            <> "/api/v4"
            <> urlPath
            <> "?per_page=100"
            <> "&page="
            <> T.pack (show i)
            <> attrs
      let request' = parseRequest_ (T.unpack url')
          request = request'
            { requestHeaders =
              [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg))]
            , responseTimeout = responseTimeoutMicro (timeout cfg)
            }
      res <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
      let numPages = totalPages res
          accum' = accum ++ parseBSMany (responseBody res)
      if numPages == i
      then return accum'
      else go (i+1) accum'

gitlabReqOne :: (MonadIO m, FromJSON a) => Text -> Text -> GitLab m (Maybe a)
gitlabReqOne urlPath attrs = do
  go
  where
    go = do
      cfg <- serverCfg <$> ask
      manager <- httpManager <$> ask
      let url' =
               url cfg
            <> "/api/v4"
            <> urlPath
            <> "?per_page=100"
            <> "&page=1"
            <> attrs
      let request' = parseRequest_ (T.unpack url')
          request = request'
            { requestHeaders =
              [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg))]
            , responseTimeout = responseTimeoutMicro (timeout cfg)
            }
      res <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
      return (parseBSOne (responseBody res))

gitlabReqText :: (MonadIO m) => Text -> GitLab m String
gitlabReqText urlPath = do
  go
  where
    go = do
      cfg <- serverCfg <$> ask
      manager <- httpManager <$> ask
      let url' =
               url cfg
            <> "/api/v4"
            <> urlPath
            <> "?per_page=100"
            <> "&page=1"
      let request' = parseRequest_ (T.unpack url')
          request = request'
            { requestHeaders =
              [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg))]
            , responseTimeout = responseTimeoutMicro (timeout cfg)
            }
      res <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
      return (C.unpack (responseBody res))

gitlab :: (MonadIO m, FromJSON a) => Text -> GitLab m [a]
gitlab addr = gitlabReq addr ""

gitlabOne :: (MonadIO m, FromJSON a) => Text -> GitLab m (Maybe a)
gitlabOne addr = gitlabReqOne addr ""

gitlabWithAttrs :: (MonadIO m, FromJSON a) => Text -> Text -> GitLab m [a]
gitlabWithAttrs  = gitlabReq

gitlabWithAttrsOne :: (MonadIO m, FromJSON a) => Text -> Text -> GitLab m (Maybe a)
gitlabWithAttrsOne  = gitlabReqOne

totalPages :: Response a -> Int
totalPages resp =
  let hdrs = responseHeaders resp
  in findPages hdrs
  where
    findPages [] = 1 -- error "cannot find X-Total-Pages in header"
    findPages (("X-Total-Pages",bs):_) = read (T.unpack (T.decodeUtf8 bs))
    findPages (_:xs) = findPages xs
