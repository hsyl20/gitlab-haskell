{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromJust)
import SystemHookTests
import Test.Tasty

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "gitlab system hook tests"
        systemHookTests
    )
