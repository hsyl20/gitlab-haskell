{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import GitLab

main :: IO ()
main =
  runGitLab
    ( defaultGitLabServer
        { url = "https://gitlab.example.com",
          token = "abcde12345"
        }
    )
    ( receive
        [ matchIf
            "create an issue against any project named test_project123"
            ( \projEvent@ProjectCreate {} -> do
                return (projectCreate_name projEvent == "test_project123")
            )
            ( \projEvent@ProjectCreate {} -> do
                Right (Just project) <- searchProjectId (projectCreate_project_id projEvent)
                void $ newIssue project "the title" "the description"
            )
        ]
    )
