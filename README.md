# A Haskell library for the GitLab web API

This library interacts with a GitLab server's API. It supports queries
about and updates to:

* Branches
* Commits
* Groups
* Issues
* Jobs
* Members
* Merge requests
* Pipelines
* Projects
* Repositories
* Repository files
* Users

The library parses JSON results into Haskell data types in the
`GitLab.Types` module.

## Example

Run all GitLab actions with `runGitLab`: 

    runGitLab ::
      (MonadUnliftIO m, MonadIO m)
       => GitLabServerConfig
       -> GitLab m a
       -> m a

For example:

    myProjects <-
      runGitLab
        (defaultGitLabServer
           { url = "https://gitlab.example.com"
           , token="my_token"} )
        (searchUser "joe" >>= userProjects . fromJust)

Which uses the functions:

    searchUser   :: Text -> GitLab m (Maybe User)
    userProjects :: User -> GitLab m (Maybe [Project])

The `gitlab-tools` command line tool for bulk GitLab transactions uses
this library [link](https://gitlab.com/robstewart57/gitlab-tools).
