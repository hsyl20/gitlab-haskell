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

    myTodos <- runGitLab
                 (defaultGitLabServer
                   { url = "https://gitlab.example.com"
                   , token="my_token"} )
                 todos

Which uses the `todos` function:

    todos :: MonadIO m => GitLab m [Todo] 

This returns all GitLab TODO items, as Haskell values of type `Todo`
for the user identified with the access token`my_token`.

The `gitlab-tools` command line tool for bulk GitLab transactions uses
this library [link](https://gitlab.com/robstewart57/gitlab-tools).
