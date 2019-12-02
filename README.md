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

## Applications using this library

* `gitlab-tools`: a command line tool for bulk transactions against a
  GitLab server. [link](https://gitlab.com/robstewart57/gitlab-tools)
