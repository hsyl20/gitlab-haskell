# A Haskell library for the GitLab web API

## gitlab-haskell library

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

## gitlab-tools executable

This package also includes a `gitlab-tools` executable.

```
> gitlab-tools --help
Program to execute bulk GitLab actions

Usage: gitlab-tools --host host [--token token] [--filename filename]
                    [--group group] [--project project]
                    [--add-reporters-to-group] [--share-projects-with-group]
                    [--registered]
  Executes actions against a GitLab server

Available options:
  --host host              URL of the GitLab server
  --token token            GitLab access token
  --filename filename      name of a CSV file
  --group group            name of a group
  --project project        name of a project
  --add-reporters-to-group Add users as reporters to a group
  --share-projects-with-group
                           Share all projects with a given name to the specific
                           group
  --registered             prints 'yes' or 'no' depending on whether a user
                           exists on the GitLab server
  -h,--help                Show this help text
```

### Examples

Sharing projects with a group:

```
> gitlab-tools \
    --host <GitLab url> --token <token> \
    --share-projects-with-group --project my_project \
    --group my_group
```

This commands finds all projects called `my_project` then adds the
`my_group` group to all of those projects as a member with the
Reporter role.

Adding users to a group:

```
> gitlab-tools \
    --host <GitLab url> --token <token> \
    --filename <filename>.csv \
    --add-reporters-to-group \
    --group my_group
```

This command reads usernames from a comma separated file, and if they
are registered on the GitLab server it adds them to the `my_group`
group with the Reporter role.
