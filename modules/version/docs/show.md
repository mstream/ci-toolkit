#### Description

Calculate a version of the current commit.

#### How to...

##### create a release tag

```shell
git tag $(npx @ci-toolkit/version show)
```

#### Reference

```
show 
  [--format ARG] 
  [--git-directory ARG]
  [--version-prefix ARG] 
```

##### format

Format of the version. 
Supported values are `calendar` and `semantic`.

##### git-directory

Git repository path. Defaults to `.` (current directory).

##### version-prefix

Prefix for version-related Git tags. 
Used only by the `semantic` version format.

