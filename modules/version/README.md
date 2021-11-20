### Description

The purpose of this suite of commands is to derive the version of
a software based on the Git commits history so it is meaningful.

### How to...

#### create a release tag

```shell
# cloning the repository 
git clone git@github.com:mstream/ci-toolkit.git

# taggin a commit with version 
git tag $(npx @ci-toolkit/version show)

# pushing tags
git push --tags master
```

### Commands

- [show](docs/show.md)
