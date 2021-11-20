#### Description

Get the last commit which passed given CI stage(s).

This command uses information produced by the [mark-commit](mark-commit.md) 
command to retrieve an identifier of the last commit which passed 
requested ci-stages.

#### How to...

##### get an ID of the last commit with passed all given CI stages

```shell
npx @ci-toolkit/pipeline get-last \ 
  --ci-stage functional-testing \
  --ci-stage integration-testing
```

#### Reference

```shell
npx @ci-toolkit/pipeline get-last \ 
  [--ci-prefix ARG] 
  [--ci-stage ARG]
  [--git-directory ARG]
```

##### ci-prefix

Prefix for stage names to differentiate the from other Git notes entries. 
Defaults to `ci-`.

##### ci-stage

Name of the stage(s) that searched commit needs to be marked with.

##### git-directory

Git repository path. Defaults to `.` (current directory).

