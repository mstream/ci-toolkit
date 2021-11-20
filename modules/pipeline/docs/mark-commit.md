#### Description

Mark a commit as passed a given CI stage.

That information is preserved in a Git repository in the form of commit
notes and used by the [get-last](get-last.md) command to query for the
last commit which passed certain stages. 

#### How to...

##### mark the current commit 

```shell
npx @ci-toolkit/pipeline mark-commit \
  --ci-stage functional-testing \ 
  --commit-ref $(git rev-parse HEAD)
```

#### Reference

```
mark-commit  
  [--ci-prefix ARG] 
  [--ci-stage ARG]  
  [--commit-ref] 
  [--dry-run] 
  [--git-directory] 
  [-v|--verbose]
```

##### ci-prefix

Prefix for stage names to differentiate the from other Git notes entries. 
Defaults to `ci-`.


##### ci-stage

Name of the stage for commit to be marked with.

##### commit-ref

ID of the commit to be marked with CI stage name.

##### dry-run

When enabled, a simulated outcome of the marking is displayed but
no change to the Git repository is made.

##### git-directory

Git repository path. Defaults to `.` (current directory).

##### verbose

Output the maximum amount of diagnostic information.

