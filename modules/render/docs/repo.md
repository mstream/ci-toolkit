#### Description

Renders the entire repository.

#### How to...

##### generate a graphical repository visualization

```shell
npx @ci-toolkit/render repo --format dot | dot -Tpng > /tmp/output.png
```

#### Reference

```
repo
  [--ci-prefix ARG] 
  [--ci-stage ARG]
  [--format ARG] 
  [--git-directory ARG]
```

##### ci-prefix

Prefix for stage names to differentiate the from other Git notes entries. 
Defaults to `ci-`.

##### ci-stage

Order in which CI stages should appear.

##### format

Format in which the output should be produced.

##### git-directory

Git repository path. Defaults to `.` (current directory).



