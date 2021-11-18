# mark-commit

## Synopsis

```shell
npx @ci-toolkit/pipeline mark-commit \ 
  --ci-stage ARG \
  --commit-ref ARG 
```

## Description

Mark a commit as passed given CI stage 

## Options

| Long Form         | Short Form | Default Value | Description | 
| :---              | :---       | :---          | :--- |
| --ci-prefix       | <NONE>     | "ci-"         | CI prefix |
| --ci-stage        | <NONE>     | <NONE>        | name of the CI stage | 
| --commit-ref      | <NONE>     | <NONE>        | commit ID | 
| --dry-run         | <NONE>     | false         | make no changes to the repository | 
| --git-directory   | <NONE>     | "."           | path to the repository | 
| --verbose         | -v         | false         | include more logs | 

## Examples

```shell
npx @ci-toolkit/pipeline mark-commit \
  --ci-stage functional-testing \ 
  --commit-ref $(git rev-parse HEAD)
```
