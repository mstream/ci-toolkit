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

`--ci-stage` the name of the stage 
`--commit-ref` the ID of the commit to be marked

## Examples

```shell
npx @ci-toolkit/pipeline mark-commit \
  --ci-stage functional-testing \ 
  --commit-ref $(git rev-parse HEAD)
```
