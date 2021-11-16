# get-last

## Synopsis

```shell
npx @ci-toolkit/pipeline get-last \ 
  [--ci-stage ARG]
```

## Description

Get the last commit which passed given CI stage(s) 

## Options

`--ci-stage` the name of the stage 

## Examples

```shell
npx @ci-toolkit/pipeline get-last \
  --ci-stage functional-testing
```

```shell
npx @ci-toolkit/pipeline get-last \ 
  --ci-stage functional-testing \
  --ci-stage integration-testing
```
