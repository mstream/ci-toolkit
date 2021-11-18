# get-last

## Synopsis

```shell
npx @ci-toolkit/pipeline get-last \ 
  [--ci-prefix ARG] 
  [--ci-stage ARG]
  [--dry-run] 
  [--git-directory ARG]
  [-v|--verbose]
```

## Description

Get the last commit which passed given CI stage(s).

## Options

| Long Form         | Short Form | Default Value | Description |
| :---              | :---       | :---          | :--- |
| --ci-prefix       | <NONE>     | "ci-"         | CI prefix |
| --ci-stage        | <NONE>     | <NONE>        | name of the CI stage | 
| --dry-run         | <NONE>     | false         | make no changes to the repository | 
| --git-directory   | <NONE>     | "."           | path to the repository | 
| --verbose         | -v         | false         | include more logs | 

## Examples

### Getting an ID of the last commit with multiple passed CI stages

```shell
npx @ci-toolkit/pipeline get-last \ 
  --ci-stage functional-testing \
  --ci-stage integration-testing
```
