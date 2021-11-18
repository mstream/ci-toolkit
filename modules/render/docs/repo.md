# repo

## Synopsis

```shell
npx @ci-toolkit/render repo \ 
  [--ci-prefix ARG] 
  [--ci-stage ARG]
  [--dry-run] 
  [--format ARG] 
  [--git-directory ARG]
  [-v|--verbose]
```

## Description

Renders the entire repository

## Options

| Long Form         | Short Form | Default Value | Description |
| :---              | :---       | :---          | :--- |
| --ci-prefix       | <NONE>     | "ci-"         | CI prefix |
| --ci-stage        | <NONE>     | <NONE>        | order of the CI stages | 
| --dry-run         | <NONE>     | false         | make no changes to the repository | 
| --format          | <NONE>     | "JSON"        | format of the output |
| --git-directory   | <NONE>     | "."           | path to the repository | 
| --verbose         | -v         | false         | include more logs | 

## Examples

```shell
npx @ci-toolkit/pipeline get-last \ 
  --format JSON
```

```shell
npx @ci-toolkit/pipeline get-last \ 
  --format DOT
```

