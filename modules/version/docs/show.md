# show

## Synopsis

```shell
npx @ci-toolkit/version show \ 
  [--dry-run] 
  [--format ARG] 
  [--git-directory ARG]
  [-v|--verbose]
  [--version-prefix ARG]
```

## Description

Calculate a version of the current commit

## Options

| Long Form         | Short Form | Default Value | Description |
| :---              | :---       | :---          | :--- |
| --ci-prefix       | <NONE>     | "ci-"         | CI prefix |
| --dry-run         | <NONE>     | false         | make no changes to the repository | 
| --format          | <NONE>     | semantic      | format of the version |
| --git-directory   | <NONE>     | "."           | path to the repository | 
| --verbose         | -v         | false         | include more logs | 
| --version-prefix  | <NONE>     | "v"           | version prefix |

## Examples

```shell
npx @ci-toolkit/version show \ 
  --format semantic
  --version-prefix v
```

```shell
npx @ci-toolkit/version show \ 
  --format calendar
```