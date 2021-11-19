export default [
  {
    input: "npx @ci-toolkit/version show --format calendar",
    output: ["2000.01.01_1"],
  },
  {
    input: "git commit --allow-empty -m 'some message'",
    output: ["[master 239f56d] some message"],
  },
  {
    input: "npx @ci-toolkit/version show --format calendar",
    output: ["2000.01.01_2"],
  },
]
