const today = new Date().toISOString().slice(0, 10).replaceAll('-','.');

export default [
  {
    input: "npx @ci-toolkit/version show --format calendar",
    output: [`${today}_1`],
  },
  {
    input: "git commit --allow-empty -m 'some message'",
    output: ["[master 239f56d] some message"],
  },
  {
    input: "npx @ci-toolkit/version show --format calendar",
    output: [`${today}_2`],
  },
]
