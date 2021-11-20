import { format } from "date-fns"

const now = new Date();
const today = format(now, "yyyy.MM.dd");

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
