import { format } from "date-fns"

const today = format(new Date(), "yyyy.MM.dd");

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
