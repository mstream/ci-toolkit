import { format, sub } from "date-fns"

const now = new Date();
const today = format(now, "eee LLL dd HH:mm:ss yyyy xx");
const commitId = "239f56d40fe5a9257bb12c2e1101f8770aed2661";
const ciStageName = "stage-2";

const output1 = [
  `commit ${commitId} (HEAD -> master)`,
  "Author: mstream <maciej.laciak@gmail.com>",
  `Date:   ${today}`,
  "",
  "    some message",
  "",
  "Notes:",
  "ci-stage-1",
];

const output2 = [...output1, "", `ci-${ciStageName}`];

export default [
  {
    input: "git log",
    output: output1,
  },
  {
    input: `npx @ci-toolkit/pipeline mark-commit --ci-stage ${ciStageName} --commit-ref ${commitId}`,
    output: [`Marking commit ${commitId} with CI stage '${ciStageName}'`],
  },
  {
    input: "git log",
    output: output2,
  },
]
