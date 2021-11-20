import { format, sub } from "date-fns"

const now = new Date();
const today = format(now, "eee LLL dd HH:mm:ss yyyy xx");
const yesterday = format(sub(now, {days: 1, hours: 2, minutes: 3, seconds: 4}), "eee LLL dd HH:mm:ss yyyy xx");
const commitId1 = "239f56d40fe5a9257bb12c2e1101f8770aed2661";
const commitId2 = "9225393c9d2434dbc6a858c675bf09478f2866a0";

export default [
  {
    input: "git log",
    output: [
      `commit ${commitId2} (HEAD -> master)`,
      "Author: mstream <maciej.laciak@gmail.com>",
      `Date:   ${today}`,
      "",
      "    another message",
      "",
      "Notes:",
      "ci-stage-1",
      "",
      `commit ${commitId1}`,
      "Author: mstream <maciej.laciak@gmail.com>",
      `Date:   ${yesterday}`,
      "",
      "    some message",
      "",
      "Notes:",
      "ci-stage-1",
      "",
      "ci-stage-2",
    ],
  },
  {
    input: "npx @ci-toolkit/pipeline get-last --ci-stage stage-1",
    output: [commitId2],
  },
  {
    input: "npx @ci-toolkit/pipeline get-last --ci-stage stage-1 --ci-stage stage-2",
    output: [commitId1],
  },
]
