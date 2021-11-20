import {
  commitIds,
  dummyCommitStep,
  gitLogOutput,
  gitTimestamps,
  versionDates,
  versionShowCommand,
} from "../utils.js";

export default [
  {
    input: "git log",
    output: gitLogOutput([
      {
        date: gitTimestamps[1],
        id: commitIds[2],
        message: "some message",
        notes: [],
      },
    ]),
  },
  {
    input: `${versionShowCommand} --format calendar`,
    output: [`${versionDates[1]}_1`],
  },
  dummyCommitStep({ id: commitIds[1], message: "another message" }),
  {
    input: `${versionShowCommand} --format calendar`,
    output: [`${versionDates[0]}_1`],
  },
  dummyCommitStep({ id: commitIds[0], message: "yet another message" }),
  {
    input: `${versionShowCommand} --format calendar`,
    output: [`${versionDates[0]}_2`],
  },
];
