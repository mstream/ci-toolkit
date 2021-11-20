import {
  commitIds,
  gitLogOutput,
  gitTimestamps,
  pipelineMarkCommitCommand,
} from "../utils.js";

export default [
  {
    input: "git log",
    output: gitLogOutput([
      {
        date: gitTimestamps[0],
        id: commitIds[0],
        message: "some message",
        notes: ["ci-stage-1"],
      },
    ]),
  },
  {
    input: `${pipelineMarkCommitCommand} --ci-stage stage-2 --commit-ref ${commitIds[0]}`,
    output: [`Marking commit ${commitIds[0]} with CI stage 'stage-2'`],
  },
  {
    input: "git log",
    output: gitLogOutput([
      {
        date: gitTimestamps[0],
        id: commitIds[0],
        message: "some message",
        notes: ["ci-stage-1", "ci-stage-2"],
      },
    ]),
  },
];
