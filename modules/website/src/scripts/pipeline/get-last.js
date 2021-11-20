import {
  commitIds,
  gitLogOutput,
  gitTimestamps,
  pipelineGetLastCommand,
} from "../utils.js";

export default [
  {
    input: "git log",
    output: gitLogOutput([
      {
        date: gitTimestamps[0],
        id: commitIds[0],
        message: "another message",
        notes: ["ci-stage-1"],
      },
      {
        date: gitTimestamps[1],
        id: commitIds[1],
        message: "some message",
        notes: ["ci-stage-1", "ci-stage-2"],
      },
    ]),
  },
  {
    input: `${pipelineGetLastCommand} --ci-stage stage-1`,
    output: [commitIds[0]],
  },
  {
    input: `${pipelineGetLastCommand} --ci-stage stage-1 --ci-stage stage-2`,
    output: [commitIds[1]],
  },
];
