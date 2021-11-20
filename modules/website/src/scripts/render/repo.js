import { renderRepoCommand } from "../utils.js";

export default [
  {
    input: `${renderRepoCommand} --format dot`,
    output: ["WIP..."],
  },
];
