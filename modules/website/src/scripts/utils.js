import { format, sub } from "date-fns";

function formatGitTimestamp(date) {
  return format(date, "eee LLL dd HH:mm:ss yyyy xx");
}

function formatVersionDate(date) {
  return format(date, "yyyy.MM.dd");
}

function commitOutput({ author, date, id, isFirst, message, notes }) {
  return [
    `commit ${id}${isFirst ? " (HEAD -> master)" : ""}`,
    `Author: ${author.name} <${author.email}>`,
    `Date:   ${date}`,
    "",
    `    ${message}`,
    "",
    ...(notes.length
      ? ["Notes:", ...notes.reduce((acc, note) => [...acc, ...[note, ""]], [])]
      : []),
  ];
}

const now = new Date();
const past = sub(now, { weeks: 1, days: 2, hours: 3, minutes: 4, seconds: 5 });

export const commitIds = [
  "239f56d40fe5a9257bb12c2e1101f8770aed2661",
  "9225393c9d2434dbc6a858c675bf09478f2866a0",
  "1237e2d4a52362dca3cd4cea5c9455a7f96ab02b",
];

export const gitTimestamps = [
  formatGitTimestamp(now),
  formatGitTimestamp(past),
];

export const versionDates = [formatVersionDate(now), formatVersionDate(past)];

export const pipelineGetLastCommand = "npx @ci-toolkit/pipeline get-last";
export const pipelineMarkCommitCommand = "npx @ci-toolkit/pipeline mark-commit";
export const renderRepoCommand = "npx @ci-toolkit/render repo";
export const versionShowCommand = "npx @ci-toolkit/version show";

export function gitLogOutput(commits) {
  return commits.reduce(
    (acc, commit, idx) => [
      ...acc,
      ...commitOutput({
        ...commit,
        author: { email: "maciej.laciak@gmail.com", name: "mstream" },
        isFirst: idx === 0,
      }),
    ],
    []
  );
}

export function dummyCommitStep({ id, message }) {
  return {
    input: `git commit --allow-empty -m '${message}'`,
    output: [`[master ${id.substring(0, 7)}] ${message}`],
  };
}
