import DemoCLI from "@gammons/demo-cli";

async function input(t, pauseInMs, cmd) {
    t.printPrompt();
    await t.wait(pauseInMs);
    await t.type(cmd);
    await t.wait(pauseInMs);
    t.enterKey();
}

async function output(t, pauseInMs, lines) {
    await t.wait(2 * pauseInMs);
    lines.forEach(line => t.println(line, {className:"base09"}));
    t.println("");
}


export async function run({containerId, pauseInMs, script}) {
  const t = new DemoCLI(`#${containerId}`, {cursor: "▋", prompt: "$ "});
  while (true) {
    for (const step of script) {
      await input(t, pauseInMs, step.input);
      await output(t, pauseInMs, step.output);
    }
    await t.wait(4 * pauseInMs);
    t.reset();
  }
}
