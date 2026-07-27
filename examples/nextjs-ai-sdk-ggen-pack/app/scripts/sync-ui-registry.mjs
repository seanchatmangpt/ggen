import { readFile } from "node:fs/promises";
import { spawnSync } from "node:child_process";

const lock = JSON.parse(await readFile(new URL("../ui-registry.lock.json", import.meta.url), "utf8"));

function run(command, args) {
  const result = spawnSync(command, args, { stdio: "inherit", shell: process.platform === "win32" });
  if (result.status !== 0) {
    throw new Error(`${command} ${args.join(" ")} failed with status ${result.status}`);
  }
}

const shadcn = lock.registries.shadcn;
run("npx", [`shadcn@${shadcn.cliVersion}`, "add", "--yes", ...shadcn.components]);

const elements = lock.registries.aiElements;
run("npx", [`ai-elements@${elements.cliVersion}`, "add", ...elements.components]);

console.log(JSON.stringify({ status: "ALIVE", lock }, null, 2));
