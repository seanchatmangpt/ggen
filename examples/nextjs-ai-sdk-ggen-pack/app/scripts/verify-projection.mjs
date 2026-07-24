import { readFile, readdir } from "node:fs/promises";
import { createHash } from "node:crypto";

const required = [
  "app/api/chat/route.ts",
  "components/chat/chat.tsx",
  "lib/ai/agent.ts",
  "lib/ai/tool-broker.ts",
  "lib/ai/tools.ts",
  "ui-registry.lock.json",
];
const digest = async (path) => createHash("sha256").update(await readFile(path)).digest("hex");
const actual = Object.fromEntries(await Promise.all(required.map(async (path) => [path, await digest(path)])));
const expected = JSON.parse(await readFile("projection-manifest.json", "utf8"));
for (const path of required) {
  if (actual[path] !== expected.files[path]) throw new Error(`projection drift: ${path}`);
}
const forbidden = (await readdir(".", { recursive: true })).filter((path) => /(^|\/)generated(\/|$)/.test(path));
if (forbidden.length) throw new Error(`forbidden generated/ paths: ${forbidden.join(", ")}`);
console.log(JSON.stringify({ status: "ALIVE", files: actual }, null, 2));
