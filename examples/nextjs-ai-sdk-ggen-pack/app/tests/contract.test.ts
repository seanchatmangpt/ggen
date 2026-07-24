import { describe, expect, it } from "vitest";
import { readFile } from "node:fs/promises";

const readJson = async (path: string) => JSON.parse(await readFile(path, "utf8"));

describe("manufactured application contract", () => {
  it("pins the admitted AI and UI registries", async () => {
    const pkg = await readJson("package.json");
    const lock = await readJson("ui-registry.lock.json");
    expect(pkg.dependencies.ai).toMatch(/^7\./);
    expect(lock.registries.aiElements.components).toContain("tool");
    expect(lock.registries.shadcn.components).toContain("button");
  });

  it("keeps mutating tools behind approval", async () => {
    const source = await readFile("lib/ai/tools.ts", "utf8");
    expect(source).toContain("notes_create");
    expect(source).toMatch(/notes_create:[\s\S]*needsApproval: true/);
  });

  it("routes execution through ToolBroker", async () => {
    const source = await readFile("lib/ai/tools.ts", "utf8");
    expect(source).toContain("toolBroker.invoke");
    expect(source).not.toContain("fetch(");
  });
});
