#!/usr/bin/env node
// verify_mermaid.mjs — mermaid.parse over raw .mmd files (FNOX-SJ-VIEW-4).
// Deps resolve from servengine-frontend (override: FRONTEND_DIR env).
// Usage: node verify_mermaid.mjs [--self-check] [file.mmd ...]
//   default files: world/world/docs/c4/{c1-context,c2-containers}.mmd
//   --self-check: also parses a corrupted scratch copy; exit 1 unless it FAILS (non-vacuity).
// Exit 0 = all OK; 1 = parse failure (or vacuous self-check); 2 = dep/file missing.
import { createRequire } from "node:module";
import { pathToFileURL, fileURLToPath } from "node:url";
import { readFileSync, existsSync, mkdtempSync, writeFileSync } from "node:fs";
import { join, dirname, resolve } from "node:path";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const FRONTEND = process.env.FRONTEND_DIR || "/Users/sac/srve/servengine-frontend";

// Real mermaid.parse inside headless Chromium (playwright); no jsdom, no network.
let browser, page;
try {
  const req = createRequire(join(FRONTEND, "package.json"));
  const { chromium } = req("playwright");
  const p = req.resolve("mermaid/package.json");
  const bundle = join(dirname(p), "dist", "mermaid.min.js");
  if (!existsSync(bundle)) throw new Error(`missing ${bundle}`);
  try {
    browser = await chromium.launch({ ...(process.env.CHROMIUM_PATH ? { executablePath: process.env.CHROMIUM_PATH } : {}), headless: true });
  } catch {
    browser = await chromium.launch({ ...(process.env.CHROMIUM_PATH ? { executablePath: process.env.CHROMIUM_PATH } : {}), headless: true, args: ["--no-sandbox"] });
  }
  page = await browser.newPage();
  await page.setContent("<!DOCTYPE html><body></body>");
  await page.addScriptTag({ path: bundle });
  await page.evaluate(() => window.mermaid.initialize({ startOnLoad: false }));
} catch (e) {
  console.error(`dep-resolve failed: ${e.message}`);
  if (browser) await browser.close();
  process.exit(2);
}
const mermaid = {
  parse: async (text) => {
    const r = await page.evaluate(async (t) => {
      try { await window.mermaid.parse(t); return { ok: true }; }
      catch (e) { return { ok: false, err: String((e && e.message) || e) }; }
    }, text);
    if (!r.ok) throw new Error(r.err);
  },
};
const done = async (code) => { await browser.close(); process.exit(code); };

const args = process.argv.slice(2);
const selfCheck = args.includes("--self-check");
let files = args.filter((a) => !a.startsWith("--")).map((f) => resolve(f));
if (files.length === 0) {
  files = ["c1-context.mmd", "c2-containers.mmd"].map((f) => join(HERE, "world/world/docs/c4", f));
}

let fails = 0;
for (const f of files) {
  if (!existsSync(f)) { console.error(`missing: ${f}`); await done(2); }
  try {
    await mermaid.parse(readFileSync(f, "utf8"));
    console.log(`OK ${f}`);
  } catch (e) {
    fails++;
    console.log(`FAIL ${f} — ${String(e.message || e).split("\n").slice(0, 3).join(" | ")}`);
  }
}

if (selfCheck) {
  const dir = mkdtempSync(join(tmpdir(), "vm4-"));
  const bad = join(dir, "corrupt.mmd");
  writeFileSync(bad, readFileSync(files[0], "utf8") + "\n  ))) --> [[ not valid {{{\n");
  let failed = false;
  try { await mermaid.parse(readFileSync(bad, "utf8")); } catch { failed = true; }
  console.log(failed ? `SELF-CHECK OK (corrupt copy rejected): ${bad}` : "SELF-CHECK VACUOUS: corrupt copy parsed");
  if (!failed) await done(1);
}
await done(fails > 0 ? 1 : 0);
