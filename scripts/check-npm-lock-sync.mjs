#!/usr/bin/env node
/**
 * Refuse package.json ↔ package-lock.json drift (Construct npm ci antifalsifier).
 *
 * Usage: node scripts/check-npm-lock-sync.mjs [example-dir…]
 * Default: examples/interview-assist
 *
 * Exit 0 when top-level dependency pins in package.json match lock packages[""].
 * Exit 1 on missing files or version mismatch (no network).
 */
import { readFileSync, existsSync } from "node:fs";
import { join, resolve } from "node:path";

const roots = process.argv.slice(2);
const targets =
  roots.length > 0
    ? roots
    : [join("examples", "interview-assist")];

function fail(msg) {
  console.error(`check-npm-lock-sync: ${msg}`);
  process.exit(1);
}

function loadJson(path) {
  if (!existsSync(path)) fail(`missing ${path}`);
  return JSON.parse(readFileSync(path, "utf8"));
}

/** Normalize caret/exact pin for comparison against lock installed version. */
function pinMatches(declared, locked) {
  if (declared == null || locked == null) return false;
  if (declared === locked) return true;
  if (declared.startsWith("^")) {
    // lock may store exact; accept when locked satisfies major.minor of caret range loosely
    return locked === declared.slice(1) || locked.startsWith(declared.slice(1).split(".").slice(0, 2).join(".") + ".");
  }
  return false;
}

let failed = false;
for (const root of targets) {
  const dir = resolve(root);
  const pkgPath = join(dir, "package.json");
  const lockPath = join(dir, "package-lock.json");
  const pkg = loadJson(pkgPath);
  const lock = loadJson(lockPath);
  const lockedRoot = lock.packages?.[""];
  if (!lockedRoot) fail(`${lockPath}: missing packages[""]`);

  const declared = {
    ...(pkg.dependencies || {}),
    ...(pkg.devDependencies || {}),
  };
  const lockedDeps = {
    ...(lockedRoot.dependencies || {}),
    ...(lockedRoot.devDependencies || {}),
  };

  for (const [name, pin] of Object.entries(declared)) {
    const locked = lockedDeps[name];
    if (locked == null) {
      console.error(`${root}: package.json has ${name}@${pin} but lock packages[""] omits it`);
      failed = true;
      continue;
    }
    if (!pinMatches(pin, locked) && locked !== pin) {
      // Also accept lock storing exact when package.json has exact
      if (pin !== locked) {
        console.error(
          `${root}: drift ${name}: package.json=${pin} lock=${locked}`
        );
        failed = true;
      }
    }
  }
  if (!failed) {
    console.log(`check-npm-lock-sync: OK ${root}`);
  }
}

if (failed) process.exit(1);
