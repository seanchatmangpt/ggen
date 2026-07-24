#!/usr/bin/env node
/**
 * Real bug found and fixed live (Phase 5, Playwright JTBD validation):
 * `lib/wasm/wasm4pm-cognition` (a real wasm-pack `--target nodejs` build
 * output, complete with its own generated package.json declaring
 * `"name": "wasm4pm-cognition"`) needs to be required from
 * `lib/adapters/cognition-adapter.ts` via a bare package specifier
 * (`require("wasm4pm-cognition")`) rather than a relative deep path, so
 * that `next.config.ts`'s `serverExternalPackages` can exempt it from
 * Turbopack's bundler -- otherwise Turbopack rewrites the generated CJS
 * module's own `__dirname` (used internally to locate its sibling
 * `wasm4pm_cognition_bg.wasm` via `readFileSync`) to a synthetic
 * `/ROOT/...` placeholder, producing a real ENOENT on every real POST
 * /api/cognition request (captured verbatim against a running `next dev`
 * server this pass; NOT caught by tsc, vitest, or `next build`, since none
 * of those exercise a real HTTP request against the lazily-required WASM
 * module -- see cognition-adapter.ts's loadCognitionModule() doc for the
 * full chain).
 *
 * A bare specifier needs `node_modules/wasm4pm-cognition` to exist. A
 * plain `file:./lib/wasm/wasm4pm-cognition` npm dependency was tried
 * first and empirically falsified: npm (11.12.1, this host, even with
 * `install-links=true` in .npmrc) always materializes a `file:` dependency
 * to a local path as a SYMLINK, never a physical copy, regardless of that
 * setting -- and Turbopack's "is this package external?" check evidently
 * resolves the symlink to its real path (back under `lib/wasm/...`, which
 * is not itself inside any `node_modules` directory) before deciding
 * whether to bundle it, so the symlinked package was still bundled and the
 * ENOENT persisted verbatim even after `serverExternalPackages` correctly
 * named it.
 *
 * This script is the actual fix: a real, physical (non-symlink) recursive
 * copy of the wasm-pack build output into `node_modules/wasm4pm-cognition`,
 * run as `postinstall` so it self-heals after every `npm install` (which
 * would otherwise prune this directory on the next run, since it is no
 * longer a declared package.json dependency -- deliberately: relying on
 * npm's own dependency materialization is exactly what caused the symlink
 * problem above).
 *
 * UX-polish pass (production-hardening): also materializes
 * `lib/wasm/wasm4pm-cognition-test-broken-fixture` -> `node_modules/
 * wasm4pm-cognition-deliberately-missing-for-tests` by the exact same
 * mechanism, for the exact same reason -- `cognition-adapter.ts`'s
 * graceful-degradation test path needs a real `require()` target that
 * physically exists on disk (so Turbopack can statically resolve it at
 * compile time -- `serverExternalPackages` externalizes BUNDLING, not
 * resolution, confirmed live this pass: a literal require() of a
 * genuinely-nonexistent package name broke Turbopack's compilation of the
 * whole route, even in a branch never executed) but genuinely throws when
 * actually loaded at runtime (see that fixture's own index.js).
 */
import { cpSync, existsSync, mkdirSync, rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const projectRoot = dirname(dirname(fileURLToPath(import.meta.url)));

const PACKAGES = [
  { source: join(projectRoot, "lib", "wasm", "wasm4pm-cognition"), name: "wasm4pm-cognition" },
  {
    source: join(projectRoot, "lib", "wasm", "wasm4pm-cognition-test-broken-fixture"),
    name: "wasm4pm-cognition-deliberately-missing-for-tests",
  },
];

for (const { source, name } of PACKAGES) {
  const dest = join(projectRoot, "node_modules", name);
  if (!existsSync(source)) {
    // The real WASM build output isn't present in this checkout (e.g. it
    // hasn't been built yet, or this install is running somewhere that
    // doesn't need the cognition bridge). Not fatal -- cognition-adapter.ts
    // only requires it lazily, at real call time, and will throw its own
    // real, disclosed error then.
    console.warn(`[materialize-wasm-cognition] source not found, skipping: ${source}`);
    continue;
  }
  rmSync(dest, { recursive: true, force: true });
  mkdirSync(dirname(dest), { recursive: true });
  cpSync(source, dest, { recursive: true, dereference: true });
  console.log(`[materialize-wasm-cognition] copied ${source} -> ${dest} (real files, not a symlink)`);
}
