import type { NextConfig } from "next";
import path from "node:path";

// Pins Turbopack's inferred workspace root to this package's own directory.
// Without this, Next auto-detects a monorepo root by walking up for the
// first lockfile it finds -- and finds an unrelated `/Users/sac/pnpm-lock.yaml`
// (a stray file well outside this repo, not this project's), which made it
// treat `/Users/sac` as the workspace root instead of
// `examples/interview-assist`. That wrong root corrupted relative asset-path
// resolution inside `.next/server` for any dependency shipping non-JS
// assets next to its code (discovered running `next build`: blake3's WASM
// fallback failed with `ENOENT .../node_modules/blake3/dist/wasm/nodejs/
// blake3_js_bg.wasm` under a bogus `/ROOT/...` path) -- this is exactly the
// scenario Next's own build warning names and its own docs point at this
// config key to fix (https://nextjs.org/docs/app/api-reference/config/next-config-js/turbopack#root-directory).
const nextConfig: NextConfig = {
  turbopack: {
    root: path.resolve(__dirname),
  },
  // `blake3`'s Node build loads its WASM fallback via a computed relative
  // path (dist/node/hash-fn.js -> dist/wasm/nodejs/blake3_js_bg.wasm).
  // Turbopack's bundler does not correctly relocate that non-JS asset for a
  // nested transitive dependency -- confirmed with a real ENOENT under a
  // bogus internal `/ROOT/...` path in BOTH `next build`'s page-data
  // collection and real `next dev` request handling (see
  // lib/adapters/checksum-adapter.ts's Import-path note for the full
  // failure chain). `serverExternalPackages` tells Next to leave this
  // package's require() calls untouched by Turbopack's bundler and resolve
  // them via plain Node module resolution instead, which works (proven by
  // this same package resolving correctly under Vitest/plain Node).
  //
  // `wasm4pm-cognition` (real bug, found live via Playwright JTBD
  // validation, Phase 5 -- NOT caught by `npx tsc --noEmit`, the vitest
  // suite, or `npx next build`, because: vitest runs in plain Node,
  // bypassing Turbopack's bundler entirely; and cognition-adapter.ts's
  // `loadCognitionModule()` require() is lazy, deferred to the first real
  // `runCognition` call, which `next build`'s page-data-collection pass
  // never triggers -- only a real HTTP request against a running server
  // does). The exact same failure class as blake3's above: the wasm-pack
  // `--target nodejs` build's own generated `wasm4pm_cognition.js` loads
  // its sibling `.wasm` binary via `readFileSync(path.join(__dirname,
  // 'wasm4pm_cognition_bg.wasm'))` -- Turbopack rewrites `__dirname` to a
  // synthetic `/ROOT/...` placeholder when it bundles this CJS module,
  // producing a real
  // `ENOENT: .../ROOT/lib/wasm/wasm4pm-cognition/wasm4pm_cognition_bg.wasm`
  // on every real POST /api/cognition request (captured verbatim from
  // /private/tmp/interview-assist-dev.log this pass). `lib/wasm/
  // wasm4pm-cognition/package.json` already declares `"name":
  // "wasm4pm-cognition"` (a real wasm-pack build artifact, not
  // hand-added), which is what lets `serverExternalPackages` match it by
  // package-boundary even though it's required via a relative path, not a
  // bare specifier -- same mechanism, not a new one.
  //
  // `wasm4pm-cognition-deliberately-missing-for-tests` (UX-polish pass,
  // production-hardening): a second, real-but-INTENTIONALLY-nonexistent
  // entry, paired with cognition-adapter.ts's `loadCognitionModule`. That
  // function needs a real `require()` call that is GUARANTEED to throw
  // `MODULE_NOT_FOUND` (to test its own graceful-degradation path), issued
  // with a LITERAL string argument -- a dynamic/variable require() target
  // was tried first and empirically falsified live this pass: Turbopack
  // statically scans every require() call in a module at compile time, and
  // a single unresolvable DYNAMIC specifier anywhere in the file (even in a
  // branch never reached by the current request) makes it emit a hard
  // "Module not found: Can't resolve <dynamic>" build error for the WHOLE
  // route, not a runtime failure the app's own error handling could ever
  // see (confirmed: curling a real running dev server returned a raw
  // Turbopack compile-error page, not the app's typed 503, for a request
  // that never even touched the broken branch). Listing this fixed,
  // nonexistent name here makes Turbopack treat ITS literal require() call
  // (see loadCognitionModule) the same way it treats the real
  // "wasm4pm-cognition" one -- deferred untouched to real Node module
  // resolution at runtime -- so the resulting failure is a genuine runtime
  // `Cannot find module` the adapter's own try/catch can observe and map,
  // exactly like a real corrupted install would produce, rather than a
  // build-time bundler error.
  serverExternalPackages: [
    "blake3",
    "wasm4pm-cognition",
    "wasm4pm-cognition-deliberately-missing-for-tests",
  ],
};

export default nextConfig;
