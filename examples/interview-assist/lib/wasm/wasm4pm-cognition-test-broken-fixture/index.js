// UX-polish pass (production-hardening): a REAL, deliberately-broken
// require() target used ONLY by lib/adapters/cognition-adapter.ts's
// graceful-degradation test path (loadCognitionModule's forceUnavailable
// branch, triggered by the x-wasm4pm-cognition-force-unavailable test
// header on POST /api/cognition -- see that adapter's own doc).
//
// Throwing at require()-time is exactly how a genuinely corrupted/partial
// real install of wasm4pm-cognition would fail in production (package.json
// present, but the actual module content broken/missing) -- this is not a
// fabricated error injected by application code; it is Node's own
// require() genuinely failing to initialize a module that exists on disk.
//
// This package is listed in next.config.ts's `serverExternalPackages`
// alongside the real "wasm4pm-cognition" so Turbopack can statically
// resolve it (a real file exists here, so compilation succeeds) while
// still deferring the actual `require()` execution to real Node module
// resolution at runtime -- where this file's own top-level throw fires.
throw new Error(
  "wasm4pm-cognition-deliberately-missing-for-tests: this package is intentionally broken. " +
    "It exists only so cognition-adapter.ts's graceful WASM-load-failure test path has a real require() target that genuinely fails.",
);
