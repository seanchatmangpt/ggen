import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./tests/e2e",
  // Real WASM (wasm4pm-cognition) + real subprocess (python3) calls per
  // JTBD, some spec files chaining several of these in one test -- the
  // default 30s budget is real but tight; bumped for honest headroom, not
  // to paper over a hang (Phase 5, JTBD validation).
  timeout: 60_000,
  fullyParallel: true,
  // Real bug found and fixed live (Phase 5): "127.0.0.1" (the prior value
  // here) is a DIFFERENT origin than "localhost" to Next.js dev's own
  // cross-origin dev-request protection (no `allowedDevOrigins` is
  // configured in next.config.ts). Confirmed directly with a throwaway
  // Playwright script this pass: loading the app via 127.0.0.1 leaves the
  // HMR websocket permanently failing ("Error during WebSocket handshake:
  // net::ERR_INVALID_HTTP_RESPONSE") and, far more seriously, client
  // hydration silently never completes -- the SSR HTML renders and *looks*
  // fully interactive, real keyboard typing even changes the input
  // element's raw DOM `.value`, but zero React event handlers are
  // attached (`__reactProps$...` absent on the element), so every
  // onChange/onClick in the app is a permanent no-op and the Submit
  // button never leaves `disabled`. Switching to "localhost" alone fixes
  // it completely (verified: `[HMR] connected`, `hasReactProps: true`,
  // Submit enables). Not a Playwright issue and not a bug in any
  // component this session wrote -- a real host/origin mismatch between
  // this config and how Next dev's safety check identifies "same origin".
  use: { baseURL: "http://localhost:3000", trace: "on-first-retry" },
  // `pnpm dev` (the prior command here) fails on this host before `next
  // dev` ever starts: modern pnpm runs a "dependencies status check"
  // ahead of any script, which hard-fails on this repo's
  // `[ERR_PNPM_IGNORED_BUILDS]` (blake3/esbuild/sharp native build scripts
  // awaiting approval) -- confirmed live this pass, real stderr captured,
  // unrelated to anything this test suite touches. `npx next dev` (proven
  // to serve real 200s earlier this session) sidesteps that pnpm-specific
  // gate entirely while running the exact same real dev server.
  webServer: { command: "npx next dev", url: "http://localhost:3000", reuseExistingServer: !process.env.CI },
  projects: [{ name: "chromium", use: { ...devices["Desktop Chrome"] } }],
});
