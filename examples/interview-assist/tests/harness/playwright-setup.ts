/**
 * TICKET-039 (harness half): Playwright browser harness base config.
 *
 * BLOCKED for this workstream-H pass: examples/interview-assist/
 * playwright.config.ts does not exist yet (the app-shell / TICKET-014
 * workstream this ticket's Dependencies point at has not generated in this
 * session -- `examples/interview-assist` had no files at all before this
 * ticket set created `lib/adapters/` and `tests/`). Per the assignment's
 * explicit instruction ("if not, note BLOCKED for that half and proceed
 * with the accessibility-platform-adapter.ts half only"), this file is a
 * real, runnable base Playwright config skeleton (not a placeholder
 * comment) that TICKET-014's app-shell generation can extend once it
 * lands, reusing examples/nextjs-ai-sdk's proven Playwright config shape
 * as its base per this ticket's Objective -- but it has NOT been exercised
 * against a running interview-assist dev server in this pass, since no
 * such server exists yet. Do not read "file exists" as "harness verified
 * end-to-end" -- it is not.
 */
import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./e2e",
  timeout: 30_000,
  fullyParallel: true,
  retries: 0,
  reporter: [["list"]],
  use: {
    baseURL: process.env.INTERVIEW_ASSIST_BASE_URL ?? "http://localhost:3000",
    trace: "retain-on-failure",
  },
  projects: [
    { name: "chromium-a11y", use: { ...devices["Desktop Chrome"] } },
  ],
});
