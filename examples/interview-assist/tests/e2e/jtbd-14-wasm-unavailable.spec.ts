/**
 * UX-polish pass, item 2: graceful WASM-load-failure handling, exercised
 * end to end through the real browser UI.
 *
 * `page.route()` here injects one extra request header
 * (`x-wasm4pm-cognition-force-unavailable`) onto the real outgoing POST
 * /api/cognition request via `route.continue({headers})` -- it does NOT
 * fabricate or intercept the response; the real running server still
 * handles the request for real. `cognition-adapter.ts`'s
 * `loadCognitionModule()` reads that header (threaded through by
 * app/api/cognition/route.ts) and, when present, `require()`s a real,
 * fixed, deliberately nonexistent package name instead of the real
 * "wasm4pm-cognition" one -- a genuine `MODULE_NOT_FOUND`, not a simulated
 * one, mirroring what a corrupted/missing real install would produce (see
 * that function's own doc for why this is a boolean flag rather than a
 * caller-supplied path -- a dynamic require() target broke Turbopack's
 * static bundling of the whole route when tried first). This is the
 * concrete case tests/api/cognition-route.test.ts proves at the route
 * level; this file proves the same real failure renders correctly all the
 * way through the browser UI, and that the app recovers afterward (the
 * module is never left broken).
 */
import { test, expect } from "./support";

test("a genuine wasm4pm-cognition require() failure surfaces a clean typed 503 (not a raw 500) and an honest 'temporarily unavailable' UI state, with a working Retry", async ({
  page,
  pageErrors,
}) => {
  await page.goto("/");

  await page.route("**/api/cognition", async (route) => {
    await route.continue({
      headers: { ...route.request().headers(), "x-wasm4pm-cognition-force-unavailable": "1" },
    });
  });

  const utterance = "I have an array of numbers to search through";
  await page.getByTestId("cognition-intent-input").fill(utterance);
  const [response] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/cognition") && r.request().method() === "POST"),
    page.getByTestId("cognition-submit").click(),
  ]);

  // Real network evidence: a clean typed 503, not a raw 500/HTML error page.
  expect(response.status()).toBe(503);
  const body = (await response.json()) as { status: string; reason?: string };
  expect(body.status).toBe("unavailable");
  expect(typeof body.reason).toBe("string");

  // Real DOM evidence: an honest message, never the raw module-resolution
  // error text, and no unhandled client-side exception.
  const unavailable = page.getByTestId("cognition-panel-unavailable");
  await expect(unavailable).toBeVisible();
  await expect(unavailable).toContainText("temporarily unavailable");
  await expect(unavailable).not.toContainText("Cannot find module");
  expect(pageErrors).toHaveLength(0);

  // Recovery: remove the broken-header route, click Retry, and confirm the
  // real happy path resumes for the SAME observed utterance -- the module
  // was never left broken by the earlier failure.
  await page.unroute("**/api/cognition");
  const [retryResponse] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/cognition") && r.request().method() === "POST"),
    page.getByTestId("cognition-panel-retry-unavailable").click(),
  ]);
  expect(retryResponse.status()).toBe(200);
  const retryBody = (await retryResponse.json()) as { status: string; selected?: string };
  expect(retryBody.status).toBe("matched");
  expect(retryBody.selected).toBe("ARRAY");
  await expect(page.getByTestId("cognition-panel-matched")).toBeVisible();
  await expect(page.getByTestId("cognition-panel-intent")).toContainText(utterance);

  expect(pageErrors).toHaveLength(0);
});
