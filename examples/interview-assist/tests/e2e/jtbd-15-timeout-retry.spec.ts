/**
 * UX-polish pass, item 3: a real client-side timeout (lib/client/fetch-with-timeout.ts)
 * plus a real Retry affordance -- the UI must never hang silently on a
 * request that takes unusually long.
 *
 * `?testTimeoutMs=N` (app/page.tsx, test-only, same discipline as the
 * pre-existing `?debug=1` gate) overrides the real, generous production
 * timeout budgets with a tiny one so this test can force the real
 * `AbortController`-backed timeout to fire deterministically and quickly,
 * without shrinking the real defaults themselves -- see
 * tests/lib/fetch-with-timeout.test.ts for the direct, real-HTTP-server
 * proof that the underlying mechanism (fetchWithTimeout) genuinely aborts a
 * slow request rather than merely swapping UI state on a timer.
 *
 * `page.route()` here again only delays delivery of an already-real
 * response (`route.fetch()`, then a real artificial wait, then
 * `route.fulfill()`) -- same non-mocking technique as
 * jtbd-13-loading-states.spec.ts. When the page's own AbortController fires
 * first, the underlying connection the route handler is waiting on may
 * already be torn down by the time `route.fulfill()` runs; that failure is
 * expected and swallowed, since this test cares about the real UI state,
 * not about the interception's own bookkeeping.
 */
import { test, expect, submitUtterance } from "./support";

const ARTIFICIAL_SERVER_DELAY_MS = 3_000;
const CLIENT_TIMEOUT_OVERRIDE_MS = 300;

test("a real client-side timeout on cognition submit clears the busy state without waiting for the full slow response, surfaces a real Retry affordance, and recovers on retry", async ({
  page,
  pageErrors,
}) => {
  await page.route("**/api/cognition", async (route) => {
    try {
      const response = await route.fetch();
      await new Promise((resolve) => setTimeout(resolve, ARTIFICIAL_SERVER_DELAY_MS));
      await route.fulfill({ response });
    } catch {
      // The page's own AbortController may have already cancelled the
      // underlying request by the time we get here -- expected once the
      // real client-side timeout below fires first.
    }
  });

  await page.goto(`/?testTimeoutMs=${CLIENT_TIMEOUT_OVERRIDE_MS}`);
  const utterance = "I have an array of numbers to search through";
  await page.getByTestId("cognition-intent-input").fill(utterance);

  const button = page.getByTestId("cognition-submit");
  await button.click();
  await expect(button).toHaveAttribute("aria-busy", "true");

  // The real 300ms client-side timeout fires well before the artificial
  // 3000ms server-side delay would ever resolve -- the busy state clearing
  // on its own, quickly, is the real evidence the request was genuinely
  // aborted rather than the UI simply waiting it out.
  await expect(button).toHaveAttribute("aria-busy", "false", { timeout: 2_000 });
  await expect(button).toHaveText("Submit");

  const errorNotice = page.getByTestId("cognition-request-error");
  await expect(errorNotice).toBeVisible();
  await expect(page.getByTestId("cognition-request-error-message")).toContainText(/longer than expected/i);
  expect(pageErrors).toHaveLength(0);

  // Retry: remove the artificial delay and click Retry -- the real happy
  // path completes normally for the SAME observed utterance.
  await page.unroute("**/api/cognition");
  const [response] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/cognition") && r.request().method() === "POST"),
    page.getByTestId("cognition-request-error-retry").click(),
  ]);
  expect(response.status()).toBe(200);
  const body = (await response.json()) as { status: string };
  expect(body.status).toBe("matched");
  await expect(page.getByTestId("cognition-panel-matched")).toBeVisible();
  await expect(page.getByTestId("cognition-request-error")).toHaveCount(0);

  expect(pageErrors).toHaveLength(0);
});

test("without the test override, the real production timeout budget is generous enough that a normal real cognition round trip never times out", async ({
  page,
}) => {
  // No route interception, no query-param override -- exercises the real
  // default COGNITION_TIMEOUT_MS (15s) against the real, fast WASM call.
  await page.goto("/");
  const { response, body } = await submitUtterance(page, "I have an array of numbers to search through");
  expect(response.status()).toBe(200);
  expect(body.status).toBe("matched");
  await expect(page.getByTestId("cognition-request-error")).toHaveCount(0);
});
