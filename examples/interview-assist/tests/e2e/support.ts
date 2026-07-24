/**
 * Phase 5 (real Playwright JTBD validation): shared test fixtures/helpers
 * for tests/e2e/*.spec.ts.
 *
 * No page.route() network mocking lives here -- every JTBD spec drives the
 * real running `next dev` server (see playwright.config.ts's webServer),
 * which itself calls the real wasm4pm-cognition WASM bridge
 * (cognition-adapter.ts), the real subprocess sandbox executor
 * (sandbox-executor.ts, real python3), and the real BLAKE3 checksum adapter
 * (checksum-adapter.ts). Chicago TDD extended into the browser: real
 * collaborators end to end, state-based assertions on real DOM/network
 * results.
 */
import { test as base, expect, type Page } from "@playwright/test";

/** Every spec gets a `pageErrors` fixture recording any real uncaught
 * exception thrown in the page's JS context during the test. JTBD3 in
 * particular asserts this stays empty for the no-track-matched path (a
 * real, disclosed non-match must never surface as an unhandled error). */
export const test = base.extend<{ pageErrors: Error[] }>({
  pageErrors: async ({ page }, use) => {
    const errors: Error[] = [];
    page.on("pageerror", (err) => errors.push(err));
    await use(errors);
  },
});

export { expect };

/** Fills the observed-utterance input and clicks Submit, waiting for the
 * real POST /api/cognition round trip (real WASM call server-side) to
 * complete. Returns the parsed JSON body app/api/cognition/route.ts sent
 * back (the real CognitionOutcome union). */
export async function submitUtterance(page: Page, intent: string) {
  const input = page.getByTestId("cognition-intent-input");
  await input.fill(intent);
  const [response] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/cognition") && r.request().method() === "POST"),
    page.getByTestId("cognition-submit").click(),
  ]);
  return { response, body: await response.json() as Record<string, unknown> };
}

/** Presses Tab repeatedly (real keyboard events, no page.click/page.focus)
 * until `document.activeElement` carries the given `data-testid`, or
 * throws after `maxSteps` presses. Used by the keyboard-only JTBDs so
 * "reaching" a control is driven purely by the same Tab-key sequence a
 * real keyboard-only candidate would use. */
export async function tabUntilTestId(page: Page, testId: string, maxSteps = 30): Promise<void> {
  for (let i = 0; i < maxSteps; i++) {
    const current = await page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null);
    if (current === testId) return;
    await page.keyboard.press("Tab");
  }
  const current = await page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null);
  throw new Error(
    `tabUntilTestId: did not reach data-testid="${testId}" within ${maxSteps} Tab presses (last activeElement testid: ${current ?? "null"})`,
  );
}

/** Reads the real sequence of `data-testid`s (or a `tag#id`/`tag.class`
 * fallback for elements without one) visited by pressing Tab `steps`
 * times from the current focus position. Pure observation -- no
 * assumptions baked in, just what the real browser reports. */
export async function readTabSequence(page: Page, steps: number): Promise<string[]> {
  const seq: string[] = [];
  for (let i = 0; i < steps; i++) {
    await page.keyboard.press("Tab");
    const label = await page.evaluate(() => {
      const el = document.activeElement;
      if (!el) return "null";
      const testId = el.getAttribute("data-testid");
      if (testId) return `testid:${testId}`;
      if (el === document.body) return "body";
      return `${el.tagName.toLowerCase()}${el.id ? "#" + el.id : ""}`;
    });
    seq.push(label);
  }
  return seq;
}

/** Opens SessionActivityDrawer (a native <details>/<summary>) if not
 * already open, via a real click on its summary. */
export async function openActivityDrawer(page: Page): Promise<void> {
  const drawer = page.getByTestId("session-activity-drawer");
  const isOpen = await drawer.evaluate((el) => (el as HTMLDetailsElement).open);
  if (!isOpen) {
    await page.getByTestId("session-activity-drawer-toggle").click();
  }
}

/** Opens SessionMenu (native <details>/<summary>) if not already open. */
export async function openSessionMenu(page: Page): Promise<void> {
  const menu = page.getByTestId("session-menu");
  const isOpen = await menu.evaluate((el) => (el as HTMLDetailsElement).open);
  if (!isOpen) {
    await page.getByTestId("session-menu-toggle").click();
  }
}

/** Reads the real recorded event-history labels (state.usedEvents) off the
 * live DOM -- the exact strings app/page.tsx's dispatch() appended, in
 * order. Used to independently reconstruct/cross-check a receipt request
 * body without reaching into React internals. */
export async function readEventHistory(page: Page): Promise<string[]> {
  await openActivityDrawer(page);
  return page.evaluate(() =>
    Array.from(document.querySelectorAll('[data-testid^="activity-transcript-event-"]')).map(
      (el) => el.textContent ?? "",
    ),
  );
}
