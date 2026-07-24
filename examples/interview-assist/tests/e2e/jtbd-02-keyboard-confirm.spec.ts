/**
 * JTBD 2: Confirm a track via keyboard only, phase advances with no manual
 * button.
 *
 * Every interaction below uses page.keyboard.press/type only -- zero
 * page.click calls anywhere in this file, and specifically zero clicks on
 * any `data-testid="advance-to-*"` element. Real network round trip
 * (POST /api/cognition) still happens; only the client-side interaction is
 * keyboard-driven.
 *
 * Real DOM fact this test relies on (verified by reading
 * components/cognition-panel.tsx directly): once a match renders, a
 * useEffect calls the question <h2 tabIndex={-1}>'s .focus() programmatically
 * -- tabIndex={-1} means it is NOT part of sequential (Tab-key) navigation,
 * but it CAN receive focus programmatically, which the real component does.
 * So after a match, document.activeElement really is the question heading,
 * and a single subsequent Tab press reaches the real "Yes" button
 * (cognition-panel-confirm) next in DOM order.
 */
import { test, expect } from "./support";

test("keyboard-only: Tab + type + Enter submits, Tab + Enter confirms, phase advances with zero clicks", async ({
  page,
}) => {
  await page.goto("/");

  // Reach the observed-utterance input using only the Tab key (no click,
  // no .focus()). Fresh-load DOM order: skip-link -> accessibility button
  // -> cognition-intent-input.
  let reachedInput = false;
  for (let i = 0; i < 10; i++) {
    const testId = await page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null);
    if (testId === "cognition-intent-input") {
      reachedInput = true;
      break;
    }
    await page.keyboard.press("Tab");
  }
  expect(reachedInput).toBe(true);

  // Type the utterance via real keyboard events, then submit with Enter
  // (the input's own onKeyDown handler calls submitCognitionUtterance()
  // on Enter -- no click on the Submit button).
  await page.keyboard.type("I have an array of numbers to search through");
  const [cognitionResponse] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/cognition") && r.request().method() === "POST"),
    page.keyboard.press("Enter"),
  ]);
  expect(cognitionResponse.status()).toBe(200);

  // Confirm the real Eliza question rendered and (per the module doc
  // above) programmatic focus landed on its heading.
  await expect(page.getByTestId("cognition-panel-question")).toBeVisible();
  await expect
    .poll(async () => page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null))
    .toBe("cognition-panel-question");

  // Real phase before confirming.
  await expect(page.getByTestId("phase-indicator")).toHaveText("CREATED");

  // One Tab press from the (programmatically-focused) question heading
  // reaches the real "Yes" button next in DOM order -- assert this
  // explicitly before activating it, so a future DOM reshuffle fails
  // loudly here rather than silently activating the wrong control.
  await page.keyboard.press("Tab");
  const focusedAfterOneTab = await page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null);
  expect(focusedAfterOneTab).toBe("cognition-panel-confirm");

  // Activate "Yes" via the keyboard (Enter triggers a real button click
  // event in Chromium) -- this is the one and only activation in this
  // test, and it is not a page.click call.
  await page.keyboard.press("Enter");

  // Real result: session phase advanced as a DIRECT consequence of the
  // HypothesisEvent dispatch (PHASE_TRANSITIONS["CREATED"] === ["PREPARING"]),
  // with zero clicks anywhere in this test and zero interaction with any
  // advance-to-* control.
  await expect(page.getByTestId("phase-indicator")).toHaveText("PREPARING");

  // The cognition panel closes (parent clears intent/outcome on confirm).
  await expect(page.getByTestId("cognition-panel")).toHaveCount(0);
});
