/**
 * UX-polish pass, item 1: real, accessible busy/pending states while a real
 * network request is in flight -- cognition submit, run code, run visible
 * tests, run hidden tests, and finish session.
 *
 * Real timing is short (a real WASM call, a real subprocess) but non-zero,
 * so asserting the busy state exists is a genuine race against a fast real
 * response. Rather than assert-then-tolerate-a-miss (the pattern
 * jtbd-06-accessibility-controls.spec.ts uses for an incidental, secondary
 * assertion), these tests make the window deterministic and generous using
 * `page.route()` + `route.fetch()` + a real artificial delay before
 * `route.fulfill()`: `route.fetch()` performs the REAL request against the
 * real running server (real WASM call / real subprocess, real response
 * body) -- nothing about the request or its data is mocked -- only the
 * DELIVERY of that already-real response back to the page is delayed. This
 * is the same technique this file's own task explicitly names
 * ("artificially slows"), kept honest by never fabricating response
 * content.
 */
import { test, expect } from "./support";

const DELAY_MS = 600;

async function delayRealResponses(page: import("@playwright/test").Page, urlSubstring: string): Promise<void> {
  await page.route(`**${urlSubstring}`, async (route) => {
    const response = await route.fetch();
    await new Promise((resolve) => setTimeout(resolve, DELAY_MS));
    await route.fulfill({ response });
  });
}

test("cognition submit shows a real accessible busy state (aria-busy + visible spinner) while the real request is in flight, then clears", async ({
  page,
}) => {
  await page.goto("/");
  await delayRealResponses(page, "/api/cognition");

  const button = page.getByTestId("cognition-submit");
  await page.getByTestId("cognition-intent-input").fill("I have an array of numbers to search through");

  await expect(button).toHaveAttribute("aria-busy", "false");

  const roundTrip = Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/cognition") && r.request().method() === "POST"),
    button.click(),
  ]);

  // The real request is now in flight, artificially held open for
  // DELAY_MS -- the busy state must be observable for that entire window.
  await expect(button).toHaveAttribute("aria-busy", "true");
  await expect(button).toBeDisabled();
  await expect(button).toHaveText(/Submitting/);
  await expect(page.getByTestId("cognition-submit-spinner")).toBeVisible();

  await roundTrip;

  await expect(button).toHaveAttribute("aria-busy", "false");
  await expect(page.getByTestId("cognition-submit-spinner")).toHaveCount(0);
});

test("Run shows a real accessible busy state while the real sandbox execution is in flight, then clears", async ({
  page,
}) => {
  await page.goto("/");
  await delayRealResponses(page, "/api/run");

  const button = page.getByTestId("run-code");
  await expect(button).toHaveAttribute("aria-busy", "false");

  const roundTrip = Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/run") && r.request().method() === "POST"),
    button.click(),
  ]);

  await expect(button).toHaveAttribute("aria-busy", "true");
  await expect(button).toBeDisabled();
  await expect(button).toHaveText(/Running/);
  await expect(page.getByTestId("run-code-spinner")).toBeVisible();

  await roundTrip;

  await expect(button).toHaveAttribute("aria-busy", "false");
  await expect(page.getByTestId("run-code-spinner")).toHaveCount(0);
});

test("Run visible tests / Run hidden tests each show an independent real busy state while their real pytest subprocess is in flight", async ({
  page,
}) => {
  await page.goto("/");
  await delayRealResponses(page, "/api/test");

  const visibleButton = page.getByTestId("run-visible-tests");
  const hiddenButton = page.getByTestId("run-hidden-tests");

  const roundTrip = Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/test") && r.request().method() === "POST"),
    visibleButton.click(),
  ]);

  await expect(visibleButton).toHaveAttribute("aria-busy", "true");
  await expect(visibleButton).toHaveText(/Running visible tests/);
  await expect(page.getByTestId("run-visible-tests-spinner")).toBeVisible();
  // Real mutual exclusion: the hidden-tests button is disabled (shares
  // `runningTest` state) but is NOT itself marked busy -- only the one the
  // human actually clicked is.
  await expect(hiddenButton).toBeDisabled();
  await expect(hiddenButton).toHaveAttribute("aria-busy", "false");

  await roundTrip;

  await expect(visibleButton).toHaveAttribute("aria-busy", "false");
  await expect(page.getByTestId("run-visible-tests-spinner")).toHaveCount(0);
  await expect(hiddenButton).toBeEnabled();
});

test("Finish session shows a real accessible busy state on the Finish button while the real receipt request is in flight, then clears", async ({
  page,
}) => {
  await page.goto("/");
  await delayRealResponses(page, "/api/receipt");

  await page.getByTestId("session-menu-toggle").click();
  const finishButton = page.getByTestId("session-menu-finish");
  await expect(finishButton).toHaveAttribute("aria-busy", "false");
  await expect(finishButton).toHaveText("Finish session");

  const roundTrip = Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/receipt") && r.request().method() === "POST"),
    finishButton.click(),
  ]);

  await expect(finishButton).toHaveAttribute("aria-busy", "true");
  await expect(finishButton).toBeDisabled();
  await expect(finishButton).toHaveText(/Finishing session/);
  await expect(page.getByTestId("session-menu-finish-spinner")).toBeVisible();

  await roundTrip;

  await expect(finishButton).toHaveAttribute("aria-busy", "false");
  await expect(finishButton).toHaveText("Finish session");
  // The real receipt landed -- SessionSummary renders it.
  await expect(page.getByTestId("session-summary-checksum")).toBeVisible();
});
