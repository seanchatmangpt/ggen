/**
 * JTBD 1: Submit an utterance, get a real cognition-proposed track.
 *
 * Real network round trip: fills the observed-utterance input, submits,
 * waits for the real POST /api/cognition response (which itself calls the
 * real wasm4pm-cognition Eliza breed, server-side, no mock), and asserts
 * one of the 4 real conclusions from lib/domain/cognition-rules.ts renders
 * in the DOM -- proving the text came from the real WASM output, not a
 * canned client-side string (imported here directly, not duplicated by
 * hand, so this test can never silently drift from the real rule set).
 */
import { test, expect, submitUtterance } from "./support";
import { COGNITION_RULES } from "../../lib/domain/cognition-rules";

test("submitting a Two-Sum-pattern utterance produces a real Eliza-proposed track", async ({ page, pageErrors }) => {
  await page.goto("/");

  const utterance = "I have an array of numbers to search through, trying to find two that match";
  const { response, body } = await submitUtterance(page, utterance);

  // Real network evidence: a 200 (matched) response from the real route.
  expect(response.status()).toBe(200);
  expect(body.status).toBe("matched");

  // The real WASM output picked a real rule; assert it is EXACTLY one of
  // the 4 rule conclusions cognition-rules.ts declares (not a hardcoded
  // duplicate string that could drift from the real ontology-generated
  // file).
  const conclusions = COGNITION_RULES.map((r) => r.conclusion);
  expect(conclusions).toContain(body.explanation);

  // The exact same real explanation text renders in the DOM.
  const question = page.getByTestId("cognition-panel-question");
  await expect(question).toHaveText(body.explanation as string);

  // The observed utterance itself is echoed back verbatim.
  await expect(page.getByTestId("cognition-panel-intent")).toContainText(utterance);

  // Confirm/reject controls are present for a real match.
  await expect(page.getByTestId("cognition-panel-confirm")).toBeVisible();
  await expect(page.getByTestId("cognition-panel-reject")).toBeVisible();

  expect(pageErrors).toHaveLength(0);
});

test("the ARRAY-keyword utterance deterministically selects the real two-sum-array rule", async ({ page }) => {
  await page.goto("/");
  const { body } = await submitUtterance(page, "I have an array of numbers to search through");
  expect(body.status).toBe("matched");
  expect(body.selected).toBe("ARRAY");
  const arrayRule = COGNITION_RULES.find((r) => r.id === "two-sum-array");
  expect(arrayRule).toBeDefined();
  expect(body.explanation).toBe(arrayRule!.conclusion);
  await expect(page.getByTestId("cognition-panel-matched")).toHaveAttribute("data-selected", "ARRAY");
});
