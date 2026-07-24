/**
 * JTBD 8: The live region announces the real cognition response.
 *
 * Reads the actual `aria-live` attribute and real text content of
 * cognition-panel.tsx's live region via direct DOM query (not asserted
 * from memory / not just checking the visible question text elsewhere on
 * the page) after a real POST /api/cognition round trip.
 */
import { test, expect, submitUtterance } from "./support";
import { COGNITION_RULES } from "../../lib/domain/cognition-rules";

test("the real aria-live region updates to contain the real Eliza explanation after submission", async ({
  page,
}) => {
  await page.goto("/");

  const liveRegion = page.getByTestId("cognition-panel-live-region");

  // Before submission: the live region exists only once a cognition
  // outcome has rendered (CognitionPanel itself is conditionally rendered
  // by app/page.tsx). Confirm the honest pre-state first.
  await expect(page.getByTestId("cognition-panel")).toHaveCount(0);

  const { body } = await submitUtterance(page, "I have an array of numbers to search through");
  expect(body.status).toBe("matched");

  // Real DOM query of the actual aria-live attribute value.
  await expect(liveRegion).toBeVisible();
  const ariaLive = await liveRegion.getAttribute("aria-live");
  expect(ariaLive).toBe("polite");

  // Real DOM query of the actual text content -- must equal one of the 4
  // real rule conclusions (imported, not duplicated by hand) and must
  // equal the exact explanation the real network response carried.
  const liveText = await liveRegion.textContent();
  expect(liveText).toBeTruthy();
  expect(liveText).toContain(body.explanation as string);
  expect(COGNITION_RULES.map((r) => r.conclusion)).toContain(body.explanation);

  // The live region's content is not merely a static duplicate -- it wraps
  // the same question heading a screen reader would announce.
  const question = liveRegion.locator('[data-testid="cognition-panel-question"]');
  await expect(question).toHaveText(body.explanation as string);
});

test("the live region also carries the real no-track-matched status message (role=status, still inside aria-live)", async ({
  page,
}) => {
  await page.goto("/");
  const { body } = await submitUtterance(page, "tell me about your weekend plans");
  expect(body.status).toBe("no-track-matched");

  const liveRegion = page.getByTestId("cognition-panel-live-region");
  await expect(liveRegion).toHaveAttribute("aria-live", "polite");
  const statusMessage = liveRegion.locator('[data-testid="cognition-panel-no-track-matched"]');
  await expect(statusMessage).toHaveAttribute("role", "status");
  const text = await statusMessage.textContent();
  expect(text).toContain("couldn't match that to a known track");
});
