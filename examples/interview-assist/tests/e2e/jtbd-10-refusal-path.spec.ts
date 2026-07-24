/**
 * JTBD 10: Full negative/refusal path stays coherent in the browser.
 *
 * Phase 4 relocated the "Trigger admission refusal (demo)" control into
 * SessionActivityDrawer's `?debug=1`-gated developer-diagnostics section
 * (session-activity-drawer.tsx) -- located here first via that real gate,
 * not assumed to still be a top-level button. Triggers the real demo
 * refusal (dispatches an event.family the reducer's real
 * KNOWN_EVENT_FAMILIES set does not admit) and asserts the UI renders a
 * real, non-crashing RefusalPresentation, and that the session stays fully
 * usable afterward.
 */
import { test, expect, submitUtterance, openActivityDrawer } from "./support";

test("the debug-gated admission-refusal demo renders a real refused state without crashing the page", async ({
  page,
  pageErrors,
}) => {
  await page.goto("/?debug=1");

  await expect(page.getByTestId("refusal-presentation")).toHaveCount(0);

  await openActivityDrawer(page);
  await expect(page.getByTestId("activity-dev-diagnostics")).toBeVisible();

  const phaseBefore = await page.getByTestId("phase-indicator").textContent();

  await page.getByTestId("trigger-admission-refusal-demo").click();

  const refusal = page.getByTestId("refusal-presentation");
  await expect(refusal).toBeVisible();
  await expect(refusal).toHaveAttribute("role", "alert");
  await expect(refusal).toHaveAttribute("data-code", "STALE_SESSION_EVENT");
  await expect(refusal).toContainText("Stale Session Event");
  await expect(refusal).toContainText('event.family "NotAnAdmittedEventFamily" is not an admitted EventFamily');

  // A refusal is a real, first-class admitted outcome (Architecture
  // Decision 13) -- it does not change the session's phase.
  await expect(page.getByTestId("phase-indicator")).toHaveText(phaseBefore ?? "");

  // The page did not throw for this real refused admission.
  expect(pageErrors).toHaveLength(0);

  // The session stays coherent and usable afterward: a real legitimate
  // cognition turn still works right after a refusal was displayed.
  const { body } = await submitUtterance(page, "I have an array of numbers to search through");
  expect(body.status).toBe("matched");
  await expect(page.getByTestId("cognition-panel-matched")).toBeVisible();
  expect(pageErrors).toHaveLength(0);
});

test("without ?debug=1 the admission-refusal demo control is not present at all", async ({ page }) => {
  await page.goto("/");
  await openActivityDrawer(page);
  await expect(page.getByTestId("activity-dev-diagnostics")).toHaveCount(0);
  await expect(page.getByTestId("trigger-admission-refusal-demo")).toHaveCount(0);
});
