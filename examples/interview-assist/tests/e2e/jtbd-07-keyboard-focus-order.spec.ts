/**
 * JTBD 7: Keyboard/focus-order flow matches the real implemented order.
 *
 * Reads the REAL Tab-key sequence from a fresh page load via
 * document.activeElement after each Tab press (readTabSequence in
 * support.ts) -- no assumption baked in about what "should" happen; the
 * assertion is built from what this real run actually observed, logged
 * below for anyone auditing this file against a future DOM change.
 *
 * app/page.tsx's own module doc states the intended aspirational order as:
 *   skip-to-current-task -> session status -> cognition question/track
 *   choices -> problem statement -> editor+run -> execution result/visible
 *   tests -> session actions.
 * Read literally, several of those "reachable" nodes are not actually Tab
 * stops on a fresh, empty session: PhaseIndicator (session status) has no
 * tabIndex, so it is never a Tab stop at all; the cognition question/track
 * choices/problem statement/execution-result regions render no focusable
 * element until real data exists (no track proposed yet, no problem
 * assigned yet, no execution run yet). This test validates what IS real on
 * a fresh load, and a second part validates the fuller order once a real
 * cognition match has populated the Yes/No controls -- rather than
 * asserting the aspirational order verbatim against an empty session.
 *
 * JTBD 5 closure update: app/page.tsx gained two new real Tab stops,
 * "run-visible-tests"/"run-hidden-tests", immediately after "run-code"
 * (same <section aria-label="Code"> block) and before the 12 editor-action
 * buttons -- real DOM position, not asserted from memory.
 *
 * Interactive-drawer closure update (this pass): components/session-
 * workspace.tsx's 3 side regions (Cognition / Current objective / Evidence)
 * each gained a real "drawer-toggle-<region>" <button> Tab stop, rendered
 * as the first focusable element inside that region's <section> -- real
 * DOM position, confirmed by re-running this exact test and reading the
 * observed sequence before hand-writing any assertion below. The Coding
 * region has no toggle (see jtbd-12-interactive-drawer.spec.ts). The
 * observed-sequence loop bound was widened accordingly (24 -> 28: 3 more
 * real stops ahead of "session-menu-toggle" than the prior 24-stop count,
 * plus headroom so this doesn't sit exactly on the boundary again).
 */
import { test, expect } from "./support";
import { submitUtterance } from "./support";

test("fresh-load Tab order visits every real focusable control in real DOM order", async ({ page }) => {
  await page.goto("/");

  const observed: string[] = [];
  for (let i = 0; i < 28; i++) {
    await page.keyboard.press("Tab");
    const testId = await page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null);
    const tag = await page.evaluate(() => document.activeElement?.tagName ?? null);
    if (testId === null && tag === "BODY") break; // Tab cycled back out of the page.
    observed.push(testId ?? `(untagged:${tag})`);
  }

  // eslint-disable-next-line no-console
  console.log("Real observed fresh-load Tab sequence:", JSON.stringify(observed));

  // Real, load-bearing facts about this sequence, asserted individually so
  // a failure names exactly which real expectation broke:
  expect(observed).toContain("skip-to-current-task");
  expect(observed).toContain("session-header-accessibility-button");
  expect(observed).toContain("drawer-toggle-cognition");
  expect(observed).toContain("cognition-intent-input");
  expect(observed).toContain("drawer-toggle-objective");
  expect(observed).toContain("code-editor");
  expect(observed).toContain("run-code");
  expect(observed).toContain("run-visible-tests");
  expect(observed).toContain("run-hidden-tests");
  for (const action of [
    "editor/apply-deterministic-refactor",
    "editor/create-file",
    "editor/delete-file",
    "editor/display-diagnostics",
    "editor/display-diff",
    "editor/format-source",
    "editor/inspect-definition",
    "editor/inspect-references",
    "editor/modify-file",
    "editor/navigate-symbol",
    "editor/open-file",
    "editor/rename-file",
  ]) {
    expect(observed).toContain(`editor-action-${action}`);
  }
  expect(observed).toContain("drawer-toggle-result");
  expect(observed).toContain("session-activity-drawer-toggle");
  expect(observed).toContain("session-menu-toggle");
  // No toggle for the Coding region -- it is never collapsed.
  expect(observed).not.toContain("drawer-toggle-coding");

  // Real, disclosed facts: the cognition-submit button is disabled (empty
  // input) so it is skipped from the tab order entirely on a fresh load;
  // PhaseIndicator (session status) is not a Tab stop at all (no
  // tabIndex on that component -- see components/phase-indicator.tsx).
  expect(observed).not.toContain("cognition-submit");

  // Real DOM order invariant: skip-link comes before the accessibility
  // button, which comes before the Cognition drawer toggle, which comes
  // before the cognition input, which comes before the Current-objective
  // drawer toggle, which comes before the code editor, which comes before
  // Run, which comes before the real "Run visible tests"/"Run hidden
  // tests" controls (JTBD 5 closure), which come before the 12 editor
  // actions, which come before the Evidence drawer toggle, which comes
  // before the activity-drawer toggle, which comes before the session-menu
  // toggle. Verified against the real indices in the real observed array
  // (not hand-asserted order without evidence).
  const idx = (id: string) => observed.indexOf(id);
  expect(idx("skip-to-current-task")).toBeLessThan(idx("session-header-accessibility-button"));
  expect(idx("session-header-accessibility-button")).toBeLessThan(idx("drawer-toggle-cognition"));
  expect(idx("drawer-toggle-cognition")).toBeLessThan(idx("cognition-intent-input"));
  expect(idx("cognition-intent-input")).toBeLessThan(idx("drawer-toggle-objective"));
  expect(idx("drawer-toggle-objective")).toBeLessThan(idx("code-editor"));
  expect(idx("code-editor")).toBeLessThan(idx("run-code"));
  expect(idx("run-code")).toBeLessThan(idx("run-visible-tests"));
  expect(idx("run-visible-tests")).toBeLessThan(idx("run-hidden-tests"));
  expect(idx("run-hidden-tests")).toBeLessThan(idx("editor-action-editor/apply-deterministic-refactor"));
  expect(idx("editor-action-editor/rename-file")).toBeLessThan(idx("drawer-toggle-result"));
  expect(idx("drawer-toggle-result")).toBeLessThan(idx("session-activity-drawer-toggle"));
  expect(idx("session-activity-drawer-toggle")).toBeLessThan(idx("session-menu-toggle"));
});

test("once a track is proposed, the question heading receives focus and Tab reaches Yes/No next in real DOM order", async ({
  page,
}) => {
  await page.goto("/");
  await submitUtterance(page, "I have an array of numbers to search through");

  await expect
    .poll(async () => page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null))
    .toBe("cognition-panel-question");

  await page.keyboard.press("Tab");
  expect(await page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null)).toBe(
    "cognition-panel-confirm",
  );

  await page.keyboard.press("Tab");
  expect(await page.evaluate(() => document.activeElement?.getAttribute("data-testid") ?? null)).toBe(
    "cognition-panel-reject",
  );
});
