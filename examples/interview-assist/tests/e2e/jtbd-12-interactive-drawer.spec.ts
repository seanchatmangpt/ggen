/**
 * JTBD (drawer gap closure): the tablet-width side "drawer" columns
 * (Cognition / Current objective / Evidence) are now a REAL interactively
 * collapsible drawer, not just the pre-existing CSS-only narrowing
 * (app/globals.css's `@media (min-width: 700px)` block, still present and
 * unchanged). Real browser, real viewport resize, real click, real
 * `aria-expanded` attribute read directly off the live DOM, real
 * show/hide of content verified via Playwright's `toBeVisible`/
 * `toBeHidden` (backed by the actual `hidden` attribute
 * components/session-workspace.tsx's `DrawerSection` toggles).
 *
 * Run at an explicit tablet-width viewport (800x900, inside the 700-1100px
 * breakpoint band) rather than the default desktop viewport
 * (playwright.config.ts) -- the task is specifically about the tablet
 * "drawer" treatment, even though the underlying React toggle logic itself
 * is viewport-independent (see session-workspace.tsx's module doc: the CSS
 * visual "drawer" look is still tablet-gated, but the interactive
 * collapse/expand behavior added here works at any width by design, so it
 * doesn't regress at 700-1100px specifically).
 */
import { test, expect } from "./support";

test.use({ viewport: { width: 800, height: 900 } });

test("the Cognition drawer really collapses and expands at tablet width, with real aria-expanded and content visibility", async ({
  page,
}) => {
  await page.goto("/");

  const toggle = page.getByTestId("drawer-toggle-cognition");
  const body = page.getByTestId("workspace-region-cognition-body");

  // Real default state: expanded, content visible, aria-expanded="true".
  await expect(toggle).toHaveAttribute("aria-expanded", "true");
  await expect(body).toBeVisible();
  await expect(page.getByTestId("cognition-intent-input")).toBeVisible();

  // Real click collapses it: aria-expanded flips, content is really hidden
  // (not just visually de-emphasized).
  await toggle.click();
  await expect(toggle).toHaveAttribute("aria-expanded", "false");
  await expect(body).toBeHidden();
  await expect(page.getByTestId("cognition-intent-input")).toBeHidden();
  await expect(toggle).toHaveText("Expand");

  // Real click expands it again: content reappears.
  await toggle.click();
  await expect(toggle).toHaveAttribute("aria-expanded", "true");
  await expect(body).toBeVisible();
  await expect(page.getByTestId("cognition-intent-input")).toBeVisible();
  await expect(toggle).toHaveText("Collapse");
});

test("the Evidence and Current-objective drawers each toggle independently of Cognition and of each other", async ({
  page,
}) => {
  await page.goto("/");

  const cognitionToggle = page.getByTestId("drawer-toggle-cognition");
  const objectiveToggle = page.getByTestId("drawer-toggle-objective");
  const resultToggle = page.getByTestId("drawer-toggle-result");
  const objectiveBody = page.getByTestId("workspace-region-objective-body");
  const resultBody = page.getByTestId("workspace-region-result-body");

  // Collapse only the Evidence (result) drawer.
  await resultToggle.click();
  await expect(resultToggle).toHaveAttribute("aria-expanded", "false");
  await expect(resultBody).toBeHidden();

  // Cognition and Current objective are untouched -- independent state.
  await expect(cognitionToggle).toHaveAttribute("aria-expanded", "true");
  await expect(objectiveToggle).toHaveAttribute("aria-expanded", "true");
  await expect(objectiveBody).toBeVisible();

  // Collapse Current objective too; Evidence stays collapsed, Cognition
  // stays expanded -- three genuinely independent pieces of real state.
  await objectiveToggle.click();
  await expect(objectiveToggle).toHaveAttribute("aria-expanded", "false");
  await expect(objectiveBody).toBeHidden();
  await expect(resultToggle).toHaveAttribute("aria-expanded", "false");
  await expect(cognitionToggle).toHaveAttribute("aria-expanded", "true");
});

test("the Coding region has no drawer toggle -- it is never collapsed", async ({ page }) => {
  await page.goto("/");
  await expect(page.getByTestId("drawer-toggle-coding")).toHaveCount(0);
  await expect(page.getByTestId("code-editor")).toBeVisible();
});
