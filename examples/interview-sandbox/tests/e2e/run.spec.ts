import { expect, test } from "@playwright/test";

/**
 * Chicago TDD: real Next.js production server (Playwright's webServer),
 * real Chromium, real Monaco editor, real Python/Rust subprocess execution
 * via lib/executor.ts. No mocked fetch, no stubbed executor.
 */

test("home page loads with the Monaco editor and language selector", async ({ page }) => {
  await page.goto("/");
  await expect(page).toHaveTitle("Interview Sandbox");
  await expect(page.getByRole("heading", { name: "Interview Sandbox" })).toBeVisible();
  await expect(page.getByLabel("Language")).toBeVisible();
  await expect(page.getByRole("button", { name: "Run" })).toBeVisible();
  // Monaco mounts into a .monaco-editor element once loaded.
  await expect(page.locator(".monaco-editor")).toBeVisible({ timeout: 30_000 });
});

test("running the default Python starter code prints real output", async ({ page }) => {
  await page.goto("/");
  await expect(page.locator(".monaco-editor")).toBeVisible({ timeout: 30_000 });
  await page.getByRole("button", { name: "Run" }).click();
  await expect(page.getByTestId("console-output")).toContainText("hello from the sandbox", { timeout: 15_000 });
  await expect(page.getByTestId("console-output")).toContainText("exit 0");
});

test("switching to Rust and running prints real compiled output", async ({ page }) => {
  await page.goto("/");
  await expect(page.locator(".monaco-editor")).toBeVisible({ timeout: 30_000 });
  await page.getByLabel("Language").selectOption("rust");
  await page.getByRole("button", { name: "Run" }).click();
  await expect(page.getByTestId("console-output")).toContainText("hello from the sandbox", { timeout: 30_000 });
  await expect(page.getByTestId("console-output")).toContainText("exit 0");
});
