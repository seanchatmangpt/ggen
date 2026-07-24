import { expect, test } from "@playwright/test";

/**
 * Chicago-TDD sanity check: real Next.js production server (Playwright's
 * webServer), real Chromium, real HTTP requests. No route interception, no
 * mocked fetch, no fabricated session/database state. Assertions are on
 * actually-observed page and response content, tolerant of whichever real
 * infra state (configured vs. unconfigured DATABASE_URL/BETTER_AUTH_SECRET/
 * AI_GATEWAY_API_KEY) this run happens to have, since that's exactly the
 * typed-refusal boundary the app itself is supposed to report honestly.
 */

test("home page boots without a client-side crash and reaches a stable auth state", async ({ page }) => {
  const pageErrors: Error[] = [];
  page.on("pageerror", (error) => pageErrors.push(error));

  await page.goto("/");
  await expect(page).toHaveTitle("Next.js AI SDK");

  // AuthGate resolves out of its "Loading session" placeholder into either
  // the sign-up/sign-in form (no session) or the chat workspace (session
  // present) -- it must not hang on the loading state.
  await expect(
    page.getByRole("heading", { name: /Create account|Sign in|Admitted AI Workspace/ }),
  ).toBeVisible({ timeout: 15_000 });

  expect(pageErrors, `uncaught client-side error(s): ${pageErrors.map((e) => e.message).join("; ")}`).toEqual([]);
});

test("unauthenticated visitor sees the auth form, never the chat workspace", async ({ page }) => {
  await page.goto("/");
  await expect(page.getByRole("heading", { name: /Create account|Sign in/ })).toBeVisible({ timeout: 15_000 });
  await expect(page.getByRole("heading", { name: "Admitted AI Workspace" })).not.toBeVisible();
  await expect(page.getByPlaceholder("Ask or request an action")).not.toBeVisible();
});

test("auth form renders real required fields and starts in sign-up mode", async ({ page }) => {
  await page.goto("/");
  await expect(page.getByRole("heading", { name: "Create account" })).toBeVisible({ timeout: 15_000 });
  await expect(page.getByLabel("Name")).toBeVisible();
  await expect(page.getByLabel("Email")).toBeVisible();
  await expect(page.getByLabel("Password")).toBeVisible();
  await expect(page.getByRole("button", { name: "Create account" })).toBeVisible();

  await page.getByRole("button", { name: "Use an existing account" }).click();
  await expect(page.getByRole("heading", { name: "Sign in" })).toBeVisible();
  await expect(page.getByLabel("Name")).not.toBeVisible();
});

test("/api/health reports a real, internally consistent boundary status", async ({ request }) => {
  const response = await request.get("/api/health");
  const body = await response.json();

  if (body.status === "BLOCKED") {
    // Unconfigured infra: the endpoint must fail closed (503), not pretend success.
    expect(response.status()).toBe(503);
    expect(Array.isArray(body.missing) || body.code === "DATABASE_UNREACHABLE").toBe(true);
  } else {
    // Configured infra: a real `select 1` round-trip against Postgres succeeded.
    expect(body.status).toBe("ALIVE");
    expect(response.status()).toBe(200);
    expect(body.database).toBe("reachable");
    expect(typeof body.model).toBe("string");
  }
});

test("/api/chat refuses an unauthenticated request rather than silently succeeding", async ({ request }) => {
  const response = await request.post("/api/chat", { data: {} });
  expect(response.status()).toBeGreaterThanOrEqual(400);
});
