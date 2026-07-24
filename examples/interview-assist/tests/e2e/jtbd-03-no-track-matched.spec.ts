/**
 * JTBD 3: A non-matching utterance gets an honest "couldn't match" message,
 * not a crash.
 *
 * Real network round trip: an utterance containing none of the 4 real
 * cognition-rules.ts keywords (ARRAY/INDICES/SUM/TARGET) makes the real
 * wasm4pm-cognition Eliza breed throw its real fail-closed
 * "empty inference trace" error server-side (see cognition-adapter.ts's
 * module doc) -- app/api/cognition/route.ts maps this to a 422
 * "no-track-matched" outcome, and cognition-panel.tsx renders the honest
 * message. This is a real, deliberate anti-fraud refusal, not a bug --
 * asserted here to render without ever surfacing as an unhandled page
 * error.
 */
import { test, expect } from "./support";

test("a no-keyword utterance renders the real no-track-matched message with zero page errors", async ({
  page,
  pageErrors,
}) => {
  await page.goto("/");

  const utterance = "What's your favorite programming language and why do you enjoy backend work?";
  const input = page.getByTestId("cognition-intent-input");
  await input.fill(utterance);

  const [response] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/cognition") && r.request().method() === "POST"),
    page.getByTestId("cognition-submit").click(),
  ]);

  // Real network evidence: the documented 422 convention for a real,
  // well-formed non-match (app/api/cognition/route.ts's own comment).
  expect(response.status()).toBe(422);
  const body = (await response.json()) as { status: string; reason: string };
  expect(body.status).toBe("no-track-matched");
  expect(body.reason).toContain("empty inference trace");

  // The honest, non-crashing message renders (role="status", not an
  // error/alert -- this is a disclosed non-match, not a failure).
  const noMatch = page.getByTestId("cognition-panel-no-track-matched");
  await expect(noMatch).toBeVisible();
  await expect(noMatch).toHaveAttribute("role", "status");
  await expect(noMatch).toContainText("couldn't match that to a known track");

  // No Yes/No confirm controls for a non-match.
  await expect(page.getByTestId("cognition-panel-confirm")).toHaveCount(0);
  await expect(page.getByTestId("cognition-panel-reject")).toHaveCount(0);

  // Session phase is untouched by a non-match (no HypothesisEvent was
  // ever dispatched for it).
  await expect(page.getByTestId("phase-indicator")).toHaveText("CREATED");

  // The core requirement: the page never threw an unhandled JS exception
  // for this real fail-closed WASM error path.
  expect(pageErrors).toHaveLength(0);

  // The page remains fully interactive afterward -- not stuck in a broken
  // state. Prove it by successfully submitting a real matching utterance
  // right after.
  await input.fill("I have an array of numbers to search through");
  const [secondResponse] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/cognition") && r.request().method() === "POST"),
    page.getByTestId("cognition-submit").click(),
  ]);
  expect(secondResponse.status()).toBe(200);
  await expect(page.getByTestId("cognition-panel-matched")).toBeVisible();
  expect(pageErrors).toHaveLength(0);
});
