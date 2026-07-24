/**
 * JTBD 9: Session completion produces a real receipt.
 *
 * Drives the flow through a real utterance + confirmation (producing real
 * recorded event-history labels), clicks "Finish session" (SessionMenu),
 * and asserts a real receipt (BLAKE3 checksum) is shown in the DOM AND
 * matches what the real /api/receipt route independently returns when
 * called directly with the exact same recorded event labels read back off
 * the live DOM -- an actual cross-check, not a trust-the-UI assumption.
 * The BLAKE3 hash is a deterministic pure function of its canonical input
 * (see app/api/receipt/route.ts), so two independent calls with identical
 * input must produce an identical checksum.
 */
import { test, expect, submitUtterance, openSessionMenu, readEventHistory } from "./support";

test("Finish session produces a real receipt whose checksum matches an independent direct /api/receipt call", async ({
  page,
}) => {
  await page.goto("/");

  // Build up real recorded session events: a real cognition match plus a
  // real confirmation.
  await submitUtterance(page, "I have an array of numbers to search through");
  await expect(page.getByTestId("cognition-panel-confirm")).toBeVisible();
  await page.getByTestId("cognition-panel-confirm").click();
  await expect(page.getByTestId("phase-indicator")).toHaveText("PREPARING");

  // Read the real recorded event-history labels straight off the live DOM
  // (state.usedEvents, rendered 1:1 by SessionActivityDrawer) BEFORE
  // finishing, so the independent cross-check below sends the identical
  // input the real client is about to send.
  const events = await readEventHistory(page);
  expect(events.length).toBeGreaterThanOrEqual(2);
  expect(events.some((e) => e.startsWith("SpeechEvent:"))).toBe(true);
  expect(events.some((e) => e.startsWith("HypothesisEvent:"))).toBe(true);

  await openSessionMenu(page);
  const [receiptResponse] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/receipt") && r.request().method() === "POST"),
    page.getByTestId("session-menu-finish").click(),
  ]);
  expect(receiptResponse.status()).toBe(200);
  const receiptBody = (await receiptResponse.json()) as {
    receipt: { checksum: { algorithm: string; checksumValue: string }; used: string[] };
  };
  expect(receiptBody.receipt.checksum.algorithm).toBe("BLAKE3");
  expect(receiptBody.receipt.checksum.checksumValue).toMatch(/^[0-9a-f]{64}$/);
  // The real request body used the exact same recorded events read off
  // the DOM above.
  expect(receiptBody.receipt.used).toEqual(events);

  // Real DOM evidence: SessionSummary renders the same checksum.
  const summaryChecksum = page.getByTestId("session-summary-checksum");
  await expect(summaryChecksum).toBeVisible();
  await expect(summaryChecksum).toHaveText(
    `${receiptBody.receipt.checksum.algorithm}:${receiptBody.receipt.checksum.checksumValue}`,
  );

  // Independent cross-check: call the real /api/receipt route directly
  // (real BLAKE3 hashing server-side, same route, separate real HTTP
  // request) with the identical label + used array, and assert the
  // checksum matches exactly.
  const direct = await page.request.post("/api/receipt", {
    data: { label: "interview-assist-session", used: events },
  });
  expect(direct.status()).toBe(200);
  const directBody = (await direct.json()) as { receipt: { checksum: { checksumValue: string } } };
  expect(directBody.receipt.checksum.checksumValue).toBe(receiptBody.receipt.checksum.checksumValue);

  // The real receipt inspector in SessionActivityDrawer also recorded this
  // (plus the earlier cognition-run receipt) as a real chained entry.
  const receiptEntries = await page.evaluate(() =>
    Array.from(document.querySelectorAll('[data-testid^="activity-receipt-checksum-"]')).map(
      (el) => el.textContent ?? "",
    ),
  );
  expect(receiptEntries.length).toBeGreaterThanOrEqual(1);
  expect(receiptEntries).toContain(
    `${receiptBody.receipt.checksum.algorithm}:${receiptBody.receipt.checksum.checksumValue}`,
  );
});
