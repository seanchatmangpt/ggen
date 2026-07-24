/**
 * Shared test-only fixture (TICKET-050 + TICKET-053): real React SSR
 * composition of the real GENERATED presentation components (TICKET-030/
 * 032/033) plus a real Playwright Chromium accessibility-tree audit
 * helper. Extracted out of accessibility-projection.test.tsx so
 * TICKET-053's decisive acceptance test can reuse the EXACT SAME real
 * audit mechanism for acceptance-step/7 rather than re-implementing it
 * (this backlog's own Template responsibility: "shared fixture-building
 * utilities, reused across all 14 scenarios"). See
 * accessibility-projection.test.tsx's module doc for the full real
 * evidence of why app/page.tsx itself cannot be rendered in a real
 * browser this pass.
 */
import { renderToStaticMarkup } from "react-dom/server";
import { chromium, type Browser } from "@playwright/test";
import { existsSync } from "node:fs";
import { PhaseIndicator } from "../../../components/phase-indicator";
import { ProblemPanel } from "../../../components/problem-panel";
import { TrackCandidatePanel } from "../../../components/track-candidate-panel";
import { TestResultView } from "../../../components/test-result-view";
import { AccessibilityControls } from "../../../components/accessibility-controls";
import { SessionSummary } from "../../../components/session-summary";
import { ACCESSIBILITY_DEFAULTS } from "../../../lib/accessibility/defaults";

const FALLBACK_CHROMIUM_PATHS = [
  "/Users/sac/Library/Caches/ms-playwright/chromium_headless_shell-1181/chrome-mac/headless_shell",
  "/Users/sac/Library/Caches/ms-playwright/chromium-1181/chrome-mac/Chromium.app/Contents/MacOS/Chromium",
];

export function resolveExecutablePath(): string | undefined {
  return FALLBACK_CHROMIUM_PATHS.find((p) => existsSync(p));
}

export function renderComposedSessionPage(): string {
  const body = renderToStaticMarkup(
    <main>
      <h1>InterviewAssist</h1>
      <PhaseIndicator phase="COMPLETE" />
      <ProblemPanel problem={{ "problem/statement-of-work": "two-sum" }} />
      <section aria-label="Track candidates controls">
        <TrackCandidatePanel candidates={[{ id: "family-hash-map", rank: 1, evidence: {} }]} />
      </section>
      <TestResultView verification={{ "verification/run-example": true }} />
      <AccessibilityControls settings={ACCESSIBILITY_DEFAULTS} onChange={() => {}} />
      <SessionSummary receipt={{ used: ["demo"], checksum: { algorithm: "BLAKE3", checksumValue: "a".repeat(64) } }} />
    </main>,
  );
  return `<!doctype html><html lang="en"><head><meta charset="utf-8"><title>InterviewAssist (SSR audit fixture)</title></head><body>${body}</body></html>`;
}

export interface AuditResult {
  landmarkGroupCount: number;
  namedCheckboxCount: number;
  totalCheckboxCount: number;
  tabReachedTestIds: string[];
}

export async function auditAccessibility(browser: Browser, html: string, tabPresses: number): Promise<AuditResult> {
  const page = await browser.newPage();
  try {
    await page.setContent(html);
    const landmarkGroupCount = await page.getByRole("group", { name: "Accessibility settings" }).count();
    const ariaSnapshot = await page.locator("body").ariaSnapshot();
    const totalCheckboxCount = (ariaSnapshot.match(/checkbox/g) ?? []).length;
    const namedCheckboxCount = (ariaSnapshot.match(/checkbox "/g) ?? []).length;

    const tabReachedTestIds: string[] = [];
    for (let i = 0; i < tabPresses; i++) {
      await page.keyboard.press("Tab");
      const testId = await page.evaluate(
        () => document.activeElement?.closest("[data-testid]")?.getAttribute("data-testid") ?? null,
      );
      if (testId) tabReachedTestIds.push(testId);
    }
    return { landmarkGroupCount, namedCheckboxCount, totalCheckboxCount, tabReachedTestIds };
  } finally {
    await page.close();
  }
}

export { chromium };
export type { Browser };
