/**
 * Real component test for components/session-header.tsx, following the
 * established SSR pattern (renderToStaticMarkup, no jsdom/testing-library
 * in this project -- see cognition-panel.test.tsx's module doc for the
 * same disclosed scope note, which applies identically here).
 */
import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { SessionHeader } from "../../components/session-header";

describe("SessionHeader (real React SSR render)", () => {
  it("renders a Title Case mode badge derived from the real PolicyId resource id, the current phase, and an idle input status", () => {
    const html = renderToStaticMarkup(
      <SessionHeader
        mode="policy/practice-mode"
        phase="CREATED"
        inputStatus="idle"
        onOpenAccessibilityPreferences={() => {}}
      />,
    );
    expect(html).toContain('data-testid="session-header-mode"');
    expect(html).toContain('data-mode="policy/practice-mode"');
    expect(html).toContain("Practice Mode");
    expect(html).toContain('data-testid="phase-indicator"');
    expect(html).toContain("CREATED");
    expect(html).toMatch(/data-testid="session-header-input-status"[^>]*data-status="idle"/);
    expect(html).toContain("Idle");
  });

  it("reflects a processing input status while a cognition request is in flight", () => {
    const html = renderToStaticMarkup(
      <SessionHeader
        mode="policy/assessment-mode"
        phase="PREPARING"
        inputStatus="processing"
        onOpenAccessibilityPreferences={() => {}}
      />,
    );
    expect(html).toMatch(/data-testid="session-header-input-status"[^>]*data-status="processing"/);
    expect(html).toContain("Processing");
    expect(html).toContain("Assessment Mode");
  });

  it("renders the accessibility-preferences entry point and a skip link to the current task", () => {
    const html = renderToStaticMarkup(
      <SessionHeader
        mode="policy/practice-mode"
        phase="CREATED"
        inputStatus="idle"
        onOpenAccessibilityPreferences={() => {}}
      />,
    );
    expect(html).toContain('data-testid="session-header-accessibility-button"');
    expect(html).toContain("Accessibility preferences");
    expect(html).toMatch(/<a href="#coding-region"[^>]*data-testid="skip-to-current-task"/);
  });
});
