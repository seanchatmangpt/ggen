/**
 * Real component test for components/session-activity-drawer.tsx (SSR
 * pattern, see cognition-panel.test.tsx's module doc for the disclosed
 * jsdom-absence scope note).
 */
import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { SessionActivityDrawer } from "../../components/session-activity-drawer";
import type { TransitionReceipt } from "../../lib/domain/receipt";

const RECEIPT: TransitionReceipt = {
  label: "admission",
  used: ["SessionEvent:CREATED->PREPARING"],
  checksum: { algorithm: "BLAKE3", checksumValue: "a".repeat(64) },
};

describe("SessionActivityDrawer (real React SSR render)", () => {
  it("renders real event labels and real receipt checksums when provided", () => {
    const html = renderToStaticMarkup(
      <SessionActivityDrawer
        events={["SessionEvent:CREATED->PREPARING", "SpeechEvent:hello"]}
        receipts={[RECEIPT]}
        debug={false}
        phase="PREPARING"
        nextPhases={["READY"]}
        onAdvance={() => {}}
        onTriggerAdmissionRefusalDemo={() => {}}
      />,
    );
    expect(html).toContain('data-testid="session-activity-drawer"');
    // renderToStaticMarkup HTML-escapes ">" as "&gt;" inside text content.
    expect(html).toContain("SessionEvent:CREATED-&gt;PREPARING");
    expect(html).toContain("SpeechEvent:hello");
    expect(html).toContain(`BLAKE3:${"a".repeat(64)}`);
  });

  it("shows honest empty states when no events or receipts have been recorded yet", () => {
    const html = renderToStaticMarkup(
      <SessionActivityDrawer
        events={[]}
        receipts={[]}
        debug={false}
        phase="CREATED"
        nextPhases={["PREPARING"]}
        onAdvance={() => {}}
        onTriggerAdmissionRefusalDemo={() => {}}
      />,
    );
    expect(html).toContain('data-testid="activity-transcript-empty"');
    expect(html).toContain('data-testid="activity-receipts-empty"');
  });

  it("hides the developer-diagnostics section (manual phase-advance + admission-refusal demo) when debug is false", () => {
    const html = renderToStaticMarkup(
      <SessionActivityDrawer
        events={[]}
        receipts={[]}
        debug={false}
        phase="CREATED"
        nextPhases={["PREPARING"]}
        onAdvance={() => {}}
        onTriggerAdmissionRefusalDemo={() => {}}
      />,
    );
    expect(html).not.toContain('data-testid="activity-dev-diagnostics"');
    expect(html).not.toContain('data-testid="advance-to-PREPARING"');
    expect(html).not.toContain('data-testid="trigger-admission-refusal-demo"');
  });

  it("reveals the developer-diagnostics section, including a real Advance-to button per legal next phase, when debug is true", () => {
    const html = renderToStaticMarkup(
      <SessionActivityDrawer
        events={[]}
        receipts={[]}
        debug={true}
        phase="DEBUGGING"
        nextPhases={["EXPLANATION", "IMPLEMENTATION"]}
        onAdvance={() => {}}
        onTriggerAdmissionRefusalDemo={() => {}}
      />,
    );
    expect(html).toContain('data-testid="activity-dev-diagnostics"');
    expect(html).toContain('data-testid="advance-to-EXPLANATION"');
    expect(html).toContain('data-testid="advance-to-IMPLEMENTATION"');
    expect(html).toContain('data-testid="trigger-admission-refusal-demo"');
    expect(html).toContain("Raw phase: DEBUGGING");
  });
});
