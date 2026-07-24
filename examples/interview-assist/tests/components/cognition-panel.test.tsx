/**
 * Real component test for components/cognition-panel.tsx, following the
 * exact pattern already established by tests/components/refusal-presentation.test.tsx
 * and accessibility-controls.test.tsx: real React SSR (`renderToStaticMarkup`,
 * the real production rendering function -- not a mock, not a snapshot of
 * hand-typed HTML), asserting on the real rendered markup for each real
 * `CognitionOutcome` branch (`matched` / `no-track-matched` / `refused`).
 *
 * Scope note (stated honestly, not silently assumed): this vitest project's
 * `environment` is `"node"` (vitest.config.ts), and neither `jsdom` nor
 * `@testing-library/react` is a project dependency (checked package.json
 * before writing this file, per the task instruction). `renderToStaticMarkup`
 * does not run `useEffect` and produces no live DOM, so the actual
 * `headingRef.current?.focus()` call inside cognition-panel.tsx's
 * effect cannot be exercised as a real focus-movement assertion here --
 * this test instead verifies the STRUCTURAL prerequisite for that behavior
 * (`tabIndex={-1}` rendered as `tabindex="-1"` on the exact heading element
 * the ref is attached to, inside the real `aria-live="polite"` region), and
 * does not claim to have observed the browser actually move focus. A real
 * DOM-level focus assertion would need a Playwright/Chromium harness like
 * tests/scenarios/fixtures/accessibility-audit.tsx's -- that fixture is
 * explicitly out of scope to touch this session (a separate, concurrent
 * TICKET-050/052/053 workflow owns it).
 */
import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { CognitionPanel } from "../../components/cognition-panel";
import type { CognitionOutcome } from "../../lib/adapters/cognition-adapter";

// Phase 3 note: `receipt`/`signature`/`publicKeyId`/`signatureAlgorithm` are
// real fields on a real CognitionOutcome (see cognition-adapter.ts) but
// CognitionPanel never renders any of them -- this component test only
// exercises rendering behavior, so these fixture values are synthetic
// placeholders shaped like the real fields (64-hex checksum, 128-hex
// Ed25519 signature), not claimed to be real WASM output. The real,
// WASM-produced shape of these fields is verified separately by
// tests/adapters/cognition-adapter.test.ts and
// tests/scenarios/cognition-first-decisive.test.ts against the actual
// binary.
const FIXTURE_RECEIPT = {
  label: "cognition-run: matched",
  used: ["fixture-intent"],
  checksum: { algorithm: "BLAKE3" as const, checksumValue: "a".repeat(64) },
};

const MATCHED: CognitionOutcome = {
  status: "matched",
  selected: "ARRAY",
  explanation:
    "Is this a Two Sum-style problem -- finding two values in an array whose sum equals a target?",
  runId: "run-real-1",
  conformance: { fitness: 1, modelId: "eliza", refusals: [] },
  signature: "b".repeat(128),
  publicKeyId: "c".repeat(64),
  signatureAlgorithm: "ed25519",
  receipt: FIXTURE_RECEIPT,
};

const NO_TRACK_MATCHED: CognitionOutcome = {
  status: "no-track-matched",
  reason: "eliza: postcondition failed: empty inference trace (fraud signal)",
  receipt: { ...FIXTURE_RECEIPT, label: "cognition-run: no-track-matched" },
};

const REFUSED: CognitionOutcome = {
  status: "refused",
  reason: "intent must be a non-empty string",
};

// UX-polish pass (graceful WASM-load-failure handling): a real
// CognitionUnavailableOutcome shape has no receipt (see
// cognition-adapter.ts's doc for why) -- this fixture reflects that
// honestly rather than attaching one.
const UNAVAILABLE: CognitionOutcome = {
  status: "unavailable",
  reason: "Cannot find module '/definitely/does/not/exist/wasm4pm-cognition'",
};

describe("CognitionPanel (real React SSR render, real CognitionOutcome shapes)", () => {
  it("renders the observed intent and, for a matched outcome, the real explanation plus Yes/No controls", () => {
    const html = renderToStaticMarkup(
      <CognitionPanel
        intent="I have an array of numbers to search through"
        outcome={MATCHED}
        onConfirm={() => {}}
        onReject={() => {}}
      />,
    );
    expect(html).toContain("I have an array of numbers to search through");
    expect(html).toContain(
      "Is this a Two Sum-style problem -- finding two values in an array whose sum equals a target?",
    );
    expect(html).toContain('data-testid="cognition-panel-confirm"');
    expect(html).toContain('data-testid="cognition-panel-reject"');
    expect(html).toContain('data-selected="ARRAY"');
    // aria-live region wraps the real explanation -- screen readers get a
    // real polite announcement, not an assertive interruption.
    expect(html).toMatch(/aria-live="polite"[^>]*>[\s\S]*Two Sum-style/);
    // Structural prerequisite for the real focus-on-render effect: the
    // question heading is tabIndex={-1} (programmatically focusable, not
    // in the normal Tab order) -- see this file's module doc for why the
    // actual focus() call is not exercised here.
    expect(html).toMatch(/<h2 tabindex="-1"[^>]*data-testid="cognition-panel-question"/);
  });

  it("renders an honest no-track-matched message, never a fabricated/guessed track, and no Yes/No controls", () => {
    const html = renderToStaticMarkup(
      <CognitionPanel intent="hello there, nice weather today" outcome={NO_TRACK_MATCHED} onConfirm={() => {}} onReject={() => {}} />,
    );
    expect(html).toContain('data-testid="cognition-panel-no-track-matched"');
    expect(html).toContain("couldn");
    expect(html).not.toContain('data-testid="cognition-panel-confirm"');
    expect(html).not.toContain('data-testid="cognition-panel-reject"');
  });

  it("renders a real refusal reason for a refused outcome, as an alert", () => {
    const html = renderToStaticMarkup(
      <CognitionPanel intent="" outcome={REFUSED} onConfirm={() => {}} onReject={() => {}} />,
    );
    expect(html).toContain('data-testid="cognition-panel-refused"');
    expect(html).toContain("intent must be a non-empty string");
    expect(html).toMatch(/role="alert"[^>]*data-testid="cognition-panel-refused"/);
  });

  it("renders neither matched nor non-matched branches when outcome is null (no proposal yet)", () => {
    const html = renderToStaticMarkup(
      <CognitionPanel intent="" outcome={null} onConfirm={() => {}} onReject={() => {}} />,
    );
    expect(html).not.toContain("cognition-panel-matched");
    expect(html).not.toContain("cognition-panel-no-track-matched");
    expect(html).not.toContain("cognition-panel-refused");
  });

  it("renders an honest 'temporarily unavailable' state (never a raw error/stack trace) for a real unavailable outcome, as an alert, with a Retry control", () => {
    const html = renderToStaticMarkup(
      <CognitionPanel
        intent="I have an array of numbers to search through"
        outcome={UNAVAILABLE}
        onConfirm={() => {}}
        onReject={() => {}}
        onRetryUnavailable={() => {}}
      />,
    );
    expect(html).toContain('data-testid="cognition-panel-unavailable"');
    expect(html).toContain("temporarily unavailable");
    expect(html).toMatch(/role="alert"[^>]*data-testid="cognition-panel-unavailable"/);
    // Never leaks the raw module-resolution error text into the UI.
    expect(html).not.toContain("Cannot find module");
    expect(html).toContain('data-testid="cognition-panel-retry-unavailable"');
    // No Yes/No controls for an infra-level failure -- there is no real
    // hypothesis to confirm or reject.
    expect(html).not.toContain('data-testid="cognition-panel-confirm"');
    expect(html).not.toContain('data-testid="cognition-panel-reject"');
  });

  it("omits the Retry control when onRetryUnavailable is not supplied", () => {
    const html = renderToStaticMarkup(
      <CognitionPanel intent="x" outcome={UNAVAILABLE} onConfirm={() => {}} onReject={() => {}} />,
    );
    expect(html).toContain('data-testid="cognition-panel-unavailable"');
    expect(html).not.toContain('data-testid="cognition-panel-retry-unavailable"');
  });
});
