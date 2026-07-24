import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { AccessibilityControls } from "../../components/accessibility-controls";
import type { AccessibilityDefaults } from "../../lib/accessibility/defaults";

const ALL_FALSE: AccessibilityDefaults = {
  "augmentative-communication-projection": false,
  "braille-display-output": false,
  "caption-driven-operation": false,
  "configurable-information-density": false,
  "controllable-audio-retention": false,
  "controllable-transcript-retention": false,
  "extended-processing-time-mode": false,
  "high-contrast-projection": false,
  "keyboard-only-operation": false,
  "magnified-single-cue-projection": false,
  "non-color-dependent-status": false,
  "reduced-motion-mode": false,
  "screen-reader-semantic-regions": false,
  "stable-layout-mode": false,
  "text-to-speech-projection": false,
  "zero-motor-input-operation": false,
};

describe("AccessibilityControls (real React SSR render, real RDF-derived settings)", () => {
  it("renders exactly 16 controls, matching AccessibilityDefaults' real key count", () => {
    const html = renderToStaticMarkup(
      <AccessibilityControls settings={ALL_FALSE} onChange={() => {}} />
    );
    expect(Object.keys(ALL_FALSE).length).toBe(16);
    expect(html).toContain('data-control-count="16"');
    // every real key must appear as a real testid in the rendered markup
    for (const key of Object.keys(ALL_FALSE)) {
      expect(html).toContain(`data-testid="accessibility-control-${key}"`);
    }
  });
});
