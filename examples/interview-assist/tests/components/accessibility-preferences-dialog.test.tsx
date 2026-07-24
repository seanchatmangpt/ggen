/**
 * Real component test for components/accessibility-preferences-dialog.tsx
 * (SSR pattern, see cognition-panel.test.tsx's module doc for the
 * disclosed jsdom-absence scope note). `renderToStaticMarkup` does not run
 * `useEffect`, so the real `showModal()`/`close()` imperative calls this
 * component makes in response to its `open` prop are not exercised here --
 * only the fact that the dialog always renders its content (the real
 * generated AccessibilityControls, unmodified) is verified structurally,
 * matching the same disclosed limitation cognition-panel.test.tsx already
 * states for its own ref-driven focus effect.
 */
import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { AccessibilityPreferencesDialog } from "../../components/accessibility-preferences-dialog";
import { ACCESSIBILITY_DEFAULTS } from "../../lib/accessibility/defaults";

describe("AccessibilityPreferencesDialog (real React SSR render)", () => {
  it("wraps the real 16-control AccessibilityControls component inside a labeled dialog", () => {
    const html = renderToStaticMarkup(
      <AccessibilityPreferencesDialog
        open={true}
        settings={ACCESSIBILITY_DEFAULTS}
        onChange={() => {}}
        onClose={() => {}}
      />,
    );
    expect(html).toContain('data-testid="accessibility-preferences-dialog"');
    expect(html).toContain('aria-label="Accessibility preferences"');
    expect(html).toContain('data-testid="accessibility-controls"');
    expect(html).toContain('data-control-count="16"');
    for (const key of Object.keys(ACCESSIBILITY_DEFAULTS)) {
      expect(html).toContain(`data-testid="accessibility-control-${key}"`);
    }
    expect(html).toContain('data-testid="accessibility-preferences-close"');
  });
});
