/**
 * Real component test for components/session-workspace.tsx (SSR pattern,
 * see cognition-panel.test.tsx's module doc for the disclosed jsdom-absence
 * scope note). Verifies the four named regions render in the required
 * mobile-first DOM order (cognition -> objective -> coding -> result) --
 * the CSS grid-template-areas that visually reposition them per breakpoint
 * live in app/globals.css and are not exercised by this SSR-only test (no
 * layout engine), but DOM order is real, checkable evidence that the
 * keyboard/focus-order sequence app/page.tsx's module doc describes is
 * actually what gets rendered, independent of the CSS.
 *
 * Also verifies the structural prerequisites for the real interactive
 * drawer (DrawerSection's default-expanded React state, rendered as a
 * literal `aria-expanded="true"` on a real `<button>`, with no `hidden`
 * attribute on the body it controls) -- this SSR-only test cannot click the
 * button or observe the toggle actually happen (no live DOM/event loop),
 * that real click-driven behavior is covered by
 * tests/e2e/jtbd-12-interactive-drawer.spec.ts against a real browser.
 */
import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { SessionWorkspace } from "../../components/session-workspace";

describe("SessionWorkspace (real React SSR render)", () => {
  it("renders the four regions in cognition -> objective -> coding -> result DOM order, each forwarding its own content", () => {
    const html = renderToStaticMarkup(
      <SessionWorkspace
        cognition={<p data-testid="fixture-cognition">cognition content</p>}
        objective={<p data-testid="fixture-objective">objective content</p>}
        coding={<p data-testid="fixture-coding">coding content</p>}
        result={<p data-testid="fixture-result">result content</p>}
      />,
    );
    expect(html).toContain('data-testid="session-workspace"');
    expect(html).toContain('data-testid="workspace-region-cognition"');
    expect(html).toContain('data-testid="workspace-region-objective"');
    expect(html).toContain('id="coding-region"');
    expect(html).toContain('data-testid="workspace-region-coding"');
    expect(html).toContain('data-testid="workspace-region-result"');

    const order = [
      html.indexOf("fixture-cognition"),
      html.indexOf("fixture-objective"),
      html.indexOf("fixture-coding"),
      html.indexOf("fixture-result"),
    ];
    expect(order.every((i) => i >= 0)).toBe(true);
    expect(order).toEqual([...order].sort((a, b) => a - b));
  });

  it("labels the objective region as the current objective (heading text, not just aria-label)", () => {
    const html = renderToStaticMarkup(
      <SessionWorkspace cognition={null} objective={<p>problem</p>} coding={null} result={null} />,
    );
    expect(html).toContain("Current objective");
  });

  it("renders a real, default-expanded drawer toggle button for each of the 3 side regions (not the always-visible Coding region)", () => {
    const html = renderToStaticMarkup(
      <SessionWorkspace
        cognition={<p data-testid="fixture-cognition">cognition content</p>}
        objective={<p data-testid="fixture-objective">objective content</p>}
        coding={<p data-testid="fixture-coding">coding content</p>}
        result={<p data-testid="fixture-result">result content</p>}
      />,
    );

    for (const regionId of ["cognition", "objective", "result"]) {
      // Match the whole real <button> tag (attribute order not assumed) so
      // "aria-expanded" and "aria-controls" are asserted on the SAME
      // element as the matching data-testid, not just present somewhere in
      // the document.
      const buttonTag = html.match(new RegExp(`<button[^>]*data-testid="drawer-toggle-${regionId}"[^>]*>`));
      expect(buttonTag).not.toBeNull();
      expect(buttonTag![0]).toContain('aria-expanded="true"');
      expect(buttonTag![0]).toContain(`aria-controls="workspace-region-${regionId}-body"`);
      expect(html).toContain(`data-testid="workspace-region-${regionId}-body"`);
    }
    // No toggle exists for the Coding region -- it is never collapsed.
    expect(html).not.toContain('data-testid="drawer-toggle-coding"');

    // Default React state is expanded: no `hidden` attribute rendered on
    // any region body (SSR omits boolean attributes entirely when false).
    expect(html).not.toContain("hidden");
  });
});
