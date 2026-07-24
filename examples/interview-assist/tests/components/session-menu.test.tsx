/**
 * Real component test for components/session-menu.tsx (SSR pattern, see
 * cognition-panel.test.tsx's module doc for the disclosed jsdom-absence
 * scope note).
 */
import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { SessionMenu } from "../../components/session-menu";

describe("SessionMenu (real React SSR render)", () => {
  it("renders both Refuse session and Finish session as menu items when canRefuse is true", () => {
    const html = renderToStaticMarkup(
      <SessionMenu canRefuse={true} onRefuse={() => {}} onFinish={() => {}} />,
    );
    expect(html).toContain('data-testid="session-menu"');
    expect(html).toContain('data-testid="session-menu-toggle"');
    expect(html).toContain("Session actions");
    expect(html).toContain('data-testid="session-menu-refuse"');
    expect(html).toContain("Refuse session");
    expect(html).toContain('data-testid="session-menu-finish"');
    expect(html).toContain("Finish session");
    expect(html).toMatch(/role="menu"[^>]*>/);
  });

  it("omits Refuse session once the session has reached a terminal phase (canRefuse false), mirroring the original page.tsx conditional", () => {
    const html = renderToStaticMarkup(
      <SessionMenu canRefuse={false} onRefuse={() => {}} onFinish={() => {}} />,
    );
    expect(html).not.toContain('data-testid="session-menu-refuse"');
    expect(html).toContain('data-testid="session-menu-finish"');
  });

  // UX-polish pass: real busy state for the async Finish session action.
  it("renders a real busy Finish-session button (aria-busy, disabled, spinner, relabeled text) when finishing is true", () => {
    const html = renderToStaticMarkup(
      <SessionMenu canRefuse={true} onRefuse={() => {}} onFinish={() => {}} finishing={true} />,
    );
    expect(html).toMatch(/data-testid="session-menu-finish"[^>]*aria-busy="true"/);
    expect(html).toMatch(/data-testid="session-menu-finish"[^>]*disabled=""/);
    expect(html).toContain("Finishing session...");
    expect(html).not.toContain(">Finish session<");
    expect(html).toContain('data-testid="session-menu-finish-spinner"');
  });

  it("defaults to a non-busy Finish-session button when finishing is omitted", () => {
    const html = renderToStaticMarkup(
      <SessionMenu canRefuse={true} onRefuse={() => {}} onFinish={() => {}} />,
    );
    expect(html).toMatch(/data-testid="session-menu-finish"[^>]*aria-busy="false"/);
    expect(html).not.toContain('data-testid="session-menu-finish-spinner"');
  });
});
