"use client";

/**
 * Hand-authored (Phase 4 -- UI/UX redesign; no ontology resource backs this
 * component). Responsive layout shell for the four content regions the
 * redesign brief describes as three "regions" (Cognition | Coding |
 * Evidence), with the problem statement broken out into its own named grid
 * area (`objective`) so its DOM position can satisfy the required mobile
 * reading order (cognition -> problem -> editor -> result) independently of
 * its visual placement in the Evidence column on wider viewports -- CSS
 * Grid's `grid-template-areas` repositions grid items visually without
 * reordering the DOM/tab order, which is exactly the property this
 * component needs (see app/globals.css's `.session-workspace` rules and the
 * keyboard/focus-order note in app/page.tsx).
 *
 * Breakpoints (app/globals.css):
 *   - default (mobile, <700px): single column, DOM order
 *     cognition -> objective -> coding -> result.
 *   - >=700px (tablet): narrow side columns ("drawers") flank a wider
 *     coding column; objective/result share the right-hand column.
 *   - >=1100px (desktop): full 3-column grid, side columns get more room.
 *
 * Real interactive drawer (later pass, closes the disclosed gap that the
 * tablet-width "drawer" treatment was CSS-only narrowing, not an actually
 * collapsible drawer): the three side regions -- Cognition, Current
 * objective, Evidence -- are each wrapped in `DrawerSection`, a real
 * client-side toggle backed by React `useState`, not native
 * `<details>/<summary>` (used elsewhere in this codebase, e.g.
 * session-menu.tsx/session-activity-drawer.tsx, for genuinely simpler
 * disclosures). `<summary>`'s expanded/collapsed state is exposed only via
 * the accessibility tree in Chromium, not as a literal DOM attribute, so a
 * test asserting `aria-expanded` directly (the concrete, checkable form of
 * "real ARIA" this gap calls for) needs an explicit `aria-expanded={...}`
 * on a real `<button>` instead. The Coding region (the primary work area)
 * is intentionally NOT wrapped -- it is never collapsed. Defaults to
 * expanded for every region, so no prior rendering/test expectation
 * regresses: the four regions are visible on first render exactly as
 * before, this only adds the ability to collapse them.
 */
import { useState, type ReactNode } from "react";

export interface SessionWorkspaceProps {
  cognition: ReactNode;
  objective: ReactNode;
  coding: ReactNode;
  result: ReactNode;
}

interface DrawerSectionProps {
  regionId: "cognition" | "objective" | "result";
  label: string;
  headingText: string;
  children: ReactNode;
}

function DrawerSection({ regionId, label, headingText, children }: DrawerSectionProps) {
  const [expanded, setExpanded] = useState(true);
  const bodyId = `workspace-region-${regionId}-body`;
  return (
    <section
      className={`workspace-region region-${regionId}`}
      aria-label={label}
      data-testid={`workspace-region-${regionId}`}
    >
      <div className="workspace-region-header">
        <h2 className="region-heading">{headingText}</h2>
        <button
          type="button"
          className="drawer-toggle"
          aria-expanded={expanded}
          aria-controls={bodyId}
          data-testid={`drawer-toggle-${regionId}`}
          onClick={() => setExpanded((value) => !value)}
        >
          {expanded ? "Collapse" : "Expand"}
        </button>
      </div>
      <div id={bodyId} className="workspace-region-body" hidden={!expanded} data-testid={`workspace-region-${regionId}-body`}>
        {children}
      </div>
    </section>
  );
}

export function SessionWorkspace({ cognition, objective, coding, result }: SessionWorkspaceProps) {
  return (
    <div className="session-workspace" data-testid="session-workspace">
      <DrawerSection regionId="cognition" label="Cognition" headingText="Cognition">
        {cognition}
      </DrawerSection>
      <DrawerSection regionId="objective" label="Current objective" headingText="Current objective">
        {objective}
      </DrawerSection>
      <section
        id="coding-region"
        className="workspace-region region-coding"
        aria-label="Coding"
        data-testid="workspace-region-coding"
      >
        {coding}
      </section>
      <DrawerSection regionId="result" label="Evidence" headingText="Evidence">
        {result}
      </DrawerSection>
    </div>
  );
}
