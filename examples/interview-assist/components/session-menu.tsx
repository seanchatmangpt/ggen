/**
 * Hand-authored (Phase 4 -- UI/UX redesign; no ontology resource backs this
 * component). Combines the two previously-separate top-level buttons
 * ("Refuse session", "Finish session") into one coherent session-actions
 * menu, per the redesign brief's control-replacement table.
 *
 * Uses a native <details>/<summary> disclosure rather than a hand-rolled
 * popup: no extra JS state machine needed for open/close, Escape/outside
 * click, or focus return -- the browser provides all of that natively, and
 * it degrades to a plain expandable list if CSS fails to load. `canRefuse`
 * mirrors app/page.tsx's original conditional exactly (hidden once the
 * session has already reached REFUSED or COMPLETE).
 *
 * UX-polish pass (real loading state for Finish session): `finishing`
 * reflects app/page.tsx's real pending state for the async POST
 * /api/receipt request Finish triggers. Deliberately does NOT auto-close
 * the menu on Finish the way Refuse does -- Refuse is a synchronous local
 * `dispatch()` call with no network round trip, so closing immediately is
 * safe, but Finish is real async work, and closing the disclosure would
 * hide the only visible pending indicator for its entire duration. The
 * button itself becomes the busy indicator (spinner + relabeled text +
 * `aria-busy` + `disabled`) and stays visible until the real request
 * settles.
 */
import { useRef } from "react";
import { Spinner } from "./spinner";

export interface SessionMenuProps {
  canRefuse: boolean;
  onRefuse: () => void;
  onFinish: () => void;
  /** Real busy state while the async "Finish session" request is in
   * flight. Optional/defaults to false so existing callers (and the SSR
   * component test) don't need to supply it. */
  finishing?: boolean;
}

export function SessionMenu({ canRefuse, onRefuse, onFinish, finishing = false }: SessionMenuProps) {
  const detailsRef = useRef<HTMLDetailsElement | null>(null);

  function closeMenu(): void {
    if (detailsRef.current) detailsRef.current.open = false;
  }

  return (
    <details className="session-menu" ref={detailsRef} data-testid="session-menu">
      <summary data-testid="session-menu-toggle">Session actions</summary>
      <div role="menu" aria-label="Session actions" className="session-menu-list">
        {canRefuse && (
          <button
            type="button"
            role="menuitem"
            data-testid="session-menu-refuse"
            onClick={() => {
              closeMenu();
              onRefuse();
            }}
          >
            Refuse session
          </button>
        )}
        <button
          type="button"
          role="menuitem"
          data-testid="session-menu-finish"
          aria-busy={finishing}
          disabled={finishing}
          onClick={onFinish}
        >
          {finishing && <Spinner data-testid="session-menu-finish-spinner" />}
          {finishing ? "Finishing session..." : "Finish session"}
        </button>
      </div>
    </details>
  );
}
