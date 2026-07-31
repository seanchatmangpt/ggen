/**
 * Hand-authored (not ggen-generated -- no ontology resource backs this
 * component; see the memory note this session started from: TICKET numbers
 * 030-033 cover the other 11 components, this is a new 12th). Renders one
 * real wasm4pm-cognition Eliza turn: the observed intent, the real
 * clarifying `explanation`, and Yes/No/no-match controls.
 *
 * Deliberately dumb: this component owns no fetch call and no reducer
 * dispatch. It receives a `CognitionOutcome | null` (the exact typed union
 * app/api/cognition/route.ts returns, imported type-only from
 * cognition-adapter.ts -- type-only imports are erased at compile time, so
 * this does not pull the server-only wasm-bindgen require() into the client
 * bundle, the same discipline app/page.tsx already applies to
 * sandbox-executor.ts's CapabilityId) and two callbacks (onConfirm/onReject)
 * from its parent, which owns both concerns.
 *
 * Accessibility (real, testable):
 *   - the whole panel is an <aside> landmark (props.intent starts empty and
 *     renders nothing until a real submission occurs, handled by the
 *     parent's conditional render, not this component).
 *   - the clarifying question is inside an `aria-live="polite"` region so
 *     screen readers announce it as it arrives, without an interruptive
 *     `assertive` politeness that would cut off whatever the user is
 *     currently reading.
 *   - focus moves programmatically to the question heading (not straight to
 *     "Yes") whenever a new `outcome.status === "matched"` renders, via a
 *     tabIndex={-1} + ref.focus() effect -- verified in the real component
 *     test below via jsdom's document.activeElement, not asserted from
 *     markup alone.
 *
 * UX-polish pass (graceful WASM-load-failure handling): a fourth real
 * outcome branch, `status === "unavailable"`, distinct from `refused` --
 * see cognition-adapter.ts's `CognitionUnavailableOutcome` doc for why it's
 * a separate status rather than folded into `refused`. Renders an honest
 * "temporarily unavailable" message (never a raw error/stack trace) plus a
 * real Retry control that re-submits the SAME observed intent, wired via
 * the optional `onRetryUnavailable` callback.
 */
import { useEffect, useRef } from "react";
import type { CognitionOutcome } from "../lib/adapters/cognition-adapter";

export interface CognitionPanelProps {
  intent: string;
  outcome: CognitionOutcome | null;
  onConfirm: () => void;
  onReject: () => void;
  /** Called when the human clicks "Retry" on a real
   * `outcome.status === "unavailable"`. Optional so existing callers (and
   * the SSR component test) don't need to supply a no-op when this branch
   * is never exercised. */
  onRetryUnavailable?: () => void;
}

export function CognitionPanel({ intent, outcome, onConfirm, onReject, onRetryUnavailable }: CognitionPanelProps) {
  const headingRef = useRef<HTMLHeadingElement | null>(null);

  useEffect(() => {
    if (outcome?.status === "matched") {
      headingRef.current?.focus();
    }
  }, [outcome]);

  return (
    <aside aria-label="Cognition proposal" data-testid="cognition-panel">
      <p data-testid="cognition-panel-intent">
        Observed: <q>{intent}</q>
      </p>

      <div aria-live="polite" data-testid="cognition-panel-live-region">
        {outcome?.status === "matched" && (
          <div data-testid="cognition-panel-matched" data-selected={outcome.selected}>
            <h2 tabIndex={-1} ref={headingRef} data-testid="cognition-panel-question">
              {outcome.explanation}
            </h2>
            <div role="group" aria-label="Confirm cognition proposal">
              <button type="button" onClick={onConfirm} data-testid="cognition-panel-confirm">
                Yes
              </button>
              <button type="button" onClick={onReject} data-testid="cognition-panel-reject">
                No
              </button>
            </div>
          </div>
        )}

        {outcome?.status === "no-track-matched" && (
          <p role="status" data-testid="cognition-panel-no-track-matched">
            I couldn&apos;t match that to a known track yet. Try describing the problem
            differently.
          </p>
        )}

        {outcome?.status === "refused" && (
          <p role="alert" data-testid="cognition-panel-refused">
            That utterance was refused: {outcome.reason}
          </p>
        )}

        {outcome?.status === "unavailable" && (
          <div role="alert" data-testid="cognition-panel-unavailable">
            <p>Cognition is temporarily unavailable. Please try again in a moment.</p>
            {onRetryUnavailable && (
              <button type="button" onClick={onRetryUnavailable} data-testid="cognition-panel-retry-unavailable">
                Retry
              </button>
            )}
          </div>
        )}
      </div>
    </aside>
  );
}
