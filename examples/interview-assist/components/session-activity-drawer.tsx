/**
 * Hand-authored (Phase 4 -- UI/UX redesign; no ontology resource backs this
 * component). Collapsible drawer housing three things that were previously
 * scattered across the main page body or absent entirely:
 *
 *   1. Transcript/event history -- the real event labels app/page.tsx's
 *      dispatch() already accumulates into state.usedEvents (unchanged
 *      data, just newly surfaced).
 *   2. Receipt inspector -- the real per-step TransitionReceipt chain
 *      (admission / cognition-run / sandbox-execution / test-result /
 *      accessibility-projection, per TICKET-055) app/page.tsx now collects
 *      as each step actually happens.
 *   3. A developer-diagnostics panel, gated behind `debug` (app/page.tsx
 *      sets this from `?debug=1` in the URL, read client-side only -- see
 *      that file's module doc for why `useSearchParams` was avoided). This
 *      is where the original manual "Advance to <Phase>" phase-jump
 *      buttons and the "Trigger admission refusal (demo)" button now live:
 *      every manual phase jump is, by construction, outside the
 *      cognition-confirmed path (that path only ever advances the phase as
 *      a direct consequence of one real HypothesisEvent per confirmed
 *      turn -- see app/page.tsx's confirmCognitionProposal), so all of them
 *      move here rather than singling out a subset.
 *
 * Uses <details>/<summary> for the same reason session-menu.tsx does: free
 * native collapse/expand semantics, no extra open/close state.
 */
import type { Phase } from "../lib/domain/phase";
import type { TransitionReceipt } from "../lib/domain/receipt";

export interface SessionActivityDrawerProps {
  events: string[];
  receipts: TransitionReceipt[];
  debug: boolean;
  phase: Phase;
  nextPhases: readonly Phase[];
  onAdvance: (phase: Phase) => void;
  onTriggerAdmissionRefusalDemo: () => void;
}

export function SessionActivityDrawer({
  events,
  receipts,
  debug,
  phase,
  nextPhases,
  onAdvance,
  onTriggerAdmissionRefusalDemo,
}: SessionActivityDrawerProps) {
  return (
    <details className="session-activity-drawer" data-testid="session-activity-drawer">
      <summary data-testid="session-activity-drawer-toggle">Session activity</summary>

      <section aria-label="Transcript and event history" data-testid="activity-transcript">
        <h3 className="region-heading">Event history</h3>
        {events.length === 0 ? (
          <p data-testid="activity-transcript-empty">No events recorded yet.</p>
        ) : (
          <ol>
            {events.map((label, i) => (
              <li key={`${i}-${label}`} data-testid={`activity-transcript-event-${i}`}>
                {label}
              </li>
            ))}
          </ol>
        )}
      </section>

      <section aria-label="Receipt inspector" data-testid="activity-receipts">
        <h3 className="region-heading">Receipt chain</h3>
        {receipts.length === 0 ? (
          <p data-testid="activity-receipts-empty">No receipts recorded yet.</p>
        ) : (
          <ol>
            {receipts.map((r, i) => (
              <li key={`${i}-${r.checksum.checksumValue}`} data-testid={`activity-receipt-${i}`}>
                <strong>{r.label ?? `receipt ${i}`}</strong>
                <code data-testid={`activity-receipt-checksum-${i}`}>
                  {r.checksum.algorithm}:{r.checksum.checksumValue}
                </code>
              </li>
            ))}
          </ol>
        )}
      </section>

      {debug && (
        <section aria-label="Developer diagnostics" data-testid="activity-dev-diagnostics">
          <h3 className="region-heading">Developer diagnostics (?debug=1)</h3>
          <p data-testid="activity-dev-phase">Raw phase: {phase}</p>
          <div role="group" aria-label="Manual phase advance" data-testid="activity-dev-advance-group">
            {nextPhases.map((p) => (
              <button key={p} type="button" onClick={() => onAdvance(p)} data-testid={`advance-to-${p}`}>
                Advance to {p}
              </button>
            ))}
          </div>
          <button
            type="button"
            onClick={onTriggerAdmissionRefusalDemo}
            data-testid="trigger-admission-refusal-demo"
          >
            Trigger admission refusal (demo)
          </button>
        </section>
      )}
    </details>
  );
}
