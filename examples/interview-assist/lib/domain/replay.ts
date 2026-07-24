/**
 * TICKET-025: session replay -- re-derives, never trusts.
 *
 * HAND-AUTHORED (this ticket's classification: "Template: 80% / Custom
 * code: 20%" -- the replay-loop skeleton is reusable structure, but
 * folding the reducer over a persisted log is control-flow design
 * judgment, same basis as TICKET-023). Reuses TICKET-023's sessionReducer
 * and (transitively, through it) TICKET-021's isLegalTransition exactly --
 * no separate replay-specific transition table, per Architecture
 * Decision 12: replay must independently revalidate every transition
 * rather than trust a persisted final state.
 */
import { sessionReducer, type SessionEvent, type SessionState } from "./reducer";
import type { AdmissionResult } from "./refusal";
import { ALL_PHASES, type Phase } from "./phase";

/** The session lifecycle's first phase (ALL_PHASES is skos:broader-chain
 * ordered, so this is CREATED -- read from the generated table instead of
 * a literal so this file carries no phase string of its own). The
 * explicit undefined-check (not a `!` assertion) is a real invariant
 * guard: phase.ts's own generation falsifier (TICKET-016) already proves
 * ALL_PHASES has exactly 14 members, so this throw is not expected to
 * fire -- but replay.ts asserts it rather than assuming it silently. */
const ALL_PHASES_FIRST = ALL_PHASES[0];
if (ALL_PHASES_FIRST === undefined) {
  throw new Error("ALL_PHASES is empty -- phase.ts generation invariant violated");
}
const INITIAL_PHASE: Phase = ALL_PHASES_FIRST;

/**
 * Replay a persisted event log from the session's initial phase, folding
 * sessionReducer over every event in order. Stops (does not continue
 * folding) at the first refusal, since a refused event never mutated
 * state in the live session either -- continuing past it would replay a
 * different sequence than what actually happened.
 *
 * Given an untampered log, the returned AdmissionResult is exactly what
 * the live session produced. Given a log with any event's payload
 * altered, re-running isLegalTransition (via sessionReducer) against the
 * altered payload independently re-derives a result that will diverge
 * from the untampered replay whenever the alteration changes an admitted
 * transition's legality or the final phase reached -- this divergence is
 * the tamper-detection mechanism TICKET-049 depends on.
 */
export function replaySession(
  eventLog: readonly SessionEvent[]
): AdmissionResult<SessionState> {
  let current: AdmissionResult<SessionState> = {
    status: "admitted",
    value: { phase: INITIAL_PHASE },
  };
  for (const event of eventLog) {
    if (current.status === "refused") {
      break;
    }
    current = sessionReducer(current.value, event);
  }
  return current;
}
