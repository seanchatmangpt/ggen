/**
 * TICKET-023: deterministic session reducer.
 *
 * HAND-AUTHORED (not RDF-row-generated -- this ticket's own classification
 * is "Template: 75% / Custom code: 25%": the dispatch/immutable-update
 * *skeleton* is reusable structural machinery, containing only calls into
 * the imported RDF-derived tables). Every domain decision is delegated:
 *
 *   - legal-transition check  -> phase-transitions.ts's isLegalTransition()
 *     (TICKET-021, generated from the 13 admitted transition-plan/*
 *     resources plus phase/refused's skos:related wildcard rule)
 *   - event-family recognition -> event-family.ts's ALL_EVENT_FAMILIES
 *     (TICKET-016, generated from the 15 admitted event-family/* concepts)
 *   - routing-slot lookup      -> event-routing.ts's EVENT_ROUTING
 *     (TICKET-022, generated from the same 15 event-family/* concepts)
 *   - refusal outcome shape    -> refusal.ts's AdmissionResult/RefusalCode
 *     (TICKET-017, generated from the 16 admitted req/ard-refusal-* codes)
 *
 * This file contains ZERO phase or event-family string literal outside its
 * own import statements. Falsifier (run for real, see TICKET-023's
 * Implementation notes below for pasted output):
 *
 *   grep -E '"(CREATED|PREPARING|READY|INTRODUCTION|PROBLEM_PRESENTATION|
 *   CLARIFICATION|PLANNING|IMPLEMENTATION|EXECUTION|DEBUGGING|EXPLANATION|
 *   FOLLOW_UP|COMPLETE|REFUSED)"' lib/domain/reducer.ts
 *
 * must match nothing (excluding import lines, which this file has none of
 * that reference such literals anyway -- the check is over the whole file).
 */
import type { Phase } from "./phase";
import { ALL_EVENT_FAMILIES, type EventFamily } from "./event-family";
import type { AdmissionResult, RefusalCode } from "./refusal";
import { isLegalTransition } from "./phase-transitions";
import { EVENT_ROUTING } from "./event-routing";

/** Session state: a phase plus an open bag of session-scoped fields
 * (problem/workspace/track-candidate/verification/accessibility/authority
 * state from TICKET-018/019, attached by the caller -- this reducer does
 * not know or care about their shape). */
export interface SessionState {
  phase: Phase;
  [key: string]: unknown;
}

/** An incoming session event. `family` is a plain `string`, not
 * `EventFamily`, on purpose: recognizing (or refusing) an unrecognized
 * family value is this reducer's own first job, so the type must admit
 * values outside the closed union for that check to be meaningful. */
export interface SessionEvent {
  family: string;
  type?: string;
  targetPhase?: Phase;
  [key: string]: unknown;
}

const KNOWN_EVENT_FAMILIES: ReadonlySet<string> = new Set<string>(ALL_EVENT_FAMILIES);

/**
 * The session reducer (Architecture Decision 13: refusal is a first-class
 * outcome, never a thrown exception).
 *
 * 1. Unrecognized `event.family` (not one of the 15 admitted EventFamily
 *    members) -> refused, never silently dropped.
 * 2. Recognized family with no admission-routing slot (would only happen
 *    if event-routing.ts's generated table ever drifted out of sync with
 *    event-family.ts -- both are generated from the same 15-concept RDF
 *    scheme, so this branch is a consistency guard, not expected to fire
 *    in practice) -> refused.
 * 3. `event.targetPhase` present but `isLegalTransition(state.phase,
 *    event.targetPhase)` is false -> refused, state unchanged.
 * 4. Otherwise -> admitted, with `state.phase` updated to `targetPhase`
 *    when one was supplied, else the state passes through unchanged
 *    (a routed event that carries no phase transition, e.g. a
 *    ReceiptEvent or AccessibilityEvent, is still a legitimate admitted
 *    event).
 */
export function sessionReducer(
  state: SessionState,
  event: SessionEvent
): AdmissionResult<SessionState> {
  if (!KNOWN_EVENT_FAMILIES.has(event.family)) {
    return {
      status: "refused",
      code: unrecognizedFamilyCode(),
      reason: `event.family "${event.family}" is not an admitted EventFamily member`,
    };
  }

  const handlerSlot = EVENT_ROUTING[event.family as EventFamily];
  if (handlerSlot === undefined) {
    return {
      status: "refused",
      code: unrecognizedFamilyCode(),
      reason: `event.family "${event.family}" has no admitted routing-table entry`,
    };
  }

  if (event.targetPhase !== undefined) {
    if (!isLegalTransition(state.phase, event.targetPhase)) {
      return {
        status: "refused",
        code: illegalTransitionCode(),
        reason: `illegal transition (source phase -> requested targetPhase not in the admitted transition-plan set or refusal wildcard)`,
      };
    }
    return { status: "admitted", value: { ...state, phase: event.targetPhase } };
  }

  return { status: "admitted", value: state };
}

/**
 * Both refusal branches below resolve to the same admitted RefusalCode
 * (STALE_SESSION_EVENT, from refusal.ts's 16-member ARD Section 11
 * taxonomy): an event whose family is unrecognized, or whose requested
 * transition does not apply to the session's current phase, is in both
 * cases "an event that does not apply to this session as it currently
 * stands" -- STALE_SESSION_EVENT's plain-English fit. No other one of the
 * 16 admitted codes names phase-transition or event-family legality more
 * specifically; this reducer does not invent a 17th code to split the two
 * cases apart. Kept as two named functions (not one shared constant) so
 * each call site documents its own reasoning independently.
 */
function unrecognizedFamilyCode(): RefusalCode {
  return "STALE_SESSION_EVENT";
}

function illegalTransitionCode(): RefusalCode {
  return "STALE_SESSION_EVENT";
}
