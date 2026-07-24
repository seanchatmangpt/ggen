/**
 * Shared test-only fixture: a real, legal multi-step session event log,
 * reused across TICKET-048/049/053's scenarios (this backlog's own
 * Template responsibility for workstream I: "Shared Playwright harness
 * (TICKET-039) and shared fixture-building utilities, reused across all
 * 14 scenarios"). NOT new production logic -- every targetPhase
 * transition below is independently asserted (not merely commented)
 * against the real generated phase-transitions.ts table by
 * assertFixtureLegalAgainstRealTable(), and every event's admission is
 * asserted at each call site that folds it through the real
 * sessionReducer.
 *
 * Reuses the exact event shapes already proven in TICKET-041's
 * first-interaction.test.ts (ParticipantEvent + WorkflowEvent) and
 * TICKET-042's track-confirmation.test.ts (ad hoc SessionEvent field
 * extensions), extended to a full CREATED -> COMPLETE walk with a few
 * non-phase-transition events (EditorEvent/SpeechEvent/TestEvent)
 * interleaved so the persisted log is not merely a bare phase sequence.
 */
import type { SessionEvent } from "../../../lib/domain/reducer";
import { PHASE_TRANSITIONS } from "../../../lib/domain/phase-transitions";
import type { Phase } from "../../../lib/domain/phase";
import type { EventLogEntry } from "../../../lib/adapters/persistence-adapter";

export function buildRealSessionEventLog(): SessionEvent[] {
  return [
    { family: "ParticipantEvent", type: "identify-participant-roles" },
    { family: "WorkflowEvent", targetPhase: "PREPARING" },
    { family: "WorkflowEvent", targetPhase: "READY" },
    { family: "WorkflowEvent", targetPhase: "INTRODUCTION" },
    { family: "WorkflowEvent", targetPhase: "PROBLEM_PRESENTATION" },
    { family: "EditorEvent", type: "editor/open-file" },
    { family: "WorkflowEvent", targetPhase: "CLARIFICATION" },
    { family: "SpeechEvent", type: "speech/classified-approach" },
    { family: "WorkflowEvent", targetPhase: "PLANNING" },
    { family: "WorkflowEvent", targetPhase: "IMPLEMENTATION" },
    { family: "EditorEvent", type: "editor/modify-file" },
    { family: "WorkflowEvent", targetPhase: "EXECUTION" },
    { family: "TestEvent", type: "run-visible-test" },
    { family: "WorkflowEvent", targetPhase: "DEBUGGING" },
    { family: "WorkflowEvent", targetPhase: "EXPLANATION" },
    { family: "WorkflowEvent", targetPhase: "FOLLOW_UP" },
    { family: "WorkflowEvent", targetPhase: "COMPLETE" },
  ];
}

/**
 * Re-derives every targetPhase hop in `log` against the real generated
 * PHASE_TRANSITIONS table (TICKET-021) and throws loudly on the first
 * hop that is not one of the real admitted transition-plan/* edges. Call
 * sites assert this passes rather than trusting the fixture's own
 * comments -- a future edit to either this fixture or the generated
 * table that silently drifts them apart fails the test suite instead of
 * producing a quietly-wrong replay.
 */
export function assertFixtureLegalAgainstRealTable(log: readonly SessionEvent[]): void {
  let phase: Phase = "CREATED";
  for (const event of log) {
    if (event.targetPhase === undefined) continue;
    const legalTargets = PHASE_TRANSITIONS[phase] ?? [];
    if (!legalTargets.includes(event.targetPhase)) {
      throw new Error(
        `fixture drift: ${phase} -> ${event.targetPhase} is not a real admitted transition-plan edge in phase-transitions.ts`,
      );
    }
    phase = event.targetPhase;
  }
}

/** Wraps a real SessionEvent[] into persistence-adapter.ts's real
 * EventLogEntry[] shape (TICKET-036). Deterministic timestamps so the
 * persisted file's byte content is reproducible across test runs. */
export function toEventLogEntries(events: readonly SessionEvent[]): EventLogEntry[] {
  return events.map((event, i) => ({
    seq: i + 1,
    type: event.family,
    payload: event,
    timestampMs: 1_700_000_000_000 + i * 1000,
  }));
}

/** Inverse of toEventLogEntries: real ordering by seq (not array order,
 * in case a store ever returns entries out of insertion order), payload
 * cast back to SessionEvent. */
export function fromEventLogEntries(entries: readonly EventLogEntry[]): SessionEvent[] {
  return entries
    .slice()
    .sort((a, b) => a.seq - b.seq)
    .map((e) => e.payload as SessionEvent);
}
