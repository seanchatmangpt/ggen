// TICKET-025: replaySession tests -- match-on-untampered-log, and the
// tamper-detection mechanism TICKET-049 will later depend on.
import { describe, it, expect } from "vitest";
import { replaySession } from "../../lib/domain/replay";
import { sessionReducer, type SessionEvent, type SessionState } from "../../lib/domain/reducer";

// A real, small (4-event) event log covering the first 4 real admitted
// transition-plan edges (CREATED->PREPARING->READY->INTRODUCTION->
// PROBLEM_PRESENTATION).
const REAL_EVENT_LOG: SessionEvent[] = [
  { family: "SessionEvent", targetPhase: "PREPARING" },
  { family: "SessionEvent", targetPhase: "READY" },
  { family: "SessionEvent", targetPhase: "INTRODUCTION" },
  { family: "SessionEvent", targetPhase: "PROBLEM_PRESENTATION" },
];

function replayLiveSequentially(log: readonly SessionEvent[]): SessionState {
  let state: SessionState = { phase: "CREATED" };
  for (const event of log) {
    const result = sessionReducer(state, event);
    if (result.status === "refused") {
      return state;
    }
    state = result.value;
  }
  return state;
}

describe("replaySession (TICKET-025)", () => {
  it("reproduces exactly what live sequential reduction produces for an untampered log", () => {
    const live = replayLiveSequentially(REAL_EVENT_LOG);
    const replayed = replaySession(REAL_EVENT_LOG);
    expect(replayed.status).toBe("admitted");
    if (replayed.status === "admitted") {
      expect(replayed.value).toEqual(live);
      expect(replayed.value.phase).toBe("PROBLEM_PRESENTATION");
    }
  });

  it("starts from CREATED and folds every event in order", () => {
    const oneEvent = replaySession([{ family: "SessionEvent", targetPhase: "PREPARING" }]);
    expect(oneEvent.status).toBe("admitted");
    if (oneEvent.status === "admitted") {
      expect(oneEvent.value.phase).toBe("PREPARING");
    }
  });

  it("tamper detection: mutating one event's targetPhase changes the replayed outcome (final phase or admission status) vs the untampered replay", () => {
    const untampered = replaySession(REAL_EVENT_LOG);

    const tamperedLog: SessionEvent[] = REAL_EVENT_LOG.map((e) => ({ ...e }));
    // Mutate the 3rd event: instead of READY -> INTRODUCTION (legal),
    // attempt READY -> COMPLETE (illegal, not an admitted transition-plan edge).
    tamperedLog[2] = { family: "SessionEvent", targetPhase: "COMPLETE" };
    const tampered = replaySession(tamperedLog);

    // Untampered replay admits all 4 events and ends at PROBLEM_PRESENTATION;
    // tampered replay is refused partway through -- genuinely different
    // outcomes, proving replay re-derives rather than trusting the log.
    expect(untampered.status).toBe("admitted");
    expect(tampered.status).toBe("refused");
    expect(tampered).not.toEqual(untampered);
  });

  it("tamper detection also fires when the mutated transition is still individually legal but changes the final phase reached", () => {
    const untampered = replaySession(REAL_EVENT_LOG);
    const tamperedLog: SessionEvent[] = REAL_EVENT_LOG.slice(0, 3).map((e) => ({ ...e }));
    // Drop the 4th event entirely (a truncation tamper) -- final phase
    // differs (INTRODUCTION vs PROBLEM_PRESENTATION).
    const tampered = replaySession(tamperedLog);
    expect(tampered).not.toEqual(untampered);
    if (tampered.status === "admitted" && untampered.status === "admitted") {
      expect(tampered.value.phase).not.toBe(untampered.value.phase);
    }
  });
});
