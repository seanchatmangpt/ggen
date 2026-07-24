/**
 * TICKET-041: Vertical scenario -- First interaction.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence (`next build` currently fails --
 * checksum-adapter.ts's `node:module` import reaching app/page.tsx's client
 * bundle via reducer.ts -> receipt-emitter.ts). Authored here as a real
 * vitest test against the real reducer/replay/transition-table
 * collaborators -- no mocked core collaborator.
 */
import { describe, it, expect } from "vitest";
import { replaySession } from "../../lib/domain/replay";
import { sessionReducer, type SessionEvent } from "../../lib/domain/reducer";
import { PHASE_TRANSITIONS, isLegalTransition } from "../../lib/domain/phase-transitions";

describe("TICKET-041 first interaction (real reducer + real transition-plan table, no mocks)", () => {
  it("identify-participant-roles event sequence legally advances CREATED -> PREPARING -> READY per the real admitted transition-plan edges", () => {
    const eventLog: SessionEvent[] = [
      { family: "ParticipantEvent", type: "identify-participant-roles" },
      { family: "WorkflowEvent", targetPhase: "PREPARING" },
      { family: "WorkflowEvent", targetPhase: "READY" },
    ];
    const result = replaySession(eventLog);
    expect(result.status).toBe("admitted");
    if (result.status === "admitted") {
      expect(result.value.phase).toBe("READY");
    }
    // Sourced from the real generated table rather than restated as an
    // independent literal: transition-plan/created-to-preparing's and
    // transition-plan/preparing-to-ready's schema:result values.
    expect(PHASE_TRANSITIONS["CREATED"]).toEqual(["PREPARING"]);
    expect(PHASE_TRANSITIONS["PREPARING"]).toEqual(["READY"]);
  });

  it("negative: skipping straight from CREATED to READY (no intermediate PREPARING event) is refused by the real reducer", () => {
    const result = sessionReducer({ phase: "CREATED" }, { family: "WorkflowEvent", targetPhase: "READY" });
    expect(result.status).toBe("refused");
    if (result.status === "refused") {
      expect(result.code).toBe("STALE_SESSION_EVENT");
    }
    expect(isLegalTransition("CREATED", "READY")).toBe(false);
  });

  it("negative (replay form): the same illegal jump inside a full event log halts replay AT the refusal, proving replay never continues past a refused event", () => {
    const eventLog: SessionEvent[] = [
      { family: "ParticipantEvent", type: "identify-participant-roles" },
      { family: "WorkflowEvent", targetPhase: "READY" }, // illegal: CREATED has no direct edge to READY
      { family: "WorkflowEvent", targetPhase: "PREPARING" }, // would be legal from CREATED, but must never run -- replay already halted
    ];
    const result = replaySession(eventLog);
    expect(result.status).toBe("refused");
  });
});
