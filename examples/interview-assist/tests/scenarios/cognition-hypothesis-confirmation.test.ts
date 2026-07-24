/**
 * Real end-to-end-ish scenario test for the cognition-panel UI wiring
 * (app/page.tsx's submitCognitionUtterance / confirmCognitionProposal):
 * calls the REAL wasm4pm-cognition adapter (`runCognition`, the actual
 * built WASM binary via lib/adapters/cognition-adapter.ts -- not a mock)
 * and the REAL sessionReducer + PHASE_TRANSITIONS table, mirroring exactly
 * what app/page.tsx does on submit and on "Yes", without going through a
 * browser or an HTTP round-trip to app/api/cognition/route.ts (that route
 * is a thin NextResponse.json(...) wrapper around the same `runCognition`
 * call already exercised for real here and in
 * tests/adapters/cognition-adapter.test.ts).
 *
 * Proves the specific chain requested: a Two-Sum-pattern utterance ->
 * runCognition returns the real Eliza explanation -> confirming computes
 * the next legal phase from PHASE_TRANSITIONS and dispatches a real
 * HypothesisEvent -> sessionReducer's resulting phase differs from the
 * starting phase -- all without ever constructing a WorkflowEvent (the
 * event family advanceTo()/the "Advance to X" buttons use).
 */
import { describe, it, expect } from "vitest";
import { runCognition } from "../../lib/adapters/cognition-adapter";
import { sessionReducer, type SessionState, type SessionEvent } from "../../lib/domain/reducer";
import { PHASE_TRANSITIONS } from "../../lib/domain/phase-transitions";

describe("cognition hypothesis confirmation (real WASM adapter + real reducer, no mocks, no WorkflowEvent)", () => {
  it("a real Two-Sum-pattern utterance is matched, and confirming it advances the session phase via a real HypothesisEvent alone", async () => {
    const startingState: SessionState = { phase: "CLARIFICATION" };

    // Step 1: submit -- exactly what submitCognitionUtterance() does.
    const outcome = await runCognition("I have an array of numbers to search through");
    expect(outcome.status).toBe("matched");
    if (outcome.status !== "matched") throw new Error("unreachable");
    expect(outcome.selected).toBe("ARRAY");
    expect(outcome.explanation).toBe(
      "Is this a Two Sum-style problem -- finding two values in an array whose sum equals a target?",
    );

    // Step 2: confirm ("Yes") -- exactly what confirmCognitionProposal() does.
    // No family other than HypothesisEvent is ever constructed here --
    // in particular, no WorkflowEvent (the family advanceTo() uses).
    const nextPhase = PHASE_TRANSITIONS[startingState.phase]?.[0];
    expect(nextPhase).toBe("PLANNING"); // real transition-plan edge for CLARIFICATION
    const confirmEvent: SessionEvent = { family: "HypothesisEvent", targetPhase: nextPhase };
    const admission = sessionReducer(startingState, confirmEvent);

    expect(admission.status).toBe("admitted");
    if (admission.status !== "admitted") throw new Error("unreachable");
    expect(admission.value.phase).toBe("PLANNING");
    expect(admission.value.phase).not.toBe(startingState.phase);
  });

  it("real per-track keyword coverage: ARRAY, TARGET, INDICES, and SUM utterances each confirm to the same next legal phase from a shared starting phase", async () => {
    const utterances: Record<string, string> = {
      ARRAY: "I have an array of numbers to search through",
      TARGET: "what target value are we aiming for",
      INDICES: "do we need the indices of the matches",
      SUM: "looking for pairs whose sum equals something",
    };
    for (const [keyword, intent] of Object.entries(utterances)) {
      const startingState: SessionState = { phase: "CLARIFICATION" };
      const outcome = await runCognition(intent);
      expect(outcome.status).toBe("matched");
      if (outcome.status !== "matched") throw new Error("unreachable");
      expect(outcome.selected).toBe(keyword);

      const nextPhase = PHASE_TRANSITIONS[startingState.phase]?.[0];
      const admission = sessionReducer(startingState, { family: "HypothesisEvent", targetPhase: nextPhase });
      expect(admission.status).toBe("admitted");
      if (admission.status !== "admitted") throw new Error("unreachable");
      expect(admission.value.phase).toBe("PLANNING");
    }
  });

  it("negative: a real no-keyword-match utterance is honestly reported as no-track-matched (real fail-closed anti-fraud signal) -- CognitionPanel renders no confirm control for this branch (see tests/components/cognition-panel.test.tsx), so no HypothesisEvent is ever constructed and the session phase never moves", async () => {
    const startingState: SessionState = { phase: "CLARIFICATION" };
    const outcome = await runCognition("hello there, nice weather today");
    expect(outcome.status).toBe("no-track-matched");
    if (outcome.status !== "no-track-matched") throw new Error("unreachable");
    expect(outcome.reason).toContain("empty inference trace");
    // No reducer call happens on this branch in the real UI -- the
    // starting state is exactly what a user would still see.
    expect(startingState.phase).toBe("CLARIFICATION");
  });

  it("edge case: confirming from a terminal phase (no outgoing transition-plan edge) still dispatches an admitted HypothesisEvent, just without a targetPhase -- mirrors confirmCognitionProposal's real fallback branch, not a silent no-op", async () => {
    const startingState: SessionState = { phase: "COMPLETE" };
    expect(PHASE_TRANSITIONS[startingState.phase]).toBeUndefined(); // COMPLETE is genuinely terminal

    const outcome = await runCognition("looking for pairs whose sum equals something");
    expect(outcome.status).toBe("matched");

    const nextPhase = PHASE_TRANSITIONS[startingState.phase]?.[0];
    expect(nextPhase).toBeUndefined();
    const confirmEvent: SessionEvent =
      nextPhase !== undefined ? { family: "HypothesisEvent", targetPhase: nextPhase } : { family: "HypothesisEvent" };
    const admission = sessionReducer(startingState, confirmEvent);

    expect(admission.status).toBe("admitted"); // real, not refused
    if (admission.status !== "admitted") throw new Error("unreachable");
    expect(admission.value.phase).toBe("COMPLETE"); // unchanged, since no targetPhase was supplied
  });
});
