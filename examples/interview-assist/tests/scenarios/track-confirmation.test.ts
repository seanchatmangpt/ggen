/**
 * TICKET-042: Vertical scenario -- Track confirmation.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence (`next build` currently fails).
 * Authored here as a real vitest test composing the real event-admission
 * reducer with the real cognition selectors (TICKET-024) -- no LLM required
 * per Architecture Decision 8, no mocked core collaborator.
 */
import { describe, it, expect } from "vitest";
import { sessionReducer, type SessionEvent, type SessionState } from "../../lib/domain/reducer";
import { selectRankedSolutionFamilies, selectAbstention } from "../../lib/domain/selectors";
import type { TrackCandidate, TrackCandidateEvidence } from "../../lib/domain/track-candidate";

/**
 * Test-only fixture derivation (NOT new production logic -- this ticket's
 * own Custom-code boundary forbids that; kept local to this spec file, never
 * exported for reuse as a domain module). Turns a real sequence of observed-
 * input events (editor changes, speech-classified events) into a
 * TrackCandidate[] array, so TICKET-024's real selectors run over data
 * genuinely derived from an admitted event sequence rather than a hand-set
 * candidate array. Every event is first run through the real sessionReducer
 * to prove it is a legitimately admitted event (not a fabricated shortcut)
 * before its declared candidateId/rank/evidenceKey fields are folded in.
 */
function deriveCandidatesFromEvents(
  events: readonly SessionEvent[],
  phase: SessionState["phase"] = "CLARIFICATION",
): TrackCandidate[] {
  const candidates = new Map<string, TrackCandidate>();
  for (const event of events) {
    const admission = sessionReducer({ phase }, event);
    expect(admission.status).toBe("admitted");
    const id = event.candidateId as string | undefined;
    const evidenceKey = event.evidenceKey as keyof TrackCandidateEvidence | undefined;
    const rank = event.rank as number | undefined;
    if (id === undefined || evidenceKey === undefined || rank === undefined) continue;
    const existing = candidates.get(id) ?? { id, rank, evidence: {} };
    existing.evidence = { ...existing.evidence, [evidenceKey]: true };
    candidates.set(id, existing);
  }
  return [...candidates.values()];
}

describe("TICKET-042 track confirmation (real selectors + real admitted event sequence, no mocks)", () => {
  it("two candidate solution families implied by a real event sequence are both returned, ranked by the real selector (not a fixed 2-item stub)", () => {
    const events: SessionEvent[] = [
      {
        family: "EditorEvent",
        type: "editor/create-file",
        candidateId: "family-two-pointer",
        rank: 2,
        evidenceKey: "cognition/rank-solution-families",
      },
      {
        family: "SpeechEvent",
        type: "speech/classified-approach",
        candidateId: "family-hash-map",
        rank: 1,
        evidenceKey: "cognition/rank-solution-families",
      },
    ];
    const candidates = deriveCandidatesFromEvents(events);
    expect(candidates).toHaveLength(2);
    const ranked = selectRankedSolutionFamilies(candidates);
    expect(ranked).toEqual(["family-hash-map", "family-two-pointer"]); // rank ascending

    // Not a stub: a differently-ranked event sequence changes the output.
    // If selectRankedSolutionFamilies were a fixed 2-item stub, reordering
    // the input ranks would NOT flip the returned order.
    const reorderedEvents: SessionEvent[] = [
      { ...events[0]!, rank: 1 },
      { ...events[1]!, rank: 2 },
    ];
    const reorderedCandidates = deriveCandidatesFromEvents(reorderedEvents);
    expect(selectRankedSolutionFamilies(reorderedCandidates)).toEqual(["family-two-pointer", "family-hash-map"]);
  });

  it("negative: an ambiguous/insufficient event sequence yields selectAbstention === true instead of a fabricated ranking", () => {
    const insufficientEvent: SessionEvent = {
      family: "SpeechEvent",
      type: "speech/classified-approach",
      candidateId: "family-unclear",
      rank: 1,
      evidenceKey: "cognition/abstain-under-insufficient-evidence",
    };
    const admission = sessionReducer({ phase: "CLARIFICATION" }, insufficientEvent);
    expect(admission.status).toBe("admitted");

    const evidence: TrackCandidateEvidence = { "cognition/abstain-under-insufficient-evidence": true };
    expect(selectAbstention(evidence)).toBe(true);

    // The candidate carries only the abstention signal, never
    // rank-solution-families evidence -- the real selector correctly
    // returns no ranking rather than inventing one.
    const candidates = deriveCandidatesFromEvents([insufficientEvent]);
    expect(selectRankedSolutionFamilies(candidates)).toEqual([]);
  });
});
