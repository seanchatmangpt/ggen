// TICKET-024: selectors.ts tests, including both directions of
// selectAbstention per the ticket's explicit negative-test requirement.
import { describe, it, expect } from "vitest";
import {
  selectAbstention,
  selectRankedSolutionFamilies,
  selectMarkConceptCoveredEvidence,
  COGNITION_CAPABILITY_COUNT,
} from "../../lib/domain/selectors";
import type { TrackCandidate, TrackCandidateEvidence } from "../../lib/domain/track-candidate";

describe("selectAbstention (TICKET-024, capability/cognition/abstain-under-insufficient-evidence)", () => {
  it("returns true when the evidence bag marks insufficient-evidence truthy", () => {
    const evidence: TrackCandidateEvidence = {
      "cognition/abstain-under-insufficient-evidence": true,
    };
    expect(selectAbstention(evidence)).toBe(true);
  });

  it("returns false when the evidence bag has strong evidence (field absent/falsy) -- not a constant", () => {
    const strongEvidence: TrackCandidateEvidence = {
      "cognition/retrieve-candidate-evidence": true,
      "cognition/rank-solution-families": true,
    };
    expect(selectAbstention(strongEvidence)).toBe(false);

    const explicitlyFalse: TrackCandidateEvidence = {
      "cognition/abstain-under-insufficient-evidence": false,
    };
    expect(selectAbstention(explicitlyFalse)).toBe(false);
  });
});

describe("selectRankedSolutionFamilies (TICKET-024, capability/cognition/rank-solution-families)", () => {
  it("orders candidates by rank ascending and filters to those carrying this capability's evidence", () => {
    const candidates: TrackCandidate[] = [
      { id: "c-high-rank", rank: 3, evidence: { "cognition/rank-solution-families": true } },
      { id: "c-low-rank", rank: 1, evidence: { "cognition/rank-solution-families": true } },
      { id: "c-mid-rank", rank: 2, evidence: { "cognition/rank-solution-families": true } },
      { id: "c-no-evidence", rank: 0, evidence: {} },
    ];
    expect(selectRankedSolutionFamilies(candidates)).toEqual([
      "c-low-rank",
      "c-mid-rank",
      "c-high-rank",
    ]);
  });

  it("returns an empty array when no candidate carries this capability's evidence", () => {
    const candidates: TrackCandidate[] = [{ id: "c1", rank: 0, evidence: {} }];
    expect(selectRankedSolutionFamilies(candidates)).toEqual([]);
  });
});

describe("selectMarkConceptCoveredEvidence (generic mechanical selector, cross-check)", () => {
  it("returns candidate ids carrying truthy evidence at this capability's key only", () => {
    const candidates: TrackCandidate[] = [
      { id: "covered", rank: 0, evidence: { "cognition/mark-concept-covered": true } },
      { id: "not-covered", rank: 1, evidence: {} },
    ];
    expect(selectMarkConceptCoveredEvidence(candidates)).toEqual(["covered"]);
  });
});

describe("COGNITION_CAPABILITY_COUNT", () => {
  it("matches the 13 admitted capability/cognition/* resources", () => {
    expect(COGNITION_CAPABILITY_COUNT).toBe(13);
  });
});
