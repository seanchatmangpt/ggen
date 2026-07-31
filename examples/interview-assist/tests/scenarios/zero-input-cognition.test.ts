/**
 * TICKET-051: Vertical scenario -- Zero-input cognition.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence. Authored here as a real vitest
 * test composing the real event-admission reducer (TICKET-023) with the
 * real cognition selectors (TICKET-024) -- no LLM required per Architecture
 * Decision 8, no mocked core collaborator. Reuses TICKET-042's
 * track-confirmation.test.ts `deriveCandidatesFromEvents` pattern exactly
 * (test-only fixture derivation, kept local, not new production logic):
 * every event is first run through the real sessionReducer to prove it is
 * a legitimately admitted event before its evidenceKey is folded into a
 * TrackCandidate for the real selector to read.
 *
 * "No explicit confirmation/button-press event" is read literally against
 * this app's own real event model: app/page.tsx's button handlers (e.g.
 * `advanceTo`, `refuseSession`) all dispatch through `family: "WorkflowEvent"`
 * or `family: "SessionEvent"` (the two families that carry an explicit
 * `targetPhase`, i.e. a deliberate confirmed state change). The event
 * families genuinely capable of carrying PASSIVE observed input --
 * editor keystrokes and speech classification, with no human "confirm"
 * action attached -- are `EditorEvent` and `SpeechEvent`. This fixture
 * therefore contains ONLY EditorEvent/SpeechEvent events and asserts that
 * structural fact explicitly (not just by omission) before relying on it.
 */
import { describe, it, expect } from "vitest";
import { sessionReducer, type SessionEvent, type SessionState } from "../../lib/domain/reducer";
import { selectMarkConceptCoveredEvidence, selectAbstention } from "../../lib/domain/selectors";
import type { TrackCandidate, TrackCandidateEvidence } from "../../lib/domain/track-candidate";

const CONFIRMATION_FAMILIES = new Set(["WorkflowEvent", "SessionEvent"]);

/** Identical derivation pattern to TICKET-042's track-confirmation.test.ts
 * (test-only, kept local to this spec file). */
function deriveCandidatesFromEvents(
  events: readonly SessionEvent[],
  phase: SessionState["phase"] = "IMPLEMENTATION",
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

describe("TICKET-051 zero-input cognition (real reducer + real selectors, zero confirmation events, no mocks)", () => {
  it("mark-concept-covered fires from purely passive EditorEvent/SpeechEvent observation, with ZERO WorkflowEvent/SessionEvent confirmation events in the sequence", () => {
    const events: SessionEvent[] = [
      { family: "EditorEvent", type: "editor/modify-file", candidateId: "concept-two-sum", rank: 1, evidenceKey: "cognition/mark-concept-covered" },
      { family: "SpeechEvent", type: "speech/classified-approach", candidateId: "concept-two-sum", rank: 1, evidenceKey: "cognition/detect-speech-act" },
      { family: "EditorEvent", type: "editor/display-diagnostics", candidateId: "concept-two-sum", rank: 1, evidenceKey: "cognition/detect-missing-explanation" },
    ];

    // Structural proof, not an eyeballed comment: no event in this fixture
    // is a confirmation/button-press family.
    expect(events.some((e) => CONFIRMATION_FAMILIES.has(e.family))).toBe(false);
    expect(events.every((e) => e.family === "EditorEvent" || e.family === "SpeechEvent")).toBe(true);

    const candidates = deriveCandidatesFromEvents(events);
    const covered = selectMarkConceptCoveredEvidence(candidates);

    // Genuine zero-input operation: at least one concept/track is marked
    // covered despite no explicit confirmation event ever having been
    // admitted -- not a hidden dependency on an unstated confirmation step.
    expect(covered.length).toBeGreaterThanOrEqual(1);
    expect(covered).toContain("concept-two-sum");
  });

  it("negative: insufficient signal (no event carries the mark-concept-covered evidence key) does NOT falsely mark any concept covered", () => {
    const events: SessionEvent[] = [
      { family: "SpeechEvent", type: "speech/classified-approach", candidateId: "concept-unclear", rank: 1, evidenceKey: "cognition/detect-speech-act" },
      { family: "EditorEvent", type: "editor/open-file", candidateId: "concept-unclear", rank: 1, evidenceKey: "cognition/detect-topic-transition" },
    ];
    expect(events.every((e) => e.family === "EditorEvent" || e.family === "SpeechEvent")).toBe(true);

    const candidates = deriveCandidatesFromEvents(events);
    const covered = selectMarkConceptCoveredEvidence(candidates);

    // Real negative: the selector correctly returns nothing rather than
    // trivially-always-marking every observed candidate as covered.
    expect(covered).toEqual([]);
  });

  it("negative companion: an event sequence carrying explicit abstention evidence is read as abstention, not fabricated coverage (real selectAbstention, not merely absence of mark-concept-covered)", () => {
    const events: SessionEvent[] = [
      { family: "SpeechEvent", type: "speech/classified-approach", candidateId: "concept-ambiguous", rank: 1, evidenceKey: "cognition/abstain-under-insufficient-evidence" },
    ];
    const candidates = deriveCandidatesFromEvents(events);
    expect(candidates).toHaveLength(1);
    const evidence: TrackCandidateEvidence = candidates[0]!.evidence;
    expect(selectAbstention(evidence)).toBe(true);
    expect(selectMarkConceptCoveredEvidence(candidates)).toEqual([]);
  });
});
