// TICKET-021: exhaustive 14x14 truth-table test for isLegalTransition().
// Chicago TDD: no mock of the RDF/SPARQL layer -- the expected-edge set
// below is transcribed directly from packs/wasm4pm-interview-assist-pack/
// ontology/40-events-workflow.ttl's transition-plan/* resources and
// phase/refused's skos:related set (the same two admitted RDF facts
// phase-transitions.ts itself was generated from), so this test is a real
// second, independent encoding of the RDF truth, not a mirror of the
// generated file's own logic.
import { describe, it, expect } from "vitest";
import { ALL_PHASES, type Phase } from "../../lib/domain/phase";
import { isLegalTransition } from "../../lib/domain/phase-transitions";

// The 13 admitted transition-plan/* schema:object -> schema:result edges.
const EXPECTED_EDGES: ReadonlyArray<readonly [Phase, Phase]> = [
  ["CREATED", "PREPARING"],
  ["PREPARING", "READY"],
  ["READY", "INTRODUCTION"],
  ["INTRODUCTION", "PROBLEM_PRESENTATION"],
  ["PROBLEM_PRESENTATION", "CLARIFICATION"],
  ["CLARIFICATION", "PLANNING"],
  ["PLANNING", "IMPLEMENTATION"],
  ["IMPLEMENTATION", "EXECUTION"],
  ["EXECUTION", "DEBUGGING"],
  ["DEBUGGING", "EXPLANATION"],
  ["DEBUGGING", "IMPLEMENTATION"],
  ["EXPLANATION", "FOLLOW_UP"],
  ["FOLLOW_UP", "COMPLETE"],
];

// phase/refused's 12 skos:related members: every non-terminal phase except
// REFUSED and COMPLETE (both terminal).
const WILDCARD_SOURCES: readonly Phase[] = ALL_PHASES.filter(
  (p) => p !== "COMPLETE" && p !== "REFUSED"
);

function expectedLegal(from: Phase, to: Phase): boolean {
  if (to === "REFUSED") {
    return WILDCARD_SOURCES.includes(from);
  }
  return EXPECTED_EDGES.some(([f, t]) => f === from && t === to);
}

describe("isLegalTransition (TICKET-021 exhaustive 14x14 truth table)", () => {
  it("matches the RDF-admitted edge set for all 196 (14x14) phase pairs", () => {
    const mismatches: string[] = [];
    let trueCount = 0;
    for (const from of ALL_PHASES) {
      for (const to of ALL_PHASES) {
        const expected = expectedLegal(from, to);
        const actual = isLegalTransition(from, to);
        if (expected) trueCount++;
        if (expected !== actual) {
          mismatches.push(`${from} -> ${to}: expected ${expected}, got ${actual}`);
        }
      }
    }
    expect(mismatches).toEqual([]);
    // 13 forward/backward edges + 12 wildcard-to-REFUSED edges = 25 true pairs.
    expect(trueCount).toBe(25);
  });

  it("acceptance criteria: DEBUGGING <-> EXPLANATION (forward) and DEBUGGING <-> IMPLEMENTATION (backward) are both legal", () => {
    expect(isLegalTransition("DEBUGGING", "EXPLANATION")).toBe(true);
    expect(isLegalTransition("DEBUGGING", "IMPLEMENTATION")).toBe(true);
  });

  it("negative test: CREATED -> COMPLETE (a non-adjacent jump) is illegal -- not a permissive pass-through", () => {
    expect(isLegalTransition("CREATED", "COMPLETE")).toBe(false);
  });

  it("REFUSED is reachable from every non-terminal phase but not from COMPLETE or REFUSED itself", () => {
    for (const p of WILDCARD_SOURCES) {
      expect(isLegalTransition(p, "REFUSED")).toBe(true);
    }
    expect(isLegalTransition("COMPLETE", "REFUSED")).toBe(false);
    expect(isLegalTransition("REFUSED", "REFUSED")).toBe(false);
  });
});
