/**
 * TICKET-016: session phase union type.
 *
 * Projected from `<phase-scheme>`'s skos:Concept members in
 * packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl, in
 * skos:broader chain order (verified via
 * packs/wasm4pm-interview-assist-pack/queries/phases.rq — real rdflib run,
 * 2026-07-23: 14 rows). `refused` is the wildcard terminal phase (no
 * skos:broader — see phase-transitions.ts / TICKET-021 for the wildcard
 * transition rule).
 *
 * DO NOT hand-edit member names without re-running phases.rq against
 * ontology.ttl and updating this comment's row count.
 */
export type Phase =
  | "CREATED"
  | "PREPARING"
  | "READY"
  | "INTRODUCTION"
  | "PROBLEM_PRESENTATION"
  | "CLARIFICATION"
  | "PLANNING"
  | "IMPLEMENTATION"
  | "EXECUTION"
  | "DEBUGGING"
  | "EXPLANATION"
  | "FOLLOW_UP"
  | "COMPLETE"
  | "REFUSED";

export const ALL_PHASES: readonly Phase[] = [
  "CREATED",
  "PREPARING",
  "READY",
  "INTRODUCTION",
  "PROBLEM_PRESENTATION",
  "CLARIFICATION",
  "PLANNING",
  "IMPLEMENTATION",
  "EXECUTION",
  "DEBUGGING",
  "EXPLANATION",
  "FOLLOW_UP",
  "COMPLETE",
  "REFUSED",
] as const;
