/**
 * TICKET-016: event-family union type.
 *
 * Projected from `<event-family-scheme>`'s skos:Concept members
 * (ARD §7) in packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl.
 * Verified via packs/wasm4pm-interview-assist-pack/queries/event-families.rq
 * (real rdflib run, 2026-07-23: 15 rows), ORDER BY ?family (URI order, which
 * is also alphabetical label order for this scheme).
 *
 * DO NOT hand-edit member names without re-running event-families.rq against
 * ontology.ttl and updating this comment's row count.
 */
export type EventFamily =
  | "AccessibilityEvent"
  | "CompilerEvent"
  | "EditorEvent"
  | "ExecutionEvent"
  | "FileEvent"
  | "HypothesisEvent"
  | "ParticipantEvent"
  | "PolicyEvent"
  | "ProjectionEvent"
  | "QuestionEvent"
  | "ReceiptEvent"
  | "SessionEvent"
  | "SpeechEvent"
  | "TestEvent"
  | "WorkflowEvent";

export const ALL_EVENT_FAMILIES: readonly EventFamily[] = [
  "AccessibilityEvent",
  "CompilerEvent",
  "EditorEvent",
  "ExecutionEvent",
  "FileEvent",
  "HypothesisEvent",
  "ParticipantEvent",
  "PolicyEvent",
  "ProjectionEvent",
  "QuestionEvent",
  "ReceiptEvent",
  "SessionEvent",
  "SpeechEvent",
  "TestEvent",
  "WorkflowEvent",
] as const;
