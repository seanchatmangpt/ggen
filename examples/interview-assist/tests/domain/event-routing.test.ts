// TICKET-022: event-routing.ts real-query-backed test. Chicago TDD: the
// "15 entries" expectation is not a hardcoded constant here -- it is
// re-derived by running the *actual* SPARQL query (queries/event-families.rq)
// against the *actual* ontology via a real rdflib subprocess (same pattern
// as lib/domain/__tests__/capability.count.test.mjs), and compared against
// the generated module's live export.
import { describe, it, expect } from "vitest";
import { execFileSync } from "node:child_process";
import { ALL_EVENT_FAMILIES } from "../../lib/domain/event-family";
import { EVENT_ROUTING, EVENT_ROUTING_ENTRY_COUNT } from "../../lib/domain/event-routing";

const PACK = "/Users/sac/ggen/packs/wasm4pm-interview-assist-pack";

function liveEventFamilyCount(): number {
  const py = `
import sys
from rdflib import Graph
g = Graph()
g.parse("${PACK}/ontology.ttl", format="turtle")
q = open("${PACK}/queries/event-families.rq").read()
print(len(list(g.query(q))))
`;
  return parseInt(execFileSync("python3", ["-c", py]).toString().trim(), 10);
}

describe("event-routing.ts (TICKET-022, real rdflib subprocess, no mocks)", () => {
  it("has exactly as many routing entries as the live SPARQL query returns event families", () => {
    const liveCount = liveEventFamilyCount();
    expect(liveCount).toBe(15);
    expect(EVENT_ROUTING_ENTRY_COUNT).toBe(liveCount);
    expect(Object.keys(EVENT_ROUTING)).toHaveLength(liveCount);
  });

  it("has one routing entry per EventFamily member, no more, no fewer", () => {
    for (const family of ALL_EVENT_FAMILIES) {
      expect(EVENT_ROUTING[family]).toBeTypeOf("string");
      expect(EVENT_ROUTING[family].length).toBeGreaterThan(0);
    }
    expect(Object.keys(EVENT_ROUTING).sort()).toEqual([...ALL_EVENT_FAMILIES].sort());
  });

  it("routes SessionEvent to a handler named after it, not a generic default", () => {
    expect(EVENT_ROUTING.SessionEvent).toBe("handleSessionEvent");
  });
});
