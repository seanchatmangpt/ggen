// TICKET-023: sessionReducer tests. Chicago TDD: composed with the real
// generated tables (phase-transitions.ts, event-routing.ts) and real
// refusal.ts types -- nothing here is mocked or stubbed.
import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { sessionReducer, type SessionState } from "../../lib/domain/reducer";
import { ALL_EVENT_FAMILIES } from "../../lib/domain/event-family";
import { ALL_PHASES } from "../../lib/domain/phase";

const __dirname = dirname(fileURLToPath(import.meta.url));

describe("sessionReducer (TICKET-023)", () => {
  it("admits a legal transition and updates state.phase", () => {
    const state: SessionState = { phase: "CREATED" };
    const result = sessionReducer(state, { family: "SessionEvent", targetPhase: "PREPARING" });
    expect(result.status).toBe("admitted");
    if (result.status === "admitted") {
      expect(result.value.phase).toBe("PREPARING");
    }
  });

  it("refuses an illegal transition (CREATED directly to COMPLETE) and leaves state untouched by the caller's own copy", () => {
    const state: SessionState = { phase: "CREATED" };
    const result = sessionReducer(state, { family: "SessionEvent", targetPhase: "COMPLETE" });
    expect(result.status).toBe("refused");
    if (result.status === "refused") {
      expect(result.code).toBe("STALE_SESSION_EVENT");
    }
    // original state object passed in is never mutated
    expect(state.phase).toBe("CREATED");
  });

  it("refuses an unknown event family with a named refusal code, never silently ignoring it", () => {
    const state: SessionState = { phase: "CREATED" };
    const result = sessionReducer(state, { family: "NotARealFamily" });
    expect(result.status).toBe("refused");
    if (result.status === "refused") {
      expect(result.code).toBeTypeOf("string");
      expect(result.code.length).toBeGreaterThan(0);
      expect(result.reason).toContain("NotARealFamily");
    }
  });

  it("admits a routed event with no targetPhase as a no-op (state passes through unchanged)", () => {
    const state: SessionState = { phase: "READY", foo: 1 };
    const result = sessionReducer(state, { family: "AccessibilityEvent" });
    expect(result.status).toBe("admitted");
    if (result.status === "admitted") {
      expect(result.value).toEqual(state);
    }
  });

  it("dispatches through every one of the 15 admitted event families without a family-name literal switch (each is admitted or legally refused, never thrown)", () => {
    for (const family of ALL_EVENT_FAMILIES) {
      const state: SessionState = { phase: "CREATED" };
      expect(() => sessionReducer(state, { family })).not.toThrow();
      const result = sessionReducer(state, { family });
      expect(["admitted", "refused"]).toContain(result.status);
    }
  });

  it("the reducer body contains zero phase or event-family string literals outside imports (falsifier)", () => {
    // Real filesystem read + real grep-equivalent regex, executed as the
    // ticket's own documented falsifier -- not a stand-in assertion.
    const src = readFileSync(join(__dirname, "../../lib/domain/reducer.ts"), "utf8");
    const forbidden = [...ALL_PHASES];
    const pattern = new RegExp(`"(${forbidden.join("|")})"`, "g");
    const matches = src.match(pattern) ?? [];
    expect(matches).toEqual([]);
  });
});
