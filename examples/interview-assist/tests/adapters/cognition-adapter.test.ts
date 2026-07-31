import { describe, it, expect } from "vitest";
import { runCognition } from "../../lib/adapters/cognition-adapter";

describe("cognition-adapter (real wasm4pm-cognition WASM binary, no mocks)", () => {
  it("matches an ARRAY-keyword intent and returns the real corresponding clarifying question", async () => {
    const outcome = await runCognition("I have an array of numbers to search through");
    expect(outcome.status).toBe("matched");
    if (outcome.status !== "matched") throw new Error("unreachable");
    expect(outcome.selected).toBe("ARRAY");
    expect(outcome.explanation).toBe(
      "Is this a Two Sum-style problem -- finding two values in an array whose sum equals a target?",
    );
    expect(outcome.runId).toBeTruthy();
    expect(outcome.conformance.fitness).toBe(1);
    expect(outcome.conformance.modelId).toBe("eliza");
    expect(outcome.conformance.refusals).toEqual([]);
  });

  it("matches a TARGET-keyword intent", async () => {
    const outcome = await runCognition("what target value are we aiming for");
    expect(outcome.status).toBe("matched");
    if (outcome.status !== "matched") throw new Error("unreachable");
    expect(outcome.selected).toBe("TARGET");
    expect(outcome.explanation).toBe("Are you looking for two numbers that add up to a target value?");
  });

  it("matches an INDICES-keyword intent", async () => {
    const outcome = await runCognition("do we need the indices of the matches");
    expect(outcome.status).toBe("matched");
    if (outcome.status !== "matched") throw new Error("unreachable");
    expect(outcome.selected).toBe("INDICES");
  });

  it("matches a SUM-keyword intent", async () => {
    const outcome = await runCognition("looking for pairs whose sum equals something");
    expect(outcome.status).toBe("matched");
    if (outcome.status !== "matched") throw new Error("unreachable");
    expect(outcome.selected).toBe("SUM");
  });

  it("returns no-track-matched for an intent with none of the four keywords (real fail-closed anti-fraud check)", async () => {
    const outcome = await runCognition("hello there, nice weather today");
    expect(outcome.status).toBe("no-track-matched");
    if (outcome.status !== "no-track-matched") throw new Error("unreachable");
    expect(outcome.reason).toContain("empty inference trace");
  });

  it("refuses an empty-string intent without calling the WASM module", async () => {
    const outcome = await runCognition("");
    expect(outcome).toEqual({ status: "refused", reason: "intent must be a non-empty string" });
  });

  it("refuses a whitespace-only intent without calling the WASM module", async () => {
    const outcome = await runCognition("   \n\t  ");
    expect(outcome.status).toBe("refused");
  });

  describe("graceful WASM-load-failure handling (production-hardening pass)", () => {
    it("returns a typed 'unavailable' outcome (not a thrown exception) when the real require() genuinely fails to resolve the module", async () => {
      const outcome = await runCognition("I have an array of numbers to search through", undefined, true);
      expect(outcome.status).toBe("unavailable");
      if (outcome.status !== "unavailable") throw new Error("unreachable");
      // Real thrown-Error text from the real, deliberately-broken fixture
      // package's own module code (see lib/wasm/wasm4pm-cognition-test-broken-fixture/index.js)
      // -- a genuine require()-time failure, not a fabricated/generic message.
      expect(outcome.reason.length).toBeGreaterThan(0);
      expect(outcome.reason).toMatch(/intentionally broken/i);
    });

    it("never fabricates a receipt for an unavailable outcome -- no real cognition action occurred", async () => {
      const outcome = await runCognition("looking for pairs whose sum equals something", undefined, true);
      expect(outcome.status).toBe("unavailable");
      expect("receipt" in outcome).toBe(false);
    });

    it("recovers on the very next call once forceUnavailable is omitted -- a broken test load never poisons the real module cache", async () => {
      const broken = await runCognition("I have an array of numbers to search through", undefined, true);
      expect(broken.status).toBe("unavailable");

      const recovered = await runCognition("I have an array of numbers to search through");
      expect(recovered.status).toBe("matched");
      if (recovered.status !== "matched") throw new Error("unreachable");
      expect(recovered.selected).toBe("ARRAY");
    });
  });
});
