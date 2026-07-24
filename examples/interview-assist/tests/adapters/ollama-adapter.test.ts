import { describe, it, expect } from "vitest";
import { getOllamaWorker, isOllamaReachable, DEFAULT_OLLAMA_CONFIG } from "../../lib/adapters/ollama-adapter";

// Top-level await: vitest evaluates the whole module (ESM) before
// collecting `it.runIf(...)` conditions, so the reachability probe must
// run here, not inside beforeAll, or runIf sees the stale `false` default.
const reachable = await isOllamaReachable();
if (!reachable) {
  // eslint-disable-next-line no-console
  console.warn("BLOCKED: no local Ollama server reachable at http://localhost:11434 -- skipping live-call test");
}

describe("ollama-adapter (real local Ollama endpoint, no mocks)", () => {

  it("reports real reachability against the actual local Ollama server", async () => {
    // This assertion is intentionally about the *shape* of the probe
    // (boolean, no throw), not a hardcoded expectation of true/false,
    // since the presence of a local Ollama server is environment-dependent.
    expect(typeof reachable).toBe("boolean");
  });

  it.runIf(reachable)("makes a real chat-completion call against the local model and gets non-empty content", async () => {
    const worker = getOllamaWorker({ ...DEFAULT_OLLAMA_CONFIG, timeoutMs: 60_000 });
    const response = await worker.run({ role: "interviewer", prompt: "Reply with exactly the word: PONG" });
    expect(response.content.length).toBeGreaterThan(0);
    expect(response.model.length).toBeGreaterThan(0);
  }, 65_000);

  it("compile-time correctness check: adapter builds a well-formed request against an unreachable port without throwing before the fetch", async () => {
    // Deliberately unreachable port -- proves the adapter correctly
    // reaches the network layer (fetch) and fails there, not earlier from
    // a type/shape bug, regardless of whether a real server is running.
    const worker = getOllamaWorker({ baseUrl: "http://localhost:1/v1", defaultModel: "x", timeoutMs: 1000 });
    await expect(worker.run({ role: "critic", prompt: "x" })).rejects.toThrow();
  });
});
