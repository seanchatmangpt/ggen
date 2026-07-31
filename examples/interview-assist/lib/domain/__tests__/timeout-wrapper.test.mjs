// Chicago TDD: real async wall-clock test, no mocks. Wraps a
// never-resolving Promise and confirms withTimeout still returns within its
// configured bound (TICKET-029's non-negotiable falsifier).
import assert from "node:assert/strict";
import { withTimeout } from "../timeout-wrapper.ts";

const boundMs = 300;
const start = Date.now();
const result = await withTimeout(
  "capability/runtime/execute",
  () => new Promise(() => {}), // never resolves
  boundMs
);
const elapsed = Date.now() - start;

console.log("withTimeout(never-resolving, bound=%dms) -> status=%s elapsedMs=%d realElapsed=%d",
  boundMs, result.status, result.elapsedMs, elapsed);

assert.equal(result.status, "refused", "never-resolving handler must resolve as refused");
assert.equal(result.refusal, "capability/runtime/enforce-timeout");
assert.ok(elapsed < boundMs + 150, `real wall-clock elapsed (${elapsed}ms) must be bounded near ${boundMs}ms, not hang`);
assert.ok(elapsed >= boundMs, "must not resolve before the configured bound");

// fast-resolving handler completes normally, not refused
const fastResult = await withTimeout("capability/runtime/select-language", async () => 42, boundMs);
console.log("withTimeout(fast handler) -> status=%s result=%s", fastResult.status, fastResult.result);
assert.equal(fastResult.status, "ok");
assert.equal(fastResult.result, 42);

console.log("PASS timeout-wrapper.test.mjs: bounded-return-time confirmed (%dms real elapsed vs %dms bound)", elapsed, boundMs);
