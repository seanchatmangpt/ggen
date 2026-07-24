import { expect, test } from "@playwright/test";

/**
 * Chicago TDD, adversarial cases: real HTTP requests against the real
 * running server and real lib/executor.ts -- no mocked fetch, no stubbed
 * executor. Each test actually attempts the exploit/failure mode and
 * asserts the real refusal, not just that a type exists.
 */

test("path traversal in a file key is refused, and no file is written outside the workspace", async ({ request }) => {
  const response = await request.post("/api/sandbox/execute_python", {
    data: { files: { "../../../tmp/playwright-pwned-proof.py": "print(1)" } },
  });
  expect(response.status()).toBe(422);
  const body = await response.json();
  expect(body.error).toBe("EXECUTOR_UNAVAILABLE");
  expect(body.reason).toContain("escapes the sandbox workspace");

  // Confirm the file genuinely was not written outside the workspace --
  // not just that the API returned an error while still writing it.
  const fs = await import("node:fs/promises");
  await expect(fs.access("/tmp/playwright-pwned-proof.py")).rejects.toThrow();
});

test("oversized program output is truncated and the process is killed, not left to grow unbounded", async ({ request }) => {
  const response = await request.post("/api/sandbox/execute_python", {
    data: { files: { "solution.py": 'print("x" * 2_000_000)' } },
  });
  expect(response.status()).toBe(200);
  const body = await response.json();
  // exitCode -1 signals the process was killed for exceeding the output cap
  // (same signal used for timeout), not a clean exit.
  expect(body.exitCode).toBe(-1);
  expect(body.stdout).toContain("[output truncated");
  expect(body.stdout.length).toBeLessThan(1_100_000);
});

test("an oversized request body is rejected before execution, based on Content-Length alone", async ({ request }) => {
  const hugePayload = "x".repeat(3_000_000);
  const response = await request.post("/api/sandbox/execute_python", {
    data: { files: { "solution.py": `print(len("${hugePayload}"))` } },
  });
  expect(response.status()).toBe(413);
  const body = await response.json();
  expect(body.error).toBe("PAYLOAD_TOO_LARGE");
});

test("a cataloged but unimplemented capability returns an honest 501, not a misleading 400", async ({ request }) => {
  const response = await request.post("/api/sandbox/create_session", { data: { files: { a: "b" } } });
  expect(response.status()).toBe(501);
  const body = await response.json();
  expect(body.error).toBe("NOT_IMPLEMENTED_IN_PHASE_0");
});

test("an infinite loop is killed at the capability's declared timeout, not left hanging", async ({ request }) => {
  const start = Date.now();
  const response = await request.post("/api/sandbox/execute_python", {
    data: { files: { "solution.py": "while True: pass" } },
    timeout: 20_000,
  });
  const elapsed = Date.now() - start;
  const body = await response.json();
  expect(body.exitCode).toBe(-1);
  expect(body.stderr).toContain("[timed out]");
  // execute_python's catalog timeout is 10_000ms -- confirm it actually
  // fired near that bound rather than running to the 20s test timeout.
  expect(elapsed).toBeLessThan(15_000);
});

test("rapid repeated requests are rate-limited", async ({ request }) => {
  // Uses the fast, non-executing create_session route (no subprocess
  // spawn per request) since the rate limit is global across every
  // /api/sandbox/* route -- this keeps the test itself fast and avoids
  // meaningfully consuming the budget other tests in this suite rely on
  // for their own real execution requests. The limit (100/60s) is set
  // well above this whole suite's normal request count so ordinary test
  // traffic never trips it by accident -- only this test's own deliberate
  // burst does.
  const results: number[] = [];
  for (let i = 0; i < 110; i++) {
    const response = await request.post("/api/sandbox/create_session", { data: { files: { a: "b" } } });
    results.push(response.status());
  }
  expect(results).toContain(429);
  // Every response before the first 429 should be the route's normal 501
  // (not-implemented) -- proves the 429s are genuinely rate-limit-caused,
  // not a pre-existing failure mode of this route, without assuming this
  // test started from a completely untouched quota.
  const firstRateLimitedIndex = results.indexOf(429);
  expect(firstRateLimitedIndex).toBeGreaterThan(0);
  expect(results.slice(0, firstRateLimitedIndex).every((status) => status === 501)).toBe(true);
});
