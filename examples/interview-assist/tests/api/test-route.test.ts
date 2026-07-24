/**
 * Real Chicago-TDD test for app/api/test/route.ts (JTBD 5 closure), no
 * mocks: invokes the real Next.js route handler function directly (same
 * module the running `next dev`/`next start` server dispatches this route
 * to) with a real `NextRequest`, which internally calls the real,
 * subprocess-spawning `getSandboxExecutor().execute({capability:"run_pytest"})`
 * (real python3/pytest, no fakes/doubles).
 */
import { describe, it, expect } from "vitest";
import { NextRequest } from "next/server";
import { POST } from "../../app/api/test/route";
import {
  VISIBLE_TEST_FILENAME,
  VISIBLE_TEST_SOURCE,
  HIDDEN_TEST_FILENAME,
  HIDDEN_TEST_SOURCE,
} from "../../lib/domain/two-sum-test-fixtures";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";

const CORRECT_TWO_SUM =
  "def two_sum(nums, target):\n" +
  "    seen = {}\n" +
  "    for i, n in enumerate(nums):\n" +
  "        complement = target - n\n" +
  "        if complement in seen:\n" +
  "            return [seen[complement], i]\n" +
  "        seen[n] = i\n" +
  "    return []\n";

const WRONG_TWO_SUM = "def two_sum(nums, target):\n    return []\n"; // never matches -> real failure

/** Two SEPARATE real pytest subprocess invocations of the identical
 * fixture legitimately differ in ways that carry no assertion-relevant
 * information: the wall-clock duration pytest's own `-q` summary line
 * reports (e.g. "1 passed in 0.05s" vs "1 passed in 0.08s"), and the
 * right-padding width of its dot-progress "[100%]" line, which pytest
 * computes from `shutil.get_terminal_size()` and can differ across two
 * separately-spawned subprocesses even with identical fixtures (verified
 * directly: see tests/e2e/jtbd-05-visible-tests.spec.ts's identical
 * helper, added after that suite's own first run hit exactly this
 * padding-width difference across two different parent-process
 * environments). Both are real, expected non-determinism from real
 * subprocesses, not a mock artifact. Collapsing whitespace runs and the
 * duration substring still lets the cross-check assert on every
 * substantive character pytest printed. */
function normalizePytestDuration(stdout: string): string {
  return stdout
    .replace(/in \d+(\.\d+)?s/g, "in <duration>")
    .replace(/[ \t]+/g, " ")
    .trim();
}

function postRequest(body: unknown): NextRequest {
  return new NextRequest("http://localhost:3000/api/test", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  });
}

describe("app/api/test/route.ts POST (real run_pytest dispatch, no mocks)", () => {
  it("a correct solution passes the real visible-test fixture, matching a direct sandbox-executor call for the identical fixture", async () => {
    const response = await POST(postRequest({ testKind: "visible", code: CORRECT_TWO_SUM }));
    expect(response.status).toBe(200);
    const json = (await response.json()) as {
      receipt?: { exitCode: number; stdout: string; stderr: string };
      testKind?: string;
    };
    expect(json.testKind).toBe("visible");
    expect(json.receipt).toBeDefined();
    expect(json.receipt!.exitCode).toBe(0);
    expect(json.receipt!.stdout).toMatch(/1 passed/);

    // Independent cross-check: the exact same real fixture, called
    // directly against the real executor (same module the route imports),
    // not a re-read of the route's own self-reported result.
    const direct = await getSandboxExecutor().execute({
      capability: "run_pytest",
      files: { "solution.py": CORRECT_TWO_SUM, [VISIBLE_TEST_FILENAME]: VISIBLE_TEST_SOURCE },
      timeoutMs: 15_000,
    });
    expect(isExecutionRefusal(direct)).toBe(false);
    if (isExecutionRefusal(direct)) throw new Error("unreachable");
    expect(direct.exitCode).toBe(0);
    expect(normalizePytestDuration(direct.stdout)).toBe(normalizePytestDuration(json.receipt!.stdout));
  });

  it("an incorrect solution fails the real visible-test fixture with a real non-zero exit code and a real assertion diff", async () => {
    const response = await POST(postRequest({ testKind: "visible", code: WRONG_TWO_SUM }));
    const json = (await response.json()) as { receipt?: { exitCode: number; stdout: string } };
    expect(json.receipt).toBeDefined();
    expect(json.receipt!.exitCode).not.toBe(0);
    expect(json.receipt!.stdout).toMatch(/1 failed/);
  });

  it("a correct solution passes the real hidden-test fixture too, matching a direct sandbox-executor call for the identical fixture", async () => {
    const response = await POST(postRequest({ testKind: "hidden", code: CORRECT_TWO_SUM }));
    const json = (await response.json()) as {
      receipt?: { exitCode: number; stdout: string };
      testKind?: string;
    };
    expect(json.testKind).toBe("hidden");
    expect(json.receipt).toBeDefined();
    expect(json.receipt!.exitCode).toBe(0);
    expect(json.receipt!.stdout).toMatch(/1 passed/);

    const direct = await getSandboxExecutor().execute({
      capability: "run_pytest",
      files: { "solution.py": CORRECT_TWO_SUM, [HIDDEN_TEST_FILENAME]: HIDDEN_TEST_SOURCE },
      timeoutMs: 15_000,
    });
    expect(isExecutionRefusal(direct)).toBe(false);
    if (isExecutionRefusal(direct)) throw new Error("unreachable");
    expect(direct.exitCode).toBe(0);
    expect(normalizePytestDuration(direct.stdout)).toBe(normalizePytestDuration(json.receipt!.stdout));
  });

  it("threads a real chained receipt when prevReceipt is supplied", async () => {
    const first = await POST(postRequest({ testKind: "visible", code: CORRECT_TWO_SUM }));
    const firstJson = (await first.json()) as {
      receipt?: { transitionReceipt?: { checksum: { checksumValue: string } } };
    };
    expect(firstJson.receipt?.transitionReceipt).toBeDefined();

    const second = await POST(
      postRequest({ testKind: "hidden", code: CORRECT_TWO_SUM, prevReceipt: firstJson.receipt!.transitionReceipt }),
    );
    const secondJson = (await second.json()) as {
      receipt?: { transitionReceipt?: { derivedFrom?: string; relation?: string } };
    };
    expect(secondJson.receipt?.transitionReceipt?.derivedFrom).toBe(
      firstJson.receipt!.transitionReceipt!.checksum.checksumValue,
    );
    expect(secondJson.receipt?.transitionReceipt?.relation).toBe(
      firstJson.receipt!.transitionReceipt!.checksum.checksumValue,
    );
  });

  it("returns 400 for a request body with no code", async () => {
    const response = await POST(postRequest({ testKind: "visible" }));
    expect(response.status).toBe(400);
  });

  it("returns 400 for an unrecognized testKind", async () => {
    const response = await POST(postRequest({ testKind: "bogus", code: CORRECT_TWO_SUM }));
    expect(response.status).toBe(400);
  });

  it("real policy denial: run_pytest is refused under a mode that prohibits execute-code, without spawning anything", async () => {
    const response = await POST(
      postRequest({ testKind: "visible", code: CORRECT_TWO_SUM, activeMode: "policy/authority-broker-default" }),
    );
    const json = (await response.json()) as { refusal?: { kind: string } };
    expect(json.refusal).toBeDefined();
    expect(json.refusal!.kind).toBe("policy_denied");
  });
});
