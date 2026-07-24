// TICKET-055: real full-session receipt chain integration test. Chicago
// TDD -- real reducer, real subprocess execution (python3), real BLAKE3
// hashing. Nothing mocked. Exercises all 4 real manufacturing-chain steps
// from packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl
// (admission -> sandbox-execution -> test-result -> accessibility-projection)
// in one real session and verifies the emitted receipts are correctly
// chained (TICKET-055's acceptance criteria), AND that the failure path
// (a real Python syntax error) still emits a receipt (TICKET-055's own
// falsifier / negative test).
//
// Phase 3 extends this with a real 5-step chain: admission -> cognition-run
// (the real wasm4pm-cognition Eliza breed, via 90-cognition-bridge.ttl's
// <manufacturing-chain/cognition-activity>, inserted between admission and
// sandbox-execution) -> sandbox-execution -> test-result ->
// accessibility-projection.
import { describe, it, expect } from "vitest";
import { admitWithReceipt } from "../../lib/domain/reducer-with-receipts";
import type { SessionState } from "../../lib/domain/reducer";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";
import { buildAnnouncement } from "../../lib/adapters/accessibility-platform-adapter";
import { runCognition } from "../../lib/adapters/cognition-adapter";
import type { TransitionReceipt } from "../../lib/domain/receipt";

describe("real end-to-end manufacturing-chain receipt emission (TICKET-055)", () => {
  it("emits exactly 4 real, correctly-chained receipts across a real session traversing all 4 steps", async () => {
    const receipts: TransitionReceipt[] = [];

    // ---- Step 1: admission (real reducer, real legal transition) ----
    const state0: SessionState = { phase: "CREATED" };
    const { result: admitResult, receipt: receipt1 } = admitWithReceipt(
      state0,
      { family: "SessionEvent", targetPhase: "PREPARING" },
    );
    expect(admitResult.status).toBe("admitted");
    expect(receipt1).toBeDefined();
    receipts.push(receipt1!);

    // ---- Step 2: sandbox-execution (real python3 subprocess) ----
    const executor = getSandboxExecutor();
    const execResult = await executor.execute({
      capability: "execute_python",
      files: { "solution.py": "print(1+1)" },
      timeoutMs: 10_000,
      prevReceipt: receipts[receipts.length - 1],
    });
    expect(isExecutionRefusal(execResult)).toBe(false);
    if (isExecutionRefusal(execResult)) throw new Error("unreachable");
    expect(execResult.stdout.trim()).toBe("2");
    expect(execResult.exitCode).toBe(0);
    expect(execResult.transitionReceipt).toBeDefined();
    receipts.push(execResult.transitionReceipt!);

    // ---- Step 3: test-result (real pytest run) ----
    const testResult = await executor.execute({
      capability: "run_pytest",
      files: {
        "test_probe.py": "def test_addition():\n    assert 1 + 1 == 2\n",
      },
      timeoutMs: 20_000,
      prevReceipt: receipts[receipts.length - 1],
    });
    expect(isExecutionRefusal(testResult)).toBe(false);
    if (isExecutionRefusal(testResult)) throw new Error("unreachable");
    expect(testResult.exitCode).toBe(0); // real pytest run, 1 real passing test
    expect(testResult.transitionReceipt).toBeDefined();
    receipts.push(testResult.transitionReceipt!);

    // ---- Step 4: accessibility-projection (real announcement build) ----
    const announcement = buildAnnouncement(
      "info",
      "Test passed: test_addition",
      undefined,
      receipts[receipts.length - 1],
    );
    expect(announcement.receipt).toBeDefined();
    receipts.push(announcement.receipt);

    // ---- Acceptance criteria: exactly 4 receipts, correctly chained ----
    expect(receipts).toHaveLength(4);
    for (const r of receipts) {
      expect(r.checksum.algorithm).toBe("BLAKE3");
      expect(r.checksum.checksumValue).toMatch(/^[0-9a-f]{64}$/);
    }
    // receipt[0] (admission) has no predecessor
    expect(receipts[0]!.derivedFrom).toBeUndefined();
    // receipt[1..3] each derive from the immediately-prior receipt's real
    // checksum -- the same pattern 60-provenance-receipts.ttl's own
    // <receipt/entry-2-final> prov:wasDerivedFrom <receipt/entry-1> uses.
    for (let i = 1; i < receipts.length; i++) {
      expect(receipts[i]!.derivedFrom).toBe(receipts[i - 1]!.checksum.checksumValue);
      expect(receipts[i]!.relation).toBe(receipts[i - 1]!.checksum.checksumValue);
    }
    // all 4 checksums are real and distinct (no fabricated/duplicate hash)
    const distinctChecksums = new Set(receipts.map((r) => r.checksum.checksumValue));
    expect(distinctChecksums.size).toBe(4);
  }, 30_000);

  it("FALSIFIER (negative test): a real sandbox-execution failure (Python syntax error) still emits a receipt recording the failure, not silently dropped", async () => {
    const executor = getSandboxExecutor();
    const result = await executor.execute({
      capability: "execute_python",
      files: { "solution.py": "def broken(:\n    pass\n" }, // real syntax error
      timeoutMs: 10_000,
    });
    expect(isExecutionRefusal(result)).toBe(false); // this is a real completed
    // execution that failed, NOT a pre-execution refusal (policy_denied /
    // no_source_provided) -- ExecutionRefusal is a different, disjoint type.
    if (isExecutionRefusal(result)) throw new Error("unreachable");
    expect(result.exitCode).not.toBe(0); // real failure: python3 rejects the syntax
    expect(result.stderr).toContain("SyntaxError");

    // The falsifier: the receipt MUST still be present, recording this
    // real failed step (used=[filename], generated=`exitCode=<nonzero>`).
    expect(result.transitionReceipt).toBeDefined();
    const receipt = result.transitionReceipt!;
    expect(receipt.checksum.algorithm).toBe("BLAKE3");
    expect(receipt.checksum.checksumValue).toMatch(/^[0-9a-f]{64}$/);
    expect(receipt.used).toEqual(["solution.py"]);
    expect(receipt.generated).toBe(`exitCode=${result.exitCode}`);
  });

  it("emits exactly 5 real, correctly-chained receipts across a real session including a real cognition-run step (admission -> cognition-run -> sandbox-execution -> test-result -> accessibility-projection)", async () => {
    const receipts: TransitionReceipt[] = [];

    // ---- Step 1: admission (real reducer, real legal transition) ----
    const state0: SessionState = { phase: "CREATED" };
    const { result: admitResult, receipt: receipt1 } = admitWithReceipt(
      state0,
      { family: "SessionEvent", targetPhase: "PREPARING" },
    );
    expect(admitResult.status).toBe("admitted");
    expect(receipt1).toBeDefined();
    receipts.push(receipt1!);

    // ---- Step 2: cognition-run (real wasm4pm-cognition Eliza breed call) ----
    const cognitionOutcome = await runCognition(
      "I have an array of numbers to search through",
      receipts[receipts.length - 1],
    );
    expect(cognitionOutcome.status).toBe("matched");
    if (cognitionOutcome.status !== "matched") throw new Error("unreachable");
    expect(cognitionOutcome.selected).toBe("ARRAY");
    expect(cognitionOutcome.receipt).toBeDefined();
    receipts.push(cognitionOutcome.receipt);

    // ---- Step 3: sandbox-execution (real python3 subprocess) ----
    const executor = getSandboxExecutor();
    const execResult = await executor.execute({
      capability: "execute_python",
      files: { "solution.py": "print(1+1)" },
      timeoutMs: 10_000,
      prevReceipt: receipts[receipts.length - 1],
    });
    expect(isExecutionRefusal(execResult)).toBe(false);
    if (isExecutionRefusal(execResult)) throw new Error("unreachable");
    expect(execResult.stdout.trim()).toBe("2");
    expect(execResult.exitCode).toBe(0);
    expect(execResult.transitionReceipt).toBeDefined();
    receipts.push(execResult.transitionReceipt!);

    // ---- Step 4: test-result (real pytest run) ----
    const testResult = await executor.execute({
      capability: "run_pytest",
      files: {
        "test_probe.py": "def test_addition():\n    assert 1 + 1 == 2\n",
      },
      timeoutMs: 20_000,
      prevReceipt: receipts[receipts.length - 1],
    });
    expect(isExecutionRefusal(testResult)).toBe(false);
    if (isExecutionRefusal(testResult)) throw new Error("unreachable");
    expect(testResult.exitCode).toBe(0); // real pytest run, 1 real passing test
    expect(testResult.transitionReceipt).toBeDefined();
    receipts.push(testResult.transitionReceipt!);

    // ---- Step 5: accessibility-projection (real announcement build) ----
    const announcement = buildAnnouncement(
      "info",
      "Test passed: test_addition",
      undefined,
      receipts[receipts.length - 1],
    );
    expect(announcement.receipt).toBeDefined();
    receipts.push(announcement.receipt);

    // ---- Acceptance criteria: exactly 5 receipts, correctly chained ----
    expect(receipts).toHaveLength(5);
    for (const r of receipts) {
      expect(r.checksum.algorithm).toBe("BLAKE3");
      expect(r.checksum.checksumValue).toMatch(/^[0-9a-f]{64}$/);
    }
    // receipt[0] (admission) has no predecessor
    expect(receipts[0]!.derivedFrom).toBeUndefined();
    // receipt[1..4] each derive from the immediately-prior receipt's real
    // checksum -- the same pattern 60-provenance-receipts.ttl's own
    // <receipt/entry-2-final> prov:wasDerivedFrom <receipt/entry-1> uses.
    for (let i = 1; i < receipts.length; i++) {
      expect(receipts[i]!.derivedFrom).toBe(receipts[i - 1]!.checksum.checksumValue);
      expect(receipts[i]!.relation).toBe(receipts[i - 1]!.checksum.checksumValue);
    }
    // all 5 checksums are real and distinct (no fabricated/duplicate hash)
    const distinctChecksums = new Set(receipts.map((r) => r.checksum.checksumValue));
    expect(distinctChecksums.size).toBe(5);
  }, 30_000);

  it("FALSIFIER (negative test): a real cognition-run failure (no keyword match) still emits a receipt recording the failure, not silently dropped", async () => {
    const outcome = await runCognition("hello there, nice weather today");
    expect(outcome.status).toBe("no-track-matched"); // real fail-closed anti-fraud check
    if (outcome.status !== "no-track-matched") throw new Error("unreachable");
    expect(outcome.reason).toContain("empty inference trace");

    const receipt = outcome.receipt;
    expect(receipt).toBeDefined();
    expect(receipt.checksum.algorithm).toBe("BLAKE3");
    expect(receipt.checksum.checksumValue).toMatch(/^[0-9a-f]{64}$/);
    expect(receipt.label).toContain("cognition-run");
  });

  it("FALSIFIER (negative test): a real test-result failure (a failing pytest assertion) still emits a receipt", async () => {
    const executor = getSandboxExecutor();
    const result = await executor.execute({
      capability: "run_pytest",
      files: {
        "test_fail_probe.py": "def test_deliberately_false():\n    assert 1 + 1 == 3\n",
      },
      timeoutMs: 20_000,
    });
    expect(isExecutionRefusal(result)).toBe(false);
    if (isExecutionRefusal(result)) throw new Error("unreachable");
    expect(result.exitCode).not.toBe(0); // real pytest failure (assertion false)
    expect(result.transitionReceipt).toBeDefined();
    expect(result.transitionReceipt!.label).toContain("test-result");
  }, 25_000);
});
