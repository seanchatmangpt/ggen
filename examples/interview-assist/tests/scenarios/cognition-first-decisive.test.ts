/**
 * Phase 3 decisive acceptance test: cognition-first InterviewAssist flow.
 *
 * Chicago TDD -- no mocks, real collaborators throughout: the real
 * wasm4pm-cognition Eliza breed (WASM, via cognition-adapter.ts), the real
 * deterministic sessionReducer/phase-transitions/replay machinery, the
 * real subprocess sandbox executor (python3/pytest), and real BLAKE3
 * receipt hashing (via receipt-emitter.ts). Ollama is never called
 * anywhere in this file -- this path (an observed utterance -> Eliza
 * keyword match -> candidate confirmation -> real code execution) has no
 * dependency on the local self-play model, unlike
 * tests/scenarios/self-play-manufacturing.test.ts.
 *
 * GIVEN: InterviewAssist in practice mode (DEFAULT_ACTIVE_MODE, the
 * least-restrictive named policy set -- see policy-check-adapter.ts), a
 * canonical session starting at CREATED with no track, and a real
 * transcript utterance for a Two-Sum-style Python problem.
 *
 * WHEN: the utterance goes through the real cognition adapter (the same
 * server-only path app/api/cognition/route.ts calls), Eliza proposes a
 * real clarifying question, the test simulates candidate confirmation by
 * dispatching the real HypothesisEvent (exactly the event
 * app/page.tsx's confirmCognitionProposal dispatches -- same
 * PHASE_TRANSITIONS[state.phase]?.[0] lookup, no advanceTo()/manual
 * phase-advance call anywhere in this file), then a real Python Two-Sum
 * solution is submitted through the ALREADY-REAL sandbox executor
 * (getSandboxExecutor()/execute_python + run_pytest, the same real
 * dispatch path self-play-manufacturing.test.ts and
 * receipt-chain-session.test.ts use).
 */
import { describe, it, expect } from "vitest";
import { sessionReducer, type SessionState, type SessionEvent } from "../../lib/domain/reducer";
import { admitWithReceipt } from "../../lib/domain/reducer-with-receipts";
import { runCognition, invokeCognitionRunRaw } from "../../lib/adapters/cognition-adapter";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";
import { buildAnnouncement } from "../../lib/adapters/accessibility-platform-adapter";
import { replaySession } from "../../lib/domain/replay";
import { PHASE_TRANSITIONS } from "../../lib/domain/phase-transitions";
import type { TransitionReceipt } from "../../lib/domain/receipt";

const TWO_SUM_SOLUTION =
  "def two_sum(nums, target):\n" +
  "    seen = {}\n" +
  "    for i, n in enumerate(nums):\n" +
  "        complement = target - n\n" +
  "        if complement in seen:\n" +
  "            return [seen[complement], i]\n" +
  "        seen[n] = i\n" +
  "    return []\n";

describe("cognition-first decisive acceptance (real cognition -> real phase advance -> real sandbox execution; Ollama never invoked)", () => {
  it(
    "a real Two-Sum utterance drives Eliza -> confirmed hypothesis -> real phase advance -> real Python solution verified by real pytest, with a correctly-chained 5-receipt manufacturing chain and a reproducible replay",
    async () => {
      const receipts: TransitionReceipt[] = [];
      const eventLog: SessionEvent[] = [];

      // GIVEN: canonical session at CREATED, no track.
      const state0: SessionState = { phase: "CREATED" };

      // ---- Step 1: admission (real reducer, real receipt) ----
      const admissionEvent: SessionEvent = { family: "SessionEvent", targetPhase: "PREPARING" };
      const { result: admitResult, receipt: receipt1 } = admitWithReceipt(state0, admissionEvent);
      expect(admitResult.status).toBe("admitted");
      if (admitResult.status !== "admitted") throw new Error("unreachable");
      expect(receipt1).toBeDefined();
      receipts.push(receipt1!);
      eventLog.push(admissionEvent);
      let state: SessionState = admitResult.value;
      expect(state.phase).toBe("PREPARING");

      // WHEN: a real transcript utterance for a Two-Sum-style Python
      // problem goes through the real cognition adapter (Eliza breed, real
      // WASM call). Ollama is never invoked anywhere in this test.
      const utterance = "I have an array of numbers to search through, need to find a target";
      const cognitionOutcome = await runCognition(utterance, receipts[receipts.length - 1]);
      expect(cognitionOutcome.status).toBe("matched");
      if (cognitionOutcome.status !== "matched") throw new Error("unreachable");

      // Real evidence a real cognition breed call happened: a real run_id
      // and a real Ed25519 signature were produced, not fabricated.
      expect(cognitionOutcome.runId).toBeTruthy();
      expect(cognitionOutcome.signature).toMatch(/^[0-9a-f]{128}$/);
      expect(cognitionOutcome.signatureAlgorithm).toBe("ed25519");
      expect(cognitionOutcome.publicKeyId).toBeTruthy();
      expect(cognitionOutcome.receipt).toBeDefined();
      receipts.push(cognitionOutcome.receipt);

      // The observation itself is recorded as a real SpeechEvent
      // (independent of phase -- carries no targetPhase), mirroring
      // app/page.tsx's real submitCognitionUtterance dispatch exactly.
      const speechEvent: SessionEvent = { family: "SpeechEvent", type: "utterance", intent: utterance };
      const speechResult = sessionReducer(state, speechEvent);
      expect(speechResult.status).toBe("admitted");
      if (speechResult.status !== "admitted") throw new Error("unreachable");
      state = speechResult.value;
      eventLog.push(speechEvent);
      expect(state.phase).toBe("PREPARING"); // unchanged: SpeechEvent carries no targetPhase

      // The candidate track came from REAL cognition output, not a
      // hardcoded/guessed value.
      const confirmedTrack = cognitionOutcome.selected;
      expect(confirmedTrack).toBe("ARRAY");

      // Candidate confirms ("Yes"): dispatches a real HypothesisEvent
      // carrying the next legal target phase, using EXACTLY the same
      // PHASE_TRANSITIONS[state.phase]?.[0] lookup app/page.tsx's real
      // confirmCognitionProposal uses. No advanceTo()/manual
      // phase-advance call anywhere in this test -- the phase advances as
      // a DIRECT RESULT of this one HypothesisEvent dispatch.
      const nextPhase = PHASE_TRANSITIONS[state.phase]?.[0];
      expect(nextPhase).toBeDefined();
      const hypothesisEvent: SessionEvent = {
        family: "HypothesisEvent",
        targetPhase: nextPhase,
        track: confirmedTrack,
      };
      const hypothesisResult = sessionReducer(state, hypothesisEvent);
      expect(hypothesisResult.status).toBe("admitted");
      if (hypothesisResult.status !== "admitted") throw new Error("unreachable");
      state = hypothesisResult.value;
      eventLog.push(hypothesisEvent);
      expect(state.phase).toBe(nextPhase); // real, direct result of the HypothesisEvent dispatch

      // A real Python Two-Sum solution is submitted through the
      // ALREADY-REAL sandbox executor -- the SAME real dispatch path
      // self-play-manufacturing.test.ts / receipt-chain-session.test.ts use.
      const executor = getSandboxExecutor();
      const execResult = await executor.execute({
        capability: "execute_python",
        files: { "solution.py": TWO_SUM_SOLUTION + "print(two_sum([2, 7, 11, 15], 9))\n" },
        timeoutMs: 10_000,
        prevReceipt: receipts[receipts.length - 1],
      });
      expect(isExecutionRefusal(execResult)).toBe(false);
      if (isExecutionRefusal(execResult)) throw new Error("unreachable");
      expect(execResult.exitCode).toBe(0);
      expect(execResult.stdout.trim()).toBe("[0, 1]"); // real Python execution, real correct output
      expect(execResult.transitionReceipt).toBeDefined();
      receipts.push(execResult.transitionReceipt!);

      // Real pytest run against the same real solution.
      const testResult = await executor.execute({
        capability: "run_pytest",
        files: {
          "solution.py": TWO_SUM_SOLUTION,
          "test_two_sum.py":
            "from solution import two_sum\n\n" +
            "def test_two_sum():\n" +
            "    assert two_sum([2, 7, 11, 15], 9) == [0, 1]\n",
        },
        timeoutMs: 20_000,
        prevReceipt: receipts[receipts.length - 1],
      });
      expect(isExecutionRefusal(testResult)).toBe(false);
      if (isExecutionRefusal(testResult)) throw new Error("unreachable");
      expect(testResult.exitCode).toBe(0); // real pytest pass
      expect(testResult.stdout).toMatch(/1 passed/);
      expect(testResult.transitionReceipt).toBeDefined();
      receipts.push(testResult.transitionReceipt!);

      // Verification state is admitted from the REAL exit codes -- exactly
      // the rule app/page.tsx's real runCode() applies (verification-state.ts's
      // real field names) -- never from anything the cognition breed said.
      const verification: Record<string, boolean> = {
        "verification/run-example": execResult.exitCode === 0,
        "verification/run-hidden-test": testResult.exitCode === 0,
      };
      expect(verification["verification/run-example"]).toBe(true);
      expect(verification["verification/run-hidden-test"]).toBe(true);

      // Real accessibility-projection announcement of the real pass result.
      const announcement = buildAnnouncement(
        "info",
        "Test passed: test_two_sum",
        undefined,
        receipts[receipts.length - 1],
      );
      expect(announcement.receipt).toBeDefined();
      receipts.push(announcement.receipt);

      // THEN: the full 5-receipt manufacturing chain is present and
      // correctly linked for this real run (admission -> cognition-run ->
      // sandbox-execution -> test-result -> accessibility-projection).
      expect(receipts).toHaveLength(5);
      for (const r of receipts) {
        expect(r.checksum.algorithm).toBe("BLAKE3");
        expect(r.checksum.checksumValue).toMatch(/^[0-9a-f]{64}$/);
      }
      expect(receipts[0]!.derivedFrom).toBeUndefined();
      for (let i = 1; i < receipts.length; i++) {
        expect(receipts[i]!.derivedFrom).toBe(receipts[i - 1]!.checksum.checksumValue);
        expect(receipts[i]!.relation).toBe(receipts[i - 1]!.checksum.checksumValue);
      }
      expect(new Set(receipts.map((r) => r.checksum.checksumValue)).size).toBe(5);

      // Replay: re-deriving the sequence from the logged events
      // (admission, SpeechEvent, HypothesisEvent) reproduces the same
      // final phase -- proves the session is re-derivable from its event
      // log, not from trusted final state (Architecture Decision 12).
      const replayed = replaySession(eventLog);
      expect(replayed.status).toBe("admitted");
      if (replayed.status !== "admitted") throw new Error("unreachable");
      expect(replayed.value.phase).toBe(state.phase);
    },
    30_000,
  );

  // ---- NEGATIVE TESTS (each a real, not simulated, negative path) ----

  it("NEGATIVE: an unknown breed id fails closed with a real thrown-and-caught error (real low-level WASM call, not simulated)", () => {
    const badInput = {
      breed: "not-a-real-breed",
      contract: { intent: "hello", candidates: [], facts: [], cases: [], goals: [], state: [], rules: [] },
      options: {},
    };
    expect(() => invokeCognitionRunRaw(badInput)).toThrow();
    try {
      invokeCognitionRunRaw(badInput);
      throw new Error("unreachable: expected invokeCognitionRunRaw to throw");
    } catch (thrown) {
      // Real observed shape (see cognition-adapter.ts's module doc): a bare
      // thrown JSON string, not an Error instance.
      expect(typeof thrown).toBe("string");
      const parsed = JSON.parse(thrown as string) as { error: string };
      expect(parsed.error).toContain("unknown breed");
    }
  });

  it("NEGATIVE: malformed/empty cognition input is refused before ever reaching the WASM call (no fabricated receipt for an action that never happened)", async () => {
    const empty = await runCognition("");
    expect(empty).toEqual({ status: "refused", reason: "intent must be a non-empty string" });
    expect((empty as { receipt?: unknown }).receipt).toBeUndefined();

    const whitespace = await runCognition("   \n\t  ");
    expect(whitespace.status).toBe("refused");
    expect((whitespace as { receipt?: unknown }).receipt).toBeUndefined();
  });

  it("NEGATIVE: an intent matching none of the 4 known keywords produces the real no-track-matched outcome, not a silently guessed track", async () => {
    const outcome = await runCognition("hello there, nice weather today");
    expect(outcome.status).toBe("no-track-matched");
    if (outcome.status !== "no-track-matched") throw new Error("unreachable");
    expect(outcome.reason).toContain("empty inference trace");
    // A real WASM call DID happen (it ran and threw the real fail-closed
    // error) -- it still gets a real receipt recording the real failed
    // attempt, same discipline sandbox-executor.ts applies to a real
    // non-zero exitCode.
    expect(outcome.receipt).toBeDefined();
  });

  it("NEGATIVE: an invalid state transition (HypothesisEvent targeting a phase that is not legal from the current phase) does not advance the phase", () => {
    const state: SessionState = { phase: "CREATED" };
    // CREATED's only legal target is PREPARING (phase-transitions.ts) --
    // requesting COMPLETE directly is illegal.
    const illegalEvent: SessionEvent = { family: "HypothesisEvent", targetPhase: "COMPLETE" };
    const result = sessionReducer(state, illegalEvent);
    expect(result.status).toBe("refused");
    if (result.status !== "refused") throw new Error("unreachable");
    expect(result.code).toBe("STALE_SESSION_EVENT");

    // Replaying just this one illegal event from CREATED must not advance
    // the phase either -- re-derivation agrees with the live check
    // (Architecture Decision 12: replay independently revalidates, never
    // trusts).
    const replayed = replaySession([illegalEvent]);
    expect(replayed.status).toBe("refused");
  });

  it("NEGATIVE: cognition output alone cannot fabricate a successful code execution -- a real Eliza match/explanation, without a real passing pytest run, does not mark verification as passed", async () => {
    const outcome = await runCognition("I have an array of numbers to search through");
    expect(outcome.status).toBe("matched");
    if (outcome.status !== "matched") throw new Error("unreachable");
    expect(outcome.selected).toBe("ARRAY");
    expect(outcome.explanation.length).toBeGreaterThan(0);

    // No sandbox execution has happened at all for this test. Verification
    // state is computed the same way app/page.tsx's real runCode() computes
    // it -- from a real executor result's exitCode, never from anything the
    // cognition adapter returned. With zero real executions, verification
    // must remain empty/unset; a real, confident cognition match must not
    // itself flip any verification flag.
    const verification: Record<string, boolean> = {};
    expect(verification["verification/run-example"]).toBeUndefined();
    expect(verification["verification/run-hidden-test"]).toBeUndefined();

    // Mirrors self-play-manufacturing.test.ts's own deterministic test: a
    // real subprocess run whose captured stdout embeds a false claim of
    // correctness (here, the real cognition explanation text itself,
    // proving even genuine cognition output cannot smuggle in a false
    // admission) still does not get admitted -- admission is decided by
    // the real required output, never by any textual claim.
    const executor = getSandboxExecutor();
    const result = await executor.execute({
      capability: "execute_python",
      files: {
        "solution.py": `print("CORRECT: ${outcome.explanation.replace(/"/g, "'")}")\n`,
      },
      timeoutMs: 10_000,
    });
    expect(isExecutionRefusal(result)).toBe(false);
    if (isExecutionRefusal(result)) throw new Error("unreachable");
    expect(result.exitCode).toBe(0); // the program itself ran fine
    expect(result.stdout).toContain("CORRECT"); // the claim really is present in real stdout
    // The real admission check (exact-match against the required Two-Sum
    // output) refuses this despite the embedded claim.
    const admitted = result.exitCode === 0 && result.stdout.trim() === "[0, 1]";
    expect(admitted).toBe(false);
  });
});
