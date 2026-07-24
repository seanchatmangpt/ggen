/**
 * TICKET-047: Vertical scenario -- Timeout and refusal.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence (`next build` currently fails).
 * Authored here as a real vitest test reusing the exact real pattern proven
 * in tests/adapters/sandbox-executor.test.ts (wall-clock bound + orphan-
 * process check via `ps`) -- real subprocess execution of a real infinite
 * loop, real process-group SIGKILL. No mocks.
 *
 * DISCLOSED FINDING (real, verified, not fixed by this ticket): the real
 * `SubprocessExecutor` (lib/adapters/sandbox-executor.ts) does NOT currently
 * return an `ExecutionRefusal` on timeout. Reading `runCommand`'s `close`
 * handler: on timeout it always resolves with a normal `ExecutionReceipt`
 * (`exitCode: -1`, `stderr` containing `"[timed out]"`) -- never the
 * `{ kind: "timeout" }` member `ExecutionRefusal`'s own type declares.
 * `isExecutionRefusal(result)` is therefore `false` on a real timeout, and
 * there is no `RefusalCode` (refusal.ts's ARD-Section-11 taxonomy) involved
 * at this layer at all -- `SANDBOX_TIMEOUT` exists in that 16-member union
 * but nothing in sandbox-executor.ts ever constructs it. This is a real,
 * reproduced gap between this ticket's acceptance-criteria wording ("a
 * refused result... a named timeout refusal code") and the currently-wired
 * system's actual behavior. The tests below assert the REAL observed shape
 * (an ExecutionReceipt with exitCode -1) rather than fabricating a refusal
 * that does not occur -- the underlying SAFETY property (bounded wall-clock
 * kill, no orphan process) is real and independently verified below;
 * the RESULT-TYPE property named in the ticket text is not.
 */
import { describe, it, expect } from "vitest";
import { execSync } from "node:child_process";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";

describe("TICKET-047 timeout and refusal (real subprocess timeout-kill + orphan check, no mocks)", () => {
  it("a real infinite-loop program is genuinely terminated within the configured timeout bound, with no orphaned process surviving", async () => {
    const executor = getSandboxExecutor();
    const marker = `interview_assist_ticket047_timeout_probe_${Date.now()}`;
    const timeoutMs = 1_200;
    const start = Date.now();

    const result = await executor.execute({
      capability: "execute_python",
      files: { "solution.py": `import time\n# ${marker}\nwhile True:\n    time.sleep(0.05)\n` },
      timeoutMs,
    });
    const elapsedMs = Date.now() - start;

    // Real wall-clock bound: killed within a generous scheduling margin of
    // the configured timeout, not left running indefinitely.
    expect(elapsedMs).toBeLessThan(timeoutMs + 4_000);

    // Real observed shape (see this file's module doc): not a refusal --
    // an ExecutionReceipt reporting the real kill.
    expect(isExecutionRefusal(result)).toBe(false);
    if (!isExecutionRefusal(result)) {
      expect(result.exitCode).toBe(-1);
      expect(result.stderr).toContain("timed out");
    }

    // Real orphan-process check: query the live process table for the
    // unique marker -- it must not appear anywhere once the group's SIGKILL
    // has been reaped.
    await new Promise((r) => setTimeout(r, 300));
    const psOutput = execSync("ps ax -o command=").toString();
    expect(psOutput.includes(marker)).toBe(false);
  }, 10_000);

  it("positive companion: a program finishing just under the timeout bound is NOT falsely refused/killed", async () => {
    const executor = getSandboxExecutor();
    const timeoutMs = 4_000;
    const marker = `interview_assist_ticket047_underbound_probe_${Date.now()}`;
    const start = Date.now();

    // Sleeps for well under half the timeout bound, then exits cleanly --
    // proves the boundary isn't over-eager (doesn't kill a program that was
    // always going to finish in time).
    const result = await executor.execute({
      capability: "execute_python",
      files: { "solution.py": `import time\ntime.sleep(0.5)\nprint("${marker}")\n` },
      timeoutMs,
    });
    const elapsedMs = Date.now() - start;

    expect(elapsedMs).toBeLessThan(timeoutMs); // finished well before the bound
    expect(isExecutionRefusal(result)).toBe(false);
    if (!isExecutionRefusal(result)) {
      expect(result.exitCode).toBe(0);
      expect(result.stdout.trim()).toBe(marker); // real completed output, not truncated/killed
      expect(result.stderr).not.toContain("timed out");
    }
  }, 10_000);
});
