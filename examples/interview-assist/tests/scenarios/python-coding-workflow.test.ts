/**
 * TICKET-043: Vertical scenario -- Python coding workflow.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence (`next build` currently fails).
 * Authored here as a real vitest test driving the real subprocess sandbox
 * executor (TICKET-035) -- real python3 subprocesses, no mocks. Distinct
 * from tests/adapters/sandbox-executor.test.ts (which proves execute_python
 * alone): this scenario exercises the full create-file -> compile ->
 * execute flow the ticket names, and adds the syntax-error negative case
 * that file does not cover.
 */
import { describe, it, expect } from "vitest";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";

describe("TICKET-043 Python coding workflow (real subprocess compile+execute, no mocks)", () => {
  it("create-file -> compile (py_compile) -> execute: real captured stdout for a known program", async () => {
    const executor = getSandboxExecutor();
    const files = { "solution.py": "print(1 + 1)\n" };

    const compiled = await executor.execute({ capability: "compile_python", files, timeoutMs: 10_000 });
    expect(isExecutionRefusal(compiled)).toBe(false);
    if (!isExecutionRefusal(compiled)) {
      expect(compiled.exitCode).toBe(0); // real py_compile syntax check passed
    }

    const executed = await executor.execute({ capability: "execute_python", files, timeoutMs: 10_000 });
    expect(isExecutionRefusal(executed)).toBe(false);
    if (!isExecutionRefusal(executed)) {
      // Genuine execution, not a stub: matches acceptance criteria's
      // 'print(1+1)' -> exactly '2'.
      expect(executed.stdout.trim()).toBe("2");
      expect(executed.exitCode).toBe(0);
    }
  });

  it("negative: real Python syntax error fails compile with a real, specific diagnostic (not a generic failure)", async () => {
    const executor = getSandboxExecutor();
    // Real, unambiguous syntax error (unbalanced parens on a `def` line) --
    // verified independently against a bare `python3 -m py_compile` before
    // wiring this assertion (see this ticket's Implementation notes).
    const files = { "solution.py": "def broken(:\n    print('unbalanced'\n" };

    const compiled = await executor.execute({ capability: "compile_python", files, timeoutMs: 10_000 });
    expect(isExecutionRefusal(compiled)).toBe(false);
    if (!isExecutionRefusal(compiled)) {
      expect(compiled.exitCode).not.toBe(0);
      expect(compiled.stderr).toContain("SyntaxError"); // real python3 diagnostic text
      expect(compiled.stderr).toContain("solution.py");
    }
  });
});
