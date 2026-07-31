/**
 * TICKET-045: Vertical scenario -- Visible tests.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence (`next build` currently fails).
 * Authored here as a real vitest test driving a real pytest subprocess
 * through the sandbox executor's `run_pytest` capability -- no mocks. The
 * positive case's "independent manual run" (this ticket's own acceptance
 * criteria: "matching an independent manual run of the same test") is a
 * SEPARATE, directly-spawned `python3 -m pytest` process outside the
 * executor under test, in its own temp directory -- not a re-read of the
 * executor's own self-reported result.
 */
import { describe, it, expect } from "vitest";
import { execSync } from "node:child_process";
import { mkdtempSync, writeFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";

const VISIBLE_TEST_SOURCE = `def test_add_visible():
    from solution import add
    assert add(2, 3) == 5
`;

describe("TICKET-045 visible tests (real pytest run against a real submission, no mocks)", () => {
  it("a correct real implementation reports pass, matching an independent manual pytest run", async () => {
    const executor = getSandboxExecutor();
    const files = {
      "solution.py": "def add(a, b):\n    return a + b\n",
      "test_visible.py": VISIBLE_TEST_SOURCE,
    };
    const result = await executor.execute({ capability: "run_pytest", files, timeoutMs: 15_000 });
    expect(isExecutionRefusal(result)).toBe(false);
    if (!isExecutionRefusal(result)) {
      expect(result.exitCode).toBe(0);
      expect(result.stdout).toMatch(/1 passed/);
    }

    // Independent manual verification: a fresh, separately-spawned pytest
    // process in its own temp dir, not the executor under test.
    const dir = mkdtempSync(join(tmpdir(), "visible-test-manual-verify-"));
    try {
      writeFileSync(join(dir, "solution.py"), files["solution.py"]);
      writeFileSync(join(dir, "test_visible.py"), files["test_visible.py"]);
      const manual = execSync("python3 -m pytest -q", { cwd: dir }).toString();
      expect(manual).toMatch(/1 passed/);
    } finally {
      rmSync(dir, { recursive: true, force: true });
    }
  });

  it("negative: a deliberately incorrect implementation reports fail with the real assertion diff, not a generic 'test failed' message", async () => {
    const executor = getSandboxExecutor();
    const files = {
      "solution.py": "def add(a, b):\n    return a - b\n", // deliberately wrong: subtracts instead of adds
      "test_visible.py": VISIBLE_TEST_SOURCE,
    };
    const result = await executor.execute({ capability: "run_pytest", files, timeoutMs: 15_000 });
    expect(isExecutionRefusal(result)).toBe(false);
    if (!isExecutionRefusal(result)) {
      expect(result.exitCode).not.toBe(0);
      expect(result.stdout).toMatch(/1 failed/);
      // Real pytest assertion-rewrite diff (add(2,3) -> -1, expected 5),
      // verified against a real bare pytest run before wiring this
      // assertion -- not a generic "failed" string.
      expect(result.stdout).toContain("assert -1 == 5");
    }
  });
});
