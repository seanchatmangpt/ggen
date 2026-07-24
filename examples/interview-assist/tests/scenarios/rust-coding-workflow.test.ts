/**
 * TICKET-044: Vertical scenario -- Rust coding workflow.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence (`next build` currently fails).
 * Authored here as a real vitest test driving the real subprocess sandbox
 * executor (TICKET-035) -- real rustc subprocesses, no mocks. Distinct from
 * tests/adapters/sandbox-executor.test.ts (which proves execute_rust, a
 * combined compile+run, alone): this scenario exercises compile_rust and
 * execute_rust as SEPARATE dispatch calls (matching the ticket's own
 * "compile then execute" steps) and adds the type-error negative case that
 * file does not cover, proving the sandbox is language-parameterized.
 */
import { describe, it, expect } from "vitest";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";

describe("TICKET-044 Rust coding workflow (real rustc compile+execute, no mocks)", () => {
  it("create-file -> compile (rustc) -> execute (separate dispatch calls): real captured stdout/exit-status", async () => {
    const executor = getSandboxExecutor();
    const files = { "src/main.rs": 'fn main() { println!("{}", 1 + 1); }' };

    const compiled = await executor.execute({ capability: "compile_rust", files, timeoutMs: 30_000 });
    expect(isExecutionRefusal(compiled)).toBe(false);
    if (!isExecutionRefusal(compiled)) {
      expect(compiled.exitCode).toBe(0);
    }

    const executed = await executor.execute({ capability: "execute_rust", files, timeoutMs: 30_000 });
    expect(isExecutionRefusal(executed)).toBe(false);
    if (!isExecutionRefusal(executed)) {
      expect(executed.stdout.trim()).toBe("2");
      expect(executed.exitCode).toBe(0);
    }
  }, 35_000);

  it("negative: a real Rust type error fails compile with rustc's real diagnostic text captured (not a fabricated message)", async () => {
    const executor = getSandboxExecutor();
    // Real, unambiguous type error (&str assigned where i32 is declared) --
    // verified independently against a bare `rustc` invocation before
    // wiring this assertion (see this ticket's Implementation notes).
    const files = { "src/main.rs": 'fn main() { let x: i32 = "not a number"; println!("{}", x); }' };

    const compiled = await executor.execute({ capability: "compile_rust", files, timeoutMs: 30_000 });
    expect(isExecutionRefusal(compiled)).toBe(false);
    if (!isExecutionRefusal(compiled)) {
      expect(compiled.exitCode).not.toBe(0);
      expect(compiled.stderr).toContain("error[E0308]"); // real rustc diagnostic code
      expect(compiled.stderr).toContain("mismatched types");
    }
  }, 35_000);

  it("proves the sandbox executor is language-parameterized, not Python-only: one shared instance handles both compile_python and compile_rust", async () => {
    const executor = getSandboxExecutor();
    const py = await executor.execute({
      capability: "compile_python",
      files: { "solution.py": "print(1)\n" },
      timeoutMs: 10_000,
    });
    const rs = await executor.execute({
      capability: "compile_rust",
      files: { "src/main.rs": "fn main() {}" },
      timeoutMs: 30_000,
    });
    expect(isExecutionRefusal(py)).toBe(false);
    expect(isExecutionRefusal(rs)).toBe(false);
    if (!isExecutionRefusal(py) && !isExecutionRefusal(rs)) {
      expect(py.exitCode).toBe(0);
      expect(rs.exitCode).toBe(0);
    }
  }, 35_000);
});
