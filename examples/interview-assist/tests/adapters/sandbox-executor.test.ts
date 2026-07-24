import { describe, it, expect } from "vitest";
import { execSync } from "node:child_process";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";

describe("sandbox-executor (real subprocess, no mocks)", () => {
  it("executes a real python3 subprocess and captures exact stdout", async () => {
    const executor = getSandboxExecutor();
    const result = await executor.execute({
      capability: "execute_python",
      files: { "solution.py": "print(1+1)" },
      timeoutMs: 10_000,
    });
    expect(isExecutionRefusal(result)).toBe(false);
    if (!isExecutionRefusal(result)) {
      expect(result.stdout.trim()).toBe("2");
      expect(result.exitCode).toBe(0);
    }
  });

  it("compiles and runs a real rustc program", async () => {
    const executor = getSandboxExecutor();
    const result = await executor.execute({
      capability: "execute_rust",
      files: { "src/main.rs": 'fn main() { println!("{}", 1 + 1); }' },
      timeoutMs: 30_000,
    });
    expect(isExecutionRefusal(result)).toBe(false);
    if (!isExecutionRefusal(result)) {
      expect(result.stdout.trim()).toBe("2");
      expect(result.exitCode).toBe(0);
    }
  }, 35_000);

  it("kills an infinite-loop python program within the timeout bound and leaves no orphan process", async () => {
    const executor = getSandboxExecutor();
    const marker = `interview_assist_orphan_probe_${Date.now()}`;
    const start = Date.now();
    const result = await executor.execute({
      capability: "execute_python",
      files: {
        "solution.py": `import time\n# ${marker}\nwhile True:\n    time.sleep(0.05)\n`,
      },
      timeoutMs: 1_500,
    });
    const elapsedMs = Date.now() - start;

    // Killed within bound: wall clock should not run away past the
    // timeout by more than a generous scheduling margin.
    expect(elapsedMs).toBeLessThan(5_000);
    expect(isExecutionRefusal(result)).toBe(false);
    if (!isExecutionRefusal(result)) {
      expect(result.exitCode).toBe(-1);
      expect(result.stderr).toContain("timed out");
    }

    // Real orphan-process check: grep the live process table (not a
    // simulated collaborator) for the marker string that would only appear
    // in `ps` output if the interpreter (or a child of it) were still
    // alive after SIGKILL of its process group.
    await new Promise((r) => setTimeout(r, 300)); // let the OS reap the killed group
    const psOutput = execSync("ps ax -o command=").toString();
    expect(psOutput.includes(marker)).toBe(false);
  }, 10_000);

  it("refuses without spawning when no source files are provided", async () => {
    const executor = getSandboxExecutor();
    const result = await executor.execute({ capability: "execute_python", files: {}, timeoutMs: 5_000 });
    expect(isExecutionRefusal(result)).toBe(true);
    if (isExecutionRefusal(result)) {
      expect(result.kind).toBe("no_source_provided");
    }
  });

  it("refuses a workspace-escape attempt (path traversal) without executing it", async () => {
    const executor = getSandboxExecutor();
    const result = await executor.execute({
      capability: "execute_python",
      files: { "../../etc/escape.py": "print('escaped')" },
      timeoutMs: 5_000,
    });
    expect(isExecutionRefusal(result)).toBe(true);
    if (isExecutionRefusal(result)) {
      expect(result.kind).toBe("executor_unavailable");
      expect((result as { reason: string }).reason).toContain("escapes the sandbox workspace");
    }
  });
});
