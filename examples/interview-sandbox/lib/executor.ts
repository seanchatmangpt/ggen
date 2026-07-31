/**
 * Real sandbox executor -- hand-written, implements the generated
 * `Executor` contract from `lib/executor-contract.ts`. This is the one
 * piece of actual subprocess/execution code in the project; everything
 * else under `lib/`/`app/api/` is ggen-generated catalog/dispatch surface.
 *
 * Deliberately minimal (Phase 0 bar): real subprocess isolation via a
 * scratch temp directory + wall-clock timeout + output cap + process-group
 * kill, no container/microVM.
 *
 * KNOWN, DISCLOSED, UNFIXED RISKS (not silently absent -- read this before
 * deploying anywhere beyond a single-operator localhost session):
 *   - NO NETWORK ISOLATION. A submitted Python/Rust program can make
 *     arbitrary outbound network calls (e.g. `urllib.request.urlopen`,
 *     `std::net::TcpStream`). Fixing this properly requires a network
 *     namespace or firewall rule, not something this file attempts.
 *   - NO FILESYSTEM ISOLATION beyond the write-side workspace-escape check
 *     below. A submitted program still runs as the same OS user as this
 *     Next.js process and can `open()` any path that user can read,
 *     including SSH keys / credentials in the operator's home directory.
 *     Fixing this properly requires a chroot/container/seccomp profile.
 *   - NO PER-CLIENT AUTH OR PERSISTENT RATE LIMITING beyond the in-memory
 *     limiter in the API route.
 * Do not expose this beyond localhost / a trusted single operator until
 * real OS-level sandboxing (container, gVisor, microVM) replaces this.
 */
import { spawn } from "node:child_process";
import { mkdtemp, writeFile, mkdir, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, dirname, resolve, sep } from "node:path";
import type { CapabilityId } from "./capabilities";
import type { Executor, ExecutionRequest, ExecutionReceipt, ExecutionRefusal } from "./executor-contract";

interface SpawnResult {
  exitCode: number;
  stdout: string;
  stderr: string;
}

/** Hard cap on accumulated stdout+stderr per command -- prevents an
 * unbounded submission (e.g. `print("x" * 10**9)` in a loop) from growing
 * the Node process's memory without limit before the wall-clock timeout
 * has a chance to fire. */
const MAX_OUTPUT_BYTES = 1_000_000;

function runCommand(cmd: string, args: string[], cwd: string, timeoutMs: number): Promise<SpawnResult> {
  return new Promise<SpawnResult>((resolvePromise) => {
    // detached:true puts the child in its own process group so the
    // timeout path below can kill the whole tree (e.g. `cargo test`'s
    // rustc/test-binary children), not just the directly spawned PID.
    const child = spawn(cmd, args, { cwd, detached: true });
    let stdout = "";
    let stderr = "";
    let timedOut = false;
    let outputExceeded = false;
    let settled = false;

    function killGroup(): void {
      if (typeof child.pid === "number") {
        try {
          process.kill(-child.pid, "SIGKILL");
        } catch {
          child.kill("SIGKILL");
        }
      } else {
        child.kill("SIGKILL");
      }
    }

    const timer = setTimeout(() => {
      timedOut = true;
      killGroup();
    }, timeoutMs);

    function appendCapped(current: string, chunk: Buffer): string {
      if (outputExceeded) return current;
      const next = current + chunk.toString();
      if (next.length > MAX_OUTPUT_BYTES) {
        outputExceeded = true;
        killGroup();
        return next.slice(0, MAX_OUTPUT_BYTES) + "\n[output truncated -- exceeded 1,000,000 bytes]";
      }
      return next;
    }

    child.stdout.on("data", (chunk: Buffer) => (stdout = appendCapped(stdout, chunk)));
    child.stderr.on("data", (chunk: Buffer) => (stderr = appendCapped(stderr, chunk)));
    child.on("error", (err) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      resolvePromise({ exitCode: -1, stdout, stderr: stderr + `\n${err.message}` });
    });
    child.on("close", (code) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      const suffix = timedOut ? "\n[timed out]" : outputExceeded ? "" : "";
      resolvePromise({ exitCode: timedOut || outputExceeded ? -1 : (code ?? -1), stdout, stderr: stderr + suffix });
    });
  });
}

class WorkspaceEscapeError extends Error {
  constructor(path: string) {
    super(`file path escapes the sandbox workspace: ${path}`);
  }
}

/** Resolves `path` under `workspace` and refuses anything that would
 * escape it (`..` traversal, or an absolute path pointing elsewhere) --
 * `path.join` alone does NOT protect against `..` segments. */
function resolveWithinWorkspace(workspace: string, path: string): string {
  const full = resolve(workspace, path);
  if (full !== workspace && !full.startsWith(workspace + sep)) {
    throw new WorkspaceEscapeError(path);
  }
  return full;
}

async function writeFiles(workspace: string, files: Record<string, string>): Promise<void> {
  for (const [path, content] of Object.entries(files)) {
    const full = resolveWithinWorkspace(workspace, path);
    await mkdir(dirname(full), { recursive: true });
    await writeFile(full, content, "utf8");
  }
}

function firstFile(files: Record<string, string>, fallback: string): string {
  return Object.keys(files)[0] ?? fallback;
}

async function runInWorkspace(
  capability: CapabilityId,
  files: Record<string, string>,
  run: (workspace: string) => Promise<SpawnResult>,
): Promise<ExecutionReceipt> {
  const workspace = await mkdtemp(join(tmpdir(), "sandbox-"));
  const start = Date.now();
  try {
    await writeFiles(workspace, files);
    const { exitCode, stdout, stderr } = await run(workspace);
    return { capability, exitCode, stdout, stderr, durationMs: Date.now() - start };
  } finally {
    await rm(workspace, { recursive: true, force: true }).catch(() => undefined);
  }
}

class SubprocessExecutor implements Executor {
  async execute(request: ExecutionRequest): Promise<ExecutionReceipt | ExecutionRefusal> {
    if (Object.keys(request.files).length === 0) return { kind: "no_source_provided" };

    try {
      switch (request.capability) {
        case "compile_python": {
          const file = firstFile(request.files, "solution.py");
          return await runInWorkspace(request.capability, request.files, (ws) =>
            runCommand("python3", ["-m", "py_compile", file], ws, request.timeoutMs),
          );
        }
        case "execute_python": {
          const file = firstFile(request.files, "solution.py");
          return await runInWorkspace(request.capability, request.files, (ws) =>
            runCommand("python3", [file], ws, request.timeoutMs),
          );
        }
        case "run_pytest": {
          return await runInWorkspace(request.capability, request.files, (ws) =>
            runCommand("python3", ["-m", "pytest", "-q"], ws, request.timeoutMs),
          );
        }
        case "compile_rust": {
          const file = firstFile(request.files, "src/main.rs");
          return await runInWorkspace(request.capability, request.files, (ws) =>
            runCommand("rustc", [file, "-o", join(ws, "a.out")], ws, request.timeoutMs),
          );
        }
        case "execute_rust": {
          const file = firstFile(request.files, "src/main.rs");
          return await runInWorkspace(request.capability, request.files, async (ws) => {
            const compiled = await runCommand("rustc", [file, "-o", join(ws, "a.out")], ws, request.timeoutMs);
            if (compiled.exitCode !== 0) return compiled;
            return runCommand(join(ws, "a.out"), [], ws, request.timeoutMs);
          });
        }
        case "run_cargo_test": {
          return await runInWorkspace(request.capability, request.files, (ws) =>
            runCommand("cargo", ["test"], ws, request.timeoutMs),
          );
        }
        default:
          return { kind: "executor_unavailable", reason: `no executor wired for capability ${request.capability}` };
      }
    } catch (err) {
      if (err instanceof WorkspaceEscapeError) {
        return { kind: "executor_unavailable", reason: err.message };
      }
      return { kind: "executor_unavailable", reason: err instanceof Error ? err.message : String(err) };
    }
  }
}

let instance: Executor | undefined;
export function getExecutor(): Executor {
  instance ??= new SubprocessExecutor();
  return instance;
}
