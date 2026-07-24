/**
 * TICKET-035: Subprocess sandbox executor (custom) — the most important of
 * the 6 workstream H adapters.
 *
 * Real compile/execute dispatch for Python and Rust behind TICKET-027's
 * generated capability-dispatch.ts slots. TICKET-027 has not generated yet
 * in this session, so `CapabilityId`/`ExecutionRequest`/`ExecutionReceipt`/
 * `ExecutionRefusal` below are copied verbatim (not reinvented) from the
 * already-proven contract at
 * examples/interview-sandbox/lib/executor-contract.ts, and the dispatch
 * body reuses that same file's hardened patterns:
 *   - path.resolve-based workspace-escape prevention (resolveWithinWorkspace)
 *   - output-size capping with process kill (MAX_OUTPUT_BYTES)
 *   - process-group (detached + negative-PID SIGKILL) full-tree cleanup
 *
 * Per TICKET-035's falsifier: the real action (spawning a subprocess) must
 * never happen before the policy/precondition check. `execute()` calls
 * `checkPolicy` first and returns a refusal without spawning anything if
 * denied.
 */
import { spawn } from "node:child_process";
import { mkdtemp, writeFile, mkdir, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, dirname, resolve, sep } from "node:path";
import { checkPolicy, DEFAULT_ACTIVE_MODE, type PolicyId } from "./policy-check-adapter";
import { emitReceipt } from "../domain/receipt-emitter";
import type { TransitionReceipt } from "../domain/receipt";

export type CapabilityId =
  | "compile_python"
  | "execute_python"
  | "run_pytest"
  | "compile_rust"
  | "execute_rust"
  | "run_cargo_test";

/** The two run_* capabilities are the real <manufacturing-chain/test-result>
 * step (60-provenance-receipts.ttl); every other capability is the real
 * <manufacturing-chain/sandbox-execution-activity> step. Fixed by the
 * ontology, not invented here. */
const TEST_RUN_CAPABILITIES: ReadonlySet<CapabilityId> = new Set<CapabilityId>([
  "run_pytest",
  "run_cargo_test",
]);

export interface ExecutionRequest {
  capability: CapabilityId;
  files: Record<string, string>;
  timeoutMs: number;
  /** Optional: which operating-mode policy set governs this call. Defaults
   * to `policy-check-adapter`'s `DEFAULT_ACTIVE_MODE` (`policy/practice-mode`)
   * when omitted -- see that module's doc for why. */
  activeMode?: PolicyId;
  /** TICKET-055: the prior receipt in this session's chain, if any --
   * threaded through so this step's emitted receipt correctly chains
   * (derivedFrom/relation) off the previous manufacturing-chain step. */
  prevReceipt?: TransitionReceipt;
}

export interface ExecutionReceipt {
  capability: CapabilityId;
  exitCode: number;
  stdout: string;
  stderr: string;
  durationMs: number;
  /** TICKET-055: the real TransitionReceipt emitted for this real
   * execution (manufacturing-chain step "sandbox-execution" or
   * "test-result", depending on `capability`). Always present on a real
   * ExecutionReceipt -- covers BOTH success (exitCode 0) and real failure
   * (non-zero exitCode, e.g. a syntax error) paths, since both are real
   * actions that actually occurred. Optional only because
   * ExecutionRefusal (a real action that never ran) never carries one. */
  transitionReceipt?: TransitionReceipt;
}

export type ExecutionRefusal =
  | { kind: "no_source_provided" }
  | { kind: "timeout" }
  | { kind: "payload_too_large"; maxBytes: number }
  | { kind: "policy_denied"; reason: string }
  | { kind: "executor_unavailable"; reason: string };

export interface Executor {
  execute(request: ExecutionRequest): Promise<ExecutionReceipt | ExecutionRefusal>;
}

export function isExecutionRefusal(result: ExecutionReceipt | ExecutionRefusal): result is ExecutionRefusal {
  return "kind" in result;
}

interface SpawnResult {
  exitCode: number;
  stdout: string;
  stderr: string;
}

const MAX_OUTPUT_BYTES = 1_000_000;

function runCommand(cmd: string, args: string[], cwd: string, timeoutMs: number): Promise<SpawnResult> {
  return new Promise<SpawnResult>((resolvePromise) => {
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
      const suffix = timedOut ? "\n[timed out]" : "";
      resolvePromise({ exitCode: timedOut || outputExceeded ? -1 : (code ?? -1), stdout, stderr: stderr + suffix });
    });
  });
}

class WorkspaceEscapeError extends Error {
  constructor(path: string) {
    super(`file path escapes the sandbox workspace: ${path}`);
  }
}

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
  const workspace = await mkdtemp(join(tmpdir(), "interview-assist-sandbox-"));
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
    // Policy/precondition check happens BEFORE any real action -- see
    // TICKET-035's falsifier. Routed through the real RDF-driven policy
    // check (policy-check-adapter -> lib/domain/policy-check.ts) as of the
    // TICKET-028 wiring closure; no longer the default-allow stub.
    const decision = checkPolicy(request.capability, request.activeMode ?? DEFAULT_ACTIVE_MODE);
    if (!decision.allowed) {
      return { kind: "policy_denied", reason: decision.reason ?? "denied" };
    }

    if (Object.keys(request.files).length === 0) return { kind: "no_source_provided" };

    const outcome = await this.runCapability(request);
    if (isExecutionRefusal(outcome)) return outcome;

    // TICKET-055: real receipt emission, unconditionally, for every real
    // action that actually ran -- success (exitCode 0) AND real failure
    // (non-zero exitCode, e.g. a Python syntax error) alike. Refusals
    // above (policy_denied / no_source_provided) never reach here because
    // no manufacturing-chain step was actually traversed for them (this
    // ticket's own Exclusions clause: "no receipt data fabricated -- every
    // emitted receipt must reflect a REAL action that actually occurred").
    const step = TEST_RUN_CAPABILITIES.has(request.capability) ? "test-result" : "sandbox-execution";
    const transitionReceipt = emitReceipt(step, {
      used: Object.keys(request.files),
      label: `${step}: ${request.capability}`,
      generated: `exitCode=${outcome.exitCode}`,
      timestamp: Date.now(),
      prevReceipt: request.prevReceipt,
    });
    return { ...outcome, transitionReceipt };
  }

  private async runCapability(request: ExecutionRequest): Promise<ExecutionReceipt | ExecutionRefusal> {
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
export function getSandboxExecutor(): Executor {
  instance ??= new SubprocessExecutor();
  return instance;
}

/**
 * Reduction path: if the sandbox migrates to a WASM-based execution model,
 * more of this isolation logic (workspace scoping, output capping) could
 * become declarative sandbox-policy data consumed by a shared runner rather
 * than imperative OS calls duplicated per-language; process-tree
 * termination itself stays irreducibly OS-level either way.
 */
export const REDUCTION_PATH_NOTE =
  "workspace-escape + output-cap + process-group-kill are irreducible OS " +
  "interactions today; only the per-language command tables are candidates " +
  "for future declarative-data reduction.";
