"use client";

/**
 * First real InterviewAssist session page. Not tied to a single numbered
 * ticket (workstreams C/G/H each produced pieces this composes) -- this is
 * the groundwork workstream I's Playwright scenarios need: a real,
 * type-checked, client-rendered page that actually drives
 * lib/domain/reducer.ts's sessionReducer and actually renders workstream
 * G's real components, backed by real server-side execution (via
 * app/api/run) and real BLAKE3 receipt hashing (via app/api/receipt) --
 * not fabricated data.
 *
 * Phase 4 (this pass) is a UI/UX restructuring on top of the already-proven
 * cognition loop (Phases 1-3: real wasm4pm-cognition Eliza breed ->
 * confirmed hypothesis -> real phase advance -> real sandbox execution,
 * exercised end to end by tests/scenarios/cognition-first-decisive.test.ts).
 * The functional dispatch/fetch logic below is UNCHANGED from that proven
 * loop; only composition/layout/control-grouping changed. See the new
 * hand-authored components this pass adds: session-header.tsx,
 * session-workspace.tsx, session-activity-drawer.tsx, session-menu.tsx,
 * execution-result-card.tsx, accessibility-preferences-dialog.tsx.
 *
 * Keyboard/focus-order (DOM source order, since CSS Grid repositions
 * regions visually without changing tab order -- see session-workspace.tsx):
 *   skip-to-current-task -> session status (SessionHeader) -> cognition
 *   question + track choices (Cognition region) -> problem statement
 *   (Objective region) -> editor + run (Coding region) -> execution result
 *   + visible tests (Result region) -> session actions (SessionMenu).
 * "Authorized guidance" from the redesign brief's diagram has no backing
 * feature anywhere in this codebase yet (grepped; nothing named
 * guidance/hint exists) -- rather than fabricate a panel with invented
 * content, that step is simply not represented here; disclosed honestly
 * rather than papered over per this repo's Evidence-First principle.
 */
import { useEffect, useState } from "react";
import { SessionHeader, type InputStatus } from "../components/session-header";
import { SessionWorkspace } from "../components/session-workspace";
import { SessionActivityDrawer } from "../components/session-activity-drawer";
import { SessionMenu } from "../components/session-menu";
import { ExecutionResultCard } from "../components/execution-result-card";
import { AccessibilityPreferencesDialog } from "../components/accessibility-preferences-dialog";
import { ProblemPanel } from "../components/problem-panel";
import { TrackCandidatePanel } from "../components/track-candidate-panel";
import { CognitionPanel } from "../components/cognition-panel";
import { EditorShell, type EditorShellProps } from "../components/editor-shell";
import { DiagnosticsPanel, type Diagnostic } from "../components/diagnostics-panel";
import { ConsolePanel } from "../components/console-panel";
import { RefusalPresentation } from "../components/refusal-presentation";
import { SessionSummary } from "../components/session-summary";
import { Spinner } from "../components/spinner";
import { RequestErrorNotice } from "../components/request-error-notice";
import { fetchWithTimeout, describeFetchError } from "../lib/client/fetch-with-timeout";

import { sessionReducer, type SessionState, type SessionEvent } from "../lib/domain/reducer";
import type { Phase } from "../lib/domain/phase";
import { PHASE_TRANSITIONS } from "../lib/domain/phase-transitions";
import type { ProblemState } from "../lib/domain/problem-state";
import type { TrackCandidate } from "../lib/domain/track-candidate";
import type { VerificationState } from "../lib/domain/verification-state";
import type { RefusalCode } from "../lib/domain/refusal";
import type { TransitionReceipt } from "../lib/domain/receipt";
import { ACCESSIBILITY_DEFAULTS, type AccessibilityDefaults } from "../lib/accessibility/defaults";
import { DEFAULT_ACTIVE_MODE } from "../lib/adapters/policy-check-adapter";
// Type-only: erased at compile time, so importing sandbox-executor.ts's
// exported type here does not pull node:child_process into the client
// bundle (the real executor call happens server-side, via app/api/run).
import type { CapabilityId } from "../lib/adapters/sandbox-executor";
// Type-only, same discipline: cognition-adapter.ts is a server-only module
// (wraps a wasm-bindgen Node require()) reachable ONLY via app/api/cognition
// -- this page never imports runCognition itself, only the erased
// CognitionOutcome type describing what that route returns as JSON.
import type { CognitionOutcome } from "../lib/adapters/cognition-adapter";

interface AppState extends SessionState {
  phase: Phase;
  problem: ProblemState;
  trackCandidates: TrackCandidate[];
  verification: VerificationState;
  accessibility: AccessibilityDefaults;
  diagnostics: Diagnostic[];
  stdout: string;
  stderr: string;
  exitCode?: number;
  code: string;
  usedEvents: string[];
  refusal?: { code: RefusalCode; reason?: string };
  receipt?: TransitionReceipt;
  /** JTBD 5 closure: real outcome of the most recent "Run visible tests" /
   * "Run hidden tests" click (real run_pytest via /api/test -> real
   * sandbox-executor.ts, no mocks). Undefined until the corresponding
   * button has actually been clicked once. */
  visibleTest?: { exitCode: number; stdout: string; stderr: string };
  hiddenTest?: { exitCode: number; stdout: string; stderr: string };
}

const INITIAL_STATE: AppState = {
  phase: "CREATED",
  problem: {},
  trackCandidates: [],
  verification: {},
  accessibility: ACCESSIBILITY_DEFAULTS,
  diagnostics: [],
  stdout: "",
  stderr: "",
  code: "print('hello, interview-assist')\n",
  usedEvents: [],
};

/** UX-polish pass (timeout/retry): client-side fetchWithTimeout budgets,
 * each set comfortably above the corresponding real SERVER-side timeout
 * (sandbox-executor.ts's `ExecutionRequest.timeoutMs`, passed explicitly in
 * each request body below) so the server's own real timeout/refusal path
 * fires first under normal conditions -- these are a backstop against the
 * request never coming back at all (e.g. the dev server itself hanging),
 * not a tighter budget that would race a legitimate slow-but-successful
 * run. /api/cognition has no server-side timeoutMs (a WASM call, not a
 * subprocess), so its budget is a flat, generous default. */
const COGNITION_TIMEOUT_MS = 15_000;
const RUN_TIMEOUT_MS = 20_000; // server-side timeoutMs below is 10_000
const TEST_TIMEOUT_MS = 25_000; // server-side timeoutMs below is 15_000
const RECEIPT_TIMEOUT_MS = 10_000;

export default function InterviewAssistPage() {
  const [state, setState] = useState<AppState>(INITIAL_STATE);
  const [running, setRunning] = useState(false);
  const [runError, setRunError] = useState<string | null>(null);
  const [runningTest, setRunningTest] = useState<"visible" | "hidden" | null>(null);
  const [testError, setTestError] = useState<{ kind: "visible" | "hidden"; message: string } | null>(null);
  const [cognitionInputValue, setCognitionInputValue] = useState("");
  const [cognitionIntent, setCognitionIntent] = useState<string | null>(null);
  const [cognitionOutcome, setCognitionOutcome] = useState<CognitionOutcome | null>(null);
  const [cognitionSubmitting, setCognitionSubmitting] = useState(false);
  const [cognitionError, setCognitionError] = useState<string | null>(null);
  const [receipts, setReceipts] = useState<TransitionReceipt[]>([]);
  const [accessibilityDialogOpen, setAccessibilityDialogOpen] = useState(false);
  const [debug, setDebug] = useState(false);
  const [finishing, setFinishing] = useState(false);
  const [finishError, setFinishError] = useState<string | null>(null);
  const [testTimeoutOverrideMs, setTestTimeoutOverrideMs] = useState<number | null>(null);

  // `?debug=1` gate for SessionActivityDrawer's developer-diagnostics
  // section (session-activity-drawer.tsx). Read from window.location on
  // mount rather than next/navigation's useSearchParams(), which requires
  // wrapping the reading component in a <Suspense> boundary to avoid a
  // build-time "should be wrapped in a suspense boundary" error/de-opt --
  // this page is already entirely client-rendered ("use client" at the
  // top), so a plain post-mount effect is simpler and has no SSR/hydration
  // mismatch risk (both server and first client render see debug=false;
  // it only flips true after mount, same pattern as any client-only
  // browser-API read).
  //
  // `?testTimeoutMs=N` (UX-polish pass, test-only, same discipline as
  // `?debug=1` above): overrides every real `fetchWithTimeout` budget below
  // with a tiny value so a Playwright test can deterministically force the
  // real client-side timeout/retry path (tests/e2e/jtbd-13-loading-states.spec.ts's
  // sibling timeout test) without shrinking the real production defaults
  // (COGNITION_TIMEOUT_MS etc.) themselves. Absent (the real app), this is
  // `null` and every flow uses its own real, generous default.
  useEffect(() => {
    const params = new URLSearchParams(window.location.search);
    setDebug(params.get("debug") === "1");
    const rawTimeout = params.get("testTimeoutMs");
    const parsedTimeout = rawTimeout !== null ? Number(rawTimeout) : NaN;
    setTestTimeoutOverrideMs(Number.isFinite(parsedTimeout) && parsedTimeout > 0 ? parsedTimeout : null);
  }, []);

  /** JTBD 6 closure: projects three of the 16 real AccessibilityDefaults
   * keys onto `document.documentElement.dataset`, which app/globals.css
   * reads via `:root[data-*="true"]` selectors to produce real, computed-
   * style-verifiable effects (see that file's own comment for exactly
   * which three and why). `state.accessibility` -- not the DOM attribute --
   * remains the single source of truth; this effect is a one-way
   * projection of it, run after every change so the dataset never drifts
   * from the lifted React state (including on first mount, where it sets
   * the real "false" defaults rather than relying on the attribute being
   * absent). The other 13 keys are real typed state with no downstream
   * visual effect yet -- not touched here, not silently claimed. */
  useEffect(() => {
    const root = document.documentElement;
    root.dataset.reducedMotion = String(state.accessibility["reduced-motion-mode"]);
    root.dataset.highContrast = String(state.accessibility["high-contrast-projection"]);
    root.dataset.compactDensity = String(state.accessibility["configurable-information-density"]);
  }, [state.accessibility]);

  /** Dispatches through the real sessionReducer. `sessionReducer`'s own
   * signature types its return as AdmissionResult<SessionState> (not
   * generic over AppState) even though at runtime it only ever spreads or
   * passes through whatever state object it was given -- the `as AppState`
   * cast below reflects that real, verified behavior (see reducer.ts),
   * not an assumption. */
  function dispatch(event: SessionEvent, label: string): void {
    const result = sessionReducer(state, event);
    if (result.status === "refused") {
      setState((prev) => ({ ...prev, refusal: { code: result.code, reason: result.reason } }));
      return;
    }
    setState((prev) => ({
      ...(result.value as AppState),
      refusal: undefined,
      usedEvents: [...prev.usedEvents, label],
    }));
  }

  const nextPhases = PHASE_TRANSITIONS[state.phase] ?? [];

  function advanceTo(target: Phase): void {
    dispatch({ family: "WorkflowEvent", targetPhase: target }, `WorkflowEvent:${state.phase}->${target}`);
  }

  function refuseSession(): void {
    dispatch({ family: "SessionEvent", targetPhase: "REFUSED" }, `SessionEvent:${state.phase}->REFUSED`);
  }

  /** Deliberate demo affordance: dispatches an event.family the reducer's
   * real KNOWN_EVENT_FAMILIES set does not admit, so this always exercises
   * the real "refused" branch (RefusalCode STALE_SESSION_EVENT) regardless
   * of current phase -- for manual/Playwright verification that
   * RefusalPresentation renders a real refused AdmissionResult, not a
   * fabricated one. Relocated (Phase 4) into SessionActivityDrawer's
   * `?debug=1`-gated developer-diagnostics section -- it is a demo/test
   * affordance, not part of the real candidate-facing flow. */
  function triggerAdmissionRefusalDemo(): void {
    dispatch({ family: "NotAnAdmittedEventFamily" }, "demo:unrecognized-family");
  }

  function onEditorAction(key: string): void {
    setState((prev) => ({
      ...prev,
      diagnostics: [...prev.diagnostics, { message: `editor action: ${key}`, severity: "info" }],
    }));
    dispatch({ family: "EditorEvent", type: key }, `EditorEvent:${key}`);
  }

  const editorProps: EditorShellProps = {
    "editor/apply-deterministic-refactor": () => onEditorAction("editor/apply-deterministic-refactor"),
    "editor/create-file": () => onEditorAction("editor/create-file"),
    "editor/delete-file": () => onEditorAction("editor/delete-file"),
    "editor/display-diagnostics": () => onEditorAction("editor/display-diagnostics"),
    "editor/display-diff": () => onEditorAction("editor/display-diff"),
    "editor/format-source": () => onEditorAction("editor/format-source"),
    "editor/inspect-definition": () => onEditorAction("editor/inspect-definition"),
    "editor/inspect-references": () => onEditorAction("editor/inspect-references"),
    "editor/modify-file": () => onEditorAction("editor/modify-file"),
    "editor/navigate-symbol": () => onEditorAction("editor/navigate-symbol"),
    "editor/open-file": () => onEditorAction("editor/open-file"),
    "editor/rename-file": () => onEditorAction("editor/rename-file"),
  };

  /** Real end-to-end execution: POSTs to app/api/run, which calls the real
   * subprocess-spawning sandbox-executor.ts server-side (real python3, no
   * mocks). Only dispatches the IMPLEMENTATION->EXECUTION phase event on
   * the first run from IMPLEMENTATION -- re-running while already in
   * EXECUTION is real, but re-requesting the same phase transition would
   * be a genuinely illegal transition per phase-transitions.ts (EXECUTION's
   * only legal target is DEBUGGING), so it is intentionally skipped rather
   * than surfacing a spurious refusal on every re-run.
   *
   * Phase 4 addition: the real ExecutionReceipt's `transitionReceipt` field
   * (already returned by /api/run, previously read but discarded by the
   * client) is now also appended to `receipts` for SessionActivityDrawer's
   * receipt inspector -- no change to what the server computes or returns,
   * only to what the client keeps.
   *
   * UX-polish pass: real busy state (`running`, already existed) now also
   * drives `aria-busy` + a visible spinner on the "Run" button (see JSX
   * below), and the fetch itself is wrapped with a real client-side
   * timeout (fetchWithTimeout) -- if it fires or the network genuinely
   * fails, `runError` is set and a real Retry affordance renders instead
   * of the request hanging silently. */
  async function runCode(): Promise<void> {
    setRunning(true);
    setRunError(null);
    try {
      const capability: CapabilityId = "execute_python";
      const res = await fetchWithTimeout(
        "/api/run",
        {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({ capability, files: { "solution.py": state.code }, timeoutMs: 10_000 }),
        },
        testTimeoutOverrideMs ?? RUN_TIMEOUT_MS,
      );
      const json = (await res.json()) as {
        refusal?: { kind: string; reason?: string };
        receipt?: { stdout: string; stderr: string; exitCode: number; transitionReceipt?: TransitionReceipt };
      };
      if (json.refusal) {
        setState((prev) => ({
          ...prev,
          stdout: "",
          stderr: "",
          diagnostics: [
            ...prev.diagnostics,
            { message: `sandbox refused: ${json.refusal!.kind}${json.refusal!.reason ? ` (${json.refusal!.reason})` : ""}`, severity: "error" },
          ],
        }));
      } else if (json.receipt) {
        const { stdout, stderr, exitCode, transitionReceipt } = json.receipt;
        setState((prev) => ({
          ...prev,
          stdout,
          stderr,
          exitCode,
          verification: { ...prev.verification, "verification/run-example": exitCode === 0 },
          diagnostics:
            exitCode === 0
              ? prev.diagnostics
              : [...prev.diagnostics, { message: `execution exited with code ${exitCode}`, severity: "error" }],
        }));
        if (transitionReceipt) setReceipts((prev) => [...prev, transitionReceipt]);
      }
      if (state.phase === "IMPLEMENTATION") {
        dispatch({ family: "ExecutionEvent", targetPhase: "EXECUTION" }, "ExecutionEvent:run");
      }
    } catch (err) {
      setRunError(describeFetchError(err));
    } finally {
      setRunning(false);
    }
  }

  /** JTBD 5 closure: real end-to-end pytest run, distinct from runCode()
   * above (which only ever dispatches "execute_python"). POSTs the
   * candidate's current code plus a `testKind` to the new app/api/test
   * route, which server-side pairs it with the real two-sum visible/hidden
   * pytest fixture (lib/domain/two-sum-test-fixtures.ts, never imported
   * here) and calls the same real, subprocess-spawning
   * getSandboxExecutor().execute({capability:"run_pytest", ...}) that
   * tests/scenarios/*-tests.test.ts and tests/scenarios/cognition-first-
   * decisive.test.ts already exercise Node-side. Sets the corresponding
   * real VerificationState key ("verification/run-visible-test" /
   * "verification/run-hidden-test") from the real exitCode, exactly the
   * same "never trust cognition output, only real exit codes" rule
   * runCode() already applies to "verification/run-example".
   *
   * UX-polish pass: same real-timeout/retry discipline as runCode() above
   * -- `testError` is scoped to `{kind, message}` since visible/hidden
   * tests share this one function but must surface an independent failure
   * for whichever kind was actually running. */
  async function runTests(testKind: "visible" | "hidden"): Promise<void> {
    setRunningTest(testKind);
    setTestError(null);
    try {
      const res = await fetchWithTimeout(
        "/api/test",
        {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({ testKind, code: state.code, timeoutMs: 15_000, prevReceipt: receipts[receipts.length - 1] }),
        },
        testTimeoutOverrideMs ?? TEST_TIMEOUT_MS,
      );
      const json = (await res.json()) as {
        refusal?: { kind: string; reason?: string };
        receipt?: { stdout: string; stderr: string; exitCode: number; transitionReceipt?: TransitionReceipt };
      };
      if (json.refusal) {
        setState((prev) => ({
          ...prev,
          diagnostics: [
            ...prev.diagnostics,
            {
              message: `${testKind} tests refused: ${json.refusal!.kind}${json.refusal!.reason ? ` (${json.refusal!.reason})` : ""}`,
              severity: "error",
            },
          ],
        }));
      } else if (json.receipt) {
        const { stdout, stderr, exitCode, transitionReceipt } = json.receipt;
        const verificationKey =
          testKind === "visible" ? "verification/run-visible-test" : "verification/run-hidden-test";
        setState((prev) => ({
          ...prev,
          verification: { ...prev.verification, [verificationKey]: exitCode === 0 },
          ...(testKind === "visible" ? { visibleTest: { exitCode, stdout, stderr } } : { hiddenTest: { exitCode, stdout, stderr } }),
        }));
        if (transitionReceipt) setReceipts((prev) => [...prev, transitionReceipt]);
      }
    } catch (err) {
      setTestError({ kind: testKind, message: describeFetchError(err) });
    } finally {
      setRunningTest(null);
    }
  }

  /** Real end-to-end cognition turn: POSTs the observed utterance to
   * app/api/cognition, which calls the real wasm4pm-cognition Eliza breed
   * server-side (no mock, no client-side WASM). Records the utterance as a
   * real SpeechEvent (the family the memory note identifies as already
   * fitting "the observed transcript utterance going IN") regardless of
   * whether the WASM breed matched a track -- the observation itself is a
   * real admitted session event either way. The 422/503 branches
   * (no-track-matched / refused / unavailable) are well-formed, typed
   * outcomes, not client-side errors -- all three status codes parse to the
   * same CognitionOutcome JSON shape.
   *
   * `intentOverride` (UX-polish pass): when supplied, submits that exact
   * text instead of reading+clearing `cognitionInputValue` -- used by
   * CognitionPanel's "unavailable" Retry control to genuinely re-submit
   * the SAME observed utterance rather than requiring the human to retype
   * it, without racing a `setCognitionInputValue` update against the fetch
   * that follows it. The input field's own value is left untouched by an
   * override submission (it was never touched to fill it in the first
   * place).
   *
   * Timeout/retry: wraps the request in `fetchWithTimeout`; a real timeout
   * or network-level failure (NOT a well-formed 4xx/5xx JSON response --
   * those are typed CognitionOutcomes handled above, not exceptions) sets
   * `cognitionError`, rendered with a real Retry affordance instead of the
   * UI hanging silently. */
  async function submitCognitionUtterance(intentOverride?: string): Promise<void> {
    const intent = (intentOverride ?? cognitionInputValue).trim();
    if (intent.length === 0) return;
    setCognitionSubmitting(true);
    setCognitionError(null);
    try {
      const res = await fetchWithTimeout(
        "/api/cognition",
        {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({ intent, prevReceipt: receipts[receipts.length - 1] }),
        },
        testTimeoutOverrideMs ?? COGNITION_TIMEOUT_MS,
      );
      const outcome = (await res.json()) as CognitionOutcome;
      setCognitionIntent(intent);
      setCognitionOutcome(outcome);
      if (intentOverride === undefined) setCognitionInputValue("");
      if ("receipt" in outcome && outcome.receipt) setReceipts((prev) => [...prev, outcome.receipt as TransitionReceipt]);
      dispatch({ family: "SpeechEvent", type: "utterance", intent }, `SpeechEvent:${intent}`);
    } catch (err) {
      setCognitionError(describeFetchError(err));
    } finally {
      setCognitionSubmitting(false);
    }
  }

  /** "Yes": the human confirms the real Eliza-proposed track. Dispatches a
   * real HypothesisEvent carrying the next legal target phase (per
   * phase-transitions.ts's admitted transition-plan edges for the session's
   * CURRENT phase) so the phase advances as a direct consequence of
   * confirming the hypothesis -- no separate "Advance to X" click needed on
   * this path. If the current phase has no outgoing transition (a terminal
   * phase), the HypothesisEvent is still dispatched without a targetPhase --
   * a legitimate admitted event per reducer.ts's own documented semantics
   * (an event that carries no phase transition is still real), rather than
   * silently doing nothing. */
  function confirmCognitionProposal(): void {
    const nextPhase = PHASE_TRANSITIONS[state.phase]?.[0];
    const event: SessionEvent =
      nextPhase !== undefined
        ? { family: "HypothesisEvent", targetPhase: nextPhase }
        : { family: "HypothesisEvent" };
    dispatch(
      event,
      nextPhase !== undefined
        ? `HypothesisEvent:${state.phase}->${nextPhase}`
        : `HypothesisEvent:${state.phase}(no-transition)`,
    );
    setCognitionIntent(null);
    setCognitionOutcome(null);
  }

  /** "No": the human rejects the proposed track. No session event is
   * dispatched -- rejecting a not-yet-admitted hypothesis is not itself a
   * session-level fact worth recording, it just clears local UI state so a
   * different utterance can be submitted. */
  function rejectCognitionProposal(): void {
    setCognitionIntent(null);
    setCognitionOutcome(null);
  }

  /** Real end-to-end receipt: POSTs to app/api/receipt, which calls the
   * real checksum-adapter (real blake3 hash, server-side) over the
   * session's recorded event labels.
   *
   * UX-polish pass: `finishing` drives SessionMenu's real busy state
   * (spinner + aria-busy + disabled on the Finish button, see that
   * component); the request itself is wrapped with a real client-side
   * timeout, and a real failure sets `finishError`, rendered with a Retry
   * affordance rather than the session silently never completing. */
  async function finishSession(): Promise<void> {
    setFinishing(true);
    setFinishError(null);
    try {
      const res = await fetchWithTimeout(
        "/api/receipt",
        {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({ label: "interview-assist-session", used: state.usedEvents }),
        },
        testTimeoutOverrideMs ?? RECEIPT_TIMEOUT_MS,
      );
      const json = (await res.json()) as { receipt: TransitionReceipt };
      setState((prev) => ({ ...prev, receipt: json.receipt }));
      setReceipts((prev) => [...prev, json.receipt]);
    } catch (err) {
      setFinishError(describeFetchError(err));
    } finally {
      setFinishing(false);
    }
  }

  const inputStatus: InputStatus = cognitionSubmitting
    ? "processing"
    : cognitionInputValue.trim().length > 0
      ? "listening"
      : "idle";
  const canRefuse = state.phase !== "REFUSED" && state.phase !== "COMPLETE";

  return (
    <main>
      <h1>InterviewAssist</h1>

      <SessionHeader
        mode={DEFAULT_ACTIVE_MODE}
        phase={state.phase}
        inputStatus={inputStatus}
        onOpenAccessibilityPreferences={() => setAccessibilityDialogOpen(true)}
      />

      {state.refusal && <RefusalPresentation code={state.refusal.code} reason={state.refusal.reason} />}

      <SessionWorkspace
        cognition={
          <>
            <section aria-label="Observed utterance">
              <label htmlFor="cognition-intent-input">Observed utterance</label>
              <input
                id="cognition-intent-input"
                type="text"
                value={cognitionInputValue}
                onChange={(e) => setCognitionInputValue(e.target.value)}
                onKeyDown={(e) => {
                  if (e.key === "Enter") void submitCognitionUtterance();
                }}
                data-testid="cognition-intent-input"
                placeholder="e.g. I have an array of numbers to search through"
              />
              <button
                type="button"
                onClick={() => void submitCognitionUtterance()}
                disabled={cognitionSubmitting || cognitionInputValue.trim().length === 0}
                aria-busy={cognitionSubmitting}
                data-testid="cognition-submit"
              >
                {cognitionSubmitting && <Spinner data-testid="cognition-submit-spinner" />}
                {cognitionSubmitting ? "Submitting..." : "Submit"}
              </button>
              {cognitionError && (
                <RequestErrorNotice
                  message={cognitionError}
                  onRetry={() => void submitCognitionUtterance()}
                  data-testid="cognition-request-error"
                />
              )}
            </section>

            {cognitionIntent !== null && cognitionOutcome !== null && (
              <CognitionPanel
                intent={cognitionIntent}
                outcome={cognitionOutcome}
                onConfirm={confirmCognitionProposal}
                onReject={rejectCognitionProposal}
                onRetryUnavailable={() => void submitCognitionUtterance(cognitionIntent)}
              />
            )}

            <section aria-label="Track candidates">
              <h3 className="region-heading">Track choices</h3>
              <TrackCandidatePanel candidates={state.trackCandidates} />
            </section>
          </>
        }
        objective={<ProblemPanel problem={state.problem} />}
        coding={
          <>
            <section aria-label="Code">
              <textarea
                data-testid="code-editor"
                value={state.code}
                onChange={(e) => setState((prev) => ({ ...prev, code: e.target.value }))}
              />
              <button
                type="button"
                onClick={() => void runCode()}
                disabled={running}
                aria-busy={running}
                data-testid="run-code"
              >
                {running && <Spinner data-testid="run-code-spinner" />}
                {running ? "Running..." : "Run"}
              </button>
              {runError && (
                <RequestErrorNotice message={runError} onRetry={() => void runCode()} data-testid="run-code-error" />
              )}
              <button
                type="button"
                onClick={() => void runTests("visible")}
                disabled={runningTest !== null}
                aria-busy={runningTest === "visible"}
                data-testid="run-visible-tests"
              >
                {runningTest === "visible" && <Spinner data-testid="run-visible-tests-spinner" />}
                {runningTest === "visible" ? "Running visible tests..." : "Run visible tests"}
              </button>
              <button
                type="button"
                onClick={() => void runTests("hidden")}
                disabled={runningTest !== null}
                aria-busy={runningTest === "hidden"}
                data-testid="run-hidden-tests"
              >
                {runningTest === "hidden" && <Spinner data-testid="run-hidden-tests-spinner" />}
                {runningTest === "hidden" ? "Running hidden tests..." : "Run hidden tests"}
              </button>
              {testError && (
                <RequestErrorNotice
                  message={`${testError.kind === "visible" ? "Visible" : "Hidden"} tests: ${testError.message}`}
                  onRetry={() => void runTests(testError.kind)}
                  data-testid="run-tests-error"
                />
              )}
            </section>
            <EditorShell {...editorProps} />
            <DiagnosticsPanel diagnostics={state.diagnostics} />
            <ConsolePanel stdout={state.stdout} stderr={state.stderr} exitCode={state.exitCode} />
          </>
        }
        result={
          <ExecutionResultCard
            exitCode={state.exitCode}
            verification={state.verification}
            receiptCount={receipts.length}
            visibleTest={state.visibleTest}
            hiddenTest={state.hiddenTest}
          />
        }
      />

      <SessionActivityDrawer
        events={state.usedEvents}
        receipts={receipts}
        debug={debug}
        phase={state.phase}
        nextPhases={nextPhases}
        onAdvance={advanceTo}
        onTriggerAdmissionRefusalDemo={triggerAdmissionRefusalDemo}
      />

      <SessionMenu
        canRefuse={canRefuse}
        onRefuse={refuseSession}
        onFinish={() => void finishSession()}
        finishing={finishing}
      />
      {finishError && (
        <RequestErrorNotice
          message={finishError}
          onRetry={() => void finishSession()}
          data-testid="finish-session-error"
        />
      )}
      {state.receipt && <SessionSummary receipt={state.receipt} />}

      <AccessibilityPreferencesDialog
        open={accessibilityDialogOpen}
        settings={state.accessibility}
        onChange={(key, value) =>
          setState((prev) => ({ ...prev, accessibility: { ...prev.accessibility, [key]: value } }))
        }
        onClose={() => setAccessibilityDialogOpen(false)}
      />
    </main>
  );
}
