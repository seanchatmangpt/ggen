/**
 * Hand-authored (Phase 4 -- UI/UX redesign; no ontology resource backs this
 * component). Replaces the prior scattered presentation (a raw exit-code
 * paragraph, a separate diagnostics message, and an unrelated test-result
 * list) with "one coherent execution-result card" per the redesign brief.
 *
 * Composes the existing GENERATED TestResultView (TICKET-032) unchanged --
 * this card adds only a pass/fail status summary above it, computed from
 * the same `exitCode` app/page.tsx's real runCode() already receives from
 * /api/run. ConsolePanel (raw stdout/stderr) intentionally stays in the
 * Coding region per the redesign brief's own instruction that
 * EditorShell/DiagnosticsPanel/ConsolePanel remain grouped there,
 * untouched -- this card is the Evidence-region half of execution
 * reporting, not a replacement for the console.
 *
 * JTBD 5 closure addition (this pass): `visibleTest`/`hiddenTest` render
 * the real pytest outcome app/page.tsx's new runTests() receives from
 * /api/test (real run_pytest via sandbox-executor.ts, no mocks). Both
 * props are optional and additive -- no existing prop/behavior changed,
 * per this task's "extend, do not replace" instruction.
 */
import { TestResultView } from "./test-result-view";
import type { VerificationState } from "../lib/domain/verification-state";

export interface PytestRunOutcome {
  exitCode: number;
  stdout: string;
  stderr: string;
}

export interface ExecutionResultCardProps {
  exitCode?: number;
  verification: VerificationState;
  /** Count of real manufacturing-chain receipts recorded so far this
   * session (admission / cognition-run / sandbox-execution / test-result /
   * accessibility-projection -- TICKET-055). A summary only: the full,
   * per-step receipt inspector lives in SessionActivityDrawer, not here --
   * this is the redesign brief's "session evidence" line for the Evidence
   * region, not a duplicate of the drawer's inspector. */
  receiptCount: number;
  /** Real outcome of the most recent "Run visible tests" click (real
   * run_pytest against the two-sum visible fixture), if any has happened
   * yet this session. */
  visibleTest?: PytestRunOutcome;
  /** Real outcome of the most recent "Run hidden tests" click (real
   * run_pytest against the two-sum hidden fixture), if any has happened
   * yet this session. Rendered honestly, not suppressed -- see
   * lib/domain/two-sum-test-fixtures.ts's module doc for the disclosed,
   * pre-existing stdout-echo-on-failure leak this does not attempt to
   * paper over. */
  hiddenTest?: PytestRunOutcome;
}

function PytestOutcomeSection({
  title,
  testId,
  outcome,
}: {
  title: string;
  testId: string;
  outcome?: PytestRunOutcome;
}) {
  const status = outcome === undefined ? "not-run" : outcome.exitCode === 0 ? "pass" : "fail";
  const statusLabel =
    outcome === undefined ? "Not run yet" : outcome.exitCode === 0 ? "Passed" : `Failed (exit ${outcome.exitCode})`;
  return (
    <div data-testid={testId}>
      <h3 className="region-heading">{title}</h3>
      <span className="badge" data-testid={`${testId}-status`} data-status={status}>
        {statusLabel}
      </span>
      {outcome !== undefined && <pre data-testid={`${testId}-stdout`}>{outcome.stdout}</pre>}
    </div>
  );
}

export function ExecutionResultCard({
  exitCode,
  verification,
  receiptCount,
  visibleTest,
  hiddenTest,
}: ExecutionResultCardProps) {
  const hasRun = exitCode !== undefined;
  const status = hasRun ? (exitCode === 0 ? "pass" : "fail") : "not-run";
  const statusLabel = hasRun ? (exitCode === 0 ? "Passed" : `Exited with code ${exitCode}`) : "Not run yet";

  return (
    <section className="execution-result-card" aria-label="Execution result" data-testid="execution-result-card">
      <header className="execution-result-header">
        <h2>Execution result</h2>
        <span className="badge" data-testid="execution-result-status" data-status={status}>
          {statusLabel}
        </span>
      </header>
      <p className="region-heading" data-testid="execution-result-evidence-summary">
        Session evidence: {receiptCount} receipt{receiptCount === 1 ? "" : "s"} recorded
      </p>
      <h3 className="region-heading">Verification state</h3>
      <TestResultView verification={verification} />
      <PytestOutcomeSection title="Visible tests" testId="visible-test-result" outcome={visibleTest} />
      <PytestOutcomeSection title="Hidden tests" testId="hidden-test-result" outcome={hiddenTest} />
    </section>
  );
}
