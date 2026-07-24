/**
 * Real component test for components/execution-result-card.tsx (SSR
 * pattern, see cognition-panel.test.tsx's module doc for the disclosed
 * jsdom-absence scope note).
 */
import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { ExecutionResultCard } from "../../components/execution-result-card";

describe("ExecutionResultCard (real React SSR render)", () => {
  it("shows a not-run status and TestResultView's own empty state before any execution happens", () => {
    const html = renderToStaticMarkup(
      <ExecutionResultCard exitCode={undefined} verification={{}} receiptCount={0} />,
    );
    expect(html).toMatch(/data-testid="execution-result-status"[^>]*data-status="not-run"/);
    expect(html).toContain("Not run yet");
    expect(html).toContain('data-testid="test-result-empty"');
    expect(html).toContain('data-testid="execution-result-evidence-summary"');
    expect(html).toContain("Session evidence: 0 receipts recorded");
  });

  it("shows a pass status, the real verification entries for a real exitCode 0, and a singular receipt count", () => {
    const html = renderToStaticMarkup(
      <ExecutionResultCard
        exitCode={0}
        verification={{ "verification/run-example": true }}
        receiptCount={1}
      />,
    );
    expect(html).toMatch(/data-testid="execution-result-status"[^>]*data-status="pass"/);
    expect(html).toContain("Passed");
    expect(html).toContain('data-testid="test-result-verification/run-example"');
    expect(html).toContain("Session evidence: 1 receipt recorded");
  });

  it("shows a fail status carrying the real non-zero exit code, and the plural receipt count for multiple recorded receipts", () => {
    const html = renderToStaticMarkup(
      <ExecutionResultCard
        exitCode={1}
        verification={{ "verification/run-example": false }}
        receiptCount={3}
      />,
    );
    expect(html).toMatch(/data-testid="execution-result-status"[^>]*data-status="fail"/);
    expect(html).toContain("Exited with code 1");
    expect(html).toContain("Session evidence: 3 receipts recorded");
  });

  it("JTBD 5: shows a not-run status for both visible/hidden test sections when neither has been run yet", () => {
    const html = renderToStaticMarkup(
      <ExecutionResultCard exitCode={undefined} verification={{}} receiptCount={0} />,
    );
    expect(html).toMatch(/data-testid="visible-test-result-status"[^>]*data-status="not-run"/);
    expect(html).toMatch(/data-testid="hidden-test-result-status"[^>]*data-status="not-run"/);
    expect(html).not.toContain('data-testid="visible-test-result-stdout"');
  });

  it("JTBD 5: shows a real pass status and real stdout for a passing visible-test run, independent of the hidden-test section", () => {
    const html = renderToStaticMarkup(
      <ExecutionResultCard
        exitCode={undefined}
        verification={{ "verification/run-visible-test": true }}
        receiptCount={1}
        visibleTest={{ exitCode: 0, stdout: "1 passed in 0.01s\n", stderr: "" }}
      />,
    );
    expect(html).toMatch(/data-testid="visible-test-result-status"[^>]*data-status="pass"/);
    expect(html).toContain("1 passed in 0.01s");
    expect(html).toMatch(/data-testid="hidden-test-result-status"[^>]*data-status="not-run"/);
  });

  it("JTBD 5: shows a real fail status for a failing hidden-test run", () => {
    const html = renderToStaticMarkup(
      <ExecutionResultCard
        exitCode={undefined}
        verification={{ "verification/run-hidden-test": false }}
        receiptCount={1}
        hiddenTest={{ exitCode: 1, stdout: "1 failed in 0.01s\n", stderr: "" }}
      />,
    );
    expect(html).toMatch(/data-testid="hidden-test-result-status"[^>]*data-status="fail"/);
    expect(html).toContain("Failed (exit 1)");
    expect(html).toContain("1 failed in 0.01s");
  });
});
