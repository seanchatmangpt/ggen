/**
 * JTBD 4: Write and run real Python code, see real output.
 *
 * Types real Python source into the editor, clicks Run, and asserts the
 * real stdout/exit status rendered in the DOM matches what a DIRECT call
 * to the real sandbox-executor (same module, same real python3 subprocess,
 * invoked here in this Node-side test process) produces for the identical
 * source -- a genuine independent cross-check, not an assumption that the
 * UI's reported output is correct.
 */
import { test, expect } from "./support";
import { getSandboxExecutor, isExecutionRefusal } from "../../lib/adapters/sandbox-executor";

const TWO_SUM_SOLUTION =
  "def two_sum(nums, target):\n" +
  "    seen = {}\n" +
  "    for i, n in enumerate(nums):\n" +
  "        complement = target - n\n" +
  "        if complement in seen:\n" +
  "            return [seen[complement], i]\n" +
  "        seen[n] = i\n" +
  "    return []\n" +
  "\n" +
  "print(two_sum([2, 7, 11, 15], 9))\n";

test("running real Python code in the UI matches a direct real sandbox-executor call for the same source", async ({
  page,
  pageErrors,
}) => {
  await page.goto("/");

  // Confirm the pre-run state honestly first.
  await expect(page.getByTestId("execution-result-status")).toHaveAttribute("data-status", "not-run");
  await expect(page.getByTestId("console-exit-code")).toHaveCount(0);

  await page.getByTestId("code-editor").fill(TWO_SUM_SOLUTION);

  const [runResponse] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/run") && r.request().method() === "POST"),
    page.getByTestId("run-code").click(),
  ]);
  expect(runResponse.status()).toBe(200);
  const runBody = (await runResponse.json()) as {
    receipt?: { stdout: string; stderr: string; exitCode: number };
  };
  expect(runBody.receipt).toBeDefined();

  // Real DOM evidence of the real execution result.
  await expect(page.getByTestId("console-stdout")).toHaveText(runBody.receipt!.stdout);
  await expect(page.getByTestId("console-exit-code")).toHaveText(`exit ${runBody.receipt!.exitCode}`);
  await expect(page.getByTestId("execution-result-status")).toHaveAttribute("data-status", "pass");
  await expect(page.getByTestId("test-result-verification/run-example")).toContainText(
    "verification/run-example: true",
  );

  // Independent cross-check: call the REAL sandbox-executor directly (same
  // module the server-side /api/run route calls, real python3 subprocess,
  // no mock) for the identical source, and compare.
  const executor = getSandboxExecutor();
  const direct = await executor.execute({
    capability: "execute_python",
    files: { "solution.py": TWO_SUM_SOLUTION },
    timeoutMs: 10_000,
  });
  expect(isExecutionRefusal(direct)).toBe(false);
  if (isExecutionRefusal(direct)) throw new Error("unreachable");

  expect(direct.exitCode).toBe(0);
  expect(direct.stdout.trim()).toBe("[0, 1]");
  // The UI's real reported output equals the independently-run direct
  // executor's real output -- the same real Python execution, not a
  // fabricated echo.
  expect(runBody.receipt!.stdout).toBe(direct.stdout);
  expect(runBody.receipt!.exitCode).toBe(direct.exitCode);

  expect(pageErrors).toHaveLength(0);
});

test("a real Python syntax error surfaces real non-zero-exit output in the UI, matching a direct executor call", async ({
  page,
}) => {
  await page.goto("/");

  const BROKEN = "def broken(:\n    pass\n";
  await page.getByTestId("code-editor").fill(BROKEN);

  const [runResponse] = await Promise.all([
    page.waitForResponse((r) => r.url().includes("/api/run") && r.request().method() === "POST"),
    page.getByTestId("run-code").click(),
  ]);
  const runBody = (await runResponse.json()) as { receipt?: { stdout: string; stderr: string; exitCode: number } };
  expect(runBody.receipt).toBeDefined();
  expect(runBody.receipt!.exitCode).not.toBe(0);

  await expect(page.getByTestId("execution-result-status")).toHaveAttribute("data-status", "fail");
  await expect(page.getByTestId("console-exit-code")).toHaveText(`exit ${runBody.receipt!.exitCode}`);

  const executor = getSandboxExecutor();
  const direct = await executor.execute({
    capability: "execute_python",
    files: { "solution.py": BROKEN },
    timeoutMs: 10_000,
  });
  expect(isExecutionRefusal(direct)).toBe(false);
  if (isExecutionRefusal(direct)) throw new Error("unreachable");
  expect(direct.exitCode).toBe(runBody.receipt!.exitCode);
  expect(direct.stderr.length).toBeGreaterThan(0);
});
