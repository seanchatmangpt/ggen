// Real server-side dispatch for the "run_pytest" capability (JTBD 5
// closure), invoked from app/page.tsx's "Run visible tests"/"Run hidden
// tests" actions. Same pattern as app/api/run/route.ts (which only ever
// dispatches "execute_python"): calls the real, subprocess-spawning
// `getSandboxExecutor().execute(...)` (real python3/pytest child process,
// no mocks). Distinct route rather than a branch inside app/api/run/route.ts
// because the request shape is genuinely different -- the caller sends the
// candidate's raw `code` plus which fixture (`testKind`) to pair it with,
// not a `files` map; this route owns assembling the real `files` map
// server-side (see lib/domain/two-sum-test-fixtures.ts's module doc for why
// that assembly must happen here and not in the client).
import { NextRequest, NextResponse } from "next/server";
import { getSandboxExecutor, isExecutionRefusal } from "../../../lib/adapters/sandbox-executor";
import type { PolicyId } from "../../../lib/adapters/policy-check-adapter";
import type { TransitionReceipt } from "../../../lib/domain/receipt";
import { testFixtureFor, type TestKind } from "../../../lib/domain/two-sum-test-fixtures";

interface TestRequestBody {
  testKind: TestKind;
  code: string;
  timeoutMs?: number;
  activeMode?: PolicyId;
  prevReceipt?: TransitionReceipt;
}

function isTestKind(value: unknown): value is TestKind {
  return value === "visible" || value === "hidden";
}

export async function POST(request: NextRequest) {
  const body = (await request.json()) as TestRequestBody;
  if (!body || !isTestKind(body.testKind) || typeof body.code !== "string") {
    return NextResponse.json({ error: "invalid request body" }, { status: 400 });
  }

  const { filename, source } = testFixtureFor(body.testKind);
  const executor = getSandboxExecutor();
  const result = await executor.execute({
    capability: "run_pytest",
    files: { "solution.py": body.code, [filename]: source },
    timeoutMs: body.timeoutMs ?? 15_000,
    activeMode: body.activeMode,
    prevReceipt: body.prevReceipt,
  });

  if (isExecutionRefusal(result)) {
    return NextResponse.json({ refusal: result });
  }
  return NextResponse.json({ receipt: result, testKind: body.testKind });
}
