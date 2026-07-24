// Real server-side dispatch to the sandbox-executor adapter (TICKET-035),
// invoked from app/page.tsx's "Run" action.
//
// Distinct from the pre-existing app/api/sandbox/[capability]/route.ts: that
// route only validates a capability id against a static OPERATIONS table and
// echoes `{status: "accepted"}` -- it never calls a real executor, despite
// its own name implying dispatch. This route performs the real thing: it
// calls the real, subprocess-spawning `getSandboxExecutor().execute(...)`
// (real python3/rustc/cargo child processes, no mocks), because workstream
// I's Playwright scenarios need a real end-to-end path to drive, and
// app/api/sandbox/[capability]/route.ts's decorative body does not provide
// one. That existing route is left untouched -- reconciling/removing it is
// out of this task's scope and is noted honestly rather than silently
// papered over.
import { NextRequest, NextResponse } from "next/server";
import { getSandboxExecutor, isExecutionRefusal, type CapabilityId } from "../../../lib/adapters/sandbox-executor";
import type { PolicyId } from "../../../lib/adapters/policy-check-adapter";

interface RunRequestBody {
  capability: CapabilityId;
  files: Record<string, string>;
  timeoutMs?: number;
  activeMode?: PolicyId;
}

export async function POST(request: NextRequest) {
  const body = (await request.json()) as RunRequestBody;
  if (!body || typeof body.capability !== "string" || typeof body.files !== "object") {
    return NextResponse.json({ error: "invalid request body" }, { status: 400 });
  }
  const executor = getSandboxExecutor();
  const result = await executor.execute({
    capability: body.capability,
    files: body.files,
    timeoutMs: body.timeoutMs ?? 10_000,
    activeMode: body.activeMode,
  });
  if (isExecutionRefusal(result)) {
    return NextResponse.json({ refusal: result });
  }
  return NextResponse.json({ receipt: result });
}
