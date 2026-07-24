// Generated dispatch route for sandbox capability operations (TICKET-013).
// Single dynamic route chosen over one-file-per-operation: all 9
// hydra:Operation resources share the same request/response shape (capability id + method),
// so a table-driven dispatcher avoids near-duplicate route files. See TICKET-013 notes for
// the operation-count discrepancy against the ticket's original "7" estimate.
import { NextRequest, NextResponse } from "next/server";

interface SandboxOperation {
  id: string;
  name: string;
  method: string;
}

const OPERATIONS: SandboxOperation[] = [
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/editor/create-file", name: "create file", method: "POST" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/editor/modify-file", name: "modify file", method: "POST" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/editor/open-file", name: "open file", method: "GET" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/runtime/compile", name: "compile", method: "POST" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/runtime/execute", name: "execute", method: "POST" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/session/create-session", name: "create session", method: "POST" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/verification/run-complete-test-suite", name: "run complete test suite", method: "POST" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/verification/run-hidden-test", name: "run hidden test", method: "POST" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/capability/verification/run-visible-test", name: "run visible test", method: "POST" },
];

function findOperation(capability: string): SandboxOperation | undefined {
  return OPERATIONS.find((op) => op.id.endsWith(`/${capability}`) || op.id === capability);
}

async function dispatch(request: NextRequest, capability: string) {
  const op = findOperation(capability);
  if (!op) {
    return NextResponse.json({ error: "unknown capability", capability }, { status: 404 });
  }
  if (op.method !== request.method) {
    return NextResponse.json(
      { error: "method not allowed", expected: op.method, got: request.method },
      { status: 405 },
    );
  }
  return NextResponse.json({ capability: op.id, name: op.name, status: "accepted" });
}

export async function GET(request: NextRequest, { params }: { params: Promise<{ capability: string }> }) {
  const { capability } = await params;
  return dispatch(request, capability);
}

export async function POST(request: NextRequest, { params }: { params: Promise<{ capability: string }> }) {
  const { capability } = await params;
  return dispatch(request, capability);
}
