// Real server-side receipt computation for app/page.tsx's "Finish session"
// action. Calls the real checksum-adapter (TICKET-038, real `blake3` npm
// package call, no mock hashing) to produce a TransitionReceipt (TICKET-020
// shape) with a genuine BLAKE3 digest over the session's recorded event
// labels. Kept server-side deliberately: the browser build of `blake3` needs
// an async WASM init this adapter's synchronous `hashHex` does not perform,
// so the real (Node-native-binding) path is exercised from a route handler,
// not from the "use client" page directly.
import { NextRequest, NextResponse } from "next/server";
import { getChecksum } from "../../../lib/adapters/checksum-adapter";
import type { TransitionReceipt } from "../../../lib/domain/receipt";

interface ReceiptRequestBody {
  label?: string;
  used: string[];
}

export async function POST(request: NextRequest) {
  const body = (await request.json()) as ReceiptRequestBody;
  if (!body || !Array.isArray(body.used)) {
    return NextResponse.json({ error: "invalid request body" }, { status: 400 });
  }
  const checksum = getChecksum();
  const canonical = JSON.stringify({ label: body.label ?? null, used: body.used });
  const checksumValue = checksum.hashHex(canonical);
  const receipt: TransitionReceipt = {
    label: body.label,
    used: body.used,
    checksum: { algorithm: "BLAKE3", checksumValue },
  };
  return NextResponse.json({ receipt });
}
