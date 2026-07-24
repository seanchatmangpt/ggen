// Real server-side dispatch to the wasm4pm-cognition adapter (Eliza breed),
// mirroring app/api/run/route.ts and app/api/receipt/route.ts's pattern:
// call a server-only adapter, return its typed outcome as JSON. Kept
// server-side deliberately -- lib/wasm/wasm4pm-cognition is a wasm-pack
// `--target nodejs` (CommonJS) build and is not Turbopack-client-bundle-safe
// (see cognition-adapter.ts's module doc), so this route is the only
// sanctioned entry point into it from the app.
import { NextRequest, NextResponse } from "next/server";
import { runCognition } from "../../../lib/adapters/cognition-adapter";
import type { TransitionReceipt } from "../../../lib/domain/receipt";

interface CognitionRequestBody {
  intent: string;
  /** TICKET-055 (Phase 3): the prior manufacturing-chain receipt (e.g. the
   * real session's admission receipt), if the caller has one to chain
   * from. Optional -- omitted, the emitted "cognition-run" receipt simply
   * has no `derivedFrom`/`relation`, same as a chain-head receipt. */
  prevReceipt?: TransitionReceipt;
}

/** Test-only header (production-hardening pass): when present (any
 * non-empty value), forwarded as `true` to `runCognition`'s
 * `forceUnavailable` param, which makes `cognition-adapter.ts`'s
 * `loadCognitionModule` genuinely `require()` a real, fixed, deliberately
 * nonexistent package name instead of the real "wasm4pm-cognition" one --
 * a real Node module-resolution failure, not a fabricated one (see that
 * function's own doc for why this is a boolean flag choosing between two
 * fixed literal require() calls, not a caller-supplied path string -- a
 * dynamic require() TARGET broke Turbopack's static bundling of this
 * entire route when tried first). The real app/page.tsx fetch() call never
 * sets this header; it exists solely so a Playwright test can exercise the
 * "the WASM module genuinely failed to load" path against the single
 * shared `next dev` server the e2e suite reuses (see
 * tests/e2e/jtbd-14-wasm-unavailable.spec.ts). */
const TEST_FORCE_UNAVAILABLE_HEADER = "x-wasm4pm-cognition-force-unavailable";

// HTTP status convention (documented once, here): 200 for "matched" -- a
// real, actionable clarifying question was produced. 422 for
// "no-track-matched" / "refused" -- the request was well-formed and the
// adapter ran to completion, but produced no admitted hypothesis; this is a
// real, disclosed non-match, not a server error, so it is not a 5xx, but it
// is also not a successful match, so it is not a 200. 503 for "unavailable"
// -- the real WASM dependency itself failed to load; a real infrastructure
// problem, correctly a 5xx, but a clean typed one (Service Unavailable),
// not a raw unhandled-exception 500.
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as Partial<CognitionRequestBody> | null;
    if (!body || typeof body.intent !== "string") {
      return NextResponse.json({ error: "invalid request body: intent must be a string" }, { status: 400 });
    }
    const forceUnavailable = Boolean(request.headers.get(TEST_FORCE_UNAVAILABLE_HEADER));
    const outcome = await runCognition(body.intent, body.prevReceipt, forceUnavailable);
    const status = outcome.status === "matched" ? 200 : outcome.status === "unavailable" ? 503 : 422;
    return NextResponse.json(outcome, { status });
  } catch (err) {
    // Defensive last resort: `runCognition` is documented to never throw
    // (every real failure mode, including the WASM module failing to load,
    // is mapped to a typed CognitionOutcome above), so reaching here means
    // something outside that contract broke (e.g. a malformed request body
    // that isn't valid JSON at all). Still returns a clean, typed, bounded
    // JSON error rather than letting Next's default handler surface a raw
    // 500 stack trace to the client.
    const message = err instanceof Error ? err.message : String(err);
    return NextResponse.json({ error: "internal error", detail: message }, { status: 500 });
  }
}
