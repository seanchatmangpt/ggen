/**
 * TICKET-055: real receipt emission at the reducer's admission-handling
 * branch (<manufacturing-chain/admission-activity> in
 * 60-provenance-receipts.ttl).
 *
 * SPLIT OUT of reducer.ts (real bug found and fixed live in this session):
 * reducer.ts is imported by app/page.tsx, a "use client" component.
 * receipt-emitter.ts -> checksum-adapter.ts needs real BLAKE3 hashing,
 * which pulls in Node's `node:module`/native addon -- Turbopack cannot
 * codegen that into a client bundle ("the chunking context (unknown) does
 * not support external modules (request: node:module)"), which crashed
 * every real page load with a 500. Chicago TDD's own vitest suite never
 * caught this because vitest runs reducer.ts in a plain Node environment,
 * not through Turbopack's client-bundle codegen path -- only a real
 * `next build`/`next dev` + real page load exercises that boundary.
 * `sessionReducer` itself never called `emitReceipt` (page.tsx only ever
 * used `sessionReducer` directly, never `admitWithReceipt`), so the fix is
 * a pure file-split: this module (server-only in practice -- nothing
 * client-side imports it) wraps `sessionReducer` and adds the real receipt
 * side effect, without changing `sessionReducer`'s own proven contract.
 */
import { sessionReducer, type SessionState, type SessionEvent } from "./reducer";
import type { AdmissionResult } from "./refusal";
import { emitReceipt } from "./receipt-emitter";
import type { TransitionReceipt } from "./receipt";

export function admitWithReceipt(
  state: SessionState,
  event: SessionEvent,
  prevReceipt?: TransitionReceipt
): { result: AdmissionResult<SessionState>; receipt?: TransitionReceipt } {
  const result = sessionReducer(state, event);
  if (result.status !== "admitted") {
    return { result };
  }
  const receipt = emitReceipt("admission", {
    used: event.targetPhase !== undefined ? [event.family, event.targetPhase] : [event.family],
    label: `admission: ${event.family}`,
    generated: event.targetPhase,
    timestamp: Date.now(),
    prevReceipt,
  });
  return { result, receipt };
}
