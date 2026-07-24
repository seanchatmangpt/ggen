/**
 * TICKET-055: Runtime transition/cognition/sandbox execution/test/
 * accessibility projection receipts.
 *
 * HAND-AUTHORED (this ticket's own classification: "Template: 75% /
 * Custom code: 25%" -- the emission-point wiring pattern is generic
 * structure, but the call-site placement inside the real adapters
 * requires the same design judgment as the reducer/adapters themselves).
 *
 * Wires TICKET-020's generated `TransitionReceipt` type (lib/domain/receipt.ts)
 * to real emission at the manufacturing-chain steps modeled in
 * packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl,
 * plus a 5th step added for the real wasm4pm-cognition bridge
 * (90-cognition-bridge.ttl's <manufacturing-chain/cognition-activity>),
 * inserted between admission and sandbox-execution:
 *
 *   1. <manufacturing-chain/admission-activity>       -> step "admission"
 *   2. <manufacturing-chain/cognition-activity>        -> step "cognition-run"
 *   3. <manufacturing-chain/sandbox-execution-activity> -> step "sandbox-execution"
 *   4. <manufacturing-chain/test-result>               -> step "test-result"
 *   5. <manufacturing-chain/accessibility-projection>  -> step "accessibility-projection"
 *
 * `emitReceipt` computes a REAL BLAKE3 checksum (via checksum-adapter.ts's
 * `getChecksum().hashHex`, TICKET-038 -- not a fabricated/placeholder
 * value, unlike the "000...0"/"111...1" example literals in the ontology's
 * own <receipt/entry-1>/<receipt/entry-2-final> fixtures) over a canonical
 * JSON serialization of the step, the real inputs consumed, the real
 * artifact generated (if any), a caller-supplied timestamp, and the prior
 * receipt's checksum (for chaining).
 *
 * Design note on the timestamp/prevReceipt API (left open by the ticket
 * text on purpose, "design your own reasonable API"): `emitReceipt` itself
 * stays a pure function of its arguments -- it never reads `Date.now()`
 * internally -- so it is unit-testable deterministically. The caller
 * (reducer.ts / sandbox-executor.ts / accessibility-platform-adapter.ts)
 * supplies a real `Date.now()` at the real moment the real action
 * happened, plus the immediately-prior receipt in the chain (if any).
 * Chaining is expressed the same way the ontology's own
 * <receipt/entry-2-final> expresses it against <receipt/entry-1>: both
 * `derivedFrom` (prov:wasDerivedFrom-equivalent) and `relation`
 * (dcterms:relation-equivalent) are set to the prior receipt's checksum
 * value -- the only stable identifier a checksum-only receipt (no IRI)
 * has.
 */
import { getChecksum } from "../adapters/checksum-adapter";
import type { TransitionReceipt } from "./receipt";

/** The manufacturing-chain steps that require a real emitted receipt: 4
 * modeled in 60-provenance-receipts.ttl, plus "cognition-run" modeled in
 * 90-cognition-bridge.ttl's <manufacturing-chain/cognition-activity>. Fixed
 * by those ontology files, not invented here (this ticket's own
 * Domain-data responsibility). */
export type ManufacturingChainStep =
  | "admission"
  | "cognition-run"
  | "sandbox-execution"
  | "test-result"
  | "accessibility-projection";

export interface EmitReceiptData {
  /** Identifiers of the real inputs consumed at this step (event family,
   * capability id, file paths, etc. -- never fabricated). */
  used: string[];
  /** Human-readable label; defaults to the step name if omitted. */
  label?: string;
  /** Identifier of the real artifact/state this step generated, if any
   * (e.g. the target phase reached, or "exitCode=0"). */
  generated?: string;
  /** Real wall-clock timestamp (epoch ms) of the real action this receipt
   * records. Caller-supplied (e.g. `Date.now()`) so `emitReceipt` itself
   * stays a pure, deterministically-testable function. */
  timestamp: number;
  /** The receipt this one chains from, if any. Its checksum becomes this
   * receipt's `derivedFrom`/`relation` fields. */
  prevReceipt?: TransitionReceipt;
}

/**
 * Canonical (stable-key-order, no-undefined) JSON serialization, so the
 * same logical payload always hashes to the same checksum regardless of
 * source object literal key order or JS engine.
 */
function canonicalStringify(value: unknown): string {
  if (value === undefined) return "null";
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map((entry) => canonicalStringify(entry)).join(",")}]`;
  }
  const record = value as Record<string, unknown>;
  const keys = Object.keys(record)
    .filter((key) => record[key] !== undefined)
    .sort();
  const entries = keys.map((key) => `${JSON.stringify(key)}:${canonicalStringify(record[key])}`);
  return `{${entries.join(",")}}`;
}

/**
 * Emits a real TransitionReceipt for one real manufacturing-chain step.
 *
 * Real action: computes a real BLAKE3 digest (blake3 npm package, via
 * checksum-adapter.ts) over the canonicalized `{step, used, label,
 * generated, timestamp, prevChecksum}` payload. No field is fabricated --
 * every value comes from what the caller reports actually happened.
 */
export function emitReceipt(step: ManufacturingChainStep, data: EmitReceiptData): TransitionReceipt {
  const { used, label, generated, timestamp, prevReceipt } = data;

  const payload = {
    step,
    used,
    label,
    generated,
    timestamp,
    prevChecksum: prevReceipt?.checksum.checksumValue,
  };
  const checksumValue = getChecksum().hashHex(canonicalStringify(payload));

  const receipt: TransitionReceipt = {
    label: label ?? step,
    used,
    checksum: { algorithm: "BLAKE3", checksumValue },
  };
  if (generated !== undefined) {
    receipt.generated = generated;
  }
  if (prevReceipt !== undefined) {
    receipt.derivedFrom = prevReceipt.checksum.checksumValue;
    receipt.relation = prevReceipt.checksum.checksumValue;
  }
  return receipt;
}

/**
 * Reduction path: already near-minimal -- one canonicalization helper plus
 * one hashing call. Any future growth beyond "compute a checksum over a
 * canonical payload" should be treated as a smell (e.g. persistence or
 * network I/O belongs in a real adapter, not here).
 */
export const REDUCTION_PATH_NOTE =
  "emitReceipt is a pure function: canonicalize the real step data, hash " +
  "it via the real BLAKE3 adapter, return the TransitionReceipt. No " +
  "further reduction expected.";
