/**
 * Server-only adapter for the real wasm4pm-cognition WASM bridge (Eliza
 * breed). Wraps `lib/wasm/wasm4pm-cognition/wasm4pm_cognition.js`'s real
 * `cognition_run` export -- a wasm-pack `--target nodejs` (CommonJS) build,
 * consumed here via `require()`, never imported into a "use client"
 * component (see the client/server bundle boundary note this session
 * already hit once for `checksum-adapter.ts` / `receipt-emitter.ts`).
 *
 * Real, empirically-verified contract (probed directly against the built
 * binary this session, not assumed from the .d.ts, which only declares the
 * return type as `any`):
 *   - `cognition_run(input_json: string)` returns a JSON **string** on
 *     success (`typeof ret === "string"`), not a pre-parsed object.
 *   - On failure it THROWS. The thrown value's `typeof` is `"string"` --
 *     wasm-bindgen surfaces the Rust `Err(JsValue::from_str(json))` as a
 *     bare JS string, NOT an `Error` instance (`thrown instanceof Error`
 *     is `false`). The string itself is JSON: `{"error":"..."}`.
 *   - Two real failure shapes distinguished here:
 *       1. Non-empty `rules` but no keyword in `intent` matches any rule's
 *          premise -> `{"error":"eliza: postcondition failed: empty
 *          inference trace (fraud signal)"}` -- a deliberate fail-closed
 *          anti-fraud check, mapped to `status: "no-track-matched"`.
 *       2. Any other real error (unknown breed, malformed JSON, etc.) ->
 *          mapped to `status: "refused"`.
 *   - `contract.rules` must be non-empty for a real interview turn -- an
 *     empty `rules` array makes Eliza fall back to a generic, non-interview
 *     wildcard response instead of a real match/no-match signal. This
 *     adapter always passes the full `COGNITION_RULES` set.
 *
 * TICKET-055 (Phase 3): emits a real TransitionReceipt for the
 * "cognition-run" manufacturing-chain step
 * (90-cognition-bridge.ttl's <manufacturing-chain/cognition-activity>,
 * inserted between "admission" and "sandbox-execution") for every REAL
 * `cognition_run` invocation -- whether it returns successfully (matched)
 * or throws (no-track-matched / other refusal). Both are real actions that
 * actually reached the WASM module, same discipline
 * sandbox-executor.ts applies to a real non-zero exitCode. The one case
 * that never emits a receipt is the pre-flight empty/whitespace-intent
 * check below, which returns before the WASM module is ever loaded or
 * called -- no real action occurred, so (mirroring
 * ExecutionRefusal's `no_source_provided` case) no receipt is fabricated
 * for it.
 */
import { createRequire } from "node:module";
import { COGNITION_RULES } from "../domain/cognition-rules";
import { emitReceipt } from "../domain/receipt-emitter";
import type { TransitionReceipt } from "../domain/receipt";

const nodeRequire = createRequire(import.meta.url);

interface CognitionWasmModule {
  cognition_run(input_json: string): string;
}

let cachedRealModule: CognitionWasmModule | undefined;

/**
 * Production-hardening pass: graceful degradation when the real WASM module
 * genuinely cannot be loaded (a corrupted/missing install, a bad deploy --
 * the exact real failure `scripts/materialize-wasm-cognition.mjs`'s own doc
 * describes this package as needing a real node_modules presence for).
 *
 * `forceUnavailable` resolves from two sources (either forces the same real
 * failure path in `loadCognitionModule` below):
 *   1. An explicit `true` argument -- threaded from a real, per-request
 *      `x-wasm4pm-cognition-force-unavailable` header
 *      (app/api/cognition/route.ts), test-only, never sent by the real UI
 *      fetch in app/page.tsx. Lets one Playwright test force a genuine
 *      require() failure against the single shared `next dev` server this
 *      suite reuses (playwright.config.ts) without mutating process-wide
 *      state any other, concurrently-running test would also observe.
 *   2. `WASM4PM_COGNITION_FORCE_UNAVAILABLE=1` env var -- read fresh on
 *      every call (not cached at module-init time), so ops/test tooling
 *      that controls the server process's environment can flip it without
 *      a code change.
 *
 * Deliberately a boolean, not an arbitrary path string -- see
 * `loadCognitionModule`'s doc for why a dynamic require() TARGET (as
 * opposed to a dynamic boolean choosing between two fixed, literal
 * require() calls) is unsafe under Turbopack.
 */
function shouldForceUnavailable(forceUnavailable?: boolean): boolean {
  return forceUnavailable === true || process.env.WASM4PM_COGNITION_FORCE_UNAVAILABLE === "1";
}

/** Lazily require()d on first real call, mirroring checksum-adapter.ts's
 * lazy-load discipline -- avoids a Turbopack/Next page-data-collection pass
 * evaluating a native/WASM-backed require() at module-load time for a route
 * this module is merely imported by, not yet invoked in.
 *
 * Real bug found and fixed live (Phase 5, Playwright JTBD validation): a
 * plain relative require("../wasm/wasm4pm-cognition/wasm4pm_cognition.js")
 * gets bundled by Turbopack, which rewrites the generated wasm-bindgen
 * module's own `__dirname` to a synthetic `/ROOT/...` placeholder --
 * breaking its internal `readFileSync(path.join(__dirname,
 * 'wasm4pm_cognition_bg.wasm'))` sibling-asset load with a real
 * `ENOENT .../ROOT/lib/wasm/wasm4pm-cognition/wasm4pm_cognition_bg.wasm`
 * on every real POST /api/cognition request. This is the exact same
 * failure class documented in checksum-adapter.ts's Import-path note for
 * blake3's WASM fallback. `npx tsc --noEmit`, the vitest suite (plain
 * Node, bypasses Turbopack entirely), and `npx next build` (this
 * require() is lazy, deferred past build-time page-data-collection) all
 * stayed green through this regression -- only a real HTTP request
 * against a running `next dev`/`next start` server exercises it, which is
 * exactly what this Playwright pass is for.
 *
 * Fix: give the WASM package a real node_modules presence (a `file:`
 * dependency in package.json pointing at lib/wasm/wasm4pm-cognition,
 * which already carries its own real wasm-pack-generated package.json
 * with `"name": "wasm4pm-cognition"`) and require it by that bare package
 * name instead of a relative deep path. `next.config.ts`'s
 * `serverExternalPackages` matches packages by their resolved
 * node_modules package boundary, not by relative-import path -- a plain
 * relative require, however lazy, was never eligible for that
 * externalization no matter how the config was written; a real
 * node_modules symlink is what makes the match. This is the same
 * mechanism that already works for blake3 (a real npm package), applied
 * to this local package now that it has the same node_modules shape.
 *
 * `forceUnavailable`: see `shouldForceUnavailable`'s doc above.
 *
 * REAL BUG FOUND AND FIXED LIVE (UX-polish pass, Playwright validation):
 * the first version of this graceful-degradation test hook replaced the
 * literal `nodeRequire("wasm4pm-cognition")` call below with a single
 * `nodeRequire(specifier)` call fed by a variable that merely DEFAULTED to
 * that same string. That looks equivalent at runtime, but it silently
 * broke the real production path: `next.config.ts`'s
 * `serverExternalPackages` externalization relies on Turbopack statically
 * recognizing the require() call's target at COMPILE time, which requires
 * a literal string argument. Turbopack, it turns out, statically scans
 * EVERY require() call in a module when compiling the route -- a single
 * unresolvable DYNAMIC specifier anywhere in the file, even inside a
 * branch never reached by the current request, made it emit a hard
 * "Module not found: Can't resolve <dynamic>" COMPILE error for the whole
 * route (confirmed live: curling a genuinely fresh `next dev` server, for
 * a plain ARRAY-keyword intent that never even touches the
 * force-unavailable branch, returned Turbopack's own raw compile-error
 * HTML page, not the app's typed response, purely because a dynamic
 * require() existed elsewhere in this file). Fixed by keeping BOTH
 * require() calls as fixed, literal string arguments -- one real
 * ("wasm4pm-cognition"), one a real-but-deliberately-nonexistent package
 * name ("wasm4pm-cognition-deliberately-missing-for-tests", added to
 * `serverExternalPackages` alongside the real one so Turbopack defers it
 * to runtime instead of trying to resolve it at compile time too) -- and
 * choosing between them with a boolean `if`, never with a variable fed
 * into `require()` itself. Do not reintroduce a dynamic require() target
 * here. */
function loadCognitionModule(forceUnavailable?: boolean): CognitionWasmModule {
  if (shouldForceUnavailable(forceUnavailable)) {
    // Real require() of a real, fixed, deliberately-nonexistent package
    // name -- Node's own module resolution genuinely throws
    // MODULE_NOT_FOUND every time; never cached (a broken load never
    // succeeds, so there is nothing to cache).
    return nodeRequire("wasm4pm-cognition-deliberately-missing-for-tests") as CognitionWasmModule;
  }

  cachedRealModule ??= nodeRequire("wasm4pm-cognition") as CognitionWasmModule;
  return cachedRealModule;
}

interface RawCognitionSuccess {
  status: "ok";
  run_id: string;
  output: {
    breed: string;
    selected: string;
    explanation: string;
  };
  conformance: {
    fitness: number;
    model_id: string;
    refusals: string[];
  };
  /** Real Ed25519 signature over the run, produced by the WASM module --
   * empirically verified present on every real success response this
   * session (see this file's module doc / the Phase-3 node probe that
   * confirmed the field names below, not assumed from the .d.ts). */
  signature: string;
  public_key_id: string;
  signature_algorithm: string;
}

export interface CognitionConformance {
  fitness: number;
  modelId: string;
  refusals: string[];
}

export interface CognitionMatchedOutcome {
  status: "matched";
  selected: string;
  explanation: string;
  runId: string;
  conformance: CognitionConformance;
  /** Real Ed25519 signature evidence that a real cognition breed call
   * happened -- surfaced here (rather than discarded) so callers can prove
   * the run was real, not fabricated. */
  signature: string;
  publicKeyId: string;
  signatureAlgorithm: string;
  /** TICKET-055 (Phase 3): real receipt for the "cognition-run"
   * manufacturing-chain step. Always present on a matched outcome -- a
   * real WASM call happened and returned successfully. */
  receipt: TransitionReceipt;
}

export interface CognitionNoTrackMatchedOutcome {
  status: "no-track-matched";
  reason: string;
  /** TICKET-055 (Phase 3): real receipt for the "cognition-run" step. A
   * real WASM call DID happen here (it ran and threw the real fail-closed
   * "empty inference trace" error) -- same discipline sandbox-executor.ts
   * applies to a real non-zero exitCode: the action occurred, so it gets a
   * receipt, even though it did not produce a match. */
  receipt: TransitionReceipt;
}

export interface CognitionRefusedOutcome {
  status: "refused";
  reason: string;
  /** Present only when a real WASM call actually happened and threw a
   * non-"no-track-matched" error (e.g. malformed input). Absent for the
   * pre-flight empty/whitespace-intent refusal below, which never reaches
   * the WASM module -- no real action occurred, so no receipt is
   * fabricated for it. */
  receipt?: TransitionReceipt;
}

/**
 * Production-hardening pass: distinct from `CognitionRefusedOutcome` on
 * purpose. "refused" means a real WASM call happened and the breed itself
 * rejected the input (a content-level decision). "unavailable" means the
 * WASM module never loaded at all -- an infrastructure-level failure (a
 * missing/corrupted install), not a judgment about the candidate's
 * utterance. No receipt is ever attached: no real cognition action
 * occurred, same discipline as the pre-flight empty-intent refusal above.
 */
export interface CognitionUnavailableOutcome {
  status: "unavailable";
  reason: string;
}

export type CognitionOutcome =
  | CognitionMatchedOutcome
  | CognitionNoTrackMatchedOutcome
  | CognitionRefusedOutcome
  | CognitionUnavailableOutcome;

const NO_TRACK_MATCH_SIGNATURE = "postcondition failed: empty inference trace";

/** Maps a thrown `cognition_run` failure to a typed outcome (minus the
 * receipt -- attached by the caller, since only the caller knows the real
 * `prevReceipt` to chain from). Handles the real observed shape (a bare
 * thrown JSON string) and, defensively, an Error-wrapped form, in case a
 * future build of the WASM binary changes how wasm-bindgen surfaces the
 * error. */
function mapThrownToOutcome(
  thrown: unknown,
): Omit<CognitionNoTrackMatchedOutcome, "receipt"> | Omit<CognitionRefusedOutcome, "receipt"> {
  const messageText =
    typeof thrown === "string" ? thrown : thrown instanceof Error ? thrown.message : String(thrown);

  let errorText = messageText;
  try {
    const parsed = JSON.parse(messageText) as { error?: string };
    if (typeof parsed.error === "string") errorText = parsed.error;
  } catch {
    // Not JSON -- fall back to the raw thrown text as-is.
  }

  if (errorText.includes(NO_TRACK_MATCH_SIGNATURE)) {
    return { status: "no-track-matched", reason: errorText };
  }
  return { status: "refused", reason: errorText };
}

/**
 * Exposed for adapter-level negative testing only (see
 * tests/scenarios/cognition-first-decisive.test.ts's "unknown breed id"
 * negative test): drives the real WASM module directly with a caller-built
 * input (e.g. an unrecognized `breed`), bypassing `runCognition`'s
 * intent/rules wiring entirely. Throws exactly what the real WASM module
 * throws -- no mapping, no receipt. Not used by any production code path
 * (`runCognition` always builds its own well-formed `breed: "eliza"`
 * input); this exists solely so a negative test can exercise a real
 * fail-closed WASM error for a case `runCognition` itself has no way to
 * construct.
 */
export function invokeCognitionRunRaw(input: unknown): string {
  const wasm = loadCognitionModule();
  return wasm.cognition_run(JSON.stringify(input));
}

/**
 * Run one real interview turn through the wasm4pm-cognition Eliza breed.
 * Never throws -- every real failure mode (empty intent, no keyword match,
 * any other WASM-side error) is mapped to a typed `CognitionOutcome`.
 *
 * `prevReceipt` (TICKET-055, Phase 3): the prior receipt in this session's
 * manufacturing-chain, if any -- threaded through so this step's emitted
 * receipt correctly chains (derivedFrom/relation) off the previous step,
 * mirroring `ExecutionRequest.prevReceipt` in sandbox-executor.ts.
 *
 * `forceUnavailable` (production-hardening pass): see
 * `shouldForceUnavailable`'s doc. Test-only in practice -- the real
 * app/api/cognition/route.ts only forwards `true` here when a
 * `x-wasm4pm-cognition-force-unavailable` test header was present on the
 * inbound request; the real UI never sends that header.
 */
export async function runCognition(
  intent: string,
  prevReceipt?: TransitionReceipt,
  forceUnavailable?: boolean,
): Promise<CognitionOutcome> {
  if (typeof intent !== "string" || intent.trim().length === 0) {
    return { status: "refused", reason: "intent must be a non-empty string" };
  }

  const input = {
    breed: "eliza",
    contract: {
      intent,
      candidates: [],
      facts: [],
      cases: [],
      goals: [],
      state: [],
      rules: COGNITION_RULES,
    },
    options: {},
  };

  let wasm: CognitionWasmModule;
  try {
    wasm = loadCognitionModule(forceUnavailable);
  } catch (loadError) {
    // The real require() genuinely failed -- the WASM module could not be
    // resolved/loaded at all (missing/corrupted node_modules install, or a
    // deliberately broken specifier for testing this exact path; see
    // resolveModuleSpecifier's doc). No real cognition action happened, so
    // (same discipline as the pre-flight empty-intent refusal above) no
    // receipt is fabricated for it.
    const message = loadError instanceof Error ? loadError.message : String(loadError);
    return { status: "unavailable", reason: message };
  }

  let raw: string;
  try {
    raw = wasm.cognition_run(JSON.stringify(input));
  } catch (thrown) {
    // A real WASM call happened and threw -- a real action occurred, so
    // (same discipline as sandbox-executor.ts's non-zero-exitCode case) it
    // still gets a real receipt recording the real failed attempt.
    const failure = mapThrownToOutcome(thrown);
    const receipt = emitReceipt("cognition-run", {
      used: [intent],
      label: `cognition-run: ${failure.status}`,
      generated: failure.reason,
      timestamp: Date.now(),
      prevReceipt,
    });
    return { ...failure, receipt } as CognitionNoTrackMatchedOutcome | CognitionRefusedOutcome;
  }

  const parsed = JSON.parse(raw) as RawCognitionSuccess;
  const receipt = emitReceipt("cognition-run", {
    used: [intent],
    label: "cognition-run: matched",
    generated: parsed.output.selected,
    timestamp: Date.now(),
    prevReceipt,
  });
  return {
    status: "matched",
    selected: parsed.output.selected,
    explanation: parsed.output.explanation,
    runId: parsed.run_id,
    conformance: {
      fitness: parsed.conformance.fitness,
      modelId: parsed.conformance.model_id,
      refusals: parsed.conformance.refusals,
    },
    signature: parsed.signature,
    publicKeyId: parsed.public_key_id,
    signatureAlgorithm: parsed.signature_algorithm,
    receipt,
  };
}
