/**
 * TICKET-038: Cryptographic (BLAKE3) checksum adapter (custom).
 *
 * Thinnest possible wrapper satisfying TICKET-020's generated Checksum
 * interface, reusing this repo's existing BLAKE3 usage pattern
 * (crates/ggen-engine/src/sync.rs computes `blake3::hash(&bytes).to_hex()`
 * for its receipt chain) rather than inventing a new hashing scheme --
 * this file mirrors that exact shape (hash bytes -> lowercase hex string)
 * using the `blake3` npm package (added as a real dependency in this
 * package's package.json) as the JS/WASM equivalent binding.
 *
 * TICKET-020's real generated Checksum interface has not landed yet in
 * this session; `Checksum` below is authored by hand as the minimal shape
 * (`hashHex(bytes): string`) and marked PENDING(TICKET-020).
 *
 * TICKET-028 wiring closure note: this adapter deliberately stays on
 * `policy-check-stub.ts`'s default-allow placeholder, not the real
 * `policy-check-adapter.ts`. Raw cryptographic hashing has no
 * `authority-action/*` mapping anywhere in 50-policy.ttl (verified: no
 * `dcterms:requires` chain in 30-capabilities.ttl links any capability this
 * adapter serves to an authority-action resource) -- routing it through the
 * real check would mean inventing a policy mapping the ontology does not
 * define, which coding-agent-mistakes.md's Epistemic Bypass rule forbids.
 * See lib/adapters/policy-check-adapter.ts's module doc for the full
 * reasoning shared with persistence-adapter.ts.
 *
 * Import-path note (app/page.tsx groundwork, discovered running real `next
 * build`, three real failures chased in order):
 *
 * 1. Plain `import { hash } from "blake3"` resolves through the package's
 *    `package.json` "module" field to `esm/index.js`, which does
 *    `export * from './node.js'` -- a file that does not exist on disk (the
 *    real file is `esm/node/index.js`; only classic CJS `require("./node")`
 *    resolution follows the directory-index fallback, not strict ESM
 *    resolution). Plain `node --experimental-strip-types` and Vitest both
 *    resolve the package's CJS "main" instead (`dist/index.js`, which does
 *    `require("./node")` and works), so this was invisible until Turbopack
 *    tried to bundle the ESM entry for app/api/receipt's route:
 *    `Module not found: Can't resolve './node.js'`.
 * 2. Switching to the CJS subpath `blake3/dist/node` (WASM implementation)
 *    surfaced a second, unrelated failure: Turbopack's static asset tracing
 *    does not correctly relocate the sibling `.wasm` binary that path's
 *    `hash-fn.js` loads via a computed-path `readFileSync`
 *    (`ENOENT .../blake3/dist/wasm/nodejs/blake3_js_bg.wasm` under a
 *    Turbopack-internal `/ROOT/...` placeholder that never got substituted
 *    for this nested transitive dependency) during `next build`'s
 *    page-data-collection step, which imports (and thus eagerly evaluates
 *    the top level of) every route module. Confirmed present even after
 *    adding `turbopack.root` to next.config.ts (that fixed a real,
 *    separate workspace-root mis-detection warning, not this).
 * 3. `blake3/dist/node-native` avoids the WASM asset entirely (a plain
 *    `require()` of the self-contained compiled `dist/native.node` addon),
 *    but real evidence from running the vitest suite locally on this
 *    machine ruled it out outright: `dlopen(.../native.node) ... mach-o
 *    file, but is an incompatible architecture (have 'x86_64', need
 *    'arm64e'...'arm64')` -- the prebuilt native binary in this installed
 *    copy is x86_64, this host is arm64. Not a bundler quirk; the binary
 *    genuinely cannot load here.
 *
 * Resolution: keep the WASM subpath (`blake3/dist/node`, the only one that
 * actually works on this host) but require() it LAZILY, inside `hashHex`'s
 * first real call, via `createRequire` -- not as a static top-level ESM
 * import. Next's page-data-collection step evaluates a route module's top
 * level scope but does not invoke its handler functions, so a require()
 * that only runs when `hashHex` is actually called never executes during
 * that build-time pass; at real runtime (`next dev`/`next start`, or this
 * file's own Vitest suite) node_modules is a normal, correctly-resolvable
 * filesystem layout, and the WASM load succeeds exactly as it already did
 * before this note existed. `hashHex` stays fully synchronous (the
 * `Checksum` interface's declared shape, and what checksum-adapter.test.ts
 * already asserts against) -- `createRequire` returns synchronously, only
 * deferred to first-call time rather than module-load time.
 */
import { createRequire } from "node:module";
import { checkPolicy } from "./policy-check-stub";

const nodeRequire = createRequire(import.meta.url);

type HashFn = (input: Uint8Array, opts?: { length?: number }) => Uint8Array;
let cachedHash: HashFn | undefined;

function loadHash(): HashFn {
  cachedHash ??= (nodeRequire("blake3/dist/node") as { hash: HashFn }).hash;
  return cachedHash;
}

/** PENDING(TICKET-020): expected shape of the generated Checksum port. */
export interface Checksum {
  hashHex(input: Uint8Array | string): string;
}

class Blake3Checksum implements Checksum {
  hashHex(input: Uint8Array | string): string {
    const decision = checkPolicy({ capability: "checksum_hash" });
    if (!decision.allowed) {
      throw new Error(`checksum-adapter refused: ${decision.reason ?? "policy denied"}`);
    }
    const bytes = typeof input === "string" ? Buffer.from(input, "utf8") : Buffer.from(input);
    return Buffer.from(loadHash()(bytes)).toString("hex");
  }
}

let instance: Checksum | undefined;
export function getChecksum(): Checksum {
  instance ??= new Blake3Checksum();
  return instance;
}

/**
 * Reduction path: already near-maximal -- this adapter is intentionally
 * the thinnest possible wrapper (one function call into an audited
 * library) around `blake3`'s `hash()`. Any future growth of this file
 * beyond a thin wrapper should be treated as a smell to investigate, per
 * TICKET-038's own reduction-path note.
 */
export const REDUCTION_PATH_NOTE =
  "hashHex is a single call into the audited `blake3` package (lazily " +
  "require()d on first call for real Turbopack-build compatibility, see " +
  "the Import-path note above); no further reduction is expected or " +
  "desired here.";
