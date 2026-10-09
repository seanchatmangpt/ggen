# Delta-Validation Spec

Status: PARTIAL_ALIVE. The invariant (Section 2), abort semantics
(Section 2.3), and per-language parse-back designs (Section 3) are
specified. All three language legs (Elixir, Rust, WASM) are implemented
and witnessed: the Elixir harness on a real ggen-generated artifact
(Section 6 receipt), the Rust and WASM extractors on hand-built sample
artifacts plus one real wasm4pm binary (Section 6b receipt). The
pipeline-hop question for WASM is resolved in Section 3.3.1: no
WAT/WASM template exists in ggen, so the WASM leg stays
witnessed-on-real-binary. The ggen Rust-artifact leg remains
UNVERIFIED — no Rust artifact was generated
through the ggen pipeline in the witnessing run; the Rust sample crate
is hand-built and marked as such.

## 1. Scope and epistemic status

This spec formalizes the translation-validation law **Delta(G) = 0**
for ggen's synthesis pipeline: the render+write stage behind
`ggen sync run`, implemented in
`/Users/sac/ggen/crates/ggen-engine/src/generation_rules.rs:161`
(`pub(crate) fn run(root, manifest, opts) -> Result<SyncReport>`).

One real, replayable demonstration backs the witnessed claims. A
ggen@26.9.28 run rendered one Elixir artifact from a 4-triple ontology
that declares exactly two exported functions; parsing the artifact back
yielded Delta(G) = empty and verdict ALIVE. A hand-mutated artifact
with one smuggled public function yields Delta(G) nonempty and verdict
ABORT. The run receipt, artifact, and delta report are frozen in
`fixtures/`. The Rust and WASM parse-back extractors are implemented
and witnessed (`delta_validate_rust.sh`, `delta_validate_wasm.sh`,
Section 6b receipt) — the Rust sample is hand-built and marked as
such. Everything else — write-stage integration, mix-task packaging,
CI wiring — is spec-proposed (UNVERIFIED here); each such item is
labeled. No number in this document was produced by execution
unless it appears in the Section 6 receipt or is cited with its
command.

## 2. The invariant

### 2.1 Graphs

- **G_original**: the RDF assertion graph consumed by the render
  stage — the ontology closure loaded by the pipeline. For the demo,
  the 4-triple ontology in `fixtures/ontology.ttl` (BLAKE3
  `e8c22199d881af09b09fd373e54d7f9795539fa700fb692551986fef57b71ac2`,
  from the real sync receipt).
- **G_recovered**: the assertion graph recovered by parsing the
  emitted artifact back into RDF assertions. For the Elixir surface,
  each exported function `M.f/a` becomes the assertion
  `ex:echoAgent ex:export "f/a"` against the module's
  `ex:moduleName` subject — exactly the triple shape the ontology
  used to declare the surface. Recovery runs at any point after the
  write stage.

### 2.2 Statement

Delta(G) := G_recovered \ G_original, computed over canonical triple
form. The law: for every artifact emitted by `ggen sync run`,
Delta(G) = empty.

Equivalently: any triple in the artifact absent from the source
ontology contract aborts the build pre-qualification. A template that
emits `def smuggled_admin_backdoor/1` (see `fixtures/evil.ex`) has no
ontology provenance, so G_recovered gains a triple G_original does not
contain, Delta(G) is nonempty, and the build aborts.

The converse direction (G_original \ G_recovered, an ontology-declared
export missing from the artifact) is a *completeness* violation, not a
Delta violation; the harness reports it in the per-surface counts but
does not abort on it. Extending abort to both directions is future
work (Section 7).

The law is checked post-write, pre-qualification. Integration point:
immediately after the write stage inside
`generation_rules.rs:161`'s run, keyed by artifact language
(`.ex` -> Elixir harness; `.rs` -> rustc; `.wat/.wasm` -> wat2wasm).

### 2.3 Abort semantics

Abort is a transition refusal, not a warning:

1. exit 1 from the validation step;
2. a typed delta report naming each delta triple
   (`fixtures/delta-report.txt` is the observed shape);
3. no partial admission: the run's `SyncReport`/receipt is not
   admitted into the receipt chain (`.ggen-v2/receipt.json`), so
   nothing downstream consumes an unvalidated artifact set;
4. remediation is editing the ontology or the template — never the
   emitted artifact (the artifact is a projection; hand-editing it is
   out of bounds by repo doctrine).

### 2.4 Negation-as-failure boundary

The recovery function sees only what the toolchain chunk carries. An
entity the chunk does not represent (an Erlang anonymous fn created at
runtime, a Rust `#[no_mangle]` symbol reachable only from a dependent
crate, a WASM import emitted by the embedder) is outside the
abstraction relation: what the parse-back cannot see, it cannot
refute. Delta(G) = 0 is sound with respect to the chunk, not with
respect to the full runtime behavior of the artifact.

## 3. Per-language parse-back design

Projection function pi_lang: artifact bytes -> set of surface
entities. Delta(G) is computed post-pi. Compiler-injected entities are
part of pi (normalization), not part of Delta. For Elixir that
allowlist is `__info__/1`, `module_info/0`, `module_info/1` — the
Erlang/Elixir compilers inject these into every module regardless of
the ontology. This is a bounded, known exception class: a template
emitting a hand-written `__info__/1` is indistinguishable from the
compiler-injected one and would pass; anything else smuggled through
the template is caught.

### 3.1 Elixir/Erlang (witnessed)

Toolchain anchor: the Erlang/OTP `abstract_code` chunk of the compiled
BEAM, read via `:beam_lib.chunks(beam, [:abstract_code])`, returning
`{:ok, {mod, [abstract_code: {:raw_abstract_v1, forms}]}}` (observed
on OTP 28 / Elixir 1.19.5).

Surface extraction: for each `{:attribute, _Ann, :export, Fns}` form
in the raw abstract forms, each `{Name, Arity}` yields the entity
`M.f/a`. `Code.compile_string/2` (Elixir 1.19) returns
`[{mod, beam_binary}]`; the `Elixir.` prefix on module atoms is
stripped before comparison with ontology literal strings. Only
`:export` attribute forms are read — a `defp` can never appear there,
so the recovered set is exactly the caller-touchable surface.

### 3.2 Rust (witnessed, 2026-10-09 — design corrected by evidence)

The spec-proposed anchor was validated against the real toolchain and
two parts of it are **refuted by execution**:

1. `rustc --emit=metadata` produces a bare `.rmeta` file that `nm`
   cannot read at all ("The file was not recognized as a valid object
   file"). The metadata+nm fallback as originally written cannot work;
   metadata-only emission has no symbol surface.
2. `nm -g` on a *full* rlib works for some crates, but is unreliable on
   this host: Apple `nm` (Xcode CLT) and homebrew `llvm-nm` (LLVM 21)
   both refuse certain LLVM-22 object members with
   "Unknown attribute kind (102/105)" — a two-function crate read fine
   while a third crate (same compiler) using std alloc glue was
   unreadable to both. `nm` is a degraded fallback only, never primary.

Primary anchor (witnessed): `rustdoc --output-format json -Z
unstable-options` (nightly-only). Public items are machine-extractable
from the JSON index (`visibility == "public"`), giving the surface as
`crate::item` paths without any object parsing. Requires a nightly
rustdoc; `delta_validate_rust.sh` retries through the rustup-active
nightly if the ambient toolchain is stable (outside the ggen tree the
rustup default is stable 1.97 — witnessed).

Witnessed surface: `crate::item` paths, top-level items only; nested
module paths and `#[no_mangle]` extern surfaces are future work
(Section 7).

### 3.2.1 ABB/proof-test tie-in (spec-proposed, UNVERIFIED)

ggen's own surface already has the ontology side enumerated by gates
(`every-binding-has-template.rq`,
`every-binding-has-output-pattern.rq`, `every-command-has-handler.rq`
under `/Users/sac/ggen/.specify/gates/`); the Delta harness for Rust
completes the artifact side. Until the extractor is wired into the
write stage, Rust artifacts are UNVERIFIED under this law (labeled,
not hidden) — the extractor itself is witnessed (Section 6b), the
pipeline integration is not.

### 3.3 WASM (witnessed, 2026-10-09)

Anchor: the WebAssembly text format, exactly as spec-proposed — the
design survived execution unchanged. `wat2wasm mod.wat -o /dev/null`
(wabt 1.0.42) is the syntax gate; binary `.wasm` goes through
`wasm2wat` first; the surface is the `(export "name")` clauses,
extracted by a line-oriented parse of the wat text. Witnessed on:

- hand-built `.wat` sample (2 exports) — ALIVE; adversarial `.wat`
  with a smuggled export — ABORT naming it;
- binary path: a real wasm4pm artifact
  (`/Users/sac/wasm4pm/wasm4pm/pkg/wasm4pm_bg.wasm`, sha256
  `45d5b982ce26f36eca11055f37d34ef88b9fcf83d57b9d0b08813b4c0da7e76a`,
  730 distinct export names) — ALIVE; a recompiled binary with one
  smuggled export — ABORT naming it.

praxis-graphlaw tie-in unchanged: the recovered export set is diffed
against the law object's declared API in
`crates/praxis-graphlaw/src/lib.rs` (not exercised in the witnessing
run; integration UNVERIFIED).

### 3.3.1 Pipeline hop: template gap (resolved 2026-10-09 — gap named, not closed)

The last pipeline hop for WASM — render a `.wat` module **through
`ggen sync run`** and run `delta_validate_wasm.sh` on the true render
(ALIVE) and a pipeline-rendered adversarial variant with a smuggled
export (ABORT) — is **not executed**, because no WAT/WASM template
exists anywhere in ggen's pack inventory. Established by direct
filesystem sweep on 2026-10-09 over `packs/*/templates/` and the
local marketplace cache (`~/.ggen`): zero `*.wat*` files, zero
`*wasm*.tmpl` templates, and no template body containing WebAssembly
text (`(module`, `(export`, `wat2wasm`). Nearest neighbors, neither of
which renders WAT/WASM bytes:

- `packs/tcps-wasm-pack/` — generates a Rust crate that *targets*
  `wasm32-*`; its templates emit `.rs`, not WAT.
- `packs/tcps-release-pack/templates/wasm_smoke_mjs.tmpl` (and the
  `*_sh_script.tmpl` companions) — Node/shell smoke tests that
  *load* a prebuilt `.wasm`; they consume binary, never produce it.

Consequence: the WASM leg remains witnessed on the hand-built `.wat`
samples plus the real wasm4pm binary (Section 3.3); WASM artifacts
emitted by the ggen pipeline remain a vacuous class — the pipeline
cannot currently emit one, so there is nothing to validate and the
hop is future work. Closing it requires first authoring a
`*.wat.tmpl` template + ontology declaring an export surface in some
pack, then re-running this section's two-case witnessing (ALIVE on
the render, ABORT on an ontology-unproven smuggled export) and
replacing this paragraph with the receipt. The Elixir leg's
`fixtures/` (ontology + `ggen.toml` + template + artifact) is the
shape to copy.

## 4. Harness

`delta_validate.exs` (this directory) implements the Elixir
parse-back. Real dependencies: the Elixir runtime only — no Mix
project, no deps, Chicago-style real collaborators (real compile, real
BEAM chunk read, real diff). Invocation:

    elixir delta_validate.exs <artifact.ex> <expected-surface.txt>
  [--report <path>]

Expected-surface format: one `Module.f/arity` per line, `#` comments.
The expected surface for a real project is the ontology-declared
surface; deriving it mechanically from the ontology (SPARQL SELECT
over `ex:export` literals) is the documented derivation; the demo
freezes the derivation result in `fixtures/expected-surface.txt`.

Implementation summary (all functions in `delta_validate.exs`):

- `recover/1`: `File.read!` -> `Code.compile_string/2` -> `[{mod,
  beam}]` -> `:beam_lib.chunks(beam, [:abstract_code])` ->
  `{:raw_abstract_v1, forms}` -> exports from `:export` attribute
  forms, `Elixir.` prefix stripped, compiler-injected allowlist
  rejected.
- `DeltaValidate.Main.run/3`: set difference
  `recovered -- expected`, verdict ALIVE iff empty (exit 0), ABORT
  iff nonempty (exit 1); `--report` freezes the report to a file.

## 5. Demo project (replayable)

All inputs frozen under `fixtures/` (and replayable from scratch):

- `fixtures/ggen.toml` + `fixtures/ontology.ttl` +
  `fixtures/agent.ex.tera`: a 4-triple ontology declaring
  `ex:echoAgent` with `ex:moduleName "Demo.Agents.Echo"` and
  `ex:export "init/1"`, `"handle_message/1"`, one generation rule
  rendering `agent.ex.tera` to `out/Demo.Agents.Echo.ex`.
- Replay: `cd <scratch-dir-with-those-3-files-in-shape> && ggen sync
  run --format json` (ggen@26.9.28). Real receipt from the witnessed
  run:

      {"written":["out/Demo.Agents.Echo.ex"],"skipped":[],
       "graph_hash_hex":"ad61f66ca699517ae701a40c8d60cdb34e07cb9b242a44f992181120b943f9a8",
       "decisions":{"out/Demo.Agents.Echo.ex":"written"},
       "closure":{"actuator":"ggen@26.9.28",
                  "ontology.ttl":
                    "e8c22199d881af09b09fd373e54d7f9795539fa700fb692551986fef57b71ac2",
                  "templates/agent.ex.tera":
                    "c58b05f7bad4bd24ba93160602054313affce85b125941241c8891d284984271"}}

- `fixtures/Demo.Agents.Echo.ex` — the generated artifact (byte copy
  of the run's output).
- `fixtures/expected-surface.txt` — the ontology-derived surface.
- `fixtures/delta-report.txt` — the harness report for the artifact.
- `fixtures/evil.ex` — adversarial artifact: same surface plus
  `def smuggled_admin_backdoor/1`; harness aborts naming the smuggled
  function.

## 6. Receipt (witnessed run, 2026-10-09)

Environment: ggen@26.9.28 (`/Users/sac/.local/bin/ggen`), OTP 28 /
Elixir 1.19.5, macOS (darwin). Commands and exits:

    cd /tmp/delta-demo
    ggen sync run --format json
      -> {"written":["out/Demo.Agents.Echo.ex"],"skipped":[],
          "graph_hash_hex":"ad61f66ca699517ae701a40c8d60cdb34e07cb9b242a44f992181120b943f9a8",
          "decisions":{"out/Demo.Agents.Echo.ex":"written"},
          "closure":{"actuator":"ggen@26.9.28",
                     "ontology.ttl":
                       "e8c22199d881af09b09fd373e54d7f9795539fa700fb692551986fef57b71ac2",
                     "templates/agent.ex.tera":
                       "c58b05f7bad4bd24ba93160602054313affce85b125941241c8891d284984271"}}

    elixir delta_validate.exs out/Demo.Agents.Echo.ex expected-surface.txt
      -> exit 0, report:
         delta-validation report
         subject: Demo.Agents.Echo
         G_original (ontology-declared surface): 2 exports
         G_recovered (abstract_code surface): 2 exports
         Delta(G) = G_recovered \ G_original = []
         VERDICT: ALIVE (Delta(G) = 0)

    elixir delta_validate.exs out/evil.ex expected-surface.txt
      -> exit 1, report ends:
         Delta(G) = ["Demo.Agents.Echo.smuggled_admin_backdoor/1"]
         VERDICT: ABORT (Delta(G) nonempty)

(Subject line prefix differs across runs by `Elixir.` stripping; the
frozen `fixtures/delta-report.txt` is the canonical observed bytes.)

Frozen fixture check commands:

    cd docs/specs/delta-validation
    elixir delta_validate.exs fixtures/Demo.Agents.Echo.ex fixtures/expected-surface.txt   # exit 0
    elixir delta_validate.exs fixtures/evil.ex fixtures/expected-surface.txt               # exit 1

## 6b. Receipt (Rust + WASM legs witnessed, 2026-10-09)

Environment: rustc/rustdoc 1.98.0-nightly (91fe22da8 2026-06-21,
rustup toolchain `nightly-2026-06-22-aarch64-apple-darwin`, pinned by
ggen's rust-toolchain.toml; rustup default outside the tree is stable
1.97.0 — witnessed); Apple nm (Xcode CLT, LLVM
APPLE_1_1700.6.3.2_0) and llvm-nm 21.1.7 both refuted as primary
anchors (Section 3.2); wabt 1.0.42 (wat2wasm/wasm2wat, homebrew);
macOS darwin.

Scripts: `delta_validate_rust.sh`, `delta_validate_wasm.sh` (this
directory). Fixtures: `fixtures-rust/` (hand-built sample crate,
marked as such — no Rust artifact was generated through the ggen
pipeline in this run) and `fixtures-wasm/` (hand-built `.wat`
samples; the real-binary witness is the wasm4pm artifact cited by
sha256, not frozen in-tree at 7.2 MB).

Commands and exits (all executed 2026-10-09):

    # Rust leg (rustdoc JSON primary anchor)
    bash delta_validate_rust.sh demo_agent.rs expected-surface.txt
      -> exit 0, VERDICT: ALIVE (Delta(G) = 0), 2/2 paths
    bash delta_validate_rust.sh evil/demo_agent.rs expected-surface.txt
      -> exit 1, Delta(G) = ["demo_agent::smuggled_admin_backdoor"],
         VERDICT: ABORT

    # WASM leg, hand-built text
    bash delta_validate_wasm.sh mod.wat expected-surface.txt
      -> exit 0, VERDICT: ALIVE (Delta(G) = 0), 2/2 exports
    bash delta_validate_wasm.sh evil.wat expected-surface.txt
      -> exit 1, Delta(G) = ["smuggled_admin_backdoor"], VERDICT: ABORT

    # WASM leg, real binary (wasm4pm, sha256 above)
    bash delta_validate_wasm.sh <wasm4pm_bg.wasm> <its own export set>
      -> exit 0, VERDICT: ALIVE, 730/730 exports
    bash delta_validate_wasm.sh <recompiled binary +1 smuggled export> <same expected>
      -> exit 1, Delta(G) = ["smuggled_admin_backdoor"], VERDICT: ABORT

Frozen fixture check commands:

    cd docs/specs/delta-validation
    bash delta_validate_rust.sh fixtures-rust/demo_agent.rs fixtures-rust/expected-surface.txt   # exit 0
    bash delta_validate_rust.sh fixtures-rust/evil_demo_agent.rs fixtures-rust/expected-surface.txt  # exit 1 (names smuggled_admin_backdoor)
    bash delta_validate_wasm.sh fixtures-wasm/mod.wat fixtures-wasm/expected-surface.txt          # exit 0
    bash delta_validate_wasm.sh fixtures-wasm/evil.wat fixtures-wasm/expected-surface.txt          # exit 1

Note on the negative Rust fixture: the recovered crate name derives
from the artifact filename (`crate::item`), so the adversarial artifact
must keep the same filename as the clean one — the fixture is frozen
as `evil_demo_agent.rs`; running it directly would report a full-surface
delta under crate name `evil_demo_agent`. The witnessed abort run used
`evil/demo_agent.rs`.

## 7. Alpha-gamma kernel hygiene error codes (grounded)

The alpha-gamma kernel (`crates/praxis-graphlaw/src/ggen_law.rs`) implements
the ontology hygiene laws as typed errors whose `Display` begins with a
stable string code so receipts and refusals can be matched on the code alone
(ggen_law.rs:51-96, checker `check_hygiene` at :195). These are the hygiene
refusal codes; they are not ARW/1 wire codes:

| string code | variant | meaning |
|---|---|---|
| `E_DOD9_COLLISION` | `Dod9Collision` (ggen_law.rs:56, display :73) | one IRI claimed two of Pack/ABB/SBB (pairwise disjoint) |
| `E_METAMODEL_HYGIENE_VIOLATION` | `MetamodelHygieneViolation` (:59, display :78) | a non-core source axiomatizes a core term, or a core term is redefined as an external class; wire alias formally reserved as 0xE009 (wire-protocol.md Section 6) |
| `E_AUTHORITY_ROOT_UNPINNED` | `AuthorityRootUnpinned` (:65, display :88) | an ea/togaf-family IRI outside the pinned authority root |
| `E_NAMESPACE_LEAK` | `NamespaceLeak` (:67, display :92) | a pack-internal term escaped into the domain projection; wire alias formally reserved as 0xEA01 (wire-protocol.md Section 6) |

## 8. Future work

- Abort on completeness violations (G_original \ G_recovered), not
  just soundness (Delta).
- Wire the harness as a mix task (`mix ggen.delta_validate`) or
  post-write hook inside `generation_rules.rs` (invoke
  `elixir delta_validate.exs` per emitted `.ex`; invoke
  `delta_validate_rust.sh` / `delta_validate_wasm.sh` per emitted
  `.rs` / `.wat`+`.wasm`).
- Generate a Rust artifact through the ggen pipeline and re-run the
  witnessed extractor on it (the Rust sample is hand-built today).
- Nested-module path reconstruction in the rustdoc-JSON extractor
  (current surface is top-level `crate::item` only), and a
  `#[no_mangle]` extern surface.
- Author a WAT/WASM template (`*.wat.tmpl` + export-surface ontology)
  in a pack, then close the WASM pipeline hop: render via
  `ggen sync run`, run `delta_validate_wasm.sh` on the true render
  (ALIVE) and an ontology-unproven smuggled-export variant (ABORT);
  see Section 3.3.1.
- praxis-graphlaw WASM tie-in: diff the recovered export set against
  the law object's declared API in
  `crates/praxis-graphlaw/src/lib.rs`.
- Ontology-side derivation of `expected-surface.txt` via the repo's
  SPARQL machinery instead of a frozen sidecar file.
