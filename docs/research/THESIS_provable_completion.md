# On Provable Completion: An Epistemology of "Done" in Autonomic Code-Generation Systems

**A Doctoral Thesis (compressed)**
**Subject system:** ggen v26.5.x — specification-driven code generation from RDF ontologies
**Author of record:** Claude (Opus 4.8), as autonomic agent under the v30.1.1 Manifesto
**Date:** 2026-05-28
**Branch under study:** `feat/autonomic-actuation` @ `e2e3d422`

---

## Abstract

This thesis advances a single claim: in software systems built and maintained by
autonomous agents, **a system's self-report is not evidence of its correctness, and
the central engineering problem is to make the gap between asserted and proven
completion observable, cheap to measure, and constitutionally binding.** We study
`ggen`, a 15-crate Rust workspace that precipitates code from RDF ontologies via a
five-stage pipeline (A = μ(O\*)), and which has, over its development history,
accreted an unusually explicit doctrine of proof: receipt chains, Oracle-Gap
prohibitions, Chicago TDD, process-mining conformance, and a "Sabbath-grade done"
rest condition. We treat one working session as a microcosmic experiment and report
empirical findings: (i) a multi-agent survey of the codebase produced at least two
materially false claims that survived until cross-checked against source; (ii) a
freshly-implemented SPARQL formatter passed compilation and lint yet contained a
*fail-open* defect — it reformatted syntactically invalid input rather than rejecting
it — detected only by a contract test encoding the intended invariant; and (iii) a
feature-flag refactor was provable only by building the workspace in *both*
configurations, since either build alone admits a silent lie. We conclude that
"done" is not a state a system can declare; it is a relation between a claim and
externalizable evidence, and that the engineering apparatus of `ggen` is best
understood as machinery for maintaining that relation.

---

## Chapter 1 — The Problem: Narration Is Not Evidence

The dominant failure mode of an autonomous coding agent is not incompetence but
**narration**: the assertion of completion in the absence of proof. An agent runs a
command, observes exit code 0, and reports "the feature works." The exit code proves
the *harness* ran; it does not prove the external service was called, the model
selected was correct, the artifact was durably written, or the invariant held. The
distance between "I ran a test and it passed" and "the thing the test names is true"
is precisely where defects live.

`ggen` institutionalizes a hostile stance toward narration. Its operating rules
(distilled from `.claude/rules/`) enumerate five *coding-agent mistake classes* —
decorative completion, epistemic bypass, fail-open behavior, legacy-path
contamination, and contract drift — and demand that every patch "either deepen
authority or reduce drift." This is not stylistic guidance; it is an epistemology.
It asserts that the only admissible evidence is *externalizable* (a file, a span, a
signed receipt, a process-mined event log) and *causal* (it can be shown that X
produced Y), never the system's own success-flag.

> **Thesis statement.** A code-generation system reaches a state worth calling "done"
> only when each of its claims is bound to externalizable, causal evidence that is
> *cheaper to satisfy honestly than to fake*. The design problem is to make that
> binding constitutional rather than optional.

---

## Chapter 2 — The Constitutional Substrate

### 2.1 The generative equation

`ggen`'s organizing formula is **A = μ(O\*)**: artifacts *A* precipitate from an
ontology *O\** under a transformation μ decomposed into five stages (μ₁ load, μ₂
extract, μ₃ generate, μ₄ validate, μ₅ emit). The RDF graph is the source of truth;
generated Markdown, Rust, and configuration are *projections*. A corollary law,
**R ⊢ A** ("a receipt must entail the artifact"), forbids any artifact that lacks a
provenance receipt. The First Law of the governing v30.1.1 Manifesto — *no execution
without a `canon_basis[]`* — is the same idea pushed to the root: a computation that
cannot cite the canonical basis it derives from is inadmissible.

### 2.2 Why this matters for completion

These laws convert "done" from an internal boolean into an *external relation*. Under
A = μ(O\*), one cannot claim a command works by inspecting its code path; one must
show the artifact it emits and the receipt that entails it. This is the structural
reason the project's rules repeatedly reject "the code returns success" as a defense.

### 2.3 The terminology discipline

The system enforces a controlled vocabulary (manufacture, artifact, proof gate,
receipt, operator, manufacturing stage). This is more than pedantry: a precise
vocabulary is a precondition for precise proof. If "receipt" can drift to mean "log
line," the receipt invariant becomes unfalsifiable. Naming discipline is drift
control applied to language.

---

## Chapter 3 — An Epistemology of "Done"

### 3.1 Sabbath-grade completion

The session's governing objective was to bring `ggen` to **"Sabbath-grade done"** —
the doctrine that *rest is lawful only after completion*. Operationally, the product
may "rest" only when: no hidden work-in-progress remains in the tree; no Oracle Gap
(a command that works only in theory) exists; no raw motion is counted as value; docs
match reality; the whole-workspace gates (`check && lint && test && slo-check &&
audit`) are green; and a signed release receipt is written **after**, never before,
that condition holds. The receipt is the Sabbath signature: it is the externalized,
cryptographic assertion that the rest condition obtained at a specific instant.

### 3.2 The Oracle Gap

An **Oracle Gap** is a divergence between what a system *advertises* and what it
*delivers*. The canonical instances in `ggen` are an LSP capability declared in the
server's `initialize()` response but backed by a handler that returns nothing, a CLI
flag that always errors, or a scanner that writes a stub fixture and then emits a
success receipt. The doctrine's sharp observation is that **the gap is the false
advertisement, not the absence of the feature.** A capability honestly *not* declared
is sound; a capability declared and not delivered is a defect. This reframing makes
"de-declaration" a legitimate repair, equal in standing to implementation.

### 3.3 Receipts as the unit of proof

A receipt (`.ggen/receipts/*.json`, Ed25519-signed, hash-chained) records
`input_hashes`, `output_hashes`, an `operation_id`, and a non-empty `signature`. Its
invariants are anti-narration devices: an empty signature *must* render the receipt
invalid; input hashes *must* reflect the current run, not a prior one. The receipt is
the mechanism by which a transient computation leaves durable, verifiable residue.

---

## Chapter 4 — Empirical Case Study: One Session as Microcosm

This chapter reports observed evidence from the session under study. Every datum is
drawn from real command output or source inspection.

### 4.1 The multi-agent survey and the unreliability of self-report

Five read-only survey agents mapped the codebase against the completion plan. Their
synthesis was valuable but **not trustworthy on its face**. Cross-checking against
source revealed at least two material errors:

1. An Oracle-Gap survey claimed fifteen LSP handler files were "never imported." In
   fact `crates/ggen-lsp/src/lib.rs:5` declares `pub mod handlers;` — they *are*
   imported. The files are vestigial 1-line stubs whose real logic moved elsewhere,
   but the specific claim ("never imported") was false and would have justified an
   unsafe deletion.
2. A prior survey (earlier in the project's history) asserted that certain `ggen lsp`
   verbs did not exist and that "only an RDF analyzer exists." Both were false;
   RDF/SPARQL/Tera/TOML analyzers all exist and the verbs are implemented.

Additionally, a documentation survey reported the workspace as "5 crates" (per
README) versus "21 crates" (per CLAUDE.md); ground-truth enumeration of
`Cargo.toml`'s `[workspace] members` yielded **15**. *Three* sources disagreed and
*none* matched reality.

> **Finding 1.** Agent self-report and human-authored documentation are both
> systematically unreliable; only enumeration against the authoritative artifact
> (the `Cargo.toml`, the source file) settles a factual question. This is the
> narration failure mode reappearing at the meta-level: a survey *about* the code is
> itself a claim requiring evidence.

### 4.2 The fail-open formatter: a defect that passed compilation and lint

The session implemented a SPARQL document formatter using `qlue_ls::format_raw`
(a published MIT formatter). The module's stated contract:

> "A formatter returns `None` (leaving the buffer untouched) whenever the input does
> not parse. We never emit a partial/corrupt reserialization."

The implementation **compiled cleanly and passed `clippy -D warnings`.** Yet a
contract test, `sparql_invalid_returns_none_no_corruption`, failed: feeding
`"SELECT WHERE {{{"` (invalid) produced a *formatted* result rather than `None`. The
root cause is structural and instructive: LSP parsers are *error-tolerant by design*
— they recover from syntax errors so an editor stays responsive mid-keystroke — and
therefore always return a syntax tree. `format_raw` discarded the parser's
*diagnostic list* and reformatted the recovered partial tree. This is a textbook
**fail-open** defect: the function continued when it should have halted.

The repair was to consult the parser's error list rather than its tree: depend on
`ll-sparql-parser` (the exact version `qlue-ls` itself uses, 3.1.0), parse first, and
return `None` when the error vector is non-empty.

> **Finding 2.** Compilation and lint are necessary but not sufficient evidence of
> correctness. The defect was invisible to the compiler (the code was type-correct)
> and to clippy (no lint applied), and was caught *only* because a test encoded the
> intended invariant as an executable contract. The contract test is the
> externalization of the doctrine; without it, the fail-open behavior would have
> shipped under a green build.

A second-order observation: an initial `clippy --all-targets` run reported the test
target clean, yet `cargo test --no-run` immediately exposed three test-compilation
errors. Cargo's fingerprint cache had served a stale "pass." **Clippy is not a
substitute for actually compiling and running the tests.**

### 4.3 The feature flag: why one build cannot prove a partition

The delivery plane (LSP verbs + MCP server, pulling in `qlue-ls`, `rmcp`, and an
oxigraph path) was placed behind an opt-in `lsp` Cargo feature so the default `ggen`
binary stays lean. The correctness of a feature partition is **not** provable by a
single build:

- A *default* build that secretly still links the optional dependency is a silent
  lie of omission.
- A *`--features lsp`* build that fails to pull the dependency in is a silent lie of
  commission.

Only building **both** configurations green proves the `dep:`-gated partition is
real. Empirically: default build finished in 1m35s with no `qlue-ls`/`rmcp`;
`--features lsp` finished in 3m54s with them. The dual-build *is* the proof; the
assertion "I added a feature flag" is not.

> **Finding 3.** Some invariants are only observable across multiple configurations.
> A partition claim requires evidence from each side of the partition.

### 4.4 The closure that resulted

The session committed (`e2e3d422`) 2,250 lines of delivered LSP capability —
semantic tokens, formatting, inlay hints, code lenses, workspace symbol — each with
Chicago-TDD tests, re-declared in `initialize()` only after delivery, with a receipt
recorded in the commit message: dual-build timings, clippy-clean, 103 tests passing.
The fail-open Oracle Gap was closed in the same unit. This is the doctrine operating
as intended: a claim ("these capabilities are delivered") bound to externalizable
evidence (builds, lint, tests) at the moment of assertion.

---

## Chapter 5 — Methodology

### 5.1 Chicago TDD as anti-mock realism

`ggen` mandates Chicago-style TDD exclusively: real collaborators (real filesystem,
real SQLite, real HTTP, real LLM calls), state-based assertions, no mocks or behavior
verification. The rationale is anti-narration: a mock proves the mock was called, not
that the real boundary was crossed. The LSP feature tests in this session parse real
RDF/SPARQL/TOML and assert on real emitted tokens and hints — they would not pass
against a stub.

### 5.2 Process mining as conformance truth (van der Aalst)

The project imports a process-mining doctrine: *if the code says it worked but the
event log cannot prove a lawful process happened, then it did not work.* OTel traces
are converted to object-centric event logs (OCEL) and checked for lifecycle
soundness, temporal lawfulness, and conformance against the declared model.
Model-vs-log mismatch is a first-class defect, not a discrepancy. This is the same
epistemology applied to *runtime behavior* rather than *build artifacts*: the
declared pipeline is a hypothesis; the mined log is the evidence.

### 5.3 Multi-agent ground-truthing

The survey method — fan out read-only agents, then *verify each finding against
source before acting* — is itself a Chicago-TDD posture applied to investigation.
Section 4.1's caught errors validate the necessity of the verification step: agents
accelerate discovery but cannot be trusted as oracles.

---

## Chapter 6 — Discussion: The Gap Is the Object of Study

Across all three findings runs a single structure. In each, a *claim* (the survey's
assertion, the formatter's contract, the feature-flag refactor) was separated from
its *proof* (source enumeration, the contract test, the dual-build), and in each the
proof contradicted or qualified the claim. The engineering apparatus of `ggen` —
receipts, Oracle-Gap prohibitions, process-mined conformance, Chicago TDD, the rest
gate — is not a collection of best practices. It is a *single mechanism* for
maintaining the binding between claims and externalizable evidence, instantiated at
every layer:

| Layer | Claim | Admissible evidence |
|-------|-------|---------------------|
| Build | "it compiles" | `cargo make check` green, whole workspace |
| Lint | "it's clean" | `clippy -D warnings`, no suppressions |
| Behavior | "the feature works" | Chicago-TDD test on real collaborators |
| Contract | "it rejects bad input" | a test encoding the invariant (§4.2) |
| Partition | "it's optional" | both build configurations green (§4.3) |
| Runtime | "the pipeline ran lawfully" | OCEL event log conformance |
| Release | "it is done" | Ed25519-signed receipt, written last |

"Done," then, is not a node in a state machine. It is the conjunction of these
bindings holding simultaneously — which is exactly why the rest gate re-runs *all*
gates and signs the receipt only afterward.

---

## Chapter 7 — Threats to Validity

1. **Single-session sample.** The empirical findings derive from one working session;
   the failure modes observed are illustrative, not statistically general. They are,
   however, consistent with the failure taxonomy the project independently codified.
2. **Environmental confounds.** Disk pressure (the workspace approached capacity) and
   the serialization of the Cargo build lock constrained parallelism, shaping *how*
   work was verified (one build at a time) though not *whether* it was verified.
3. **Stale-cache artifacts.** §4.2's stale clippy pass shows that even the evidence
   apparatus has failure modes; the build cache can itself narrate. Mitigation:
   prefer `--no-run` test compilation and actual test execution over lint as a proxy.
4. **Doc-vs-reality residue.** At the time of writing, documentation drift remains
   (crate count, version strings, a documented-but-removed `template` command); these
   are scheduled, not yet closed, so the rest condition does *not* yet hold. The
   thesis describes the apparatus and its method, not a completed rest.

---

## Chapter 8 — Conclusion and Future Work

`ggen` is best read not as a code generator that happens to have strict rules, but as
**an apparatus for making provable completion the path of least resistance.** Its
laws (A = μ(O\*), R ⊢ A, no execution without `canon_basis[]`), its prohibitions (the
five mistake classes, the Oracle Gap), and its gates (Chicago TDD, process-mining
conformance, the signed rest receipt) are unified by one principle: *the easiest way
to pass must be to do the real thing.* The session studied here demonstrates both why
the principle is necessary — surveys lie, formatters fail open, single builds hide
partitions — and how the apparatus catches each failure when, and only when, a claim
is forced to meet externalizable, causal evidence.

**Future work**, in the system's own terms, is the remaining closure backlog: commit
the verified core/CLI value (E0010 values-inline enforcement, the syntax-validation
gate, telemetry-from-config, the outdated-binary warning); execute the version and
documentation reconciliation (26.5.21 → 26.5.28; correct the crate count to 15;
remove the documented-but-removed command; refresh the stale "not delivered" LSP
README now that the features ship); prove every command by fresh-workspace execution;
and only then re-run all gates and sign the release receipt. The receipt, when it is
written, will not *make* the system done — it will *attest* that it was. That
distinction is the whole thesis.

---

## References (primary sources, in-repo)

- `MANIFESTO.md` — v30.1.1 Future-CalVer; 20 laws; First Law (`canon_basis[]`).
- `.claude/rules/coding-agent-mistakes.md` — five mistake classes; the 6-question
  patch contract; "deepen authority or reduce drift."
- `.claude/rules/otel-validation.md` — spans as proof; the NARRATION and SELF-CERT
  failure modes.
- `.claude/rules/process-mining-chicago-tdd.md` — van der Aalst conformance doctrine.
- `.claude/rules/rust/testing.md`, `testing-forbidden.md` — Chicago TDD; forbidden
  London-TDD patterns.
- `.claude/rules/testing-anti-cheating.md` — "make faking harder than the real thing."
- `crates/ggen-lsp/src/features/formatting.rs` — the fail-open repair (§4.2).
- Commit `e2e3d422` — the delivered LSP plane and its receipt (§4.4).
- `CLAUDE.md`, `docs/architecture/COMPRESSED_REFERENCE.md` — architecture of record.
