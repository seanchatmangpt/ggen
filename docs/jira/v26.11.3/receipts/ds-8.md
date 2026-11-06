# DS-8 Receipt — sjira CLI surface over the landed kernel

Standing: PASS

Date: 2026-11-04 · Lane: DS-8 · Repo: /Users/sac/ggen · Crate: ggen-cli-lib ·
Receipt: docs/jira/v26.11.3/receipts/ds-8.md

## Charter

Complete the `ggen sjira` CLI surface over the ggen-sjira kernel: verb
mapping (admit / frontier / execute / lease-admit / epoch-check; reconcile
deferred), typed refusals as exit codes, Chicago tests over real files.

## Diff

- `/Users/sac/ggen/crates/ggen-cli/src/cmds/sjira.rs` (moved from
  `cmds/sjira/mod.rs` — see FINDING-1) — five verb wrappers:
  - `admit <file>` — pre-existing verb, kept byte-comparable semantics
    (REFUSED:SEMANTIC_JIRA prefix preserved, rendering helper extracted).
  - `frontier --orders_path <f> [--events_path] [--evidence_path]
    [--authority_path]` — wraps `ggen_sjira::frontier`; no authority file
    ⇒ kernel fail-closed `authority_index_unavailable` for every order.
  - `execute --orders_path --ledger --authority_path [--max]
    [--lease_path] [--epoch_watermark_path] [--epoch_extensions_path]`
    — wraps `execute::run` over a REAL file ledger, including the
    mid-flight kernel growth from DS-10 (`ExecuteOpts.lease`,
    `.epoch_watermark`, `.epoch_extensions` landed under this lane while
    it ran; the CLI wires all three as optional file flags).
  - `lease_admit --lease_path --ontology_digest --keys_path` — wraps
    `lease::admit` (multi-party Ed25519). `SovereignLease`/`AdmittedLease`
    are Serialize-only, so the CLI owns the JSON decode (fail closed on
    missing fields). Keys file: signer → lowercase-hex Ed25519 verifying
    key (32 bytes); local `unhex` (no hex crate in dep tree).
  - `epoch_check --order_path [--watermark_path]` — wraps `epoch::check`.
- `/Users/sac/ggen/crates/ggen-cli/tests/cli_sjira_test.rs` — 17 Chicago
  tests: real files (TempDir), real kernel calls, real ledger IO;
  assertions on observable state (result JSON, ledger directory contents,
  typed refusal text).

No git commands were run.

## Exit-code law (picked and disclosed per charter)

- Exit 2 is SparqlError's (error.rs:154-166); exits 3/4 are other error
  families'. sjira emits NONE of these.
- Epoch-gate refusals carry the `REFUSED_STALE_LEASE` marker, mapped by the
  pre-existing v26.10.20 boundary in `crates/ggen-cli/src/main.rs` to
  exit 5 — the documented Andon class ("treat any exit-5 output as an
  Andon signal, not a retryable warning"). Witnessed at the binary:
  `epoch_check --order_path <order-with-epoch>` → exit 5, stderr
  `REFUSED_STALE_LEASE:SEMANTIC_JIRA: epoch gate refused
  (EPOCH_WATERMARK_UNAVAILABLE)...`.
- Every other typed sjira refusal exits 1 (the validation class per
  main.rs's everything-else mapping). Witnessed: malformed admit → 1.
- admit valid → exit 0 witnessed, JSON with authority=NONE + both digests.

## FINDING-1: directory-module nouns silently register zero verbs

`clap-noun-verb` derives the noun name from `file!()` at runtime
(clap-noun-verb-macros `extract_noun_name_from_file_context` → runtime
parse). A directory module (`cmds/sjira/mod.rs`) yields filename "mod", so
ALL five verbs (including SA-7's pre-existing `admit`) registered under no
reachable noun — `ggen sjira admit` failed with "unexpected argument
'admit' found", exit 1. `cmds/agent.rs` (file module) works. Fix: moved to
`cmds/sjira.rs`. Actionable elsewhere: audit for other directory-module
noun files (`cmds/*/mod.rs`) — every one silently has no CLI surface.

## Compose-or-defer (honest)

- `reconcile`: DEFERRED. DS-1's reconcile kernel API landed DURING this
  lane (reconcile.rs appeared mid-flight with its own compile errors,
  10→6→0 while this lane polled); it is exported from the kernel lib but
  had not been surveyed as a stable surface when this lane's verb set was
  fixed. `execute::run` already names the reconcile hop internally (the
  ledger append); a dedicated `ggen sjira reconcile` wrapper is the
  follow-on verb once DS-1's API settles.
- DS-10's lease/epoch gate fields on ExecuteOpts: COMPOSED (the CLI
  forwards them as optional file flags).

## Verification ladder (commands + exits)

1. `cargo check -p ggen-sjira` — exit 0 (after DS-1's in-flight compile
   freeze cleared; this lane polled, never edited another lane's file).
2. `cargo test -p ggen-cli-lib --test cli_sjira_test` — exit 0, **17
   passed / 0 failed** (final run).
3. `cargo clippy -p ggen-cli-lib --all-targets` — 0 findings in
   cmds/sjira.rs or cli_sjira_test.rs (E80-1's
   too_long_first_doc_paragraph finding did not regress; the four new verb
   doc comments are split to one-sentence first paragraphs).
4. Binary witnesses (real subprocess, `./target/debug/ggen`):
   - `sjira admit <valid order>` → exit 0, JSON: admitted true,
     authority NONE, both sha256 digests printed.
   - `sjira admit <malformed>` → exit 1, `REFUSED:SEMANTIC_JIRA:
     MalformedWorkOrder refused: Deserialization: unknown field 'epoch'
     ... remediation:` (typed remediation text on stderr).
   - `sjira epoch_check --order_path <epoch order>` (no watermark) →
     exit **5**, `REFUSED_STALE_LEASE:SEMANTIC_JIRA: ... `Andon.
   - `sjira --help` lists admit / epoch_check / frontier / execute /
     lease_admit — verbs genuinely registered (FINDING-1 fixed).

## Falsifiers that were run and killed this lane (fixture-vs-kernel-law)

Each failure below was a wrong fixture assumption, not a kernel bug; each
was corrected against the kernel's own law and the test now pins it:
- trust-root source_locator sha is 40-hex (not 64).
- authority node needs exactly one `admissionDigest` statement equal to
  `admission_digest(&node)` (which excludes that statement).
- chain-tip standing blocks re-selection (`standing=PARTIAL_ALIVE`) and
  stamped evidence unlocks dependents (`dependencies_unsatisfied` →
  eligible after the o1 event).
- fresh ledger path materializes as a DIRECTORY ledger.
- `manufacture_plan` maps path → "generated" | "residue".
- WorkOrder schema denies unknown fields — the epoch extension cannot ride
  inside `work_order` (pinned by the malformed-admit witness).
