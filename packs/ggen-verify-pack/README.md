# ggen-verify-pack

Self-hosted verification gates: `ggen sync run` refuses (FM-PACK-013) when
external check evidence is missing, red, or stale. Verification stops being a
hand-run shell matrix and becomes admitted `ver:` graph facts, enforced by the
same pack-gate mechanism as every other invariant.

## What the pack ships

| Piece | File | Role |
|---|---|---|
| Vocabulary + policy | `ontology.ttl` | `ver:Check` / `ver:RequiredCheck` (each with a runnable `ver:checkCommand`) / `ver:KnownDivergence` |
| Gates | `gates/010..050*.rq` | missing / red / stale evidence, non-Green latest sync, output-count regression |
| Generated emitter | `templates/verify_evidence_sh.tmpl` | renders `scripts/verify-evidence.sh` into the consumer -- runs every required check, records REAL exit codes into `evidence/ontology.ttl` |
| Bootstrap copy | `bootstrap/verify-evidence-bootstrap.sh` | committed, non-generated copy of the emitter for the first run (see below) |
| Report | `templates/verification_report.md.tmpl` | renders `VERIFICATION.md` from admitted evidence |

## Consumer contract

1. Wire the pack and the local evidence mini-pack in `ggen.toml`:

   ```toml
   [packs]
   ggen-verify-pack = { path = "../../packs/ggen-verify-pack" }
   verify-evidence  = { path = "evidence", lock = false }  # regenerated every run; content-pinning it would refuse every re-emit (FM-PACK-008)

   [law]
   reflexive = true   # gates 030-050 query the reflexive sync history
   ```

2. Provide `scripts/checks/byte-identity.sh` (the one consumer-specific
   required check -- its `ver:checkCommand` delegates to this hook; exit
   nonzero on any non-allowlisted divergence). The other required checks are
   plain cargo commands declared as `ver:checkCommand` facts in this pack's
   ontology.

3. Run the emitter, then sync: `scripts/verify-evidence.sh && ggen sync run`.
   The emitter always exits 0 (it records reality, it does not judge it);
   judgment is the gates' job at the next sync.

## Two-phase bootstrap (the chicken-and-egg, solved explicitly)

The gates run on **every** sync -- including the first sync that would
generate `scripts/verify-evidence.sh` itself. A fresh consumer therefore has
no emitter and no evidence, and sync correctly refuses. This is by design
(refusal-first), so the bootstrap is explicit, not hidden:

```bash
# Phase 1 (once, from the consumer root): run the committed bootstrap copy.
# It scaffolds evidence/ (pack.toml + one template + ontology.ttl with real
# exit codes) by reading the check list from this pack's own ontology.ttl.
bash packs/ggen-verify-pack/bootstrap/verify-evidence-bootstrap.sh

# Phase 2: sync now passes the gates and generates the real emitter.
ggen sync run

# Steady state: always emitter-then-sync (the generated emitter projects the
# check list from the live union graph, superseding the bootstrap copy).
scripts/verify-evidence.sh && ggen sync run
```

Freshness semantics (gate 030): evidence is bound to the graph hash of the
latest completed sync; staleness detection lags one sync by construction (the
reflexive log only carries syncs 0..N-1 at load time). Running the emitter
immediately before every sync closes the window in practice.

## Proven by

- `crates/ggen-engine/tests/verify_pack_evidence_loop_e2e.rs` -- hermetic
  Chicago e2e: bootstrap -> sync green -> generated emitter re-run -> resync
  green -> evidence sabotage -> FM-PACK-013 refusal (real `bash` subprocesses,
  real generated script, real exit codes; cargo checks satisfied by a real
  stub binary on PATH, the house subprocess-test pattern).
- `examples/tcps-generated` -- the original full-strength consumer (its own
  `scripts/verify.sh` implements the byte-identity sweep inline).
