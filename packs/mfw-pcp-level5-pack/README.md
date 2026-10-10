
# MFW PCP Level-5 candidate pack

This is a complete **Level-5 candidate**, not an unverified Level-5 claim. It carries the twelve capabilities required by the current ggen promotion contract and refuses final admission until live evidence closes every capability.

## Run

```bash
cd packs/mfw-pcp-level5-pack
ggen sync run
cd consumer/mfw-pcp-generated
cargo build && cargo test
ggen receipt verify
```

## Produced consumer

The pack generates the complete bounded verifier crate: build manifest, certificate registry, obligation and distinction ledgers, receipt contracts, replay/standing verifier, positive tests, negative witnesses, documentation, provenance, CI, release standing, and verifier script.

## Standing (real, 2026-07-21 audit -- not this pack's own optimistic self-assessment)

`L5_CAPABILITY_LEDGER.json` and `evidence/*.json` are authoritative for this pack, and reflect a
live per-capability audit run against a real `ggen` binary (26.7.26), not a script that marks
every row `ALIVE` on completion. Result: **5/12 ALIVE** (cap01, cap02, cap03, cap04, cap09),
**7/12 PARTIAL_ALIVE** (cap05, cap06, cap07, cap08, cap10, cap11, cap12). `standing` is
`PARTIAL_ALIVE` and `level5_admitted` is `false`. See `.specify/pack-l5-promotion.ttl`'s
`l5p:pack_mfw_pcp_level5` individual (this repo's own L5 pack-closure promotion program) for the
full evidentiary citation, and `docs/l5-promotion/L5_PROMOTION_PROGRAM.md` for its generated
mirror.

Two capabilities have a real, reproduced falsifier hit, not just an unaudited gap: cap11
(Generated evolution path) breaks a generated test (`certificate_inventory_is_complete`,
hardcodes `assert_eq!(certificates::ALL.len(), 10)` instead of a SPARQL-derived count) when a
new `pcp:CertificateKind` is added -- exactly the example this capability's own falsifier names.
cap12 (Consumer replacement) only succeeds after fixing a critical, pack-wide defect: every
`templates/**/*.tmpl` file originally carried a leaked, un-stripped
`---\nto:...\nsparql:...\n---` frontmatter header belonging to ggen's OTHER, mutually-exclusive
"frontmatter" `ggen.toml` schema (this pack uses the declarative-rules schema, whose pipeline
never strips it), which broke `cargo build` on every generated output (invalid TOML at
`Cargo.toml:1:4`). The templates committed here already have that header stripped (a
semantically-inert fix -- the header's `to`/`force`/`sparql` keys duplicate `ggen.toml`'s own
`[[generation.rules]]` fields); see `evidence/consumer-replacement.json` for the full
reproduction and fix detail.

## Decisive verifier -- not carried forward from the source pack

An earlier `verify-level5.sh` script existed upstream that unconditionally set all twelve
capability rows to `ALIVE` and `standing`/`level5_admitted` to `ALIVE`/`true` the moment its own
(narrower) check sequence completed without error -- it never re-verifies each capability's own
named falsifier (e.g. its "semantic mutation" check only compares output digests, not whether the
regenerated consumer still builds and passes `cargo test`, which is exactly the check that caught
the real cap11 falsifier above). Trusting that script's output would have overclaimed Level 5
admission for a pack that demonstrably does not have it. It is deliberately not included in this
pack's landed copy; `L5_CAPABILITY_LEDGER.json` plus `evidence/*.json` (real command transcripts
and digests, not narration) are the authoritative standing record instead.
