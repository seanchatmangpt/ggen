# star-toml-pack — Target-API fidelity check

**Dated hand verification (2026-07-18)** against the real `star-toml` crate
at `/Users/sac/star-toml` (`Cargo.toml`: `version = "26.7.3"`), the pack's
one external dependency named in `stp:loaderFn`.

## What was checked

`templates/star_toml_config.rs.tmpl`'s generated `StarTomlConfig::load`
delegates to the loader named by `stp:loaderFn` (`star_toml::load_file`).
The real function, `/Users/sac/star-toml/src/loader.rs:389`:

```rust
pub fn load_file<T: DeserializeOwned>(path: impl AsRef<Path>) -> Result<T> {
    let path = path.as_ref();
    let content = read_file(path)?;
    let expanded = expand_env_vars(&content);
    parse_str(&expanded, &path.display().to_string())
}
```

Signature match confirmed: generic over `T: DeserializeOwned`, takes
`impl AsRef<Path>`, returns `star_toml::Result<T>` (`Result<T, star_toml::Error>`
after crate-level type alias resolution) — matches the generated
`fn load(path: &std::path::Path) -> Result<Self, star_toml::Error>` call
site exactly (a `&Path` satisfies `impl AsRef<Path>`).

`star_toml::Error` (the error type the generated `load` signature names)
is defined in `/Users/sac/star-toml/src/error.rs` as the crate's public
error enum — confirmed present, not assumed.

## What this does and does not prove

This is an **L2**-level check per `docs/packs/PACK_MATURITY_MODEL.md`
("verified by hand against one target version, once, dated note") — a
real, dated, evidenced comparison against source, not a README
transcription. It is explicitly **not** L5: fidelity here is still
*checked* (this file), not *definitional* (the ontology does not derive
from `star-toml`'s own published API/ontology such that drift is
structurally impossible). If `star-toml` renames or changes the signature
of `load_file` in a future version, this pack's `stp:loaderFn` fact and
this note both go stale silently — nothing in this repo re-verifies the
match automatically. Closing that gap to L5 would require `star-toml`
itself to publish a machine-readable contract (e.g. its own RDF/OpenAPI-ish
schema of its public loader surface) that this pack's ontology could
import instead of naming a bare string — `star-toml` is an external
upstream project this repo does not own, so that step is out of this
pack's reach on its own.

## Re-verification (2026-07-19)

The 2026-07-18 check above compared against `/Users/sac/star-toml/src/loader.rs`
(a local working checkout), not the actual registry artifact this pack's
`stp:cargoDependency` fact (`star-toml = "26.7"`) resolves to at build time.
Closed that gap for real this session:

1. `diff /Users/sac/star-toml/src/loader.rs
   ~/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/star-toml-26.7.3/src/loader.rs`
   → **empty diff, exit 0** — the local checkout and the actual
   crates.io-published `star-toml` v26.7.3 source are byte-identical for
   this file, so the 2026-07-18 signature check does describe the real
   dependency, not just a local mirror.
2. Built a real scratch consumer (`Cargo.toml` depending on `star-toml =
   "26.7"` from crates.io, no path override) at
   `/private/tmp/.../scratchpad/star-toml-verify`, wired `ggen.toml` to this
   pack, ran `ggen sync run` (real binary, `target/debug/ggen`,
   `ggen@26.7.13`), then `cargo test` under the workspace's pinned
   `nightly-2026-06-22` toolchain (required — `star-toml`'s own dependency
   tree pulls in `wasm4pm-compat` v26.6.29, which needs
   `#![feature(generic_const_exprs)]`, so this only builds on nightly, not
   stable).
3. Result: `cargo build` pulled `Compiling star-toml v26.7.3` from the real
   crates.io registry (confirmed in build output, not a path dependency),
   and the generated proof test passed **8/8** (`structural_shape_matches_ontology`,
   `load_round_trip_matches_written_literals`,
   `unknown_key_in_admission_is_rejected`,
   `missing_required_field_is_rejected`, `missing_file_is_rejected_not_panicked`,
   `in_range_sample_rate_passes_validate`,
   `out_of_range_sample_rate_parses_but_fails_validate`,
   `optional_field_omitted_deserializes_as_none`) — up from the 5/5 this
   file previously cited (the count grew because the proof template itself
   was already ahead of this doc, not because of any change made this
   session).

This remains **L2**, not L5, by the same reasoning as above: it is a dated,
evidenced hand-check against the real published artifact (now doubly
confirmed — local checkout matches registry, and the generated code
actually compiles and passes against the registry crate, not merely "reads
correctly" by inspection). It is still not *definitional*: nothing in this
repo would notice automatically if `star-toml` 26.8 renamed `load_file` or
changed its signature.

## What was NOT changed this session

No `ontology.ttl`, template, or generated-file content was modified. This
was a verification-only pass: the existing L2 Target-API-fidelity claim and
L3 test-generation claim in `docs/packs/PACK_MATURITY_MODEL.md` are now
re-confirmed against the real registry dependency (not just a local
checkout) rather than resting on the 2026-07-18 note's narrower claim.
