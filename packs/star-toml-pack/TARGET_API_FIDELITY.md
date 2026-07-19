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
