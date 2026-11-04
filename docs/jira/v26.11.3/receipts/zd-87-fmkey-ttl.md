# ZD-87 — FM-KEY-013 TTL Declaration (Court Repair)

Standing: ok. 3 passed; 0 failed (court green post-declaration)

- **Date**: 2026-11-04
- **Lane**: zd-87 (create/update agent, repo `/Users/sac/ggen`, branch `v26-11-3-wave`)
- **Trigger**: ZD-44 flagged the court `fm_diagnostic_codes_ttl_matches_real_source`
  (`crates/ggen-engine/tests/product_mirror_conformance.rs:353`) failing because the
  FM-KEY-013 refusal (owner-only permission gate on a pre-existing `signing.key`,
  landed by xd-8) was not declared in `.specify/ggen-product.ttl`.

## Change

`.specify/ggen-product.ttl` — added after `prod:fm_KEY_12`:

```turtle
prod:fm_KEY_13 a prod:DiagnosticCode ;
    prod:family "KEY" ;
    prod:number "13"^^xsd:integer ;
    prod:citation "crates/ggen-engine/src/keys.rs:421" .
```

Shape matches the file's existing `DiagnosticCode` individuals (`family` / `number` /
`citation`; the registry doc carries the severity/band/remediation prose, the ttl does
not — grounded in `prod:fm_KEY_12` / `prod:fm_KEY_9`, not invented fields).

## Three-way cross-check

1. **Source literal** — `crates/ggen-engine/src/keys.rs:421`: `AppError::fm_key(13, ...)`
   inside `ensure_owner_only_permissions` (`#[cfg(unix)]`): fires when
   `mode & 0o077 != 0` on a pre-existing `signing.key`; message names the path and
   observed mode, "group/other accounts on this host could read the private ed25519
   seed. Remediation: `chmod 600` and rotate the key if it may have been exposed."
   Never auto-repairs (differs from `restrict_to_owner`).
2. **TTL declaration** — `prod:fm_KEY_13` (family KEY, number 13, citation
   `keys.rs:421`), added above.
3. **Registry row** — `docs/FM_REGISTRY.md:117`: `FM-KEY-013` | "pre-existing
   `signing.key` carries group/other permission bits (leaked seed, fail-closed, never
   auto-repaired)" | remediation "rotate the key: ... the refusal names the path and
   observed mode" | auto-repair `none`. Consistent with both surfaces above.

Agreement: all three surfaces describe the same fail-closed owner-only permission gate.

## Verification (real runs, this checkout)

```
cargo test -p ggen-engine --test product_mirror_conformance fm_diagnostic_codes_ttl_matches_real_source
→ run 1: ok. 1 passed; 0 failed (0.13s)
→ run 2: ok. 1 passed; 0 failed (0.10s)
cargo test -p ggen-engine --test product_mirror_conformance   (full file)
→ ok. 3 passed; 0 failed; 0 filtered (cli_verbs, pipeline_stages, fm_diagnostic_codes)
```

Pre-existing vs session-introduced: the court failure was pre-existing (missing ttl
declaration from xd-8's keys.rs change); this session's edit closes it. No other tests
touched; no git commands run.
