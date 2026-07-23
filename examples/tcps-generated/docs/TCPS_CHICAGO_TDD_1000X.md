# TCPS product-evidence fabric

The original `tcps_chicago_tdd_1000x` test declared:

```text
43 capabilities × 4 surfaces × 7 laws = 1,204 obligations
```

but discharged every obligation by reading one file. A readable source file does
not prove replayability, authority separation, idempotency, refusal behavior, or
implementation on every runtime surface. That Cartesian product was removed.

## Current evidence law

The replacement validates only claims that are actually observed.

### Product manifest

`infra/SOURCE_MANIFEST.sha256` must contain exactly 129 unique, relative paths.
Every digest must be a well-formed SHA-256 value.

### Capability population

The 43 declared TCPS capabilities are validated through their actual artifact
formats:

- 26 core Rust modules;
- four runtime entrypoints;
- six release and packaging surfaces;
- four evidence surfaces;
- three manufacturing surfaces.

Rust is parsed with `syn`. YAML, JSON, JSONL, TOML, and XML are parsed with their
real parsers. The Python lifecycle program is syntax-compiled by Python, and the
reproducible-build script is checked by `bash -n`.

Reference standing is typed rather than collapsed:

- **36 exact-manifest capabilities** must match their declared product SHA-256;
- **5 declared generated derivatives** retain a source-manifest identity and an
  exact generated Git-object identity;
- **2 independent capabilities**—the manifest itself and `ggen.lock`—use their
  own validators.

The five declared derivatives are:

- `core.kaizen` (`改善.rs`);
- `core.crypto-digest` (`暗号要約.rs`);
- `core.auto-select` (`自動選択.rs`);
- `core.blue-river-dam` (`青い川のダム.rs`);
- `runtime.std` (`crates/tcps-std/src/lib.rs`).

The standard-library adapter differs only at the disclosed module/type collision
boundary: it imports `受領証_impl` rather than the reference module path.

Auto Select has an additional source-ownership proof. The generated Rust file must
be byte-identical to the single `tcps:sourceTextOverride` literal admitted by
`schema/domain.ttl`. A missing, duplicated, mutated, or unprojected override is a
loud refusal.

An exact capability that drifts is refused. A declared derivative that changes
without a new pinned identity is refused. A declared derivative that becomes
byte-identical is also refused until its standing is reclassified.

### Manufacturing projection law

The runtime, release, package, SBOM, and provenance templates preserve source
literal bytes exactly. A source literal already carries its terminal newline; the
Tera templates use right-trimmed interpolation so they do not manufacture a second
blank line. Eleven former “derivatives” were eliminated by correcting that source
law rather than weakening comparison.

### ggen composition lock

`ggen.lock` must contain exactly these eight admitted packs:

- `ggen-verify-pack`;
- `tcps-cli-pack`;
- `tcps-core-pack`;
- `tcps-evidence`;
- `tcps-ffi-pack`;
- `tcps-release-pack`;
- `tcps-std-pack`;
- `tcps-wasm-pack`.

Every pack must use a path source and a syntactically valid BLAKE3 identity. This
checks the complete lock population and identity format; it does not claim that the
pack contents were independently rehashed by this test.

### Evidence receipt

Each capability produces a content-addressed evidence record containing its actual
SHA-256 digest, Git-object identity, typed reference standing, reference digest
when applicable, observed format facts, and a domain-separated BLAKE3 evidence
digest. The 43 records are sorted by capability identity and combined into a
domain-separated Merkle root.

No occurrence timestamp participates in semantic identity.

## Removed antipatterns

- universal-law claims inferred from file existence;
- self-fulfilling 1,204-obligation arithmetic;
- derivative artifacts masquerading as exact identity;
- an expanding derivative allowlist used to hide generator whitespace defects;
- FNV-1a represented as receipt-grade cryptography;
- hand-built JSON strings;
- nonconforming JSON labeled as OCEL;
- nonempty-output assertions used as semantic evidence;
- duplicated execution presented as coverage compression;
- fail-fast admission that concealed the complete drift population.

## Run

From `examples/tcps-generated`:

```bash
bash scripts/validate-1000x.sh
```
