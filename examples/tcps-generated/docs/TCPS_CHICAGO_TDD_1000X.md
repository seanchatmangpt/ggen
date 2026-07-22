# TCPS product-evidence fabric

The original `tcps_chicago_tdd_1000x` test declared:

```text
43 capabilities × 4 surfaces × 7 laws = 1,204 obligations
```

but discharged every obligation by reading one file. A readable source file does
not prove that the capability is replayable, authority-separated, idempotent, or
implemented on every runtime and boundary surface. That Cartesian product was
therefore removed.

## Current evidence law

The replacement validates only claims that are actually observed.

### Product manifest

`infra/SOURCE_MANIFEST.sha256` must contain exactly 129 unique, relative paths.
Every digest must be a well-formed SHA-256 value.

### Capability population

The 43 declared TCPS capabilities are validated through their real artifact
formats:

- 26 core Rust modules;
- four runtime entrypoints;
- six release and packaging surfaces;
- four evidence surfaces;
- three manufacturing surfaces.

Rust is parsed with `syn`. YAML, JSON, JSONL, TOML, and XML are parsed with their
real parsers. The Python lifecycle program is syntax-compiled by Python, and the
reproducible-build script is checked by `bash -n`.

Forty-one capability artifacts are additionally bound to their exact SHA-256
entry in the product manifest. The two exceptions are the manifest itself and
`ggen.lock`, which have independent validators.

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

Every pack must use a path source and a syntactically valid BLAKE3 content hash.

### Evidence receipt

Each capability produces a content-addressed evidence record containing its
actual SHA-256 digest, observed format facts, manifest standing, and a
domain-separated BLAKE3 evidence digest. The 43 records are sorted by capability
identity and combined into a domain-separated Merkle root.

No occurrence timestamp participates in semantic identity.

## Removed antipatterns

- universal-law claims inferred from file existence;
- self-fulfilling 1,204-obligation arithmetic;
- FNV-1a represented as receipt-grade cryptography;
- hand-built JSON strings;
- nonconforming JSON labeled as OCEL;
- nonempty-output assertions used as semantic evidence;
- duplicated execution presented as coverage compression.

## Run

From `examples/tcps-generated`:

```bash
bash scripts/validate-1000x.sh
```
