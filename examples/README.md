# ggen examples

`examples/` is the live executable corpus for ggen. A top-level directory is
**live** unless it is one of the explicitly fenced archive/meta roots below.

## Completion contract

A live example is admitted only when its top-level directory contains at least
one `ggen.toml` (directly or in a family subtree). The canonical verifier then:

1. discovers every live top-level directory from the filesystem;
2. discovers every `ggen.toml` beneath those directories;
3. executes `ggen sync run` from the manifest's own directory with a bounded timeout;
4. fails if any manifest execution fails; and
5. in check mode, fails if generation changes tracked files or creates untracked
   files under `examples/`.

That means new live example directories cannot be silently skipped. Adding a
directory without a `ggen.toml` makes the corpus verifier fail.

Run the full contract:

```bash
cargo build --locked -p ggen-cli-lib --bin ggen
GGEN_BIN="$PWD/target/debug/ggen" scripts/validate-examples.sh
```

Regenerate all live examples intentionally:

```bash
cargo build --locked -p ggen-cli-lib --bin ggen
GGEN_BIN="$PWD/target/debug/ggen" scripts/regenerate-examples.sh
git status --short -- examples/
```

`--write` leaves regenerated projections in the working tree for review. The
default validation mode requires byte-clean replay.

## Explicit exclusions

These top-level directories are preserved but are **not** part of the live
completion boundary:

- `.ggen/` — historical planning/meta material;
- `_archive/` — archived examples;
- `archive/` — archived examples and reports;
- `archive_2025/` — 2025 archive;
- `archive_ggen_core/` — retired ggen-core examples.

Everything else immediately under `examples/` is live and is covered by the
validator. Family examples such as `hygen/` may contain more than one nested
`ggen.toml`; every nested manifest is executed.

## Per-example verification

Some examples intentionally have stronger domain-specific CI (Rust tests,
Python verifiers, Lean proofs, product evidence, etc.). Those workflows remain
authoritative for their additional claims. The corpus verifier establishes the
shared ggen invariant: every live example can be projected by the current
canonical CLI without drift.
