# Examples reference

The executable source of truth for ggen examples is the live filesystem under
[`examples/`](../../examples/), not a manually maintained percentage-complete
catalog.

## What counts as a live example?

Every top-level directory under `examples/` is live except the explicitly
fenced archive/meta roots:

- `.ggen/`
- `_archive/`
- `archive/`
- `archive_2025/`
- `archive_ggen_core/`

A live directory must contain at least one `ggen.toml`, either directly or in a
nested family example. `scripts/validate-examples.sh` fails closed if a new live
directory is added without a manifest, so examples are no longer silently
omitted from validation.

## Shared definition of done

For every discovered live `ggen.toml`:

```text
manifest
  -> current canonical ggen CLI
  -> ggen sync run
  -> successful bounded execution
  -> no tracked/untracked projection drift
```

Run it locally:

```bash
cargo build --locked -p ggen-cli-lib --bin ggen
GGEN_BIN="$PWD/target/debug/ggen" scripts/validate-examples.sh
```

To intentionally refresh generated projections instead of checking for drift:

```bash
GGEN_BIN="$PWD/target/debug/ggen" scripts/regenerate-examples.sh
```

The regeneration command uses the exact same discovery and execution path as
validation; it only changes the final policy from “tree must remain clean” to
“leave generated changes for review.”

## Stronger example-specific proof

The shared contract proves reproducible ggen projection. Individual examples
may additionally carry dedicated workflows and verifiers for their own
behavioral claims—for example Rust boundary tests, Python conformance suites,
Lean proofs, or product-evidence checks. Those proofs are additive; they do not
replace the corpus-wide `ggen sync run` gate.

See [`examples/README.md`](../../examples/README.md) for the operator-facing
contract.
