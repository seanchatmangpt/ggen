# tcps-generated (pointer)

Canonical, live copy: `examples/tcps-generated/` at the repo root — the real consumer wired to
all six repo TCPS packs (`tcps-{core,release,std,ffi,wasm,cli}-pack`) with its own
`ggen.toml`, generated workspace, `ggen.lock`, and receipts. The book bundle's snapshot of this
directory (2 files, older 2-pack `ggen.toml`) was dropped to avoid a second drifting copy. A
complete acceptance run there: delete generated output, `ggen sync run`, compare against the
read-only reference at `packs/tcps-core-pack/reference/製品版/` (per the packs'
`source-manifest.json`), run the original test suite, sync again, record the receipt.
