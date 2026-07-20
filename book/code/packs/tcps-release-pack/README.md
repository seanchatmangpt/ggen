# tcps-release-pack (pointer)

Canonical, live copy: `packs/tcps-release-pack/` at the repo root. The repo copy is ahead of the
book bundle's snapshot (real workspace build, escape fixes, `gates/*.rq` instead of
`shapes.ttl`); the book's `source-manifest.json` convention (per-file sha256 vs the 132-file
v26.7.19 reference) was adopted there as `packs/tcps-release-pack/source-manifest.json` with
hashes regenerated against `packs/tcps-core-pack/reference/製品版/`. No second live copy is kept
here to avoid drift.
