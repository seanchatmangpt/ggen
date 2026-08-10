# Marketplace PRD/ARD — finish `ggen-marketplace` end-to-end

Status: DRAFT. Written for execution — this is a handoff contract, not a narrative. Every claim
below is grounded against real, just-run evidence (2026-08-10): a full validation pass of
`~/ggen-marketplace` (94 packs) and an LSP/grep-verified code survey of `crates/ggen-marketplace`
and `crates/ggen-cli` in this repo (`/Users/sac/ggen`, branch `release/v26.8.9`). Re-verify before
acting on anything here if either repo has moved.

## 1. What exists today (verified, not assumed)

### 1.1 The marketplace repository (`~/ggen-marketplace`) — real, mostly ALIVE

Ran the full validation chain from that repo's own `AGENTS.md`:

| Check | Result |
|---|---|
| `admit-config.sh` (star-toml admission of `marketplace.toml`) | `q_config=1`, `standing=ADMITTED` — required `+nightly-2026-06-22`; the repo's own docs don't disclose that `tools/marketplace-config`'s pinned `wasm4pm-compat=26.6.28` still needs `#![feature(generic_const_exprs)]`, so a stable toolchain fails outright. This is a real, disclosed gap in the repo's own onboarding docs, not fixed here. |
| `marketplace.py validate` | **94/94 packs** validate: 133 ontologies, 787 templates, 307 native gates, 2 verifier gates |
| `marketplace.py catalog` | deterministic — two runs byte-identical |
| `marketplace.py fingerprint` | `sha256:ab0afe040ab0f52370e0f27ea803888ae238ec00099029b82ef8dffd74793474`, 1919 files |
| `qualify-marketplace.sh` (real `ggen 26.8.8` binary, real `ggen sync run` per pack, real gate execution against the union graph) | **93/94 `ALIVE`**, 1 `REFUSED` |

The one failure: **`ggen-legacy-assurance-pack`** (`26.8.1`) refuses its own gate
`gates/010_required_layers.rq`, which SELECTs for 10 required `gla:AssuranceLayer` individuals
(`layer-01`…`layer-10`, each needing `gla:required true`). `ontology.ttl` declares the
`gla:AssuranceLayer` **class** (line 6) but instantiates zero individuals of it anywhere in the
pack (confirmed by grep — no `layer-0` string appears outside the class declaration and the gate
file). This is schema without data, not a wiring bug: the pack's own `pack.toml` description says
it "projects customer-controlled execution policy, hidden-challenge contracts, cryptographic
binding, independent-verifier law..." — the 10 layers' real content is domain-specific authorship,
not something to fabricate to make a gate pass.

### 1.2 GitHub Pages catalog — reported live by the user, not independently re-verified in this pass

The user states the remote GitHub Pages site publishing this catalog is up. This PRD does not
re-fetch and diff that live URL against `marketplace.py catalog`'s local output — that check
belongs in FR-001 below, as the first concrete acceptance step, not asserted here as already done.

### 1.3 `ggen-cli`/`ggen-marketplace` (this repo) — the actual gap

Evidence-grounded survey of `crates/ggen-marketplace` and `crates/ggen-cli`:

- **HTTP client exists but is dead code.** `crates/ggen-marketplace/src/marketplace/network.rs`
  defines `MarketplaceClient` (reqwest-based, `fetch_package_metadata` ~L131, `registry_url: String`
  field ~L55) hitting a made-up REST shape (`{registry_url}/packages/{id}/versions/{version}`) —
  not the static-JSON-catalog shape `marketplace.py catalog` actually produces. Zero call sites
  outside its own test module; only re-exported (`marketplace/mod.rs`) and referenced in a dead
  comment in `ggen-cli/src/cmds/ontology.rs:353`.
- **Real, working fetchers exist — for the wrong registries.** `external_fetcher.rs` has genuine
  HTTP clients for `crates.io` (~L45), `npmjs.org` (~L193), and PyPI (~L327+). None targets a
  ggen-marketplace-shaped catalog or a GitHub Pages URL.
- **Every CLI verb that touches packs is 100% local-filesystem.** `ggen pack add`
  (`ggen-cli/src/cmds/pack.rs:96`), `ggen pack search` (`pack.rs:304` → `perform_search` at
  `pack.rs:472`), and `ggen packs install` (`ggen-cli/src/cmds/packs.rs:72` → `show_pack` at
  `packs_registry/metadata.rs:133`) all resolve exclusively against a local `packs/` dir
  (`GGEN_PACKS_DIR` env var or relative paths). No network call exists in any of these paths today.
- **"Registry URL" is a hardcoded, inert literal.** `packs.rs:92-93` writes
  `PackSource::Registry { url: "https://registry.ggen.io".to_string() }` into `.ggen/packs.lock`
  as pure metadata when a pack isn't found locally — that string is never fetched, never
  validated, and has no config surface (`ggen-config` has no `[marketplace]` table, no
  `registry_url`/`catalog_url` field of any kind).
- **No stubs, no `todo!()`.** This isn't an unfinished stub to wire up — it's an entire missing
  subsystem. Nothing in the codebase parses a marketplace catalog JSON schema; nothing maps a
  catalog entry's declared hash to the `digest`/`integrity` fields already present in
  `PackageMetadata` (`network.rs:24-46`) and `LockedPack` (`packs.rs:90-98`).
- **Dependencies are already present.** `crates/ggen-marketplace/Cargo.toml:35` — `reqwest`
  (`rustls-tls`, `stream` features) plus `sha2`/`sha1`/`ed25519-dalek`/`hex` are already
  dependencies. No new HTTP/hashing crate is needed.

## 2. Product goal for this release

Make `ggen pack search`/`ggen pack add`/`ggen packs install` work against the real, live
`~/ggen-marketplace` GitHub Pages catalog end-to-end, with the same hash-verification discipline
this repo already applies to local packs (`[FM-PACK-008]` content-hash-vs-lock mismatch refusal).
Close `ggen-legacy-assurance-pack`'s data gap so the marketplace repo is 94/94 `ALIVE`, not 93/94.

### Non-goals for this release

- Publishing/authoring flow changes in `~/ggen-marketplace` itself (that repo's own
  `scripts/qualify-marketplace.sh`/`marketplace.py` toolchain is out of scope here except for the
  one pack-content gap in FR-005).
- A general-purpose plugin registry protocol (OCI, npm-style scoped packages, etc.) — scope is
  specifically "consume the existing ggen-marketplace catalog shape," not invent a new one.
  `MarketplaceClient`'s REST shape and the crates.io/npm/PyPI fetchers are explicitly **not**
  reused as the target shape for FR-001 — they solve a different registry problem.
- Publish-side commands (`ggen pack publish`) — this PRD is consume-only.
- Signing/provenance verification beyond content-hash matching (ed25519 signature verification on
  fetched packs is a natural follow-on, not required for this release's acceptance).

## 3. Functional Requirements

### MKT-FR-001 — Catalog fetch and parse

Owner: **`ggen-marketplace::marketplace::catalog`** (new module). Fetch the GitHub Pages catalog
JSON over HTTP (reqwest, already a dependency) and deserialize it into structs matching the real
schema `~/ggen-marketplace/scripts/marketplace.py catalog` emits — not `MarketplaceClient`'s
invented REST shape. Acceptance: a real HTTP GET against the live catalog URL, parsed into typed
Rust structs, with a unit test asserting the parsed pack count matches the catalog's declared
count (94, or whatever the live catalog's current count is at test time — do not hardcode 94 as a
magic number in the test; read it from the fetched payload's own length).

### MKT-FR-002 — Registry/catalog URL configuration surface

Owner: **`ggen-config`**. Add a `[marketplace]` table to the `GgenConfig`/frontmatter schema
(`crates/ggen-engine/src/config.rs`) with a `catalog_url` field, defaulting to the real GitHub
Pages URL. Acceptance: `ggen.toml` can override it; absence of the field falls back to the
default; the existing hardcoded `"https://registry.ggen.io"` literal in `packs.rs:93` is replaced
with a value sourced from this config, not a second hardcoded string.

### MKT-FR-003 — Remote-aware `ggen pack search` / `ggen pack add` / `ggen packs install`

Owner: **`ggen-cli::cmds::{pack,packs}`**. Each verb consults the remote catalog (MKT-FR-001) as a
fallback (or primary, per UX decision made during implementation) when a pack isn't resolvable
locally, rather than failing immediately on local-miss. Acceptance: a real end-to-end test —
remove a pack from the local dir, run `ggen pack add <name>` against the live (or a local
HTTP-served fixture) catalog, confirm the pack materializes locally afterward.

### MKT-FR-004 — Download, hash-verify, cache

Owner: **`ggen-marketplace::marketplace::install`**. Given a catalog entry naming a pack's
download location and declared content hash, fetch the artifact, compute its real hash (sha2,
already a dependency), and refuse (mirroring `[FM-PACK-008]`'s existing refusal discipline for
local packs) on mismatch before writing anything to the local pack cache or `.ggen/packs.lock`.
Acceptance: a sabotage test — corrupt the downloaded bytes in the test double for the HTTP layer
only (the hash-check logic itself must run against real bytes and a real hash function, not a
mocked comparison) and confirm the install path errors out rather than silently accepting it.

### MKT-FR-005 — Close `ggen-legacy-assurance-pack`'s gate failure

Owner: **pack content authoring** (in `~/ggen-marketplace`, not this repo). Author real
`gla:AssuranceLayer` individuals (`layer-01`…`layer-10`) with genuine `gla:required true` facts
matching the pack's stated purpose (customer-controlled execution policy, hidden-challenge
contracts, cryptographic binding, independent-verifier law, legal recourse). Acceptance:
`qualify-marketplace.sh` reports `94/94 ALIVE`, zero `REFUSED`.

## 4. Nonfunctional Requirements

Network calls must be least-privilege (GET-only for catalog/artifact fetch; no implicit write
back to the remote), fail closed on hash mismatch (never fall open to "install anyway"), and
degrade to a clear, typed error (not a panic) when the catalog URL is unreachable — a network
partition must not crash `ggen pack search`. All new HTTP-touching code paths get Chicago-style
tests against a real local HTTP fixture server (no `mockall`/no interaction-verification mocking
of the HTTP client itself — see this repo's global testing discipline).

## 5. Exclusions

This release does not claim: a plugin signing/trust-tier ceremony beyond hash matching, a
publish-side CLI, resolution of `ggen-legacy-assurance-pack`'s content by anyone other than a
domain owner who actually knows what the 10 assurance layers should say, or parity with
crates.io/npm-style version-range resolution (`^1.2`, `~1.2`, etc.) unless the real catalog schema
already carries that information today (verify before assuming — not confirmed in this pass).

## 6. Launch Theorem

This PRD/ARD bundle is closeable only when: MKT-FR-001 through MKT-FR-004 have real, passing,
non-mocked tests exercising an actual HTTP round trip (a local fixture server or the live catalog,
not a stubbed response); the existing `.ggen/packs.lock` hash-refusal discipline
(`[FM-PACK-008]`) is demonstrably extended to remote-sourced packs, not merely local ones; and
`~/ggen-marketplace`'s `qualify-marketplace.sh` reports 94/94 `ALIVE` (MKT-FR-005). Runtime
release/tag decisions for this repo remain separate and are not implied by this document closing.

---

# Architecture Requirements Document — Marketplace remote-consumption

## Architecture Objective

Add one new capability plane — **remote catalog consumption** — to the existing local-pack
pipeline, without altering the existing local-pack resolution path's behavior when no remote
lookup is needed (backward compatible: a fully-local `ggen.toml` continues to work exactly as
today with zero network calls).

## Components

### MKT-C-001 — Catalog Client

New module, `crates/ggen-marketplace/src/marketplace/catalog.rs`. Fetches and parses the remote
catalog JSON. Actuation: **no** (read-only network GET).

### MKT-C-002 — Marketplace Config

Extension to `ggen-config`/`ggen-engine::config`. Carries `catalog_url` and any future
marketplace-scoped settings. Actuation: **no**.

### MKT-C-003 — Remote-Aware Pack Resolver

Extension to `ggen-cli::cmds::{pack,packs}` and `ggen-marketplace::packs_registry`. Falls back to
MKT-C-001 when local resolution misses. Actuation: **no** (resolution only; does not write).

### MKT-C-004 — Fetch-Verify-Cache

New module, `crates/ggen-marketplace/src/marketplace/install.rs` (or extension of the existing
`install.rs` if one already owns local pack installation — confirm before creating a duplicate).
Downloads an artifact, hashes it, compares against the catalog-declared hash, and on success
writes into the local pack cache and `.ggen/packs.lock`. Actuation: **yes** — this is the one
component in this plane allowed to write to disk, and only after a passing hash check.

## Interfaces

MKT-C-001 exposes a typed `RemoteCatalog { packs: Vec<CatalogEntry> }` (or equivalent) that
MKT-C-003 consults by name/version. MKT-C-004 accepts a single `CatalogEntry` (never the whole
catalog) plus a target local path, keeping its blast radius to one pack per call. `.ggen/packs.lock`
remains the single on-disk authority for what was actually installed and from where — MKT-C-004 is
the only writer to it for remote-sourced entries, mirroring the existing local-install path's
single-writer discipline.

## Trust Boundaries

Untrusted remote catalog response → parsed/typed boundary (MKT-C-001) → local resolution decision
(MKT-C-003, no actuation) → untrusted remote artifact bytes → hash-verified boundary (MKT-C-004,
the only actuating step) → local pack cache / `.ggen/packs.lock`. A catalog response that fails to
parse, or an artifact that fails its hash check, must never reach the local cache — this mirrors
the existing `[FM-PACK-008]`/`[FM-WRITE-005]` refusal model for local packs and must not become a
second, weaker path.

## Verification Architecture

Unit tests for catalog JSON parsing (real fixture payloads, not hand-typed minimal structs that
happen to satisfy the parser). Integration tests for MKT-C-003/MKT-C-004 against a real local HTTP
fixture server (e.g. a `tokio`-spawned listener serving canned catalog/artifact responses) — no
`mockall`/interaction-verifying mock of the `reqwest::Client` itself, per this repo's Chicago TDD
discipline. A sabotage test corrupting artifact bytes in transit (at the fixture-server layer, not
inside the hash-check function) to prove MKT-C-004 actually refuses rather than trusting a flag.
An end-to-end test running the real `ggen` binary's `pack add`/`packs install` verbs against the
fixture server.

## Acceptance Architecture

Every MKT-FR requirement above maps to a named MKT-C component and a real, non-mocked test. No
component other than MKT-C-004 may write to `.ggen/packs.lock` or the local pack cache for
remote-sourced packs. `~/ggen-marketplace`'s own qualification (`qualify-marketplace.sh`) must
independently report `94/94 ALIVE` before this bundle is considered closed — a green build in this
repo alone does not certify the marketplace content itself.
