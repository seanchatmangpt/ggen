# Golden Path Delta — Current vs Target

## At a Glance

| Step | Command | Current | Target | Delta |
|------|---------|---------|--------|-------|
| 1 | `registry search` | Done | Done | None |
| 2 | `doctor` | Done | Done + lockfile + keys | Small |
| 3 | `pack add` | Shallow | Full | Large |
| 4 | `capability enable` | Partial | Full | Large |
| 5 | `sync --locked` | Partial | Full | Large |
| 6 | `receipt verify` | Done | Done + provenance | Small |

## Delta Detail

### Step 3: `pack add` — 3 new domain functions, 1 removed

```
CURRENT install_pack():
  load metadata
  create .ggen/packs/ dir
  write lockfile (NO digest, wrong schema)

TARGET install_pack():
  load metadata
  validate_pack_name()           ← already exists, wire it
  compute_digest()              ← NEW: SHA-256 of pack TOML
  materialize_pack()            ← NEW: copy templates/queries/sparql/packages
  write_lockfile()              ← REWRITE: single PackLockfile schema with digest
```

Files to create:
- `crates/ggen-domain/src/packs/lockfile.rs` — single lockfile read/write with PackLockfile struct
- Move lockfile logic from `install.rs` into this module

Files to change:
- `crates/ggen-domain/src/packs/install.rs` — add compute_digest, materialize_pack, use lockfile module
- `crates/ggen-cli/src/cmds/pack.rs` — remove update_lockfile helper (moved to domain)

---

### Step 4: `capability enable` — 2 new domain functions, 1 refactored

```
CURRENT capability enable:
  resolve_capability_to_packs()   ← hardcoded match arms
  update_lockfile()              ← in CLI verb, duplicate schema
  ReceiptManager.generate()       ← crypto logic in CLI verb

TARGET capability enable:
  resolve_capability_to_packs()   ← REWRITE: read from registry config or RDF
  validate_packs_exist()          ← NEW: hard-fail on missing packs
  update_lockfile()              ← SAME domain function as pack add (single entry point)
  emit_composition_receipt()     ← NEW: domain function (moved from CLI)
```

Files to create:
- `crates/ggen-domain/src/packs/capability_resolve.rs` — replace hardcoded match with config/RDF-driven resolution
- `crates/ggen-domain/src/receipts/composition.rs` — composition receipt logic

Files to change:
- `crates/ggen-domain/src/packs/capability_registry.rs` — replace hardcoded matches
- `crates/ggen-cli/src/cmds/capability.rs` — remove update_lockfile, ReceiptManager, detect_* helpers → delegate to domain

---

### Step 5: `sync --locked` — 4 new domain functions, 1 refactored

```
CURRENT sync:
  validate_sync_preconditions()  ← exists, works
  read_installed_packs()         ← reads but ignores data
  SyncExecutor.execute()         ← runs ggen.toml, ignores packs
  emit_sync_receipt()            ← pack hashes are fake strings

TARGET sync:
  validate_sync_preconditions()  ← DONE (no change)
  load_installed_packs()        ← REWRITE: returns Vec<InstalledPack> with digest verification
  merge_pack_contributions()     ← NEW: load pack templates/queries/sparql into pipeline
  enforce_profile_policy()       ← NEW: gate before emission
  SyncExecutor.execute()         ← UNCHANGED (now receives pack contributions)
  emit_sync_receipt()            ← REWRITE: real pack digests in input_hashes
```

Files to create:
- `crates/ggen-domain/src/packs/loader.rs` — load and validate installed packs from lockfile
- `crates/ggen-core/src/pipeline/pack_merge.rs` — merge pack contributions into pipeline template/query sets
- `crates/ggen-domain/src/sync_profile/enforcement.rs` — profile policy enforcement (trust tiers, unsigned rejection)

Files to change:
- `crates/ggen-cli/src/cmds/sync.rs` — read_installed_packs() uses domain, emit uses real digests

---

### Step 6: `receipt verify` — 1 new domain function

```
CURRENT receipt verify:
  read receipt file
  receipt.verify(&verifying_key)
  return VerifyOutput

TARGET receipt verify:
  read receipt file
  receipt.verify(&verifying_key)
  verify_pack_provenance()       ← NEW: cross-check against lockfile + installed packs
  return VerifyOutput
```

Files to create:
- `crates/ggen-domain/src/receipts/provenance.rs` — cross-check receipt against lockfile state

---

### New Verbs Needed

| Noun | Verb | Domain Function | Status |
|------|------|---------------|--------|
| `pack` | `list` | `lockfile::list_installed()` | New |
| `pack` | `show` | `lockfile::get_pack()` | New |
| `pack` | `verify` | `lockfile::verify_pack_integrity()` | New |
| `pack` | `graph` | `dependency_graph::render_graph()` | New |
| `pack` | `update` | `install::update_pack()` | New |

---

### Domain Files to Create (Total: 7)

| File | Purpose | Phase |
|------|---------|-------|
| `ggen-domain/src/packs/lockfile.rs` | Single PackLockfile read/write | B2 |
| `ggen-domain/src/packs/loader.rs` | Load + validate installed packs from lockfile | C1 |
| `ggen-domain/src/packs/capability_resolve.rs` | RDF/config-driven capability resolution | A1 |
| `ggen-domain/src/sync_profile/enforcement.rs` | Profile policy enforcement | E1 |
| `ggen-domain/src/receipts/composition.rs` | Composition receipt | D1 |
| `ggen-domain/src/receipts/provenance.rs` | Receipt provenance cross-check | D2 |
| `ggen-core/src/pipeline/pack_merge.rs` | Merge pack contributions into pipeline | C1 |

### Domain Files to Rewrite (Total: 4)

| File | What Changes | Phase |
|------|-------------|-------|
| `ggen-domain/src/packs/install.rs` | Add digest, materialize, use lockfile module | B1-B3 |
| `ggen-domain/src/packs/capability_registry.rs` | Replace hardcoded matches | A1 |
| `ggen-domain/src/sync_profile.rs` | Add enforcement logic | E1 |
| `ggen-cli/src/cmds/sync.rs` | Use domain loader, real digests | C1 |

### CLI Files to Rewrite (Total: 2)

| File | What Changes | Phase |
|------|-------------|-------|
| `ggen-cli/src/cmds/capability.rs` | Remove all helper logic, delegate to domain | A1 |
| `ggen-cli/src/cmds/pack.rs` | Add list/show/verify/graph/update verbs | A3 |

---

## Dependency Order (from plan)

```
Phase A1: capability_resolve.rs + registry noun
Phase A2: doctor lockfile/keys checks (small change to doctor.rs)
Phase A3: pack list/show/verify/graph/update verbs + install.rs rewrite
Phase B1-B3: materialize, lockfile schema, bundle expansion
Phase C1: loader.rs, pack_merge.rs (sync now uses installed packs)
Phase C2: CONSTRUCT-native enforcement (profile gates on derivation paths)
Phase C3: validation before emission (enforce_profile_policy)
Phase D1: composition receipt (move from CLI to domain)
Phase D2: provenance cross-check (receipt verify checks lockfile)
Phase E1: profile enforcement in sync (trust tiers, unsigned rejection)
Phase E2: fail-closed rules (hard fail on all violations)
```

## The Two Lockfile Schemas (MUST FIX FIRST)

This is the most critical fix. Two incompatible lockfile schemas currently coexist:

1. **Schema A** (`pack add` writes it): `{ "installed": [...] }`
2. **Schema B** (`capability enable` writes it, `sync` expects it): `{ "packs": { ... } }`

Fix: Create ONE lockfile domain module (`ggen-domain/src/packs/lockfile.rs`) with ONE `PackLockfile` struct. Both `pack add` and `capability enable` call the same `lockfile::add_pack()` function. Both `sync` and `pack list` call the same `lockfile::list_packs()` function.

The `install.rs` current inline JSON writing (lines 73-106) gets deleted entirely and replaced with a call to `lockfile::add_pack()`.
