# Packs Lifecycle Architecture - Visual Guide

## Current Architecture (Disconnected)

```
┌──────────────────────────────────────────────────────────────────────┐
│                         CURRENT STATE                                 │
│                        (Siloed Systems)                               │
└──────────────────────────────────────────────────────────────────────┘

┌─────────────────────┐         ┌─────────────────────┐
│   MARKETPLACE       │         │   PACKS             │
│   (mcpp-marketplace)│         │   (mcpp-domain)     │
├─────────────────────┤         ├─────────────────────┤
│ ✅ Install          │         │ ✅ List             │
│ ✅ Update           │         │ ❌ Install          │
│ ✅ Dependency res   │         │ ❌ Update           │
│ ✅ Lockfile mgmt    │         │ ❌ Regenerate       │
│ ✅ Cache mgmt       │         │ ❌ State tracking   │
│ ✅ Checksums        │         └─────────────────────┘
└─────────────────────┘                   │
         │                                 │
         │ NO CONNECTION                   │ Used once
         │                                 ▼
         │                          ┌──────────────────┐
         │                          │ Project Generator│
         │                          │ (mcpp-core)      │
         │                          ├──────────────────┤
         │                          │ ✅ Scaffolding   │
         │                          │ ❌ Regeneration  │
         │                          └──────────────────┘
         │                                 │
         │                                 │ Creates files
         │                                 ▼
         │                          ┌──────────────────┐
         ▼                          │ User Project     │
┌─────────────────────┐            ├──────────────────┤
│ ~/.mcpp/packages    │            │ src/             │
├─────────────────────┤            │ tests/           │
│ Downloaded packs    │◀── NO ──▶ │ Cargo.toml       │
│ Not used for        │   LINK    │ .mcpp/           │
│ project generation  │            │   snapshot.json? │
└─────────────────────┘            └──────────────────┘
                                          │
                                          │ Managed by
                                          ▼
                                   ┌──────────────────┐
                                   │ Lifecycle System │
                                   │ (mcpp-core)      │
                                   ├──────────────────┤
                                   │ ✅ Phase exec    │
                                   │ ✅ State track   │
                                   │ ❌ Regeneration  │
                                   │ ❌ Pack updates  │
                                   └──────────────────┘
```

---

## Target Architecture (Integrated Lifecycle)

```
┌──────────────────────────────────────────────────────────────────────────────┐
│                         TARGET STATE                                          │
│                   (Unified Pack Lifecycle)                                    │
└──────────────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────────────┐
│                          PACK REGISTRY                                      │
│                    (GitHub / mcpp-marketplace-v2)                          │
├────────────────────────────────────────────────────────────────────────────┤
│  📦 rust-web@1.0.0    │  📦 react-app@2.1.0  │  📦 fastapi@0.5.0        │
│  ├─ gpack.toml        │  ├─ gpack.toml        │  ├─ gpack.toml           │
│  ├─ templates/        │  ├─ templates/        │  ├─ templates/           │
│  ├─ graphs/           │  ├─ graphs/           │  ├─ graphs/              │
│  └─ queries/          │  └─ queries/          │  └─ queries/             │
└────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ Download & verify
                                    ▼
┌────────────────────────────────────────────────────────────────────────────┐
│                    PACK CACHE & LOCKFILE                                    │
│                      (~/.mcpp/packages)                                     │
├────────────────────────────────────────────────────────────────────────────┤
│  ~/.mcpp/packages/                  mcpp.lock (Project)                    │
│    ├─ rust-web@1.0.0/          {                                           │
│    ├─ rust-web@1.1.0/            "packs": [                                │
│    └─ react-app@2.1.0/             {                                       │
│                                      "id": "rust-web",                     │
│  [Cached, versioned]                 "version": "1.1.0",                   │
│                                      "files": [                            │
│                                        "src/main.rs" → "rust-web@1.1.0",  │
│                                        "Cargo.toml"  → "rust-web@1.1.0"   │
│                                      ]                                     │
│                                    }                                       │
│                                  ]                                         │
│                                }                                           │
└────────────────────────────────────────────────────────────────────────────┘
                                    │
                         ┌──────────┴──────────┐
                         │                     │
                         ▼                     ▼
┌─────────────────────────────┐    ┌─────────────────────────────┐
│   INITIAL GENERATION        │    │   REGENERATION              │
│   (mcpp new my-app)         │    │   (mcpp packs update)       │
├─────────────────────────────┤    ├─────────────────────────────┤
│ 1. Select pack(s)           │    │ 1. Detect pack update       │
│ 2. Download from cache      │    │ 2. Load baseline snapshot   │
│ 3. Render templates         │    │ 3. Detect manual edits      │
│ 4. Create files             │    │ 4. Regenerate with new pack │
│ 5. Create baseline snapshot │    │ 5. Three-way merge          │
│ 6. Update mcpp.lock         │    │ 6. Update snapshot          │
│ 7. Run lifecycle init       │    │ 7. Update mcpp.lock         │
└─────────────────────────────┘    └─────────────────────────────┘
                 │                                 │
                 │ Creates                         │ Updates
                 ▼                                 ▼
┌──────────────────────────────────────────────────────────────────────┐
│                         USER PROJECT                                  │
├──────────────────────────────────────────────────────────────────────┤
│  my-app/                                                             │
│    ├─ src/                                                           │
│    │   ├─ main.rs          ◀───── Generated (pack=rust-web@1.1.0)   │
│    │   ├─ models/          ◀───── Generated                         │
│    │   └─ custom.rs        ◀───── Manual edit (preserved)           │
│    ├─ tests/               ◀───── Generated + manual                │
│    ├─ Cargo.toml           ◀───── Generated (merged)                │
│    ├─ .mcpp/                                                         │
│    │   ├─ snapshot.json    ◀───── Baseline for three-way merge      │
│    │   │   {                                                         │
│    │   │     "packs": [{                                             │
│    │   │       "pack_id": "rust-web",                                │
│    │   │       "version": "1.1.0",                                   │
│    │   │       "files": ["src/main.rs", "Cargo.toml"]                │
│    │   │     }],                                                     │
│    │   │     "files": [{                                             │
│    │   │       "path": "src/main.rs",                                │
│    │   │       "hash": "abc123",                                     │
│    │   │       "regions": [                                          │
│    │   │         {"type": "generated", "start": 1, "end": 10},       │
│    │   │         {"type": "manual", "start": 11, "end": 20}          │
│    │   │       ]                                                     │
│    │   │     }]                                                      │
│    │   │   }                                                         │
│    │   ├─ state.json       ◀───── Lifecycle state                   │
│    │   └─ make.toml        ◀───── Lifecycle config                  │
│    └─ mcpp.lock            ◀───── Pack versions + file mappings     │
└──────────────────────────────────────────────────────────────────────┘
                 │
                 │ Triggers
                 ▼
┌──────────────────────────────────────────────────────────────────────┐
│                      LIFECYCLE SYSTEM                                 │
│                    (make.toml phases)                                │
├──────────────────────────────────────────────────────────────────────┤
│  Phases:                                                             │
│    init      → Initialize project (post-generation)                  │
│    template  → Regenerate from packs (NEW)                           │
│    build     → Compile project                                       │
│    test      → Run tests                                             │
│    deploy    → Deploy to production                                  │
│                                                                       │
│  Hooks:                                                              │
│    before_template → Check for conflicts                             │
│    after_template  → Run tests, format code                          │
└──────────────────────────────────────────────────────────────────────┘
```

---

## Data Flow: Pack Update Scenario

```
┌────────────────────────────────────────────────────────────────┐
│ SCENARIO: Updating rust-web from 1.0.0 → 1.1.0               │
│           (User has made manual edits to src/custom.rs)       │
└────────────────────────────────────────────────────────────────┘

Step 1: Check for Updates
─────────────────────────
$ mcpp packs update rust-web

  Registry Check: rust-web@1.1.0 available (current: 1.0.0)
  Changelog:
    - Add tracing support to main.rs
    - Update Cargo.toml with new dependencies
    - Add health check endpoint

Step 2: Load Baseline Snapshot
───────────────────────────────
  Read: .mcpp/snapshot.json
  ┌─────────────────────────────────────────┐
  │ Baseline (rust-web@1.0.0)              │
  ├─────────────────────────────────────────┤
  │ src/main.rs:                           │
  │   hash: "abc123"                        │
  │   regions:                              │
  │     [generated: lines 1-50]             │
  │                                         │
  │ src/custom.rs: (not in snapshot)       │
  │                                         │
  │ Cargo.toml:                             │
  │   hash: "def456"                        │
  │   regions:                              │
  │     [generated: lines 1-20]             │
  └─────────────────────────────────────────┘

Step 3: Detect Manual Edits
────────────────────────────
  Diff: Current files vs Baseline snapshot

  src/main.rs:
    ✓ Unchanged (hash matches)

  src/custom.rs:
    ⚠ NEW FILE (manual addition)

  Cargo.toml:
    ✓ Unchanged (hash matches)

Step 4: Download New Pack & Regenerate
───────────────────────────────────────
  Download: rust-web@1.1.0 from cache
  Render templates with same variables

  Generated (rust-web@1.1.0):
    src/main.rs → NEW version
    Cargo.toml  → NEW version

Step 5: Three-Way Merge
────────────────────────
  For each file:

  ┌─ src/main.rs ──────────────────────────┐
  │ Baseline (1.0.0):  "abc123"            │
  │ Current:           "abc123" (unchanged)│
  │ New (1.1.0):       "xyz789" (changed)  │
  │                                         │
  │ Decision: SAFE UPDATE                  │
  │   → Apply new version (no conflict)    │
  └─────────────────────────────────────────┘

  ┌─ src/custom.rs ────────────────────────┐
  │ Baseline:          (not present)       │
  │ Current:           EXISTS (manual)     │
  │ New:               (not generated)     │
  │                                         │
  │ Decision: PRESERVE                     │
  │   → Keep user's file untouched         │
  └─────────────────────────────────────────┘

  ┌─ Cargo.toml ───────────────────────────┐
  │ Baseline (1.0.0):  "def456"            │
  │ Current:           "def456" (unchanged)│
  │ New (1.1.0):       "ghi012" (changed)  │
  │                                         │
  │ Decision: SAFE UPDATE                  │
  │   → Apply new version (no conflict)    │
  └─────────────────────────────────────────┘

Step 6: Apply Changes
─────────────────────
  Write updated files:
    ✓ src/main.rs    (updated to 1.1.0)
    ✓ Cargo.toml     (updated to 1.1.0)
    ✓ src/custom.rs  (preserved)

Step 7: Update Snapshot
────────────────────────
  Write: .mcpp/snapshot.json
  {
    "packs": [{
      "pack_id": "rust-web",
      "version": "1.1.0",    ← Updated
      "files": [
        "src/main.rs",
        "Cargo.toml"
      ]
    }],
    "files": [
      {
        "path": "src/main.rs",
        "hash": "xyz789",      ← New hash
        "pack_id": "rust-web",
        "pack_version": "1.1.0"
      },
      {
        "path": "src/custom.rs",
        "hash": "manual123",
        "pack_id": null,       ← Manual file
        "pack_version": null
      },
      {
        "path": "Cargo.toml",
        "hash": "ghi012",      ← New hash
        "pack_id": "rust-web",
        "pack_version": "1.1.0"
      }
    ]
  }

Step 8: Update Lockfile
────────────────────────
  Write: mcpp.lock
  {
    "packs": [{
      "id": "rust-web",
      "version": "1.1.0",          ← Updated
      "sha256": "pack-hash-here",
      "source": "github:...",
      "files": {
        "src/main.rs": "rust-web@1.1.0",
        "Cargo.toml":  "rust-web@1.1.0"
      }
    }]
  }

Step 9: Report Results
──────────────────────
  ✓ Updated rust-web: 1.0.0 → 1.1.0

  Files updated:
    ✓ src/main.rs
    ✓ Cargo.toml

  Files preserved:
    ✓ src/custom.rs (manual edit)

  Conflicts: 0
  Manual review needed: 0

  Next steps:
    - Run tests: cargo test
    - Review changes: git diff
    - Commit: git add . && git commit -m "Update rust-web to 1.1.0"
```

---

## Conflict Resolution Flow

```
┌─────────────────────────────────────────────────────────────────┐
│ SCENARIO: Conflicting Update                                    │
│           User edited src/main.rs, pack also updated it         │
└─────────────────────────────────────────────────────────────────┘

Detection:
──────────
  Baseline (1.0.0):  hash="abc123"  ← Last known good state
  Current:           hash="manual1" ← User made changes
  New (1.1.0):       hash="xyz789"  ← Pack made changes

  CONFLICT: Both user and pack modified same file

Resolution Options:
───────────────────

Option 1: Auto-Merge (Non-overlapping regions)
  ┌─────────────────────────────────────────┐
  │ Baseline:                              │
  │   1  use actix_web;                    │
  │   2  fn main() { ... }                 │
  │   3  fn helper() { ... }               │
  │                                         │
  │ User changes (lines 3-5):              │
  │   3  fn helper() { println!("new"); }  │
  │   4  fn custom() { ... }               │
  │   5                                     │
  │                                         │
  │ Pack changes (lines 1-2):              │
  │   1  use actix_web;                    │
  │   1+ use tracing;                      │
  │   2  fn main() {                       │
  │        tracing::init();                │
  │   }                                     │
  │                                         │
  │ Merged result:                         │
  │   1  use actix_web;                    │
  │   2  use tracing;            ← Pack    │
  │   3  fn main() {             ← Pack    │
  │        tracing::init();      ← Pack    │
  │   }                          ← Pack    │
  │   6  fn helper() {           ← User    │
  │        println!("new");      ← User    │
  │   }                                     │
  │   7  fn custom() { ... }     ← User    │
  │                                         │
  │ ✓ AUTO-MERGED (non-overlapping)        │
  └─────────────────────────────────────────┘

Option 2: Manual Resolution (Overlapping regions)
  ┌─────────────────────────────────────────┐
  │ User changes (lines 2-4):              │
  │   fn main() {                          │
  │     custom_setup();  ← User added      │
  │   }                                     │
  │                                         │
  │ Pack changes (lines 2-4):              │
  │   fn main() {                          │
  │     tracing::init(); ← Pack added      │
  │   }                                     │
  │                                         │
  │ ⚠ CONFLICT: Same region modified       │
  │                                         │
  │ Actions:                                │
  │   1. Create conflict markers:          │
  │      <<<<<<< USER (manual edit)        │
  │      custom_setup();                   │
  │      =======                            │
  │      tracing::init();                  │
  │      >>>>>>> PACK (rust-web@1.1.0)     │
  │                                         │
  │   2. Prompt user:                      │
  │      "Conflict in src/main.rs"         │
  │      "Options:"                        │
  │      "  [k]eep user changes"           │
  │      "  [a]ccept pack changes"         │
  │      "  [m]erge manually"              │
  │      "  [s]kip this file"              │
  └─────────────────────────────────────────┘

Option 3: Preserve Strategy (Conservative)
  ┌─────────────────────────────────────────┐
  │ If --merge-strategy=preserve:          │
  │                                         │
  │ Never overwrite manual edits           │
  │                                         │
  │ Actions:                                │
  │   1. Keep user version as-is           │
  │   2. Save pack version to:             │
  │      src/main.rs.mcpp-new              │
  │   3. Notify user:                      │
  │      "src/main.rs updated by pack"     │
  │      "Review: src/main.rs.mcpp-new"    │
  │      "Apply: mv src/main.rs.mcpp-new"  │
  │                  "  src/main.rs"       │
  └─────────────────────────────────────────┘
```

---

## Region Tracking Implementation

```
┌──────────────────────────────────────────────────────────────────┐
│ Region Markers in Generated Files                                │
└──────────────────────────────────────────────────────────────────┘

Generated Rust file (src/main.rs):

1  // @mcpp:generated-start pack=rust-web version=1.1.0
2  use actix_web::{web, App, HttpServer};
3  use tracing_subscriber;
4
5  #[actix_web::main]
6  async fn main() -> std::io::Result<()> {
7      tracing_subscriber::init();
8
9      HttpServer::new(|| {
10         App::new()
11             .route("/", web::get().to(index))
12     })
13     .bind(("127.0.0.1", 8080))?
14     .run()
15     .await
16 }
17 // @mcpp:generated-end
18
19 // @mcpp:manual-edit-start
20 /// Custom user function
21 fn custom_handler() -> &'static str {
22     "Hello from custom code!"
23 }
24 // @mcpp:manual-edit-end
25
26 // @mcpp:generated-start pack=rust-web version=1.1.0
27 async fn index() -> &'static str {
28     "Hello, world!"
29 }
30 // @mcpp:generated-end

Parsed into regions:
{
  "path": "src/main.rs",
  "regions": [
    {
      "type": "generated",
      "start": 1,
      "end": 17,
      "pack": "rust-web",
      "version": "1.1.0"
    },
    {
      "type": "manual",
      "start": 19,
      "end": 24
    },
    {
      "type": "generated",
      "start": 26,
      "end": 30,
      "pack": "rust-web",
      "version": "1.1.0"
    }
  ]
}

During update:
  - Generated regions (lines 1-17, 26-30) → Replaced with new pack version
  - Manual regions (lines 19-24)          → Preserved exactly
```

---

## Watch Mode Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│ Watch Mode: Continuous Pack Monitoring                           │
└──────────────────────────────────────────────────────────────────┘

$ mcpp watch --packs

┌─────────────────────────────────────────────────────────────────┐
│                        WATCH LOOP                                │
└─────────────────────────────────────────────────────────────────┘
         │
         ▼
    ┌────────────────────┐
    │ Poll registry      │ ← Every 5 minutes (configurable)
    │ for pack updates   │
    └────────────────────┘
         │
         ├─── No updates → Continue polling
         │
         ▼ Updates found
    ┌────────────────────┐
    │ Check mcpp.lock    │
    │ rust-web: 1.0.0    │
    │ Registry: 1.1.0    │
    └────────────────────┘
         │
         ▼ Update available
    ┌────────────────────┐
    │ Prompt user:       │
    │                    │
    │ "Update available: │
    │  rust-web 1.1.0"   │
    │                    │
    │ [y]es / [n]o /     │
    │ [a]lways / [i]gnore│
    └────────────────────┘
         │
         ├─── [n]o → Continue polling
         ├─── [i]gnore → Add to ignore list
         ├─── [a]lways → Auto-apply future updates
         │
         ▼ [y]es selected
    ┌────────────────────┐
    │ Run pack update    │
    │ (three-way merge)  │
    └────────────────────┘
         │
         ├─── Success → Notify + Continue
         │
         ▼ Conflicts detected
    ┌────────────────────┐
    │ Pause watch mode   │
    │ Prompt for manual  │
    │ conflict resolution│
    └────────────────────┘
         │
         ▼ Conflicts resolved
    ┌────────────────────┐
    │ Resume polling     │
    └────────────────────┘
         │
         └──────────┐
                    │
                    ▼
               ┌────────────────────┐
               │ Monitor filesystem │ ← Detect mcpp.lock changes
               │ for external       │   from git pull, manual edits
               │ changes            │
               └────────────────────┘
```

This architecture provides a complete visual guide to how packs will evolve from isolated scaffolding tools into a living project dependency system.
