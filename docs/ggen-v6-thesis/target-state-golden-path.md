# Target State — Golden Path (v6.1.0)

## Flow Diagram

```
ggen registry search mcp
    │
    ▼
list_packs() scans marketplace/packs/*.toml
    │
    ▼
filter by query → return SearchOutput
    [STATUS: DONE — no changes needed]


ggen doctor
    │
    ▼
workspace_checks()        ← domain: ggen_domain::utils::doctor
    │   ├─ ggen.toml
    │   ├─ Cargo.toml
    │   ├─ .specify/
    │   ├─ .ggen/packs.lock   ← NEW: validate lockfile integrity
    │   └─ .ggen/keys/        ← NEW: validate signing key exists
    │
    ▼
toolchain_checks()       ← domain: ggen_domain::utils::doctor
    │   ├─ Rust toolchain
    │   ├─ Cargo
    │   └─ clap-noun-verb
    │
    ▼
lockfile_integrity()    ← NEW: domain function
    │   ├─ Lockfile exists?
    │   ├─ Lockfile parseable?
    │   ├─ All entries have non-empty digest?
    │   └─ Digest matches installed content?
    │
    ▼
return RunOutput
    [STATUS: DONE — add lockfile + keys checks]


ggen pack add mcp-rust
    │
    ▼
load_pack_metadata()     ← domain: ggen_domain::packs::metadata
    │
    ▼
validate_pack_name()    ← domain: ggen_domain::packs::validate
    │
    ▼
compute_digest()         ← NEW: domain: ggen_domain::packs::install
    │   SHA-256 of pack TOML content
    │
    ▼
materialize_pack()       ← NEW: domain: ggen_domain::packs::install
    │   ├─ Copy templates/ from marketplace/packs/mcp-rust/
    │   ├─ Copy queries/   from marketplace/packs/mcp-rust/
    │   ├─ Copy sparql/   from marketplace/packs/mcp-rust/
    │   └─ Copy packages/ from marketplace/packs/mcp-rust/
    │   → all go to .ggen/packs/mcp-rust/
    │
    ▼
write_lockfile()         ← NEW: domain: ggen_domain::packs::lockfile
    │   Uses PackLockfile struct (single schema)
    │   Entry:
    │   {
    │     "id": "mcp-rust",
    │     "version": "1.0.0",
    │     "installed_at": "<RFC-3339>",
    │     "digest": "sha256:<64-hex-chars>",   ← REAL digest
    │     "registry_source": "local",
    │     "trust_tier": "local",
    │     "dependencies": [],
    │     "packages_installed": ["mcp-rust/..."],
    │     "templates_available": ["..."],
    │     "install_path": ".ggen/packs/mcp-rust"
    │   }
    │
    ▼
return AddOutput
    [STATUS: NEEDS — materialize, digest, single schema]


ggen capability enable --surface mcp --projection rust --runtime axum
    │
    ▼
resolve_capability_to_packs()   ← domain: ggen_domain::packs::capability_registry
    │   Reads from RDF ontology or registry config
    │   NOT hardcoded match arms
    │
    ▼
validate_packs_exist()         ← NEW: domain function
    │   Each resolved pack passes load_pack_metadata()
    │   Hard-fail on missing packs (no soft-warn)
    │
    ▼
update_lockfile()              ← domain: ggen_domain::packs::lockfile
    │   Single PackLockfile schema
    │   Atomic write (write to temp, rename)
    │   NO duplicate writes — capability enable calls
    │   the SAME domain function as pack add
    │
    ▼
emit_composition_receipt()    ← NEW: domain: ggen_domain::receipts
    │   NOT in CLI verb
    │   Records: capability, resolved packs, profile, timestamp
    │   Signed with Ed25519
    │
    ▼
return CapabilityEnableOutput
    [STATUS: NEEDS — RDF-derived registry, single lockfile, domain receipt]


ggen sync --profile enterprise-strict --locked
    │
    ▼
validate_sync_preconditions()  ← domain: ggen_domain::sync_profile
    │   ├─ Profile parsed and validated
    │   ├─ --locked → lockfile MUST exist
    │   ├─ enterprise-strict → lockfile MUST exist
    │   └─ enterprise-strict → unsigned packs rejected
    │
    ▼
load_installed_packs()        ← NEW: domain: ggen_domain::packs::loader
    │   Parse PackLockfile → Vec<InstalledPack>
    │   Verify each entry's digest against installed content
    │   Missing/invalid digest → FAIL (enterprise-strict)
    │
    ▼
merge_pack_contributions()   ← NEW: domain: ggen_core::pipeline
    │   For each installed pack:
    │   ├─ Load templates from .ggen/packs/<id>/templates/
    │   ├─ Load queries   from .ggen/packs/<id>/queries/
    │   ├─ Load SPARQL    from .ggen/packs/<id>/sparql/
    │   └─ Merge into pipeline template/query sets
    │   THIS is where packs affect the sync pipeline
    │
    ▼
enforce_profile_policy()      ← NEW: domain: ggen_domain::sync_profile
    │   enterprise-strict checks:
    │   ├─ All pack sources are trusted
    │   ├─ No unsigned content
    │   ├─ No ambiguous runtime in templates
    │   └─ No template-owned defaults
    │   Violations → FAIL before emission
    │
    ▼
SyncExecutor.execute()       ← existing: ggen_core::codegen
    │   Runs mu1-mu5 pipeline
    │   NOW includes pack-derived templates and queries
    │
    ▼
emit_sync_receipt()          ← existing: ggen-cli::cmds::sync (keep)
    │   input_hashes NOW includes real pack digests:
    │     "pack:mcp-rust:sha256:abc123..."
    │   NOT "pack:mcp-rust@1.0.0" (the old fake hash)
    │
    ▼
return SyncOutput
    [STATUS: NEEDS — pack loading, merge, profile enforcement, real digests]


ggen receipt verify --receipt-file .ggen/receipts/latest.json
    │
    ▼
Read receipt file
    │
    ▼
receipt.verify(&verifying_key)    ← existing: ggen_receipt
    │
    ▼
verify_pack_provenance()       ← NEW: domain function
    │   Cross-check receipt input_hashes against:
    │   ├─ Current lockfile entries
    │   ├─ Installed pack digests
    │   └─ Pack metadata versions
    │
    ▼
return VerifyOutput
    [STATUS: DONE — add pack provenance cross-check]
```

## Target Lockfile Schema (SINGLE)

```json
{
  "ggen_version": "6.1.0",
  "created_at": "2026-04-02T...",
  "packs": {
    "mcp-rust": {
      "version": "1.0.0",
      "installed_at": "2026-04-02T12:00:00Z",
      "digest": "sha256:a3f2e1b4c5d6e7f8...",
      "registry_source": "local",
      "trust_tier": "local",
      "dependencies": [],
      "install_path": ".ggen/packs/mcp-rust",
      "files": ["templates/mcp-server.rs.tera", "queries/construct.rq"]
    }
  }
}
```

Every operation that modifies the lockfile goes through ONE domain function. The CLI verb never touches the file.
