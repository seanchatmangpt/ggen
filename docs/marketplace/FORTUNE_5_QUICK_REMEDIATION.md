# Fortune 5 CISO - Quick Remediation Guide

**Status:** 🟡 CONDITIONAL GO (76% Production Ready)
**Date:** 2026-03-31
**Effort:** 24-28 hours for full compliance

---

## One-Page Summary

### Release Gates

| Gate | Score | Status | Blocker? |
|------|-------|--------|---------|
| Gate 1: Install Truth | 75% | ⚠️ Partial | 🔴 YES - Signature verification |
| Gate 2: Compiler Truth | 90% | ⚠️ Partial | 🟡 NO - Pack queries/templates |
| Gate 3: Conflict Truth | 85% | ⚠️ Partial | 🟡 NO - 4 stubbed dimensions |
| Gate 4: Rendering Truth | 100% | ✅ Pass | 🟢 NO |
| Gate 5: Trust Truth | 60% | ⚠️ Partial | 🔴 YES - Policy integration |
| Gate 6: Proof Truth | 85% | ✅ Pass | 🟢 NO |

### Critical Path to Production

**3 BLOCKING fixes (7 hours 50 min) → Fortune 5 Production Ready**

---

## The 80/20 Remediation Plan

### 🔴 BLOCKING (Must Fix) - 7h 50m

**1. Wire Signature Verification** (Gate 1) - **45 min**
```rust
// File: crates/ggen-marketplace/src/install.rs:391-405
async fn verify_pack_signature(&self, data: &[u8], signature_hex: &str) -> Result<()> {
    use crate::security::MarketplaceVerifier;
    use ed25519_dalek::VerifyingKey;

    let sig_bytes = hex::decode(signature_hex)?;
    let public_key_hex = self.repository.get_marketplace_public_key().await?;
    let verifying_key = VerifyingKey::from_bytes(&hex::decode(&public_key_hex)?[..])?;

    let verifier = MarketplaceVerifier::new(verifying_key);
    let signature = MarketplaceSignature {
        signature: signature_hex.to_string(),
        public_key: public_key_hex,
        checksum: ggen_receipt::hash_data(data),
    };

    if !verifier.verify(data, &signature)? {
        return Err(Error::SignatureVerificationFailed {
            reason: "Ed25519 signature verification failed".to_string(),
        });
    }
    Ok(())
}
```

**2. Wire HTTP Download** (Gate 1) - **30 min**
```rust
// File: crates/ggen-marketplace/src/install.rs:360-374
async fn download_pack(&self, url: &str) -> Result<Vec<u8>> {
    use reqwest::Client;

    let client = Client::new();
    let response = client
        .get(url)
        .timeout(std::time::Duration::from_secs(30))
        .send()
        .await?;

    if !response.status().is_success() {
        return Err(Error::InstallationFailed {
            reason: format!("HTTP error: {}", response.status()),
        });
    }

    Ok(response.bytes().await?.to_vec())
}
```

**3. Replace Conflict Placeholders** (Gate 3) - **5 min**
```rust
// File: crates/ggen-marketplace/src/compatibility.rs
// Replace lines 500-503, 506-509, 564-567, 570-573

fn check_validators(_packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
    bail!(Error::UnimplementedDimension {
        dimension: CompatibilityDimension::ValidatorContradiction,
    })
}

fn check_policies(_packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
    bail!(Error::UnimplementedDimension {
        dimension: CompatibilityDimension::PolicyContradiction,
    })
}

fn check_receipt_schemas(_packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
    bail!(Error::UnimplementedDimension {
        dimension: CompatibilityDimension::ReceiptSchema,
    })
}

fn check_consequences(_packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
    bail!(Error::UnimplementedDimension {
        dimension: CompatibilityDimension::ConsequenceMigration,
    })
}
```

**4. Integrate Policy Enforcement** (Gate 5) - **7 hours**
```rust
// Part 1: Add Profile parameter (install.rs) - 2 hours
pub async fn install_pack(
    pack_id: &str,
    version: Option<&str>,
    project_dir: &Path,
    force: bool,
    profile: Option<&Profile>,  // NEW
) -> Result<PackInstallResult> {
    if let Some(profile) = profile {
        let report = profile.enforce(&[pack_id.into()])?;
        if !report.passed {
            return Err(Error::PolicyViolation {
                violations: report.violations,
            });
        }
    }
    // ... rest of install
}

// Part 2: Implement Package → PackContext (models.rs) - 3 hours
impl From<&Package> for PackContext {
    fn from(pkg: &Package) -> Self {
        PackContext {
            pack_id: pkg.id.clone(),
            version: pkg.version.clone(),
            has_template_defaults: pkg.manifest.has_template_defaults(),
            has_inferred_capabilities: pkg.manifest.capabilities.is_inferred(),
            has_signed_receipts: pkg.signatures.is_some(),
            trust_tier: pkg.trust_tier.clone(),
            registry_class: pkg.registry_class.clone(),
            // ... all 14 fields
        }
    }
}

// Part 3: Add --profile flag to CLI (ggen-cli/src/cmds/packs.rs) - 2 hours
struct InstallArgs {
    pack_id: String,
    version: Option<String>,
    force: bool,
    profile: Option<String>,  // NEW
}
```

---

### 🟡 HIGH PRIORITY (Should Fix) - 8h

**5. Implement Pack Query Loading** (Gate 2) - **2-3 hours**
```rust
// File: crates/ggen-core/src/pack_resolver.rs
impl PackRegistry {
    fn get_pack_queries(&self, pack: &AtomicPackId) -> Result<Vec<SparqlQuery>> {
        let queries_dir = self.cache_dir.join(pack.to_string()).join("queries");
        let mut queries = Vec::new();

        for entry in std::fs::read_dir(queries_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("rq") {
                let content = std::fs::read_to_string(&path)?;
                queries.push(SparqlQuery {
                    name: path.file_stem().unwrap().to_string_lossy().to_string(),
                    sparql: content,
                });
            }
        }
        Ok(queries)
    }
}
```

**6. Implement Pack Template Loading** (Gate 2) - **2-3 hours**
```rust
// File: crates/ggen-core/src/pack_resolver.rs
impl PackRegistry {
    fn get_pack_templates(&self, pack: &AtomicPackId) -> Result<Vec<TemplateDef>> {
        let templates_dir = self.cache_dir.join(pack.to_string()).join("templates");
        let mut templates = Vec::new();

        for entry in std::fs::read_dir(templates_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("tera") {
                let content = std::fs::read_to_string(&path)?;
                templates.push(TemplateDef {
                    path: path.strip_prefix(&self.cache_dir)?.to_path_buf(),
                    content,
                });
            }
        }
        Ok(templates)
    }
}
```

**7. Implement Validator Contradiction** (Gate 3) - **2 hours**
```rust
// File: crates/ggen-marketplace/src/compatibility.rs:500-503
fn check_validators(packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
    let mut conflicts = Vec::new();
    let mut validator_rules: HashMap<String, Vec<&AtomicPackId>> = HashMap::new();

    for pack in packs {
        let declarations = load_ownership_declarations(pack)?;
        for decl in declarations {
            if matches!(decl.target, OwnershipTarget::ProtocolField(_)) {
                if matches!(decl.class, OwnershipClass::Exclusive) {
                    validator_rules
                        .entry(format!("{:?}", decl.target))
                        .or_default()
                        .push(pack);
                }
            }
        }
    }

    for (target, owners) in validator_rules {
        if owners.len() > 1 {
            conflicts.push(Conflict {
                dimension: CompatibilityDimension::ValidatorContradiction,
                pack_a: owners[0].clone(),
                pack_b: owners[1].clone(),
                description: format!("Multiple exclusive claims on {}", target),
                severity: ConflictSeverity::Error,
                resolution: Some(ConflictResolution::Fail),
            });
        }
    }

    Ok(conflicts)
}
```

**8. Implement Policy Contradiction** (Gate 3) - **2 hours**
```rust
// File: crates/ggen-marketplace/src/compatibility.rs:506-509
fn check_policies(packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
    let mut conflicts = Vec::new();
    let mut policy_map: HashMap<String, Vec<&AtomicPackId>> = HashMap::new();

    for pack in packs {
        let policies = load_pack_policies(pack)?;
        for policy in policies {
            if matches!(policy, PolicyRule::ForbidTemplateDefaults) {
                policy_map
                    .entry("template_defaults".to_string())
                    .or_default()
                    .push(pack);
            }
        }
    }

    for (_target, packs) in policy_map {
        if packs.len() > 1 {
            conflicts.push(Conflict {
                dimension: CompatibilityDimension::PolicyContradiction,
                pack_a: packs[0].clone(),
                pack_b: packs[1].clone(),
                description: "Conflicting policy rules".to_string(),
                severity: ConflictSeverity::Error,
                resolution: Some(ConflictResolution::UserResolve),
            });
        }
    }

    Ok(conflicts)
}
```

---

### 🟢 NICE TO HAVE (Can Defer) - 6-12h

9. Unify Lockfile Format (Gate 1) - 30 min
10. Track File-to-Pack Provenance (Gate 2) - 1-2 hours
11. Populate Receipt Fields (Gate 6) - 2 hours
12. Integrate CompositionReceipt (Gate 6) - 3 hours
13. Maintain Receipt Chain (Gate 6) - 2 hours
14. Add Bundle Expansion Display (Bundle/Profile) - 30 min
15. Rename Profile (Bundle/Profile) - 5 min

---

## File Reference Guide

### Files to Modify

| Fix # | File | Lines | Change |
|-------|------|-------|--------|
| 1 | `crates/ggen-marketplace/src/install.rs` | 391-405 | Wire signature verification |
| 2 | `crates/ggen-marketplace/src/install.rs` | 360-374 | Wire HTTP download |
| 3 | `crates/ggen-marketplace/src/compatibility.rs` | 500-573 | Replace placeholders |
| 4a | `crates/ggen-marketplace/src/install.rs` | Function sig | Add profile parameter |
| 4b | `crates/ggen-marketplace/src/models.rs` | New impl | Package→PackContext |
| 4c | `crates/ggen-cli/src/cmds/packs.rs` | Struct | Add --profile flag |
| 5 | `crates/ggen-core/src/pack_resolver.rs` | New fn | get_pack_queries() |
| 6 | `crates/ggen-core/src/pack_resolver.rs` | New fn | get_pack_templates() |
| 7 | `crates/ggen-marketplace/src/compatibility.rs` | 500-503 | Implement |
| 8 | `crates/ggen-marketplace/src/compatibility.rs` | 506-509 | Implement |

---

## Validation Commands

### After Each Fix

```bash
# Verify compilation
cargo make check

# Run tests
cargo make test

# Run lint
cargo make lint

# Verify specific module
cargo test -p ggen-marketplace
cargo test -p ggen-core
cargo test -p ggen-cli
```

### Final Validation

```bash
# Full check
cargo make check && cargo make test && cargo make lint

# Verify signature verification works
RUST_LOG=trace cargo test -p ggen-marketplace --test install_test -- --nocapture

# Verify policy enforcement works
RUST_LOG=trace cargo test -p ggen-marketplace --test policy_test -- --nocapture

# Verify conflict detection works
RUST_LOG=trace cargo test -p ggen-marketplace --test compatibility_test -- --nocapture
```

---

## Progress Tracking

### Phase 1: Blocking Fixes (7h 50m) - PRODUCTION UNBLOCKED

- [ ] Fix 1: Wire Signature Verification (45 min)
- [ ] Fix 2: Wire HTTP Download (30 min)
- [ ] Fix 3: Replace Conflict Placeholders (5 min)
- [ ] Fix 4: Integrate Policy Enforcement (7 hours)

### Phase 2: High Priority (8h) - FULL FUNCTIONALITY

- [ ] Fix 5: Pack Query Loading (2-3 hours)
- [ ] Fix 6: Pack Template Loading (2-3 hours)
- [ ] Fix 7: Validator Contradiction (2 hours)
- [ ] Fix 8: Policy Contradiction (2 hours)

### Phase 3: Nice to Have (6-12h) - COMPLETE PLATFORM

- [ ] Fix 9-15: Remaining improvements

---

## Decision Matrix

| After Phase | Status | Can Ship to Fortune 5? |
|-------------|--------|----------------------|
| Now (76%) | 🟡 Conditional | ❌ NO - Security gaps |
| After Phase 1 (85%) | 🟢 Ready | ✅ YES - Core requirements met |
| After Phase 2 (95%) | 🟢 Ready | ✅ YES - Full functionality |
| After Phase 3 (100%) | 🟢 Ready | ✅ YES - Complete platform |

---

## Quick Reference

### Critical Files

```
crates/ggen-marketplace/src/
├── install.rs              # Gate 1 fixes
├── compatibility.rs        # Gate 3 fixes
├── policy.rs               # Gate 5 fixes
└── models.rs               # Gate 5 fixes

crates/ggen-core/src/
├── pack_resolver.rs        # Gate 2 fixes
└── v6/pipeline.rs          # Gate 2, 6 fixes

crates/ggen-cli/src/cmds/
└── packs.rs                # Gate 5 fixes
```

### Dependencies to Add

```toml
# In crates/ggen-marketplace/Cargo.toml
[dependencies]
reqwest = { version = "0.11", features = ["json"] }
```

---

## End State Goal

**After 24-28 hours:** ✅ **FORTUNE 5 CISO PRODUCTION APPROVED**

- All 6 gates passing (100%)
- Full supply chain security
- Complete policy enforcement
- Multi-dimensional conflict detection
- Full pack participation in sync
- Complete provenance tracking

---

**Document End**
