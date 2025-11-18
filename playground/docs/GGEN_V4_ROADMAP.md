# GGEN v4.0 ROADMAP: Full-Lifecycle Packs System

**Date**: 2025-11-18
**Version**: 1.0 - Strategic Direction
**Audience**: Architecture, Product, Engineering

---

## EXECUTIVE SUMMARY

**Vision**: Transform ggen from a code-generation tool into a **living project platform** where projects are continuously evolved through pack-based lifecycle management.

**Current State (v3.2)**:
- ✅ Marketplace: Robust package discovery, install, publish
- ✅ Packs: Initial project scaffolding from bundles
- ❌ Lifecycle: One-time generation, no updates
- ❌ Integration: Packs and lifecycle disconnected

**Target State (v4.0)**:
- ✅ Pack-Based Lifecycle: Projects track and apply pack updates
- ✅ Full Regeneration: Intelligently merge pack updates with manual edits
- ✅ Continuous Evolution: Projects stay in sync with pack improvements
- ✅ Conflict Resolution: Smart three-way merge for pack version transitions

**Timeline**:
- **MVP (Phase 1-2)**: 4-6 weeks
- **Production (Phase 1-4)**: 8-12 weeks
- **Ecosystem Ready**: 12-16 weeks

**Strategic Impact**:
- 📈 **Reduces maintenance burden**: Packs handle framework updates automatically
- 🔄 **Enables pack evolution**: Pack creators can improve without breaking projects
- 🚀 **Faster onboarding**: New projects get production-ready scaffolding + updates
- 💰 **Monetization**: "Managed packs" subscription service for enterprises
- 🌍 **Community growth**: Ecosystem of composable, updatable packs

---

## PART 1: ARCHITECTURAL VISION

### 1.1 Current Architecture vs Target

#### Current (v3.2): Disconnected Systems
```
Marketplace       Packs             Lifecycle System
├─ search        ├─ list           ├─ state_machine
├─ install       ├─ show           ├─ phase_execution
├─ publish       └─ compose        └─ regeneration
└─ update             ↓                    ↓
                    (dead end)        (unreachable from packs)
```

**Problem**: Packs are used only at `ggen project new`, then forgotten.

#### Target (v4.0): Integrated Lifecycle
```
Project Lifecycle Hub
├─ Pack Registry (versioned, with dependencies)
├─ Installation Engine (resolve → install → track)
├─ State Manager (snapshots, regions, lockfile)
├─ Update Engine (detect changes → merge → apply)
└─ Regeneration System (watch → plan → apply)
   ├─ Region Detection (preserve manual edits)
   ├─ Three-Way Merge (pack ← common → manual)
   └─ Conflict Resolution (manual overrides)
```

---

### 1.2 Core Concepts

#### Pack as Living Dependency
```rust
// v4.0: Packs become first-class project dependencies
[pack.dependencies]
"startup-essentials" = "1.2.x"
"observability-stack" = "~2.1"

// Lockfile tracks exact versions installed
[pack.lock]
"startup-essentials" = { version = "1.2.3", hash = "sha256:...", installed_at = "2025-11-18" }
```

#### Project State Tracking
```rust
// What we need to track for every file
struct GeneratedRegion {
    name: String,              // "auth_middleware", "db_schema"
    start_line: usize,
    end_line: usize,
    source: String,            // "startup-essentials/v1.2.3"
    hash: String,              // SHA256 of generated content
    is_editable: bool,         // Can user modify?
}

struct FileState {
    path: PathBuf,
    regions: Vec<GeneratedRegion>,
    manual_sections: Vec<ManualSection>,
    last_regenerated: DateTime<Utc>,
}
```

#### Regeneration Workflow
```
User runs: ggen packs update

1. Detect Changes
   ├─ Check for newer pack versions
   ├─ Load current project state (lockfile, snapshots, regions)
   └─ Compare with new pack definitions

2. Plan
   ├─ Identify regions needing updates
   ├─ Detect manual edits (preserve if possible)
   ├─ Resolve conflicts (pack upgrade vs manual edits)
   └─ Generate update plan

3. Preview
   ├─ Show what will change
   ├─ Highlight conflicts
   └─ Ask for confirmation

4. Apply
   ├─ Create backup snapshot
   ├─ Apply three-way merges
   ├─ Run pack hooks (if any)
   ├─ Verify file integrity
   └─ Update lockfile

5. Verify
   ├─ Run tests (if defined)
   ├─ Check build (if build system exists)
   └─ Report success/failures
```

---

## PART 2: DETAILED IMPLEMENTATION PHASES

### Phase 1: Foundation (Weeks 1-2)

**Goal**: Enable basic pack installation and state tracking for existing projects.

#### 1.1 Pack Installation Command
```bash
ggen packs install --pack_id startup-essentials
ggen packs install --pack_id startup-essentials --version 1.2.x
ggen packs install --pack_id startup-essentials --force  # Overwrite
```

**What to build**:
```rust
// New domain function
pub async fn install_pack(
    pack_id: &str,
    version: Option<&str>,
    project_dir: &Path,
    force: bool,
) -> Result<PackInstallResult>

// Returns
pub struct PackInstallResult {
    pub pack_id: String,
    pub version: String,
    pub files_created: usize,
    pub files_modified: usize,
    pub duration: Duration,
}
```

**Implementation**:
1. Resolve pack version (latest if not specified)
2. Load pack composition (templates + dependencies)
3. Copy/apply templates to project
4. Create/update `.ggen/packs.lock` entry
5. Create initial snapshot

**Estimated Effort**: 5 days (40% of Phase 1)

#### 1.2 Pack Lockfile System
```toml
# .ggen/packs.lock (new file)
[pack."startup-essentials"]
version = "1.2.3"
source = "registry"
integrity = "sha256:abc123..."
installed_at = "2025-11-18T13:24:00Z"
dependencies = ["observability-stack"]

[pack."observability-stack"]
version = "2.1.0"
source = "registry"
integrity = "sha256:def456..."
installed_at = "2025-11-18T13:24:05Z"
```

**What to build**:
```rust
pub struct PackLockfile {
    pub packs: BTreeMap<String, LockedPack>,
    pub updated_at: DateTime<Utc>,
    pub ggen_version: String,
}

pub struct LockedPack {
    pub version: String,
    pub source: PackSource,
    pub integrity: Option<String>,
    pub installed_at: DateTime<Utc>,
    pub dependencies: Vec<String>,
}

// Operations
impl PackLockfile {
    pub fn new() -> Self
    pub fn add_pack(&mut self, pack_id: String, locked: LockedPack) -> Result<()>
    pub fn remove_pack(&mut self, pack_id: &str) -> Result<()>
    pub fn update_pack(&mut self, pack_id: &str, version: String) -> Result<()>
    pub fn save(&self, path: &Path) -> Result<()>
    pub fn load(path: &Path) -> Result<Self>
    pub fn verify_integrity(&self) -> Result<Vec<IntegrityError>>
}
```

**Estimated Effort**: 3 days (30% of Phase 1)

#### 1.3 Pack State Snapshots
```rust
pub struct PackSnapshot {
    pub pack_id: String,
    pub version: String,
    pub created_at: DateTime<Utc>,
    pub templates_applied: Vec<AppliedTemplate>,
    pub files_created: BTreeMap<String, FileSnapshot>,
}

pub struct AppliedTemplate {
    pub name: String,
    pub source_path: PathBuf,
    pub output_dir: PathBuf,
    pub variables: BTreeMap<String, String>,
}

pub struct FileSnapshot {
    pub path: PathBuf,
    pub content_hash: String,
    pub size: usize,
    pub created_at: DateTime<Utc>,
}
```

**Storage**: `.ggen/packs/snapshots/{pack-id}-{version}-{timestamp}.json`

**Estimated Effort**: 2 days (20% of Phase 1)

#### 1.4 Phase 1 Deliverables
- ✅ `ggen packs install` command
- ✅ `.ggen/packs.lock` file management
- ✅ Pack state snapshots
- ✅ Tests: 20+ tests covering install paths
- ⏱️ **Total**: ~2 weeks

---

### Phase 2: Region Detection (Weeks 3-4)

**Goal**: Identify generated vs manual code sections for safe regeneration.

#### 2.1 Region Detection System

**Core Concept**: Mark code sections with metadata to distinguish generated from manual.

```rust
// Generated section markers
// ⚙️ GGEN[name:auth-middleware,source:startup-essentials/1.2.3] ⚙️
// ... auto-generated code ...
// ⚙️ /GGEN[name:auth-middleware] ⚙️

pub struct Region {
    pub name: String,                    // "auth-middleware"
    pub source_pack: String,             // "startup-essentials"
    pub source_version: String,          // "1.2.3"
    pub start_line: usize,
    pub end_line: usize,
    pub is_editable: bool,               // Can user modify?
    pub content_hash: String,            // Detect manual changes
}

pub fn detect_regions(
    content: &str,
    file_type: FileType,
) -> Result<Vec<Region>> {
    // Parse language-specific markers
    // Rust: // ⚙️ GGEN[...] ⚙️
    // TypeScript: /* ⚙️ GGEN[...] ⚙️ */
    // Python: # ⚙️ GGEN[...] ⚙️
    // HTML/XML: <!-- ⚙️ GGEN[...] ⚙️ -->
}
```

#### 2.2 Language-Specific Parsers

**Implement**:
1. **Rust** (most important):
   ```rust
   // ⚙️ GGEN[name:middleware,source:startup-essentials/1.2.3] ⚙️
   async fn middleware() { ... }
   // ⚙️ /GGEN[name:middleware] ⚙️
   ```

2. **TypeScript/JavaScript**:
   ```typescript
   /* ⚙️ GGEN[name:types,source:...] ⚙️ */
   export interface User { ... }
   /* ⚙️ /GGEN[name:types] ⚙️ */
   ```

3. **Python**:
   ```python
   # ⚙️ GGEN[name:models,source:...] ⚙️
   class User:
       pass
   # ⚙️ /GGEN[name:models] ⚙️
   ```

4. **SQL**:
   ```sql
   -- ⚙️ GGEN[name:schema,source:...] ⚙️
   CREATE TABLE users (...);
   -- ⚙️ /GGEN[name:schema] ⚙️
   ```

**Estimated Effort**: 5 days (50% of Phase 2)

#### 2.3 Region Injection System

**During pack install/regeneration**:
```rust
pub fn inject_region(
    template_output: &str,
    region_name: &str,
    pack_source: &str,
    file_type: FileType,
) -> String {
    let markers = get_markers_for_type(file_type);
    let start = format!("{} GGEN[name:{},source:{}] {}",
        markers.start_line, region_name, pack_source, markers.end_line);
    let end = format!("{} /GGEN[name:{}] {}",
        markers.start_line, region_name, markers.end_line);

    format!("{}\n{}\n{}", start, template_output, end)
}
```

**Estimated Effort**: 3 days (30% of Phase 2)

#### 2.4 Change Detection

```rust
pub fn detect_manual_changes(
    old_snapshot: &FileSnapshot,
    current_content: &str,
) -> Result<Vec<ManualChange>> {
    let old_regions = detect_regions(&old_snapshot.content)?;
    let new_regions = detect_regions(current_content)?;

    let mut changes = Vec::new();
    for region in old_regions {
        let old_hash = hash_content(&old_snapshot.extract_region(&region))?;
        let new_hash = hash_content(&extract_region(current_content, &region))?;

        if old_hash != new_hash && region.is_editable {
            changes.push(ManualChange {
                region,
                old_content: old_snapshot.extract_region(&region),
                new_content: extract_region(current_content, &region),
                confidence: calculate_confidence(&old_content, &new_content),
            });
        }
    }
    Ok(changes)
}
```

**Estimated Effort**: 2 days (20% of Phase 2)

#### 2.5 Phase 2 Deliverables
- ✅ Region detection for Rust, TypeScript, Python, SQL
- ✅ Region injection system
- ✅ Change detection algorithm
- ✅ Tests: 50+ tests covering detection edge cases
- ⏱️ **Total**: ~2 weeks

**Risk**: Parser edge cases. Mitigation: Start with simple parsers, add complexity iteratively.

---

### Phase 3: Update Engine (Weeks 5-7)

**Goal**: Detect, plan, and apply pack updates with conflict resolution.

#### 3.1 Update Detection

```bash
ggen packs check-updates
# Output:
# startup-essentials: 1.2.3 → 1.3.0 (breaking changes detected)
# observability-stack: 2.1.0 → 2.1.5 (patch, safe)
```

```rust
pub async fn check_pack_updates(
    project_dir: &Path,
) -> Result<Vec<AvailableUpdate>> {
    let lockfile = PackLockfile::load(&project_dir)?;
    let mut updates = Vec::new();

    for (pack_id, locked) in &lockfile.packs {
        let latest = registry.resolve(pack_id, "latest").await?;
        if latest.version != locked.version {
            let breaking_changes = detect_breaking_changes(
                &locked.version,
                &latest.version,
            )?;
            updates.push(AvailableUpdate {
                pack_id: pack_id.clone(),
                current_version: locked.version.clone(),
                latest_version: latest.version,
                breaking_changes,
                safety_level: if breaking_changes.is_empty() {
                    SafetyLevel::Safe
                } else {
                    SafetyLevel::Breaking
                },
            });
        }
    }
    Ok(updates)
}
```

**Estimated Effort**: 4 days (30% of Phase 3)

#### 3.2 Update Planning

```rust
pub fn plan_pack_update(
    pack_id: &str,
    new_version: &str,
    project_dir: &Path,
) -> Result<UpdatePlan> {
    // 1. Load old pack definition
    let old_pack = load_installed_pack(pack_id, project_dir)?;

    // 2. Load new pack definition
    let new_pack = registry.resolve(pack_id, new_version).await?;

    // 3. Detect what changed
    let template_changes = diff_templates(&old_pack.templates, &new_pack.templates)?;
    let dep_changes = diff_dependencies(&old_pack.deps, &new_pack.deps)?;

    // 4. For each changed template, plan file updates
    let mut operations = Vec::new();
    for (template_name, change) in template_changes {
        match change {
            TemplateChange::Modified { old, new } => {
                operations.push(PlanOperation::UpdateTemplate {
                    template_name,
                    old_content: old,
                    new_content: new,
                    affected_files: find_affected_files(&template_name, project_dir)?,
                    merge_strategy: determine_merge_strategy(&old, &new)?,
                });
            }
            TemplateChange::Removed { .. } => {
                operations.push(PlanOperation::RemoveTemplate { template_name });
            }
            TemplateChange::Added { .. } => {
                operations.push(PlanOperation::AddTemplate { template_name });
            }
        }
    }

    Ok(UpdatePlan {
        pack_id: pack_id.to_string(),
        from_version: old_pack.version,
        to_version: new_pack.version,
        operations,
        estimated_time: estimate_duration(&operations),
        risk_level: assess_risk(&operations),
    })
}

pub enum PlanOperation {
    UpdateTemplate {
        template_name: String,
        old_content: String,
        new_content: String,
        affected_files: Vec<PathBuf>,
        merge_strategy: MergeStrategy,
    },
    RemoveTemplate { template_name: String },
    AddTemplate { template_name: String },
    ResolveDependencyConflict {
        pack_id: String,
        conflict: DependencyConflict,
        resolution: ConflictResolution,
    },
}
```

**Estimated Effort**: 5 days (38% of Phase 3)

#### 3.3 Three-Way Merge Engine

**Most critical component** - handles conflicts between pack updates and manual edits.

```rust
pub fn three_way_merge(
    base: &str,           // Original generated content
    ours: &str,           // Pack update (new generated)
    theirs: &str,         // Current file (with manual edits)
) -> Result<MergeResult> {
    // Use existing diff3 logic, but enhanced for code

    match (diff_lines(base, ours), diff_lines(base, theirs)) {
        // Clean merge: pack updated, no manual changes
        (Some(pack_changes), None) => {
            Ok(MergeResult::Success { merged: ours.to_string() })
        }

        // Clean merge: manual changes, pack didn't touch that part
        (None, Some(_manual_changes)) => {
            Ok(MergeResult::Success { merged: theirs.to_string() })
        }

        // Both changed in same lines - conflict
        (Some(pack_changes), Some(manual_changes)) => {
            if overlaps(&pack_changes, &manual_changes) {
                Ok(MergeResult::Conflict {
                    conflicts: vec![Conflict {
                        line_range: get_overlap_range(&pack_changes, &manual_changes),
                        base_content: extract_lines(base, &range),
                        pack_content: extract_lines(ours, &range),
                        manual_content: extract_lines(theirs, &range),
                    }],
                    merge_proposal: propose_resolution(base, ours, theirs)?,
                })
            } else {
                // Changes don't overlap - merge both
                Ok(MergeResult::Success { merged: apply_both_changes(base, ours, theirs)? })
            }
        }

        // No changes in either
        (None, None) => Ok(MergeResult::Success { merged: theirs.to_string() }),
    }
}

pub enum MergeResult {
    Success { merged: String },
    Conflict {
        conflicts: Vec<Conflict>,
        merge_proposal: String,
    },
}

pub struct Conflict {
    pub line_range: (usize, usize),
    pub base_content: String,
    pub pack_content: String,
    pub manual_content: String,
}
```

**Estimated Effort**: 6 days (46% of Phase 3)

**Risk**: Merge logic bugs can cause data loss. Mitigation: Comprehensive property-based testing.

#### 3.4 Conflict Resolution

```bash
ggen packs update --pack_id startup-essentials
# Plan shows conflicts...
#
# Conflict in src/auth.rs (line 42-68)
# ⚙️ auth-middleware region
#
# Pack wants:  New JWT validation (version 2.0)
# You have:    Custom token verification
#
# Options:
# 1. Use pack update (discard manual)
# 2. Keep manual edits (reject update)
# 3. Manual merge (edit conflicts)
# 4. Show diff
```

```rust
pub enum ConflictResolution {
    UsePack,           // Accept pack update, lose manual changes
    KeepManual,        // Reject pack update, stay on old version
    ManualMerge,       // User will edit marked conflict
    CustomMerge(String), // User-provided merge
}

pub struct ConflictPrompt {
    pub file_path: PathBuf,
    pub region_name: String,
    pub line_range: (usize, usize),
    pub base: String,
    pub pack_update: String,
    pub manual_edits: String,
}

pub fn prompt_conflict_resolution(conflict: &ConflictPrompt) -> Result<ConflictResolution> {
    println!("Conflict in {} region '{}'", conflict.file_path.display(), conflict.region_name);
    println!("\nPack update:\n{}", highlight_diff(&conflict.base, &conflict.pack_update));
    println!("\nYour edits:\n{}", highlight_diff(&conflict.base, &conflict.manual_edits));

    // Interactive resolution
    loop {
        match prompt_user("Resolve: (1) Use pack, (2) Keep manual, (3) Edit manually") {
            "1" => return Ok(ConflictResolution::UsePack),
            "2" => return Ok(ConflictResolution::KeepManual),
            "3" => {
                let edited = editor::open(create_merge_template(conflict))?;
                return Ok(ConflictResolution::CustomMerge(edited));
            }
            "d" => println!("{}", show_detailed_diff(conflict)),
            _ => println!("Invalid option"),
        }
    }
}
```

**Estimated Effort**: 3 days (23% of Phase 3)

#### 3.5 Phase 3 Deliverables
- ✅ `ggen packs check-updates` command
- ✅ Update planning engine
- ✅ Three-way merge with conflict detection
- ✅ Interactive conflict resolution
- ✅ Tests: 100+ tests (merge logic is critical)
- ⏱️ **Total**: ~3 weeks

---

### Phase 4: Lifecycle Integration (Weeks 8-10)

**Goal**: Integrate pack updates into continuous regeneration workflow.

#### 4.1 Watch Mode with Pack Updates

```bash
ggen packs watch --path ./my-project --check-updates daily
# Auto-detects:
# - Template changes in pack directories
# - Pack version updates (daily check)
# - RDF/SPARQL changes
# Regenerates automatically or prompts for conflicts
```

```rust
pub struct PackWatcher {
    project_dir: PathBuf,
    check_updates_frequency: UpdateCheckFrequency,
    auto_merge_safe: bool,  // Auto-apply safe (non-breaking) updates
}

impl PackWatcher {
    pub async fn run(&mut self) -> Result<()> {
        let watcher = notify::recommended_watcher(|event| {
            match event {
                Event::Modify { paths } => {
                    if paths.iter().any(|p| self.is_pack_related(p)) {
                        self.trigger_regeneration()
                    }
                }
                Event::Custom("check_updates") => {
                    self.check_and_apply_updates()
                }
                _ => {}
            }
        })?;

        // Periodically check for updates
        if let Some(frequency) = self.check_updates_frequency {
            tokio::spawn(async move {
                loop {
                    tokio::time::sleep(frequency.as_duration()).await;
                    watcher.notify_custom("check_updates")
                }
            });
        }

        Ok(())
    }
}
```

**Estimated Effort**: 4 days (33% of Phase 4)

#### 4.2 Continuous Integration

```bash
# In CI/CD pipeline
ggen packs verify --check-updates --fail-on-conflicts

# Output:
# ✓ all packs current (startup-essentials 1.2.3)
# ✓ no manual conflicts
# ✓ build successful
# ✓ tests passing
#
# Project is production-ready
```

```rust
pub async fn verify_pack_state(
    project_dir: &Path,
    fail_on_conflicts: bool,
) -> Result<VerificationReport> {
    let lockfile = PackLockfile::load(project_dir)?;
    let mut issues = Vec::new();

    // 1. Check for available updates
    let updates = check_pack_updates(project_dir).await?;
    if !updates.is_empty() && fail_on_conflicts {
        issues.push(Issue::UpdatesAvailable { updates });
    }

    // 2. Verify integrity
    for (pack_id, locked) in &lockfile.packs {
        let hash = compute_pack_hash(pack_id, project_dir)?;
        if hash != locked.integrity.as_ref().ok_or("missing")? {
            issues.push(Issue::IntegrityCheckFailed { pack_id: pack_id.clone() });
        }
    }

    // 3. Check for unresolved conflicts
    let conflicts = find_unresolved_conflicts(project_dir)?;
    if !conflicts.is_empty() {
        issues.push(Issue::UnresolvedConflicts { conflicts });
    }

    Ok(VerificationReport {
        status: if issues.is_empty() { Status::Ok } else { Status::HasIssues },
        issues,
        lockfile_valid: lockfile_valid(lockfile),
        last_update: get_last_update_time(project_dir),
    })
}
```

**Estimated Effort**: 3 days (25% of Phase 4)

#### 4.3 Rollback System

```bash
ggen packs rollback --pack_id startup-essentials
# Reverts to previous version from backup
```

```rust
pub async fn rollback_pack_update(
    pack_id: &str,
    project_dir: &Path,
) -> Result<RollbackResult> {
    // 1. Find previous snapshot
    let snapshots = list_pack_snapshots(pack_id, project_dir)?;
    let previous = snapshots.iter().rev().nth(1)
        .ok_or("No previous version to rollback to")?;

    // 2. Restore from snapshot
    let backup_path = project_dir.join(".ggen").join("backups").join(&previous.timestamp);

    // 3. Restore files
    for (file_path, content) in &previous.files {
        fs::write(project_dir.join(file_path), content)?;
    }

    // 4. Update lockfile
    let mut lockfile = PackLockfile::load(project_dir)?;
    lockfile.update_pack(pack_id, &previous.version)?;
    lockfile.save(project_dir)?;

    Ok(RollbackResult {
        pack_id: pack_id.to_string(),
        from_version: get_current_version(pack_id, project_dir)?,
        to_version: previous.version.clone(),
        duration: previous.duration,
    })
}
```

**Estimated Effort**: 3 days (25% of Phase 4)

#### 4.4 Reporting & Metrics

```bash
ggen packs status
# Output:
# Installed Packs:
# ├─ startup-essentials (1.2.3, installed 45 days ago)
# │  ├─ Last updated: 23 days ago
# │  ├─ Updates available: 1.3.0 (breaking changes)
# │  └─ Manual edits: 3 regions
# ├─ observability-stack (2.1.0, installed 30 days ago)
# │  ├─ Last updated: 2 days ago
# │  ├─ Updates available: 2.1.5 (safe)
# │  └─ Manual edits: 0 regions
#
# Summary:
# - 2 packs with available updates
# - 3 regions with manual edits
# - 45 days since last major update
# - No conflicts detected
```

```rust
pub struct PackStatus {
    pub pack_id: String,
    pub current_version: String,
    pub latest_version: Option<String>,
    pub installed_at: DateTime<Utc>,
    pub last_updated: DateTime<Utc>,
    pub manual_edits_count: usize,
    pub regions: Vec<RegionStatus>,
}

pub fn generate_status_report(project_dir: &Path) -> Result<StatusReport> {
    let lockfile = PackLockfile::load(project_dir)?;
    let mut statuses = Vec::new();

    for (pack_id, locked) in &lockfile.packs {
        let snapshot = load_latest_snapshot(pack_id, project_dir)?;
        let manual_edits = find_manual_edits_in_pack(pack_id, project_dir)?;

        statuses.push(PackStatus {
            pack_id: pack_id.clone(),
            current_version: locked.version.clone(),
            latest_version: check_latest_version(pack_id).await.ok(),
            installed_at: locked.installed_at,
            last_updated: snapshot.created_at,
            manual_edits_count: manual_edits.len(),
            regions: manual_edits.into_iter()
                .map(|edit| RegionStatus {
                    name: edit.region_name,
                    size: edit.content.len(),
                    last_modified: edit.timestamp,
                    merge_difficulty: assess_merge_difficulty(&edit),
                })
                .collect(),
        });
    }

    Ok(StatusReport {
        packs: statuses,
        summary: generate_summary(&statuses),
        recommendations: generate_recommendations(&statuses),
    })
}
```

**Estimated Effort**: 2 days (17% of Phase 4)

#### 4.5 Phase 4 Deliverables
- ✅ `ggen packs watch` with update checks
- ✅ `ggen packs verify` for CI/CD integration
- ✅ Rollback system with snapshots
- ✅ `ggen packs status` reporting
- ✅ Tests: 80+ tests covering lifecycle
- ⏱️ **Total**: ~3 weeks

---

## PART 3: IMPLEMENTATION ROADMAP SUMMARY

```
v3.2 (Current)
├─ Marketplace: Complete
├─ Packs: Read-only (list, show, validate)
└─ Lifecycle: One-time generation only

Phase 1 (Weeks 1-2): Foundation
├─ ggen packs install command
├─ .ggen/packs.lock tracking
└─ Pack state snapshots

Phase 2 (Weeks 3-4): Region Detection
├─ Language-specific parsers (Rust, TS, Python, SQL)
├─ Region injection system
└─ Manual change detection

Phase 3 (Weeks 5-7): Update Engine
├─ Pack version checking
├─ Update planning
├─ Three-way merge (critical!)
└─ Conflict resolution UI

Phase 4 (Weeks 8-10): Lifecycle Integration
├─ Continuous watch mode
├─ CI/CD verification
├─ Rollback system
└─ Status reporting

v4.0 (Full Feature Complete)
├─ Marketplace: Enhanced (v1 + v2)
├─ Packs: Full lifecycle management
└─ Lifecycle: Continuous regeneration with updates
```

---

## PART 4: TESTING STRATEGY

### Test Coverage Target: 85% for Lifecycle Code

#### Phase 1 Testing (Pack Installation)
- ✅ 20+ unit tests for lockfile operations
- ✅ 15+ integration tests for pack installation
- ✅ 10+ tests for state snapshot creation
- ✅ Error case handling (missing packs, corrupted lockfile, etc.)

#### Phase 2 Testing (Region Detection)
- ✅ 50+ unit tests for parsers (Rust, TS, Python, SQL)
  - Single regions
  - Nested regions
  - Malformed markers
  - Edge cases (empty regions, overlapping)
- ✅ 30+ tests for change detection
- ✅ 20+ tests for region injection
- ✅ Property-based testing for parser robustness

#### Phase 3 Testing (Merge Logic)
- ✅ 100+ merge scenario tests
  - Clean merges (pack only, manual only)
  - Conflict cases (overlapping changes)
  - Multi-region files
  - Different file types
- ✅ Fuzzing for merge edge cases
- ✅ Real-world scenario testing (framework upgrades, etc.)

#### Phase 4 Testing (Lifecycle)
- ✅ End-to-end tests for full workflow
- ✅ Watch mode tests
- ✅ CI/CD integration tests
- ✅ Rollback scenario tests

---

## PART 5: RISK MANAGEMENT

### Critical Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|-----------|
| **Three-way merge bugs causing data loss** | CRITICAL | Extensive property-based testing, always create backups, conservative merge defaults |
| **Region detection parsing failures** | HIGH | Start with simple parsers, comprehensive test suite, fallback to no-region mode |
| **Circular pack dependencies** | HIGH | Use existing dependency resolution from marketplace-v2, add cycle detection tests |
| **Lockfile corruption** | MEDIUM | Atomic writes, backup before save, corruption recovery |
| **Performance regression on large projects** | MEDIUM | Benchmark suite, parallel processing where possible, caching |
| **Breaking changes in marketplace-v2 RDF** | MEDIUM | Use trait-based abstraction (already done), version marshalling |

### Testing for Safety

1. **Property-Based Testing**: Use quickcheck for merge logic
2. **Fuzzing**: Fuzz parsers with malformed inputs
3. **Scenario Testing**: Real pack upgrade scenarios
4. **Regression Testing**: Once users report issues, add tests
5. **Staged Rollout**: Beta testing with early adopters before GA

---

## PART 6: SUCCESS METRICS

### Development Metrics
- ✅ All phases completed on schedule
- ✅ 85%+ test coverage for lifecycle code
- ✅ Zero critical data loss incidents in beta
- ✅ 95%+ successful merge operations

### Product Metrics (Post-Launch)
- 📈 30%+ of new projects created with packs
- 📈 50%+ of pack-created projects apply at least one update
- 📈 10%+ adoption of managed packs (enterprise subscription)
- 📈 <2% conflict rate on pack updates
- 📈 <1 hour average time to merge pack updates

### Community Metrics
- 📈 50+ custom packs published to marketplace
- 📈 100+ pack ratings/reviews
- 📈 10x increase in template contributions
- 📈 Zero security incidents related to pack updates

---

## PART 7: GO-TO-MARKET STRATEGY

### v4.0 Launch Positioning

**Tagline**: "Keep your projects fresh with living packs"

**Key Messages**:
1. **For Individual Developers**: Never maintain boilerplate again. Packs evolve automatically.
2. **For Teams**: Unified stack updates across all projects. Stay on same versions.
3. **For Enterprises**: Managed pack service. SLA-backed updates, conflict resolution included.

### Launch Phases

**Week 1-2**: Beta Launch (Invite 50 early adopters)
- Install beta channel: `ggen install @beta`
- Test with real projects
- Gather feedback on UX

**Week 3**: GA Launch
- Full documentation
- Blog posts on key features
- Examples and templates
- Community call showcasing use cases

**Month 2**: Monetization
- Introduce "Managed Packs" (enterprise tier)
  - SLA for update timing
  - Priority conflict resolution
  - Custom pack creation
  - Support included

---

## PART 8: ROADMAP TIMELINE SUMMARY

| Phase | Duration | Effort | Delivered | Go-Live |
|-------|----------|--------|-----------|---------|
| 1: Foundation | 2 weeks | 40 days | install, lockfile, snapshots | Internal/alpha |
| 2: Region Detection | 2 weeks | 35 days | parsers, injection, detection | Internal/alpha |
| 3: Update Engine | 3 weeks | 52 days | planning, merge, conflict UI | Beta (week 7) |
| 4: Lifecycle Integration | 3 weeks | 42 days | watch, verify, rollback, status | GA (week 10) |
| **Total** | **10 weeks** | **169 days** | **Full v4.0** | **Week 10** |

---

## PART 9: STRATEGIC RECOMMENDATIONS

### 1. Start with Phase 1 Immediately
- Lowest risk, highest value
- Unblocks marketplace integration
- Establishes patterns for later phases

### 2. Invest Heavily in Phase 3 Testing
- Three-way merge is most critical component
- Budget extra time for property-based testing
- Consider hiring external code review for merge logic

### 3. Plan Enterprise Features Early
- "Managed Packs" subscription model
- Custom pack creation for enterprises
- White-label pack hosting
- This is 30-50% margin opportunity

### 4. Build Community Tools
- Pack authoring CLI (`ggen pack new`)
- Pack validation toolkit
- Pack usage analytics
- This drives ecosystem growth

### 5. Plan Marketplace-v2 Migration
- Once v4.0 lifecycle is stable
- Migrate marketplace to RDF backend
- Unlock semantic versioning, advanced queries
- 6-month post-v4.0 launch

---

## CONCLUSION

**ggen v4.0 transforms code generation from a one-time scaffolding tool into a continuous project evolution platform.** By enabling pack-based lifecycle management with intelligent update resolution, we unlock:

1. **Better Developer Experience**: Templates stay current automatically
2. **Team Efficiency**: Unified stack updates across all projects
3. **Enterprise Revenue**: Managed packs with SLA guarantees
4. **Community Growth**: Ecosystem of composable, updatable packs

The technical foundation is solid (marketplace, lifecycle system, state tracking all exist). v4.0 is about **connecting the dots** and adding intelligent merge logic to make packs truly living dependencies.

**Timeline: 10 weeks to GA, 16 weeks to enterprise-ready.**

---

**Document Version**: 1.0
**Last Updated**: 2025-11-18
**Status**: Ready for Stakeholder Review
