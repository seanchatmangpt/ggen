# Pack Lifecycle System - Gap Analysis

**Date**: 2025-11-18
**Objective**: Analyze gaps between marketplace and packs implementations to understand what's needed for packs to become a full lifecycle system.

## Executive Summary

**Current State**: Marketplace has robust installation/update mechanisms, but packs are isolated to initial project creation. There's no integration between pack-based project generation and the lifecycle regeneration system.

**Gap**: Packs cannot update existing projects, track what was installed, or regenerate when pack versions change.

**Solution Path**: Bridge packs → lifecycle → snapshots → regeneration to create a complete project lifecycle.

---

## 1. Marketplace Lifecycle Analysis

### 1.1 Installation Workflow (`ggen-marketplace/src/install.rs`)

**What It Has:**
```rust
- Dependency resolution (transitive, cycle detection)
- Atomic installation with rollback
- Package verification (checksums, signatures)
- Lockfile management (ggen.lock)
- Cache management (.ggen/cache)
- Concurrent write protection (file locking)
- Corruption recovery (backup/restore)
```

**Key Mechanisms:**
1. **Download & Verify**
   - Downloads from registry with retry logic
   - SHA256 checksum validation
   - Extracts to `~/.ggen/packages/{package_name}`

2. **Lockfile Tracking** (`ggen-domain/src/marketplace/install.rs`)
   ```rust
   pub struct LockfileEntry {
       pub name: String,
       pub version: String,
       pub resolved: String,        // Source URL
       pub integrity: Option<String>, // SHA256
       pub dependencies: HashMap<String, String>,
   }
   ```

3. **State Persistence**
   - Atomic writes (temp + rename)
   - Backup before save
   - Corruption detection and recovery

### 1.2 Update Mechanisms (`ggen-domain/src/marketplace/update.rs`)

**What It Has:**
```rust
- Version compatibility checking (semver)
- Registry freshness validation (<7 days)
- Atomic update with rollback
- Breaking change detection
- Compatibility warnings
```

**Update Flow:**
1. Check for updates in registry
2. Validate compatibility (major version checks)
3. Save lockfile state as backup
4. Install new version (force=true)
5. On error → restore lockfile backup

### 1.3 Dependency Resolution

**Capabilities:**
- Transitive dependency resolution
- Circular dependency detection (DFS cycle check)
- Topological sorting for install order
- Conflict detection
- Missing dependency warnings

**Limitations:**
- Simplified semver handling (no complex version constraints)
- No automatic dependency upgrade paths
- No parallel dependency downloads

---

## 2. Current Packs Capabilities

### 2.1 What Packs Support (`ggen-domain/src/marketplace/packs/`)

**Current Operations:**
```rust
pub mod list;  // Only command implemented
```

**That's It.** The packs system is extremely minimal:
- `list` - Show installed packages with filtering/sorting
- No `install`, `uninstall`, `update` commands
- No integration with project generation
- No state tracking

### 2.2 What Packs Are Missing

**Critical Gaps:**
1. ❌ **No install mechanism** - Can't install packs to existing projects
2. ❌ **No state tracking** - Don't know what packs are in a project
3. ❌ **No version tracking** - Can't tell which pack version was used
4. ❌ **No update capability** - Can't upgrade packs in existing projects
5. ❌ **No regeneration** - Can't reapply packs when they change
6. ❌ **No lockfile integration** - No connection to ggen.lock
7. ❌ **No dependency resolution** - Can't handle pack dependencies

---

## 3. Project Regeneration System

### 3.1 Lifecycle System (`ggen-core/src/lifecycle/`)

**What It Provides:**
```rust
// Phase execution with hooks
pub fn run_phase(ctx: &Context, phase_name: &str) -> Result<()>

// State persistence
pub struct LifecycleState {
    pub last_phase: Option<String>,
    pub phase_history: Vec<RunRecord>,
    pub generated: Vec<GeneratedFile>,  // ← Tracks what was generated
    pub cache_keys: Vec<CacheKey>,
}
```

**Regeneration Capabilities:**
- Tracks generated files with hashes
- Phase execution history
- Cache key generation for reproducibility
- Hook system (before/after)

**How It Detects Changes:**
```rust
// File-level change detection via SHA256
pub struct GeneratedFile {
    pub path: String,
    pub hash: String,  // SHA256 of content
}
```

**Limitations:**
- **No region tracking** - Can't distinguish generated vs manual sections within files
- **No three-way merge** - Can't merge pack updates with manual edits
- **No pack versioning** - Doesn't track which pack version generated what

### 3.2 Snapshot System (`ggen-core/src/snapshot.rs`)

**What It Provides:**
```rust
pub struct Snapshot {
    pub name: String,
    pub graph: GraphSnapshot,     // RDF state
    pub files: Vec<FileSnapshot>,  // Generated files
    pub templates: Vec<TemplateSnapshot>,
}

pub struct FileSnapshot {
    pub path: PathBuf,
    pub hash: String,
    pub generated_regions: Vec<Region>,  // ← For three-way merge!
    pub manual_regions: Vec<Region>,
}
```

**Key Capabilities:**
1. **Baseline Tracking** - Snapshot of initial generation
2. **Drift Detection** - Compare current vs snapshot
3. **Region Tracking** - Identify generated vs manual code
4. **Three-Way Merge** - Merge pack updates with manual edits

**Current State:**
```rust
// Placeholder implementations
fn detect_regions(_content: &str) -> Vec<Region> {
    Vec::new() // ← NOT IMPLEMENTED
}
```

**Region tracking is STUBBED OUT** - critical missing piece for safe regeneration.

### 3.3 Watch/Regeneration System

**Status**: ❌ **DOES NOT EXIST**

There's no watch system. The lifecycle system provides:
- Phase re-execution
- State tracking
- Cache invalidation

But there's no:
- File watching
- Automatic regeneration triggers
- Pack version change detection
- Incremental updates

---

## 4. State Persistence

### 4.1 Snapshot System

**What It Tracks:**
```rust
// Graph state (RDF knowledge base)
pub struct GraphSnapshot {
    pub hash: String,
    pub triple_count: usize,
    pub sources: Vec<String>,
}

// File state (generated artifacts)
pub struct FileSnapshot {
    pub hash: String,
    pub generated_regions: Vec<Region>,  // ← Stubbed
    pub manual_regions: Vec<Region>,      // ← Stubbed
}

// Template state (what was used to generate)
pub struct TemplateSnapshot {
    pub hash: String,
    pub queries: Vec<String>,  // ← Stubbed
}
```

**Gaps:**
- ❌ No pack version tracking
- ❌ No dependency graph
- ❌ No variable/context tracking
- ❌ Region detection not implemented

### 4.2 Lockfile System (`ggen-core/src/lockfile.rs`)

**What It Tracks:**
```rust
pub struct LockEntry {
    pub id: String,
    pub version: String,
    pub sha256: String,
    pub source: String,
    pub dependencies: Option<Vec<String>>,
    pub pqc_signature: Option<String>,  // Post-quantum crypto
}
```

**Strong Points:**
- Dependency resolution with caching
- PQC signature support
- Version pinning
- Transitive dependency tracking

**Gaps:**
- ❌ No integration with snapshots
- ❌ No connection to generated files
- ❌ No pack → file mapping

---

## 5. Integration Points

### 5.1 How Marketplace Interacts with Project Generation

**Current Flow:**
```
marketplace install → downloads to ~/.ggen/packages/{pack}
                    → updates ggen.lock
                    → NO integration with project generation
```

**Missing Flow:**
```
packs install → should trigger:
              1. Download pack (if not cached)
              2. Load pack manifest (gpack.toml)
              3. Execute project generator
              4. Create snapshot of generated files
              5. Update lockfile with pack version
              6. Track pack → files mapping
```

### 5.2 How Lifecycle Interacts with Packs

**Current**: ❌ **NO INTERACTION**

The lifecycle system (`make.toml` phases) and packs are completely separate:
- Lifecycle runs phases (init, build, test, deploy)
- Packs are for initial scaffolding
- No mechanism to reapply packs during lifecycle

**Needed Flow:**
```
ggen lifecycle run template →
  1. Check if pack versions changed (ggen.lock)
  2. Load snapshot of last generation
  3. Detect manual edits (diff current vs snapshot)
  4. Regenerate from updated packs
  5. Three-way merge (snapshot, manual, new)
  6. Update snapshot
  7. Update lockfile
```

### 5.3 Missing Integration: Pack-Based Project Lifecycle

**What's Missing:**

1. **Pack Metadata in Snapshots**
   ```rust
   // Should be added to Snapshot
   pub struct PackSnapshot {
       pub pack_id: String,
       pub pack_version: String,
       pub files_generated: Vec<PathBuf>,
       pub variables_used: HashMap<String, String>,
   }
   ```

2. **Pack → File Mapping**
   ```rust
   // Should track which pack generated which files
   pub struct GeneratedFile {
       pub path: PathBuf,
       pub hash: String,
       pub pack_id: String,      // ← Missing
       pub pack_version: String, // ← Missing
   }
   ```

3. **Regeneration Trigger System**
   ```rust
   // Detect when to regenerate
   pub fn should_regenerate() -> bool {
       // Check lockfile vs snapshot pack versions
       // Check for manual edits
       // Check for pack updates in registry
   }
   ```

---

## 6. Detailed Gap Analysis

### 6.1 Critical Gaps (Blockers)

| Gap | Impact | Complexity | Priority |
|-----|--------|------------|----------|
| **No pack install command** | Can't apply packs to projects | Medium | P0 |
| **No pack version tracking** | Can't detect when to update | Low | P0 |
| **No pack → file mapping** | Can't know what to regenerate | Medium | P0 |
| **Region detection stubbed** | Can't preserve manual edits | High | P0 |
| **No regeneration command** | Can't update from new pack versions | Medium | P0 |

### 6.2 Important Gaps (Needed for Production)

| Gap | Impact | Complexity | Priority |
|-----|--------|------------|----------|
| **No three-way merge** | Risk overwriting manual edits | High | P1 |
| **No dependency resolution for packs** | Can't handle pack dependencies | Medium | P1 |
| **No watch mode** | Manual regeneration only | Medium | P2 |
| **No incremental updates** | Full regeneration always | High | P2 |
| **No conflict resolution UI** | Manual merge conflicts hard | High | P2 |

### 6.3 Nice-to-Have Gaps (Future)

| Gap | Impact | Complexity | Priority |
|-----|--------|------------|----------|
| **No parallel pack installation** | Slower for multi-pack projects | Medium | P3 |
| **No pack version constraints** | Can't specify "^1.2.0" | Low | P3 |
| **No automated testing after regen** | Risk breaking changes | Medium | P3 |
| **No rollback mechanism** | Can't undo bad regeneration | Medium | P3 |

---

## 7. Solution Architecture

### 7.1 Bridging the Gap: Complete Lifecycle Flow

```
┌─────────────────────────────────────────────────────────────┐
│ 1. INITIAL PROJECT CREATION                                 │
│                                                              │
│ ggen new my-project --pack rust-web                         │
│   ├─ Download pack (if needed)                              │
│   ├─ Load pack manifest (gpack.toml)                        │
│   ├─ Execute project generator                              │
│   ├─ Create initial snapshot                                │
│   │   ├─ Graph state (RDF)                                  │
│   │   ├─ File hashes                                        │
│   │   ├─ Pack version (NEW)                                 │
│   │   └─ Variables used                                     │
│   ├─ Update ggen.lock                                       │
│   │   └─ Track pack@version → files mapping (NEW)          │
│   └─ Run lifecycle phase: init                              │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ 2. NORMAL DEVELOPMENT                                        │
│                                                              │
│ Developer edits files, adds features                         │
│   ├─ Manual edits tracked by git                            │
│   ├─ Snapshot remains from initial generation               │
│   └─ ggen.lock remains unchanged                            │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ 3. PACK UPDATE AVAILABLE                                     │
│                                                              │
│ ggen packs update rust-web                                   │
│   ├─ Check registry for new version                         │
│   ├─ Detect changes: 1.0.0 → 1.1.0                          │
│   ├─ Load current snapshot                                  │
│   ├─ Detect manual edits (diff current vs snapshot)         │
│   ├─ Download new pack version                              │
│   ├─ Regenerate files from new pack                         │
│   ├─ Three-way merge (snapshot, manual, new)                │
│   │   ├─ Auto-merge non-conflicting changes                 │
│   │   ├─ Mark conflicts for review                          │
│   │   └─ Preserve manual edits in safe regions              │
│   ├─ Create new snapshot (post-merge state)                 │
│   ├─ Update ggen.lock (new pack version)                    │
│   └─ Report: files updated, conflicts, manual review needed │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ 4. MANUAL REGENERATION                                       │
│                                                              │
│ ggen lifecycle run template                                  │
│   ├─ Check ggen.lock for pack versions                      │
│   ├─ Compare with current files                             │
│   ├─ Detect if manual edits exist                           │
│   ├─ Regenerate with three-way merge                        │
│   └─ Update snapshot                                        │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ 5. WATCH MODE (Future)                                       │
│                                                              │
│ ggen watch                                                   │
│   ├─ Monitor ggen.lock for pack version changes             │
│   ├─ Monitor registry for pack updates                      │
│   ├─ Trigger regeneration on changes                        │
│   └─ Auto-merge or prompt for conflicts                     │
└─────────────────────────────────────────────────────────────┘
```

### 7.2 Required Data Structures

**Enhanced Snapshot:**
```rust
pub struct Snapshot {
    pub name: String,
    pub graph: GraphSnapshot,
    pub files: Vec<FileSnapshot>,
    pub templates: Vec<TemplateSnapshot>,

    // NEW: Pack tracking
    pub packs: Vec<PackSnapshot>,
}

pub struct PackSnapshot {
    pub pack_id: String,
    pub pack_version: String,
    pub files_generated: Vec<PathBuf>,
    pub variables_used: HashMap<String, String>,
    pub generated_at: DateTime<Utc>,
}

pub struct FileSnapshot {
    pub path: PathBuf,
    pub hash: String,

    // NEW: Pack tracking
    pub pack_id: Option<String>,
    pub pack_version: Option<String>,

    // IMPLEMENT: Region tracking
    pub generated_regions: Vec<Region>,  // Actual implementation needed
    pub manual_regions: Vec<Region>,      // Actual implementation needed
}
```

**Enhanced Lockfile:**
```rust
pub struct Lockfile {
    pub version: String,
    pub packs: Vec<LockEntry>,

    // NEW: File mapping
    pub file_mappings: HashMap<PathBuf, PackFileMapping>,
}

pub struct PackFileMapping {
    pub pack_id: String,
    pub pack_version: String,
    pub template_path: String,
    pub last_generated: DateTime<Utc>,
}
```

**Region Detection (Implement):**
```rust
pub fn detect_regions(content: &str, language: &str) -> Vec<Region> {
    // Parse comments for region markers
    // Support:
    // - // @ggen:generated-start / @ggen:generated-end
    // - /* @ggen:manual-edit */
    // - # @ggen:protected

    // Use tree-sitter for language-aware parsing
    // Return actual regions, not Vec::new()
}
```

### 7.3 Required Commands

**Pack Management:**
```bash
# Install pack to existing project
ggen packs install <pack-name>[@version] [--path <target>]

# Update pack in project
ggen packs update <pack-name> [--merge-strategy <auto|manual|preserve>]

# Update all packs
ggen packs update --all

# Check for pack updates
ggen packs outdated

# Show what would regenerate
ggen packs check --dry-run

# Regenerate from current pack versions
ggen packs regenerate [--pack <name>]
```

**Lifecycle Integration:**
```bash
# Run template phase (regeneration)
ggen lifecycle run template

# Watch for changes and auto-regenerate
ggen watch [--packs]
```

### 7.4 Implementation Phases

**Phase 1: Basic Pack Installation (MVP)**
- [ ] Add `packs install` command
- [ ] Track pack versions in ggen.lock
- [ ] Create initial snapshots on install
- [ ] Map files → packs in lockfile

**Phase 2: Region Tracking**
- [ ] Implement `detect_regions()` for Rust
- [ ] Add region markers to generated files
- [ ] Track generated vs manual regions in snapshots
- [ ] Implement `detect_regions()` for TypeScript/JavaScript

**Phase 3: Regeneration & Update**
- [ ] Add `packs update` command
- [ ] Implement three-way merge logic
- [ ] Add conflict detection
- [ ] Add `packs regenerate` command

**Phase 4: Lifecycle Integration**
- [ ] Add `template` phase to lifecycle
- [ ] Integrate with `make.toml`
- [ ] Add regeneration hooks

**Phase 5: Advanced Features**
- [ ] Watch mode
- [ ] Parallel pack operations
- [ ] Rollback mechanism
- [ ] Automated testing after regen

---

## 8. Technical Decisions Needed

### 8.1 Region Marker Format

**Option A: Comment-based** (Recommended)
```rust
// @ggen:generated-start pack=rust-web version=1.0.0
pub struct User {
    pub id: Uuid,
    pub name: String,
}
// @ggen:generated-end

// @ggen:manual-edit
impl User {
    pub fn custom_method(&self) -> String {
        // User's custom code
    }
}
// @ggen:manual-edit-end
```

**Pros**: Language-agnostic, visible, easy to parse
**Cons**: Clutters code, can be deleted

**Option B: Metadata file** (.ggen/regions.json)
```json
{
  "src/models/user.rs": {
    "regions": [
      {"type": "generated", "start": 1, "end": 5, "pack": "rust-web"},
      {"type": "manual", "start": 7, "end": 12}
    ]
  }
}
```

**Pros**: Cleaner code, centralized
**Cons**: Can drift from actual code, harder to maintain

**Recommendation**: **Option A** with opt-out via config

### 8.2 Merge Strategies

1. **Auto-merge** (default)
   - Non-conflicting changes merged automatically
   - Conflicts marked for review
   - Safe for minor/patch updates

2. **Manual** (opt-in)
   - Show diff, require approval
   - Safe for major version changes

3. **Preserve** (conservative)
   - Never overwrite manual edits
   - Only add new files
   - Safest but least automated

### 8.3 Snapshot Storage

**Option A**: Single file `.ggen/snapshot.json`
- Simple, easy to manage
- Can grow large

**Option B**: Directory `.ggen/snapshots/`
- One file per pack
- Scales better
- More complex

**Recommendation**: Start with **Option A**, migrate to B if needed

---

## 9. Risk Analysis

### 9.1 High-Risk Areas

1. **Data Loss Risk** (Critical)
   - Three-way merge bugs could overwrite manual edits
   - **Mitigation**: Always create backups before regeneration
   - **Test**: Extensive merge scenario testing

2. **Lockfile Corruption** (High)
   - Concurrent writes could corrupt ggen.lock
   - **Mitigation**: File locking (already implemented in marketplace)
   - **Test**: Parallel operation stress tests

3. **Region Detection Failures** (Medium)
   - Incorrect region parsing could mis-categorize code
   - **Mitigation**: Conservative defaults (assume manual)
   - **Test**: Parse test suite with diverse code

### 9.2 Medium-Risk Areas

1. **Version Compatibility** (Medium)
   - Breaking pack changes could fail regeneration
   - **Mitigation**: Compatibility checks, rollback capability

2. **Performance** (Medium)
   - Snapshot diffing could be slow for large projects
   - **Mitigation**: Incremental diffing, caching

### 9.3 Low-Risk Areas

1. **Registry Sync** (Low)
   - Stale registry could miss updates
   - **Mitigation**: Freshness checks (already implemented)

---

## 10. Success Metrics

**MVP Success (Phase 1-2):**
- [ ] Can install pack to existing project
- [ ] Lockfile tracks pack versions
- [ ] Snapshots created and loadable
- [ ] Files mapped to packs

**Production Ready (Phase 3):**
- [ ] Can update pack without losing manual edits
- [ ] Three-way merge works for 90%+ cases
- [ ] Conflicts clearly reported
- [ ] Regeneration under 10 seconds for medium projects

**Advanced (Phase 4-5):**
- [ ] Watch mode works reliably
- [ ] Rollback works 100% of time
- [ ] Tests run automatically post-regen
- [ ] Zero data loss in production use

---

## 11. Recommendations

### 11.1 Immediate Actions (P0)

1. **Implement region detection**
   - Start with Rust (tree-sitter-rust)
   - Add comment markers to generated files
   - Store regions in snapshots

2. **Add packs → files mapping**
   - Extend lockfile with file mappings
   - Track pack version per file
   - Update on generation

3. **Create `packs install` command**
   - Reuse marketplace download logic
   - Trigger project generator
   - Create snapshot + lockfile entry

### 11.2 Short-term Actions (P1)

4. **Implement three-way merge**
   - Diff current vs snapshot (detect manual)
   - Diff snapshot vs new (detect pack changes)
   - Merge non-conflicting
   - Mark conflicts

5. **Add `packs update` command**
   - Check for pack updates
   - Run three-way merge
   - Update snapshot + lockfile

### 11.3 Medium-term Actions (P2)

6. **Lifecycle integration**
   - Add `template` phase
   - Hook into make.toml
   - Enable `ggen lifecycle run template`

7. **Testing & Safety**
   - Comprehensive merge tests
   - Backup/restore tests
   - Performance benchmarks

### 11.4 Long-term Actions (P3)

8. **Advanced features**
   - Watch mode
   - Rollback mechanism
   - Automated testing
   - Parallel operations

---

## 12. Conclusion

**Current State**: Packs are disconnected from the lifecycle system. They're used once at project creation and never again.

**Desired State**: Packs are living dependencies that can be updated, regenerated, and evolved throughout a project's lifetime.

**Key Insight**: The infrastructure mostly exists—marketplace has robust install/update, lifecycle has state tracking, snapshots have structure for regions. What's missing is the glue code to connect them.

**Critical Path**:
1. Region detection (enable safe updates)
2. Pack → file mapping (know what to regenerate)
3. Three-way merge (preserve manual edits)
4. Update command (trigger regeneration)

**Biggest Risk**: Region detection bugs causing data loss. Must be battle-tested before production.

**Timeline Estimate**:
- MVP (basic install): 2-3 weeks
- Production (update with merge): 4-6 weeks
- Advanced (watch, rollback): 8-12 weeks

**ROI**: High. Unlocks pack ecosystem growth and enables continuous template evolution.
