# ggen Documentation Restructure Proposal

**Aligned with**: ~/dis/astro reference structure (simplified)
**Status**: Proposal - awaiting approval
**Goal**: Clean, maintainable documentation following proven patterns

---

## Proposed Structure

```
ggen/
├── docs/
│   ├── tutorials/           # Learning-oriented (Diataxis)
│   │   ├── 01-quick-start.md
│   │   ├── 02-first-template.md
│   │   └── 03-rdf-basics.md
│   │
│   ├── how-to/             # Problem-solving guides
│   │   ├── configuration/
│   │   ├── generation/
│   │   └── deployment/
│   │
│   ├── reference/          # Information-oriented
│   │   ├── cli/
│   │   ├── configuration/
│   │   └── api/
│   │
│   ├── explanations/       # Understanding-oriented
│   │   ├── fundamentals/
│   │   └── architecture/
│   │
│   ├── contributing/       # Contributor guides
│   │   ├── GETTING_STARTED.md
│   │   ├── VALIDATION.md
│   │   └── TESTING.md
│   │
│   └── examples/          # Complete working examples
│       └── diataxis-case-study/
│
├── specs/                  # Feature specifications
│   ├── 001-rdf-engine/
│   │   ├── spec.md
│   │   ├── plan.md
│   │   └── research.md
│   │
│   ├── 002-template-system/
│   │   ├── spec.md
│   │   ├── plan.md
│   │   └── tasks.md
│   │
│   └── 003-ai-integration/
│       ├── spec.md
│       └── quickstart.md
│
├── scripts/               # Validation and build scripts
│   └── validate-docs/
│
└── templates/            # Code generation templates
```

---

## Key Changes from Current Structure

### 1. Consolidate Documentation Roots
**Problem**: Currently have 3 competing Diataxis structures
- `docs/diataxis/`
- `docs/book/`
- `docs/examples/diataxis-case-study/`

**Solution**: Single clean structure at `docs/`
- Keep only `docs/tutorials/`, `docs/how-to/`, `docs/reference/`, `docs/explanations/`
- Move case study to `docs/examples/` (keep as teaching tool)
- Remove `docs/diataxis/` and `docs/book/` (consolidate into main structure)

### 2. Add Specs Directory (New)
**Inspired by**: `~/dis/astro/specs/`

**Purpose**: Feature specifications and planning docs

**Structure** (simplified from astro):
```
specs/NNN-feature-name/
├── spec.md          # Full specification
├── plan.md          # Implementation plan
├── research.md      # Research notes (optional)
├── tasks.md         # Task breakdown (optional)
└── quickstart.md    # Quick reference (optional)
```

**Example specs for ggen**:
- `001-rdf-engine/` - Core RDF processing
- `002-template-system/` - Tera template engine
- `003-ai-integration/` - genai provider system
- `004-marketplace/` - Package management
- `005-cli-framework/` - clap-noun-verb architecture

### 3. Archive Legacy Content
**Move to**: `docs/archive/`

**What to archive**:
- `docs/analysis/` (one-time analysis)
- `docs/architecture/marketplace-v2-migration/` (completed)
- `docs/benchmark-results/` (point-in-time data)
- `docs/design/` (superseded by specs/)
- `docs/metrics/` (historical)

### 4. Clean Up Root Level
**Keep**:
- `docs/tutorials/` (Diataxis)
- `docs/how-to/` (Diataxis)
- `docs/reference/` (Diataxis)
- `docs/explanations/` (Diataxis)
- `docs/contributing/` (contributor docs)
- `docs/examples/` (working examples)

**Remove** (consolidate or archive):
- `docs/diataxis/` → merge into root-level quadrants
- `docs/book/` → merge into root-level quadrants
- `docs/getting-started/` → move to `docs/tutorials/`

---

## Migration Plan

### Phase 1: Create New Structure (No Breaking Changes)
1. Create `specs/` directory with initial 3 specs
2. Create consolidated doc structure in parallel
3. Test documentation validation with new paths

### Phase 2: Content Migration
1. Move quick-start to `docs/tutorials/01-quick-start.md`
2. Consolidate how-to guides from book/ and how-to/
3. Merge reference docs (CLI, configuration, API)
4. Merge explanations (fundamentals, architecture)

### Phase 3: Archive Legacy
1. Move historical content to `docs/archive/`
2. Remove duplicate directories
3. Update all internal links

### Phase 4: Validation
1. Run link checker on new structure
2. Verify all validation scripts work
3. Update VALIDATION.md with new paths

---

## Comparison: Astro vs Proposed ggen Structure

### Similarities (Adopted from Astro)
✅ Clean Diataxis at root level (tutorials/, how-to/, reference/, explanations/)
✅ Specs directory for feature documentation
✅ Each spec has: spec.md, plan.md, research.md
✅ Contributing guides in dedicated directory
✅ Examples directory for complete projects

### Differences (Simplified for ggen)
❌ No `checklists/` subdirectory in specs (too heavyweight)
❌ No `contracts/` subdirectory (Rust uses actual contracts)
❌ No `thesis/` directory (not academic project)
❌ Fewer spec files per feature (spec.md + plan.md sufficient)

---

## Benefits

### For Users
- ✅ **Clear navigation**: Single entry point for each doc type
- ✅ **No confusion**: No more competing Diataxis structures
- ✅ **Fast discovery**: Know where to look for tutorials vs reference

### For Contributors
- ✅ **Clear contribution path**: Know where new docs belong
- ✅ **Spec-driven development**: Feature specs guide implementation
- ✅ **Validated structure**: CI enforces documentation standards

### For Maintainers
- ✅ **Easier to maintain**: Less duplication
- ✅ **Clear ownership**: Each spec has an owner
- ✅ **Historical context**: Archive preserves decisions without clutter

---

## File Mapping (Key Moves)

| Current Location | New Location | Action |
|-----------------|--------------|--------|
| `docs/getting-started/quick-start.md` | `docs/tutorials/01-quick-start.md` | Move |
| `docs/diataxis/tutorials/*` | `docs/tutorials/*` | Merge |
| `docs/book/tutorials/*` | `docs/tutorials/*` | Merge |
| `docs/how-to/configuration/*` | `docs/how-to/configuration/*` | Keep |
| `docs/reference/commands/*` | `docs/reference/cli/*` | Rename |
| `docs/explanations/fundamentals/*` | `docs/explanations/fundamentals/*` | Keep |
| `docs/examples/diataxis-case-study/` | `docs/examples/diataxis-case-study/` | Keep |
| `docs/architecture/` | `docs/archive/architecture/` | Archive |
| `docs/analysis/` | `docs/archive/analysis/` | Archive |

---

## Validation After Restructure

### Scripts to Update
1. `scripts/validate-docs/validate-all.sh` - Update paths
2. `scripts/validate-docs/check-broken-links.sh` - New path logic
3. `.github/workflows/validate-docs.yml` - Update CI paths

### Documentation to Update
1. `README.md` - Update documentation links
2. `CONTRIBUTING.md` - Update contribution paths
3. `docs/contributing/VALIDATION.md` - Update script examples

---

## Proposed Specs (Initial Set)

### 001-rdf-engine
**Purpose**: Core RDF/SPARQL processing with Oxigraph
**Status**: Implemented ✅
**Files**: spec.md, plan.md

### 002-template-system
**Purpose**: Tera template rendering with RDF integration
**Status**: Implemented ✅
**Files**: spec.md, plan.md

### 003-ai-integration
**Purpose**: Multi-provider AI support (OpenAI, Anthropic, Ollama)
**Status**: Implemented ✅
**Files**: spec.md, plan.md, quickstart.md

### 004-marketplace
**Purpose**: Package management and template distribution
**Status**: In Progress 🚧
**Files**: spec.md, plan.md, tasks.md

### 005-cli-framework
**Purpose**: clap-noun-verb auto-discovery architecture
**Status**: Implemented ✅
**Files**: spec.md, research.md

---

## Example Spec Structure (Simplified)

### Minimal Spec (Small Feature)
```
specs/NNN-feature-name/
└── spec.md
```

### Standard Spec (Most Features)
```
specs/NNN-feature-name/
├── spec.md          # REQUIRED: Full specification
└── plan.md          # REQUIRED: Implementation plan
```

### Complex Spec (Major Features)
```
specs/NNN-feature-name/
├── spec.md          # REQUIRED: Full specification
├── plan.md          # REQUIRED: Implementation plan
├── research.md      # OPTIONAL: Research and alternatives
├── tasks.md         # OPTIONAL: Detailed task breakdown
└── quickstart.md    # OPTIONAL: Quick reference guide
```

---

## Decision Points

### ✅ Adopt from Astro
1. Clean Diataxis at `docs/` root
2. Numbered specs in `specs/` directory
3. spec.md + plan.md pattern
4. Single source of truth for each doc type

### ❌ Skip from Astro (Too Complex)
1. `checklists/requirements.md` (use TODO.md instead)
2. `contracts/` directory (Rust has type system)
3. Multiple analysis reports per spec
4. `thesis/` directory

### 🔄 Adapt for ggen
1. Fewer files per spec (keep simple)
2. Archive instead of delete (preserve history)
3. Validation-first approach (automated quality)
4. Rust-specific conventions (Cargo.toml, tests/)

---

## Approval Checklist

Before implementing:
- [ ] Review proposed structure
- [ ] Confirm specs/ pattern works for team
- [ ] Approve file moves (no data loss)
- [ ] Verify link checker handles new structure
- [ ] Confirm archive strategy acceptable

---

## Implementation Command

```bash
# Phase 1: Create new structure (non-destructive)
mkdir -p specs/{001-rdf-engine,002-template-system,003-ai-integration}

# Phase 2: Content migration (TBD - requires approval)
# ...will provide detailed migration script after approval

# Phase 3: Validation
./scripts/validate-docs/check-broken-links.sh
./scripts/run-validation-suite.sh
```

---

**Next Step**: Review and approve this structure, then proceed with migration
**Timeline**: ~2 hours for full migration after approval
**Risk**: Low (all content preserved, links can be fixed)
