# Root Directory Files Usage Analysis

**Date**: 2025-12-11  
**Purpose**: Identify where root directory test/example files are actually used

---

## Key Findings

### BDD Tests Create Files Dynamically
The BDD tests in `tests/bdd/steps/` **create files dynamically** in `world.project_dir` (a temporary test directory), not from root. The root files are **NOT** being read by these tests.

### Duplicate Files Found
Many root files have duplicates in proper directories:
- `data.ttl`, `person_data.ttl`, `entities.ttl`, `types.ttl` → duplicates in `tests/data/`
- `test-template.tmpl`, `hello.tmpl` → duplicates in `templates/`

---

## File-by-File Analysis

### Test Data Files

#### `data.ttl` (4.1K)
- **Root**: ✅ Exists
- **tests/data/**: ✅ Exists (duplicate)
- **Usage**: 
  - Referenced in many tests, but tests create their own versions
  - Used in BDD features, but BDD steps create files dynamically
  - **Verdict**: Root file appears to be unused duplicate

#### `data.rdf` (313B)
- **Root**: ✅ Exists
- **tests/data/**: ❌ Missing
- **Usage**: 
  - Only referenced in format detection tests (`RdfFormat::from_extension("data.rdf")`)
  - Tests create their own files, don't read from root
  - **Verdict**: Root file appears to be unused

#### `data1.ttl`, `data2.ttl` (67B each)
- **Root**: ✅ Exist
- **tests/data/**: ❌ Missing
- **Usage**: 
  - Referenced in BDD feature: `Given I have RDF files "data1.ttl" and "data2.ttl"`
  - But BDD steps create files dynamically, don't read from root
  - **Verdict**: Root files appear to be unused

#### `test-data.ttl` (158B)
- **Root**: ✅ Exists
- **tests/data/**: ✅ Exists (duplicate)
- **Usage**: 
  - Used in BDD steps: `world.project_dir.join("test-data.ttl")`
  - But steps create file dynamically, don't read from root
  - **Verdict**: Root file appears to be unused duplicate

#### `person_data.ttl` (169B)
- **Root**: ✅ Exists
- **tests/data/**: ✅ Exists (duplicate)
- **Usage**: 
  - Used in BDD steps: `world.project_dir.join("person_data.ttl")`
  - But steps create file dynamically with inline content
  - **Verdict**: Root file appears to be unused duplicate

#### `entities.ttl` (350B)
- **Root**: ✅ Exists
- **tests/data/**: ✅ Exists (duplicate)
- **Usage**: 
  - Referenced in `test-template.tmpl`: `rdf: entities.ttl`
  - But templates in `templates/` directory reference `entities.ttl` relative to template location
  - **Verdict**: Root file may be used by root `test-template.tmpl`, but that's also a duplicate

#### `types.ttl` (188B)
- **Root**: ✅ Exists
- **tests/data/**: ✅ Exists (duplicate)
- **Usage**: 
  - Used in BDD steps: `world.project_dir.join("types.ttl")`
  - But steps create file dynamically
  - **Verdict**: Root file appears to be unused duplicate

#### `relationships_turtle` (170B)
- **Root**: ✅ Exists
- **tests/data/**: ❌ Missing
- **Usage**: 
  - Used in BDD steps: `world.project_dir.join("relationships_turtle")`
  - But steps create file dynamically
  - **Verdict**: Root file appears to be unused

#### `named_ttl` (115B)
- **Root**: ✅ Exists
- **tests/data/**: ❌ Missing
- **Usage**: 
  - Used in BDD steps: `world.project_dir.join("named_ttl")`
  - But steps create file dynamically
  - **Verdict**: Root file appears to be unused

#### `large.turtle` (34K)
- **Root**: ✅ Exists
- **tests/data/**: ❌ Missing
- **Usage**: 
  - Used in BDD steps: `world.project_dir.join("large.turtle")`
  - But steps create file dynamically
  - **Verdict**: Root file appears to be unused

#### `example.txt` (13B)
- **Root**: ✅ Exists
- **tests/data/**: ❌ Missing
- **Usage**: 
  - Used in BDD feature: `Given I have a file "example.txt" with "Hello, World!"`
  - But BDD steps create file dynamically
  - **Verdict**: Root file appears to be unused

### Template Files

#### `hello.tmpl` (91B)
- **Root**: ✅ Exists
- **templates/**: ✅ Exists (duplicate)
- **Usage**: 
  - Referenced extensively in docs and README
  - But actual file used is in `templates/hello.tmpl`
  - Script validation uses: `templates/hello.tmpl`
  - **Verdict**: Root file is unused duplicate

#### `test-template.tmpl` (189B)
- **Root**: ✅ Exists
- **templates/**: ✅ Exists (duplicate, identical content)
- **Usage**: 
  - Used extensively in BDD tests
  - But BDD steps create file dynamically: `world.project_dir.join("test-template.tmpl")`
  - Root template references `entities.ttl` which would need to be in same directory
  - **Verdict**: Root file may be legacy, BDD tests don't use it

#### `v1.tmpl` (131B)
- **Root**: ✅ Exists
- **templates/**: ❌ Missing
- **Usage**: 
  - Used in BDD feature: `Given I have a template "v1.tmpl" with "version: 1.0"`
  - But BDD steps create file dynamically
  - **Verdict**: Root file appears to be unused

---

## Summary

### Files That Can Be Safely Removed (Unused Duplicates)
1. **Test Data Files** (all appear unused - BDD tests create dynamically):
   - `data.ttl` (duplicate of `tests/data/data.ttl`)
   - `data.rdf`
   - `data1.ttl`, `data2.ttl`
   - `test-data.ttl` (duplicate of `tests/data/test-data.ttl`)
   - `person_data.ttl` (duplicate of `tests/data/person_data.ttl`)
   - `entities.ttl` (duplicate of `tests/data/entities.ttl`)
   - `types.ttl` (duplicate of `tests/data/types.ttl`)
   - `relationships_turtle`
   - `named_ttl`
   - `large.turtle`
   - `example.txt`

2. **Template Files** (all appear unused):
   - `hello.tmpl` (duplicate of `templates/hello.tmpl`)
   - `test-template.tmpl` (duplicate of `templates/test-template.tmpl`)
   - `v1.tmpl`

### Files to Keep in Root (Config Files - Per User Request)
- `ggen.toml` - Project config
- `ggen.lock` - Lock file
- `chicago-tdd-tools.toml` - TDD tools config
- `concept_matcher_config.json` - Concept matcher config
- `claude-flow` - Claude flow config

### Files to Move (Non-Config, Non-Duplicate)
1. **Status Files** → `docs/archive/status/`:
   - `AUDIT_SUMMARY.txt`
   - `BUILD_STATUS.txt`
   - `DIATAXIS_IMPLEMENTATION_COMPLETE.txt`
   - `GGEN_COMMAND_STATUS.txt`
   - `errors.txt` (1.3M - consider deleting if obsolete)

2. **Backup Files** → Delete or `.backup/`:
   - `Makefile.toml.backup`

3. **Scripts** → `scripts/utilities/`:
   - `verify_quick_wins.sh`

4. **Diagrams** → `docs/diagrams/`:
   - `DOMAIN_MIGRATION_PLAN.puml`

---

## Recommended Action Plan

### Phase 1: Remove Unused Duplicates (Safe)
1. Delete all test data files from root (they're duplicates or unused)
2. Delete all template files from root (they're duplicates or unused)
3. Verify tests still pass (they should - they create files dynamically)

### Phase 2: Move Status/Other Files
1. Move status files to `docs/archive/status/`
2. Delete or archive backup file
3. Move script to `scripts/utilities/`
4. Move diagram to `docs/diagrams/`

### Phase 3: Validation
1. Run `cargo make test` to verify nothing broke
2. Run BDD tests to verify they still work
3. Check CI/CD scripts for any hardcoded paths

---

## Risk Assessment

### Low Risk (Safe to Delete)
- All test data files (BDD tests create dynamically)
- All template files (duplicates exist in proper locations)

### Medium Risk (Verify Before Moving)
- Status files (may be referenced in docs/scripts)
- Scripts (may be referenced in CI/CD)

### No Risk
- Backup files (can be deleted)
- Config files (keeping in root per user request)

