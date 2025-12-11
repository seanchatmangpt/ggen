# Root Directory Files Cleanup Plan

**Date**: 2025-12-11  
**Status**: Planning  
**Goal**: Organize all non-essential files from root directory into appropriate locations

---

## Overview

After archiving 54 markdown reports, the root directory still contains various test data files, templates, config files, status reports, and other artifacts that should be organized into proper directories.

---

## File Categories and Actions

### 1. Status/Summary Text Files → `docs/archive/status/`

**Files to Move:**
- `AUDIT_SUMMARY.txt` (14K) - Historical audit summary
- `BUILD_STATUS.txt` (3.3K) - Historical build status
- `DIATAXIS_IMPLEMENTATION_COMPLETE.txt` (14K) - Implementation completion marker
- `GGEN_COMMAND_STATUS.txt` (5.1K) - Command status tracking
- `errors.txt` (1.3M) - **LARGE** error log file - consider archiving or deleting if obsolete

**Action**: Create `docs/archive/status/` and move all status files there.

**Rationale**: These are historical status files that don't need to be in root.

---

### 2. Test/Example Data Files → `tests/fixtures/data/` or `examples/test-data/`

**Files to Move:**
- `data.rdf` (313B) - Test RDF data
- `data.ttl` (4.1K) - Test Turtle data
- `data1.ttl` (67B) - Test data variant
- `data2.ttl` (67B) - Test data variant
- `test-data.ttl` (158B) - Test data
- `person_data.ttl` (169B) - Person test data
- `entities.ttl` (350B) - Entity test data
- `types.ttl` (188B) - Type test data
- `relationships_turtle` (170B) - Relationship test data
- `named_ttl` (115B) - Named test data
- `large.turtle` (34K) - Large test dataset
- `example.txt` (13B) - Example text file

**Action**: 
- Move to `tests/fixtures/data/` if used by tests
- Move to `examples/test-data/` if used by examples
- Check for references and update paths if needed

**Rationale**: Test data belongs in test directories, not root.

---

### 3. Template Files → `templates/examples/` or `examples/templates/`

**Files to Move:**
- `hello.tmpl` (91B) - Example template
- `test-template.tmpl` (189B) - Test template (duplicate of `templates/test-template.tmpl`)
- `v1.tmpl` (131B) - Version 1 template

**Action**: 
- Move to `templates/examples/` if they're example templates
- Check if `test-template.tmpl` is duplicate and remove if so
- Update any references

**Rationale**: Templates belong in `templates/` directory structure.

---

### 4. Config Files → `config/` or `.config/`

**Files to Review:**
- `ggen.toml` (2.4K) - Project config (may need to stay if used by build)
- `ggen.lock` (106B) - Lock file (may need to stay if used by build)
- `chicago-tdd-tools.toml` (5.9K) - TDD tools config
- `concept_matcher_config.json` (9.6K) - Concept matcher config
- `claude-flow` (1.0K) - Claude flow config

**Action**: 
- Check if these are actively used by build/test processes
- If project-level configs (like `ggen.toml`), may need to stay in root
- If tool-specific configs, move to `config/` directory
- Consider moving to `.config/` if they're user-specific

**Rationale**: Config files should be organized, but project-level configs may need to stay in root.

---

### 5. Backup Files → Delete or `.backup/`

**Files to Handle:**
- `Makefile.toml.backup` (40K) - Backup of Makefile

**Action**: 
- If backup is obsolete, delete it
- If backup is needed for reference, move to `.backup/` directory
- Consider adding `.backup/` to `.gitignore` if created

**Rationale**: Backup files clutter the root directory.

---

### 6. Scripts → `scripts/utilities/` or `scripts/root-scripts/`

**Files to Move:**
- `verify_quick_wins.sh` (4.0K) - Verification script

**Action**: 
- Move to `scripts/utilities/` or `scripts/root-scripts/`
- Update any references in documentation or CI/CD

**Rationale**: Scripts belong in `scripts/` directory.

---

### 7. Diagram Files → `docs/diagrams/` or `analysis/`

**Files to Move:**
- `DOMAIN_MIGRATION_PLAN.puml` (9.4K) - PlantUML diagram

**Action**: 
- Move to `docs/diagrams/` or `analysis/` directory
- Update any references in documentation

**Rationale**: Diagrams belong in documentation or analysis directories.

---

## Implementation Steps

### Phase 1: Safe Moves (No Dependencies)
1. ✅ Move status files to `docs/archive/status/`
2. ✅ Move backup file (delete or archive)
3. ✅ Move diagram file to `docs/diagrams/`
4. ✅ Move script to `scripts/utilities/`

### Phase 2: Check Dependencies
1. Search codebase for references to test data files
2. Search codebase for references to template files
3. Search codebase for references to config files
4. Update all references to new locations

### Phase 3: Move Test Data
1. Determine if files are used by tests or examples
2. Move to appropriate location (`tests/fixtures/data/` or `examples/test-data/`)
3. Update test code to use new paths

### Phase 4: Move Templates
1. Check for duplicates (e.g., `test-template.tmpl`)
2. Move to `templates/examples/`
3. Update any references

### Phase 5: Organize Config Files
1. Determine which configs are project-level (must stay in root)
2. Move tool-specific configs to `config/`
3. Document config file locations

### Phase 6: Validation
1. Run `cargo make check` to verify no broken references
2. Run tests to verify test data paths work
3. Check CI/CD scripts for any hardcoded paths
4. Update documentation with new file locations

---

## Expected Outcome

**Root Directory Will Contain Only:**
- Essential Rust project files: `Cargo.toml`, `Cargo.lock`, `build.rs`
- Essential documentation: `README.md`, `CHANGELOG.md`, `CONTRIBUTING.md`, `SECURITY.md`, `TESTING.md`, `PERFORMANCE.md`, `CLAUDE.md`, `BUG_REPORTING_GUIDE.md`
- Essential config: `Makefile.toml`, `deny.toml`, `rustfmt.toml`, `VERSION`
- Legal: `LICENSE`
- Project-level configs (if required): `ggen.toml`, `ggen.lock` (if needed by build)

**Estimated Files Removed from Root**: ~30-35 files

---

## Risk Assessment

### Low Risk
- Status files (no code dependencies)
- Backup files (not used)
- Diagram files (documentation only)

### Medium Risk
- Test data files (may be referenced in tests)
- Template files (may be referenced in examples/docs)
- Scripts (may be referenced in CI/CD)

### High Risk
- Config files (may be required by build system)
  - **Mitigation**: Check build/test processes before moving

---

## Notes

- The `errors.txt` file is 1.3MB - consider if it should be archived or deleted
- `test-template.tmpl` appears to be a duplicate - verify before moving
- Some config files may need to stay in root if they're project-level configuration
- All moves should be done with `git mv` to preserve history

---

## Post-Cleanup

After cleanup, create a `docs/ROOT_DIRECTORY.md` document explaining:
- What files belong in root and why
- Where to find moved files
- How to add new files to appropriate locations

