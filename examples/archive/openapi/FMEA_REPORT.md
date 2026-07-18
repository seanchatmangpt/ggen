# FMEA Report: OpenAPI Example Failure Mode and Effects Analysis

## Executive Summary

This document provides a comprehensive Failure Mode and Effects Analysis (FMEA) for the `examples/openapi` code generation example. The analysis identifies potential failure modes, assesses their severity, occurrence, and detection, and proposes Poka-Yoke (error prevention) solutions.

**Key Findings:**
- **Total Failure Modes Identified**: 15
- **Critical (RPN ≥ 200)**: 0
- **High Priority (RPN 100-199)**: 3
- **Medium Priority (RPN 50-99)**: 6
- **Low Priority (RPN < 50)**: 6

## FMEA Rating Scales

### Severity (S) - Impact of Failure
| Rating | Description | Examples |
|--------|-------------|----------|
| 10 | Catastrophic | Data loss, security breach, system crash |
| 9 | Critical | Complete feature failure, corruption |
| 8 | Major | Significant functionality loss |
| 7 | Significant | Major feature degradation |
| 6 | Moderate | Noticeable feature impact |
| 5 | Minor | Small feature impact |
| 4 | Low | Minimal user impact |
| 3 | Very Low | Cosmetic issues |
| 2 | Negligible | Barely noticeable |
| 1 | None | No impact |

### Occurrence (O) - Likelihood of Failure
| Rating | Description | Frequency |
|--------|-------------|-----------|
| 10 | Almost Certain | > 50% of operations |
| 9 | Very High | 30-50% of operations |
| 8 | High | 20-30% of operations |
| 7 | Moderately High | 10-20% of operations |
| 6 | Moderate | 5-10% of operations |
| 5 | Low-Moderate | 2-5% of operations |
| 4 | Low | 1-2% of operations |
| 3 | Very Low | 0.5-1% of operations |
| 2 | Remote | 0.1-0.5% of operations |
| 1 | Extremely Remote | < 0.1% of operations |

### Detection (D) - Ability to Detect Before Impact
| Rating | Description | Detection Method |
|--------|-------------|------------------|
| 10 | Almost Impossible | No detection, user discovers |
| 9 | Very Remote | Detection after production |
| 8 | Remote | Detection in production |
| 7 | Low | Detection in integration testing |
| 6 | Moderate | Detection in unit testing |
| 5 | Moderate-High | Detection in code review |
| 4 | High | Detection via static analysis |
| 3 | Very High | Detection via type system |
| 2 | Almost Certain | Detection via compile-time checks |
| 1 | Certain | Impossible to miss |

### RPN Thresholds
- **Critical (RPN ≥ 200)**: Must fix immediately, blocks release
- **High (RPN 100-199)**: Fix before next release
- **Medium (RPN 50-99)**: Fix in next sprint
- **Low (RPN < 50)**: Monitor and fix when convenient

## Failure Modes

### FM-001: ggen CLI Not Installed or Not in PATH
**Component**: Prerequisites  
**Function**: Execute ggen sync command

**Failure Mode**: User attempts to run `ggen sync` but command not found

**Effects**:
- Example cannot be executed
- User receives confusing error message
- No clear guidance on how to fix

**Severity Assessment**:
- **Rating**: 10 (Catastrophic)
- **Rationale**: Example is completely unusable

**Causes**:
- ggen not installed
- ggen not in PATH
- Wrong shell environment

**Occurrence Assessment**:
- **Rating**: 3 (Very Low)
- **Frequency**: ~0.5-1% of users
- **Rationale**: Most users following README will have ggen installed

**Current Controls**:
- README mentions prerequisite
- Quick check command in README

**Detection Assessment**:
- **Rating**: 1 (Certain)
- **Method**: Immediate command failure
- **Rationale**: Command fails immediately with clear error

**Risk Priority Number**:
- **RPN**: 10 × 3 × 1 = **30**
- **Priority**: Low

**Recommended Actions**:
1. ✅ **IMPLEMENTED**: Created `check-prerequisites.mjs` script (PY-001)
2. Add prerequisite check to README quick start
3. Provide installation instructions if check fails

**Poka-Yoke Solution**: ✅ **PY-001** - Pre-flight prerequisite check script

---

### FM-002: Node.js Version Too Old (< 18)
**Component**: Prerequisites  
**Function**: Run validation scripts

**Failure Mode**: User has Node.js < 18, validation script fails

**Effects**:
- `validate.mjs` fails with syntax errors
- User cannot verify generated output
- Confusing error messages

**Severity Assessment**:
- **Rating**: 7 (Significant)
- **Rationale**: Validation is important but example can still run

**Causes**:
- Old Node.js installation
- System default Node.js version

**Occurrence Assessment**:
- **Rating**: 4 (Low)
- **Frequency**: ~1-2% of users
- **Rationale**: Most users have Node 18+

**Current Controls**:
- README mentions Node.js 18+ requirement

**Detection Assessment**:
- **Rating**: 2 (Almost Certain)
- **Method**: Script fails immediately
- **Rationale**: ES module syntax fails on old Node.js

**Risk Priority Number**:
- **RPN**: 7 × 4 × 2 = **56**
- **Priority**: Medium

**Recommended Actions**:
1. ✅ **IMPLEMENTED**: Added Node.js version check to `check-prerequisites.mjs`
2. Add clear error message in `validate.mjs` if Node.js too old

**Poka-Yoke Solution**: ✅ **PY-001** - Pre-flight prerequisite check script

---

### FM-003: Running from Wrong Directory
**Component**: Directory Structure  
**Function**: Find ontology and template files

**Failure Mode**: User runs `ggen sync` from wrong directory

**Effects**:
- Cannot find ontology files
- Cannot find template files
- Confusing error messages

**Severity Assessment**:
- **Rating**: 9 (Critical)
- **Rationale**: Complete failure, but easy to fix

**Causes**:
- User not following README instructions
- Copy-paste error
- Multiple terminal windows

**Occurrence Assessment**:
- **Rating**: 5 (Low-Moderate)
- **Frequency**: ~2-5% of users
- **Rationale**: Common user error

**Current Controls**:
- README mentions `cd examples/openapi`
- Error messages mention file paths

**Detection Assessment**:
- **Rating**: 1 (Certain)
- **Method**: Immediate file not found error
- **Rationale**: Files don't exist, error is immediate

**Risk Priority Number**:
- **RPN**: 9 × 5 × 1 = **45**
- **Priority**: Medium

**Recommended Actions**:
1. ✅ **IMPLEMENTED**: Added directory validation to `check-prerequisites.mjs`
2. Improve error messages to suggest correct directory

**Poka-Yoke Solution**: ✅ **PY-001** - Pre-flight prerequisite check script

---

### FM-004: ggen.toml Format Not Fully Supported
**Component**: CLI Compatibility  
**Function**: Execute generation rules from ggen.toml

**Failure Mode**: `ggen sync` CLI doesn't support full `ggen.toml` format yet

**Effects**:
- User cannot run example as documented
- Example appears broken
- Confusion about supported features

**Severity Assessment**:
- **Rating**: 8 (Major)
- **Rationale**: Example cannot be executed, but documented as limitation

**Causes**:
- CLI implementation lagging behind format specification
- Example prepared for future support

**Occurrence Assessment**:
- **Rating**: 10 (Almost Certain)
- **Frequency**: 100% of users trying to run
- **Rationale**: Current CLI limitation

**Current Controls**:
- README documents this limitation (line 62)
- Note explains example is ready for future support

**Detection Assessment**:
- **Rating**: 1 (Certain)
- **Method**: CLI error or wrong behavior
- **Rationale**: Immediate failure or incorrect execution

**Risk Priority Number**:
- **RPN**: 8 × 10 × 1 = **80**
- **Priority**: Medium-High

**Recommended Actions**:
1. ✅ **DOCUMENTED**: README clearly states limitation
2. Add prominent warning in quick start
3. Provide workaround instructions if available
4. Update when CLI support is added

**Poka-Yoke Solution**: ⚠️ **PY-007** - Warning in README (already implemented)

---

### FM-005: Generated Files Have Syntax Errors
**Component**: Template Rendering  
**Function**: Generate valid JavaScript/YAML

**Failure Mode**: Templates generate invalid JavaScript or YAML syntax

**Effects**:
- Generated code cannot be imported
- OpenAPI spec invalid
- User must debug template issues

**Severity Assessment**:
- **Rating**: 9 (Critical)
- **Rationale**: Generated code is unusable

**Causes**:
- Template syntax errors
- Missing variables
- Incorrect Tera syntax
- SPARQL results don't match template expectations

**Occurrence Assessment**:
- **Rating**: 3 (Very Low)
- **Frequency**: ~0.5-1% (after template fixes)
- **Rationale**: Templates are tested, but edge cases exist

**Current Controls**:
- Templates have been reviewed
- Golden files provide reference
- Validation script compares outputs

**Detection Assessment**:
- **Rating**: 6 (Moderate)
- **Method**: Syntax errors discovered when importing/validating
- **Rationale**: Not detected until code is used

**Risk Priority Number**:
- **RPN**: 9 × 3 × 6 = **162**
- **Priority**: High

**Recommended Actions**:
1. ⚠️ **PLANNED**: Add syntax validation to post-generation checks (PY-002)
2. Test generated code can be imported
3. Validate OpenAPI spec with validator

**Poka-Yoke Solution**: ⚠️ **PY-002** - Post-generation validation (planned)

---

### FM-006: Generated Output Doesn't Match Ontology
**Component**: Data Transformation  
**Function**: Accurately transform ontology to code

**Failure Mode**: Generated code doesn't reflect ontology structure

**Effects**:
- Generated schemas missing properties
- Types don't match entities
- OpenAPI spec incomplete

**Severity Assessment**:
- **Rating**: 10 (Catastrophic)
- **Rationale**: Generated code is incorrect

**Causes**:
- SPARQL query errors
- Template logic errors
- Ontology changes not reflected

**Occurrence Assessment**:
- **Rating**: 2 (Remote)
- **Frequency**: ~0.1-0.5%
- **Rationale**: Queries and templates are tested

**Current Controls**:
- Golden files provide reference
- Validation script compares outputs
- Manual review of templates

**Detection Assessment**:
- **Rating**: 7 (Low)
- **Method**: Discovered during integration testing
- **Rationale**: Not obvious from generated code alone

**Risk Priority Number**:
- **RPN**: 10 × 2 × 7 = **140**
- **Priority**: High

**Recommended Actions**:
1. ⚠️ **PLANNED**: Add ontology-to-output validation (PY-003)
2. Compare SPARQL results with generated code
3. Add property count checks

**Poka-Yoke Solution**: ⚠️ **PY-003** - Ontology-to-output validation (planned)

---

### FM-007: Validation Script Fails on Windows
**Component**: Validation Script  
**Function**: Compare generated files with golden files

**Failure Mode**: `validate.mjs` or `verify.sh` fails on Windows due to path issues

**Effects**:
- User cannot verify output
- Cross-platform compatibility issue
- Confusing errors

**Severity Assessment**:
- **Rating**: 6 (Moderate)
- **Rationale**: Validation is helpful but not critical

**Causes**:
- Path separator differences (`/` vs `\`)
- Line ending differences (LF vs CRLF)
- Case sensitivity issues

**Occurrence Assessment**:
- **Rating**: 4 (Low)
- **Frequency**: ~1-2% (Windows users)
- **Rationale**: Most users on macOS/Linux

**Current Controls**:
- `validate.mjs` uses Node.js path module (cross-platform)
- `verify.sh` uses standard bash commands

**Detection Assessment**:
- **Rating**: 3 (Very High)
- **Method**: Script fails immediately on Windows
- **Rationale**: Path issues cause immediate errors

**Risk Priority Number**:
- **RPN**: 6 × 4 × 3 = **72**
- **Priority**: Medium

**Recommended Actions**:
1. Test validation scripts on Windows
2. Use Node.js path utilities (already done in validate.mjs)
3. Document Windows-specific issues if any

**Poka-Yoke Solution**: ✅ **PY-005** - Cross-platform validation script (validate.mjs uses Node.js paths)

---

### FM-008: Generated Imports Are Incorrect
**Component**: Module Resolution  
**Function**: Generate correct import/export statements

**Failure Mode**: Generated code has incorrect import paths

**Effects**:
- Module resolution fails
- Code cannot be imported
- Runtime errors

**Severity Assessment**:
- **Rating**: 8 (Major)
- **Rationale**: Generated code unusable

**Causes**:
- Template path errors
- Output directory structure changes
- Relative path calculation errors

**Occurrence Assessment**:
- **Rating**: 3 (Very Low)
- **Frequency**: ~0.5-1%
- **Rationale**: Templates are tested

**Current Controls**:
- Golden files provide reference
- Manual review of generated code

**Detection Assessment**:
- **Rating**: 4 (High)
- **Method**: Discovered when importing modules
- **Rationale**: Not detected until runtime

**Risk Priority Number**:
- **RPN**: 8 × 3 × 4 = **96**
- **Priority**: Medium

**Recommended Actions**:
1. ⚠️ **PLANNED**: Add import path validation (PY-002)
2. Test all imports resolve correctly
3. Verify module resolution

**Poka-Yoke Solution**: ⚠️ **PY-002** - Post-generation validation (planned)

---

### FM-009: OpenAPI Spec Invalid
**Component**: OpenAPI Generation  
**Function**: Generate valid OpenAPI 3.0 specification

**Failure Mode**: Generated OpenAPI spec doesn't pass validation

**Effects**:
- Cannot use with OpenAPI tools
- API documentation broken
- Client generation fails

**Severity Assessment**:
- **Rating**: 7 (Significant)
- **Rationale**: OpenAPI spec is important but not critical for core functionality

**Causes**:
- YAML syntax errors
- Invalid OpenAPI structure
- Missing required fields
- Schema reference errors

**Occurrence Assessment**:
- **Rating**: 3 (Very Low)
- **Frequency**: ~0.5-1%
- **Rationale**: Templates are tested

**Current Controls**:
- Golden files provide reference
- Manual review

**Detection Assessment**:
- **Rating**: 5 (Moderate-High)
- **Method**: Discovered when validating with OpenAPI tools
- **Rationale**: Not obvious from file content alone

**Risk Priority Number**:
- **RPN**: 7 × 3 × 5 = **105**
- **Priority**: High

**Recommended Actions**:
1. ⚠️ **PLANNED**: Add OpenAPI validation (PY-002)
2. Use OpenAPI validator tool
3. Test with OpenAPI tools

**Poka-Yoke Solution**: ⚠️ **PY-002** - Post-generation validation (planned)

---

### FM-010: Documentation Examples Don't Match Output
**Component**: Documentation  
**Function**: Provide accurate examples

**Failure Mode**: README or guides show examples that don't match actual output

**Effects**:
- User confusion
- Copy-paste errors
- Wasted time debugging

**Severity Assessment**:
- **Rating**: 5 (Minor)
- **Rationale**: Documentation issues are frustrating but not blocking

**Causes**:
- Documentation not updated after changes
- Examples from different version
- Copy-paste errors in docs

**Occurrence Assessment**:
- **Rating**: 4 (Low)
- **Frequency**: ~1-2%
- **Rationale**: Documentation is maintained but can drift

**Current Controls**:
- Documentation reviewed
- Examples tested

**Detection Assessment**:
- **Rating**: 4 (High)
- **Method**: Discovered when user tries examples
- **Rationale**: Examples fail or don't match

**Risk Priority Number**:
- **RPN**: 5 × 4 × 4 = **80**
- **Priority**: Medium

**Recommended Actions**:
1. ✅ **COMPLETED**: Verified README examples match actual structure
2. Test all code examples
3. Keep documentation in sync with code

**Poka-Yoke Solution**: ✅ Documentation verification completed

---

### FM-011: Missing Error Messages for Common Mistakes
**Component**: Error Handling  
**Function**: Provide helpful error messages

**Failure Mode**: Errors don't provide clear guidance on how to fix

**Effects**:
- User confusion
- Wasted time debugging
- Poor user experience

**Severity Assessment**:
- **Rating**: 4 (Low)
- **Rationale**: Errors are frustrating but don't break functionality

**Causes**:
- Generic error messages
- Missing context
- No suggested fixes

**Occurrence Assessment**:
- **Rating**: 6 (Moderate)
- **Frequency**: ~5-10% of errors
- **Rationale**: Some errors have good messages, others don't

**Current Controls**:
- Error messages in code
- Troubleshooting section in README

**Detection Assessment**:
- **Rating**: 3 (Very High)
- **Method**: User reports confusing errors
- **Rationale**: Errors are visible but quality varies

**Risk Priority Number**:
- **RPN**: 4 × 6 × 3 = **72**
- **Priority**: Medium

**Recommended Actions**:
1. ⚠️ **PLANNED**: Enhance error messages (PY-004)
2. Add suggested fixes to errors
3. Link to relevant documentation

**Poka-Yoke Solution**: ⚠️ **PY-004** - Enhanced error messages (planned)

---

### FM-012: Validation Script Doesn't Handle Edge Cases
**Component**: Validation Script  
**Function**: Compare files reliably

**Failure Mode**: Validation script fails on edge cases (empty files, binary files, etc.)

**Effects**:
- False positives/negatives
- Validation unreliable
- User confusion

**Severity Assessment**:
- **Rating**: 4 (Low)
- **Rationale**: Edge cases are rare

**Causes**:
- Empty files
- Binary files (shouldn't exist but could)
- Very large files
- Permission issues

**Occurrence Assessment**:
- **Rating**: 2 (Remote)
- **Frequency**: ~0.1-0.5%
- **Rationale**: Edge cases are rare

**Current Controls**:
- Validation script handles normal cases
- Error handling in script

**Detection Assessment**:
- **Rating**: 4 (High)
- **Method**: Discovered when edge case occurs
- **Rationale**: Not tested for edge cases

**Risk Priority Number**:
- **RPN**: 4 × 2 × 4 = **32**
- **Priority**: Low

**Recommended Actions**:
1. Test validation with edge cases
2. Add handling for empty files
3. Document limitations

**Poka-Yoke Solution**: ⚠️ Edge case handling (low priority)

---

### FM-013: Non-Deterministic Output
**Component**: Generation Process  
**Function**: Produce identical output for same inputs

**Failure Mode**: Running `ggen sync` twice produces different output

**Effects**:
- Validation fails randomly
- Cannot rely on generated code
- Debugging difficult

**Severity Assessment**:
- **Rating**: 9 (Critical)
- **Rationale**: Determinism is a core requirement

**Causes**:
- Timestamps in output
- Random IDs
- Non-deterministic SPARQL query order
- File system ordering

**Occurrence Assessment**:
- **Rating**: 2 (Remote)
- **Frequency**: ~0.1-0.5%
- **Rationale**: Determinism is tested but edge cases exist

**Current Controls**:
- Golden files provide reference
- Determinism is a requirement
- Templates avoid timestamps

**Detection Assessment**:
- **Rating**: 2 (Almost Certain)
- **Method**: Running twice and comparing
- **Rationale**: Easy to detect

**Risk Priority Number**:
- **RPN**: 9 × 2 × 2 = **36**
- **Priority**: Low (but important)

**Recommended Actions**:
1. ⚠️ **PLANNED**: Add determinism verification (PY-003)
2. Run generation twice and compare
3. Check for timestamps/random values

**Poka-Yoke Solution**: ⚠️ **PY-003** - Determinism verification (planned)

---

### FM-014: Golden Files Outdated
**Component**: Testing Infrastructure  
**Function**: Provide reference for validation

**Failure Mode**: Golden files don't match current expected output

**Effects**:
- Validation always fails
- False negatives
- User confusion

**Severity Assessment**:
- **Rating**: 6 (Moderate)
- **Rationale**: Validation is important but not critical

**Causes**:
- Golden files not updated after changes
- Ontology changes
- Template changes

**Occurrence Assessment**:
- **Rating**: 3 (Very Low)
- **Frequency**: ~0.5-1%
- **Rationale**: Golden files are maintained

**Current Controls**:
- Golden files are version controlled
- Changes require updates

**Detection Assessment**:
- **Rating**: 1 (Certain)
- **Method**: Validation fails immediately
- **Rationale**: Easy to detect

**Risk Priority Number**:
- **RPN**: 6 × 3 × 1 = **18**
- **Priority**: Low

**Recommended Actions**:
1. ⚠️ **PLANNED**: Add warning if golden files older than ontology (PY-006)
2. Document golden file update process
3. Add check to CI/CD

**Poka-Yoke Solution**: ⚠️ **PY-006** - Warnings for outdated golden files (planned)

---

### FM-015: lib/ Directory Already Exists
**Component**: File System  
**Function**: Generate clean output

**Failure Mode**: User runs `ggen sync` when `lib/` already exists

**Effects**:
- May overwrite user changes
- Confusion about what changed
- Potential data loss

**Severity Assessment**:
- **Rating**: 5 (Minor)
- **Rationale**: Can overwrite but usually expected

**Causes**:
- Previous generation run
- User modifications
- Git checkout

**Occurrence Assessment**:
- **Rating**: 7 (Moderately High)
- **Frequency**: ~10-20%
- **Rationale**: Common scenario

**Current Controls**:
- `ggen sync` overwrites by default
- `.gitignore` prevents committing

**Detection Assessment**:
- **Rating**: 1 (Certain)
- **Method**: Directory exists check
- **Rationale**: Easy to detect

**Risk Priority Number**:
- **RPN**: 5 × 7 × 1 = **35**
- **Priority**: Low

**Recommended Actions**:
1. ⚠️ **PLANNED**: Add warning if lib/ exists (PY-006)
2. Suggest deleting for fresh generation
3. Document behavior

**Poka-Yoke Solution**: ⚠️ **PY-006** - Warnings for existing lib/ (planned)

---

## Summary by Priority

### High Priority (RPN 100-199)
1. **FM-005**: Generated files have syntax errors (RPN: 162)
2. **FM-006**: Generated output doesn't match ontology (RPN: 140)
3. **FM-009**: OpenAPI spec invalid (RPN: 105)

### Medium Priority (RPN 50-99)
1. **FM-008**: Generated imports are incorrect (RPN: 96)
2. **FM-004**: ggen.toml format not fully supported (RPN: 80)
3. **FM-010**: Documentation examples don't match output (RPN: 80)
4. **FM-007**: Validation script fails on Windows (RPN: 72)
5. **FM-011**: Missing error messages (RPN: 72)
6. **FM-002**: Node.js version too old (RPN: 56)

### Low Priority (RPN < 50)
1. **FM-003**: Running from wrong directory (RPN: 45)
2. **FM-013**: Non-deterministic output (RPN: 36)
3. **FM-015**: lib/ directory already exists (RPN: 35)
4. **FM-012**: Validation script edge cases (RPN: 32)
5. **FM-014**: Golden files outdated (RPN: 18)
6. **FM-001**: ggen CLI not installed (RPN: 30)

## Poka-Yoke Solutions Status

### ✅ Implemented
- **PY-001**: Pre-flight prerequisite check script (`check-prerequisites.mjs`)
  - Validates ggen CLI, Node.js version, directory structure, ontology files
  - Addresses: FM-001, FM-002, FM-003

### ⚠️ Planned
- **PY-002**: Post-generation validation
  - Syntax check for .mjs files
  - YAML validation for OpenAPI
  - Import path validation
  - Addresses: FM-005, FM-008, FM-009

- **PY-003**: Determinism verification
  - Run generation twice and compare
  - Check for timestamps/random values
  - Addresses: FM-006, FM-013

- **PY-004**: Enhanced error messages
  - Clear error messages with suggested fixes
  - Link to documentation
  - Addresses: FM-011

- **PY-006**: Warnings for potential issues
  - Warn if lib/ exists
  - Warn if golden files outdated
  - Addresses: FM-014, FM-015

## Recommendations

1. **Immediate Actions** (High Priority):
   - Implement PY-002 (post-generation validation) to catch syntax errors early
   - Implement PY-003 (determinism verification) to ensure consistency
   - Add OpenAPI validator to catch invalid specs

2. **Short-term Actions** (Medium Priority):
   - Enhance error messages (PY-004)
   - Test validation scripts on Windows
   - Update documentation when CLI support is added

3. **Long-term Actions** (Low Priority):
   - Add edge case handling to validation scripts
   - Implement golden file freshness checks
   - Add warnings for existing lib/ directory

## Conclusion

The OpenAPI example has a solid foundation with good documentation and validation infrastructure. The main risks are:
1. Syntax errors in generated code (FM-005, RPN: 162)
2. Output not matching ontology (FM-006, RPN: 140)
3. Invalid OpenAPI specs (FM-009, RPN: 105)

These can be addressed with post-generation validation (PY-002) and determinism verification (PY-003). The prerequisite check script (PY-001) has already been implemented and addresses several low-to-medium priority issues.

The example is well-prepared for when full `ggen.toml` support is available in the CLI.



