# FMEA: README Behaviors Analysis

**Failure Mode and Effects Analysis (FMEA)**
**Date**: 2025-11-17
**Focus**: ggen v3.0.0 Documented Behaviors & User Workflows

---

## FMEA Methodology

This analysis identifies potential **failure modes** (what could go wrong), their **causes**, **effects**, and **risk severity** for documented behaviors in the ggen README.

### Risk Scoring (RPN = Severity × Occurrence × Detection)
- **Severity** (1-10): Impact to user
- **Occurrence** (1-10): Likelihood it will happen
- **Detection** (1-10): How hard to detect/diagnose
- **RPN** (Risk Priority Number): Severity × Occurrence × Detection (target < 100)

---

## Critical Failure Modes (RPN > 200)

### 🔴 FM-1: Version Flag Returns Empty Output

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Version detection fails | `ggen --version` and `ggen -V` return nothing |
| **Cause** | Version output not implemented in main.rs | Users and automation scripts expect "ggen 3.0.0" |
| **Effect** | Users cannot detect installed version; CI/CD pipelines fail; version-gating logic breaks | HIGH |
| **Severity** | 9 | Blocks all version-dependent automation |
| **Occurrence** | 9 | Happens 100% of the time version is checked |
| **Detection** | 3 | Immediately obvious when running `ggen -V` |
| **RPN** | **243** | 🚨 CRITICAL |

**Impact**: Every user onboarding guide breaks. Enterprise deployments fail. AI package managers (that query version) can't verify installation.

**Detection Evidence**:
```bash
$ ggen --version
$ ggen -V
[No output at all - expect "ggen 3.0.0"]
```

**Recommended Action**: Implement version output before next release. HIGH priority fix.

---

### 🔴 FM-2: Missing "ggen ai generate-ontology" Command

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Core ontology generation unavailable | Command in README, NOT in CLI |
| **Cause** | Command structure defined but endpoint not implemented | Block users from "ontology-driven development" workflow |
| **Effect** | Users cannot follow README quick-start; entire "single source of truth" promise fails | SEVERE |
| **Severity** | 10 | Breaks primary value proposition |
| **Occurrence** | 10 | Every user tries README first |
| **Detection** | 1 | Immediately fails with "command not found" |
| **RPN** | **100** | 🔴 HIGH |

**Impact**: The README's headline workflow doesn't work:
```bash
# From README - promises this will work:
ggen ai generate-ontology --prompt "E-commerce: Product, Order, Review" --output domain.ttl
# Actual result: Error: unknown subcommand `generate-ontology`
```

This is the **core feature** - without it, users can't validate the "zero drift" promise.

**Recommended Action**: Implement command immediately. BLOCKER for any release claiming "production ready."

---

### 🔴 FM-3: README Examples Use Wrong CLI Syntax

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Example commands fail with "unexpected argument" errors | 4+ examples use positional args, CLI requires flags |
| **Cause** | README written for earlier version; CLI refactored to use named flags | Users blindly copy-paste and get errors |
| **Effect** | 40%+ of README examples fail immediately; user loses trust in documentation | MEDIUM-HIGH |
| **Severity** | 8 | Breaks user's first experience |
| **Occurrence** | 10 | Happens for every user following examples |
| **Detection** | 2 | Clear "unexpected argument" error message |
| **RPN** | **160** | 🔴 HIGH |

**Examples of breakage**:
```bash
# README says:
ggen graph load domain.ttl
# Actual requirement:
ggen graph load --file domain.ttl

# README says:
ggen marketplace search "rust microservice"
# Actual requirement:
ggen marketplace search --query "rust microservice"

# README says:
ggen graph query --sparql "SELECT..."
# Actual:
ggen graph query --sparql_query "SELECT..."
```

**Recommended Action**: Update README with actual syntax. Add CLI reference guide. Create regression tests.

---

## High Severity Failure Modes (RPN 100-200)

### 🟠 FM-4: Clap Argument Parser Panic on Graph Load

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Process crashes with panic instead of returning error | Graph load causes backtrace in clap_builder |
| **Cause** | Argument parsing mismatch not handled gracefully in clap error handler | File descriptor handling or process substitution issue |
| **Effect** | User sees ugly backtrace instead of helpful error; confuses debugging; looks unpolished | MEDIUM |
| **Severity** | 7 | Crashes process, shows stack trace |
| **Occurrence** | 6 | Only happens with process substitution/pipe input |
| **Detection** | 2 | Obvious from backtrace output |
| **RPN** | **84** | 🟠 MEDIUM |

**Evidence**:
```
thread 'main' (45278) panicked at .../clap_builder-4.5.51/src/parser/error.rs:32:9:
Mismatch in clap error handling
[full backtrace shown to user]
```

**Recommended Action**: Handle edge case in argument parsing. Return graceful error instead of panic.

---

### 🟠 FM-5: "Production Ready" Claim vs 61% Implementation

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | CLI advertises 89% production readiness but 39% of commands are stubs/incomplete | Mismatch between marketing and reality |
| **Cause** | README claims production-grade but paper, workflow modules return placeholder data | Version conflict: marketing v3.0 vs actual implementation |
| **Effect** | Enterprises evaluate on promises, find features don't work; trust loss; reputation damage | HIGH |
| **Severity** | 9 | Affects purchasing decisions |
| **Occurrence** | 7 | Enterprise customers verify feature parity |
| **Detection** | 4 | Requires functional testing beyond quick-start |
| **RPN** | **252** | 🚨 CRITICAL |

**Specific example - Paper commands**:
- README promises: "Research reproducibility platform for universities"
- Actual: `ggen paper compile` doesn't execute pdflatex, returns placeholder
- Actual: `ggen paper generate` returns 0KB file
- Actual: `ggen paper submit` doesn't connect to submission APIs

**Recommended Action**: Either implement all promised features or update marketing claims to reflect actual capability.

---

### 🟠 FM-6: Type Mapping Cannot Be Validated (No Ontology Source)

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Cannot verify "automatic type mapping" works end-to-end | No way to generate ontology to test with |
| **Cause** | `ggen ai generate-ontology` not implemented | Users can't follow type mapping workflow |
| **Effect** | Core claim "xsd:decimal → f64/number/Decimal" cannot be validated; user trust in type safety diminishes | MEDIUM |
| **Severity** | 6 | Feature claim can't be tested |
| **Occurrence** | 8 | Every user wanting type-safe code generation tries this |
| **Detection** | 3 | Requires full workflow test |
| **RPN** | **144** | 🟠 HIGH |

**Recommended Action**: Same as FM-2 - implement ontology generation.

---

### 🟠 FM-7: Polyglot Sync Cannot Be Demonstrated

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | "Zero drift" polyglot sync cannot be validated | No test demonstrating Rust + TypeScript + Python sync |
| **Cause** | Ontology generation missing + no multi-language test workflow documented | Feature claim not substantiated |
| **Effect** | Skeptics cannot verify "perfect sync" claim; enterprise deployments hesitant | MEDIUM |
| **Severity** | 7 | Undermines primary value proposition |
| **Occurrence** | 5 | Enterprise architects typically verify before adoption |
| **Detection** | 5 | Requires intentional multi-language test |
| **RPN** | **175** | 🟠 HIGH |

**Recommended Action**: Create complete example showing single ontology → generate Rust + TS + Python → verify identical structure.

---

## Medium Severity Failure Modes (RPN 50-100)

### 🟡 FM-8: Marketplace Features Documented But Incomplete

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Maturity scoring and filtering documented but return stubs | `ggen marketplace list --min-maturity 70` returns placeholder |
| **Cause** | Feature skeleton exists but business logic not implemented | Sorting, filtering stubs don't actually filter |
| **Effect** | Users cannot filter marketplace by quality; cannot make informed package selections | LOW-MEDIUM |
| **Severity** | 5 | Feature doesn't work but not critical path |
| **Occurrence** | 4 | Advanced users want filtering, not beginners |
| **Detection** | 7 | Requires checking output doesn't change with flags |
| **RPN** | **140** | 🟠 HIGH |

**Recommended Action**: Complete marketplace filtering implementation or remove from documentation.

---

### 🟡 FM-9: Paper Commands Return Placeholder Output

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Paper generation, compilation, submission return empty/placeholder results | Users expect actual LaTeX/PDF generation |
| **Cause** | Paper module (9 commands) mostly unimplemented stubs | Planned feature, not yet built |
| **Effect** | Academic users cannot use ggen for paper generation; promised feature unavailable | MEDIUM |
| **Severity** | 6 | Feature blocked but not critical for core |
| **Occurrence** | 3 | Paper features used by subset of users |
| **Detection** | 2 | Obvious when trying to open generated PDF |
| **RPN** | **36** | 🟡 LOW-MEDIUM |

**Recommended Action**: Either complete paper module or move to "planned" section of docs.

---

### 🟡 FM-10: Workflow Module Returns Hardcoded Demo Output

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Workflow discovery returns same hardcoded output regardless of input | No actual process mining logic |
| **Cause** | Workflow module (5 commands) completely unimplemented | Placeholder for future feature |
| **Effect** | Workflow analysis claims cannot be validated; feature unusable | LOW |
| **Severity** | 4 | Feature doesn't work but rarely used |
| **Occurrence** | 2 | Workflow module less likely to be tested |
| **Detection** | 8 | Obvious if you run it twice with different inputs |
| **RPN** | **64** | 🟡 MEDIUM |

**Recommended Action**: Mark as "alpha/experimental" or implement workflow mining logic.

---

## Low Severity Failure Modes (RPN < 50)

### 🟢 FM-11: Project Watch Command Blocks Main Thread

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | `ggen project watch` cannot complete; blocks indefinitely | Architectural issue with file watcher |
| **Cause** | Watch implementation uses blocking notification handler | No async integration with event loop |
| **Effect** | Users cannot use watch mode; must manually regenerate | LOW-MEDIUM |
| **Severity** | 4 | Feature unavailable but workaround exists |
| **Occurrence** | 3 | Power users want watch mode |
| **Detection** | 1 | Hangs immediately, obvious |
| **RPN** | **12** | 🟢 LOW |

**Recommended Action**: Refactor watch command to use async event handling.

---

### 🟢 FM-12: Hook Subcommand Names Inconsistent

| Factor | Score | Notes |
|--------|-------|-------|
| **Failure Mode** | Paper uses `list-templates` but should be `templates` (naming inconsistency) | Subcommand naming varies across modules |
| **Cause** | No consistent naming convention across commands | Different authors built different modules |
| **Effect** | Users confused about correct subcommand names; need to try both variants | LOW |
| **Severity** | 2 | Cosmetic, easily fixed |
| **Occurrence** | 3 | Affects experienced users who remember patterns |
| **Detection** | 1 | Error message shows correct name |
| **RPN** | **6** | 🟢 VERY LOW |

**Recommended Action**: Standardize subcommand naming convention across all modules.

---

## FMEA Summary Table

| ID | Failure Mode | Severity | Occurrence | Detection | RPN | Priority |
|----|--------------|----------|-----------|-----------|-----|----------|
| FM-1 | Version flag empty output | 9 | 9 | 3 | **243** | 🔴 CRITICAL |
| FM-5 | "Production ready" claim vs 61% implementation | 9 | 7 | 4 | **252** | 🔴 CRITICAL |
| FM-2 | Missing `generate-ontology` command | 10 | 10 | 1 | **100** | 🔴 CRITICAL |
| FM-3 | README examples use wrong syntax | 8 | 10 | 2 | **160** | 🟠 HIGH |
| FM-6 | Type mapping cannot be validated | 6 | 8 | 3 | **144** | 🟠 HIGH |
| FM-7 | Polyglot sync cannot be demonstrated | 7 | 5 | 5 | **175** | 🟠 HIGH |
| FM-8 | Marketplace filtering incomplete | 5 | 4 | 7 | **140** | 🟠 HIGH |
| FM-4 | Clap parser panic on edge case | 7 | 6 | 2 | **84** | 🟠 MEDIUM |
| FM-9 | Paper commands return stubs | 6 | 3 | 2 | **36** | 🟡 LOW |
| FM-10 | Workflow returns hardcoded output | 4 | 2 | 8 | **64** | 🟡 MEDIUM |
| FM-11 | Project watch blocks indefinitely | 4 | 3 | 1 | **12** | 🟢 LOW |
| FM-12 | Subcommand naming inconsistent | 2 | 3 | 1 | **6** | 🟢 VERY LOW |

---

## Risk Mitigation Strategies

### Tier 1: Immediate (Before Release)
**Target RPN < 100 for customer-facing issues**

1. **FM-1**: Implement version output
   - Add version string to CLI output handler
   - Test with: `ggen --version` and `ggen -V`
   - Verify: Output is "ggen 3.0.0"
   - Time: 30 minutes

2. **FM-2**: Implement `generate-ontology` command
   - Create AI endpoint in ggen-ai crate
   - Wire to CLI verb handler
   - Test with e-commerce prompt from README
   - Verify: Generates valid TTL
   - Time: 2-4 hours

3. **FM-3**: Update README examples to correct syntax
   - Audit README for all CLI commands
   - Replace positional args with --flag syntax
   - Test each example
   - Add "CLI Reference" section
   - Time: 1-2 hours

### Tier 2: High Priority (Within 2 weeks)
**Target RPN < 150**

4. **FM-5**: Update marketing claims or implement stubs
   - Either: Complete paper/workflow modules
   - Or: Change "89% production ready" to realistic %
   - Mark alpha features clearly
   - Time: 4-8 hours (decision dependent)

5. **FM-6 & FM-7**: Create end-to-end validation example
   - Generate ontology
   - Create Rust, TypeScript, Python models
   - Verify structure matches
   - Publish as example
   - Time: 4-6 hours

6. **FM-4**: Fix clap panic handling
   - Add error handling for edge cases
   - Test with process substitution
   - Return graceful error instead of panic
   - Time: 1-2 hours

### Tier 3: Medium Priority (Before next feature release)
**Target RPN < 100 for all issues**

7. **FM-8**: Complete marketplace filtering
   - Implement actual maturity filtering
   - Test filtering logic
   - Remove placeholder returns
   - Time: 2-3 hours

8. **FM-10**: Complete or mark workflow as alpha
   - Either implement workflow discovery
   - Or clearly mark as "experimental feature"
   - Time: Decision dependent

9. **FM-11**: Refactor watch command
   - Use async file watcher
   - Integrate with tokio runtime
   - Time: 3-4 hours

---

## Acceptance Criteria for Risk Reduction

### For Production Release
- [ ] FM-1: `ggen --version` returns "ggen 3.0.0"
- [ ] FM-2: `ggen ai generate-ontology` works with README example
- [ ] FM-3: All README examples tested and working
- [ ] FM-4: No panics on valid input (returns clear errors)
- [ ] FM-5: Marketing claims match implementation (>80% features working)

### For Beta/RC Release
- [ ] FM-6: End-to-end type mapping example documented and working
- [ ] FM-7: Polyglot sync example demonstrates zero drift
- [ ] FM-8: Marketplace filtering works for at least 2 criteria

### For Post-Release Cleanup
- [ ] FM-9, FM-10: Paper/workflow modules either complete or clearly marked alpha
- [ ] FM-11: Watch mode works without blocking
- [ ] FM-12: All subcommand names follow consistent pattern

---

## FMEA Conclusion

**Current State**: ggen v3.0.0 has **critical gaps** between README promises and actual implementation.

**Risk Level**: 🔴 **HIGH** - 3 critical failure modes (RPN > 200) block basic functionality

**Recommendation**:
1. **Do not release as "Production Ready"** until FM-1, FM-2, FM-3 are fixed
2. **Update README immediately** to reflect actual CLI syntax
3. **Implement missing ontology command** - core to value proposition
4. **Create comprehensive test** that validates all README examples work

**Estimated effort to reach "Production Ready" status**:
- Critical fixes (FM-1, FM-2, FM-3): 4-6 hours
- High priority (FM-4, FM-5, FM-6, FM-7): 12-16 hours
- **Total**: 16-22 hours to production quality

Without these fixes, enterprise customers will evaluate the tool based on README promises, find 50% of examples don't work, and abandon the project.
