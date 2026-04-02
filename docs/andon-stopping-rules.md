<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [🛑 ANDON STOPPING RULES - When to STOP and Fix](#-andon-stopping-rules---when-to-stop-and-fix)
  - [🔴 CATEGORY 1: IMMEDIATE STOP (Red Alert)](#-category-1-immediate-stop-red-alert)
    - [1.1 Test Failures](#11-test-failures)
    - [1.2 Critical/High Security Vulnerabilities](#12-criticalhigh-security-vulnerabilities)
    - [1.3 Production Deployment Failures](#13-production-deployment-failures)
    - [1.4 Performance Regression >10%](#14-performance-regression-10)
    - [1.5 Build Time >5 Seconds (Critical Threshold)](#15-build-time-5-seconds-critical-threshold)
  - [🟡 CATEGORY 2: PAUSE AND INVESTIGATE (Yellow Alert)](#-category-2-pause-and-investigate-yellow-alert)
    - [2.1 Compiler Warnings >3](#21-compiler-warnings-3)
    - [2.2 Binary Size >5MB](#22-binary-size-5mb)
    - [2.3 Test Coverage <90% (Critical Paths)](#23-test-coverage-90-critical-paths)
    - [2.4 Low Severity Security Issues (CVSS 3.0-6.9)](#24-low-severity-security-issues-cvss-30-69)
    - [2.5 Unmaintained Dependencies](#25-unmaintained-dependencies)
  - [🟢 CATEGORY 3: MONITOR AND IMPROVE (Green Alert)](#-category-3-monitor-and-improve-green-alert)
    - [3.1 Build Time 3-5 Seconds](#31-build-time-3-5-seconds)
    - [3.2 Test Count Growth](#32-test-count-growth)
    - [3.3 Dependency Count Growth](#33-dependency-count-growth)
  - [📊 DECISION MATRIX](#-decision-matrix)
  - [🚨 ANDON CORD PULLING PROTOCOL](#-andon-cord-pulling-protocol)
    - [How to Pull the Andon Cord](#how-to-pull-the-andon-cord)
    - [Cultural Expectations](#cultural-expectations)
  - [📈 METRICS TO TRACK](#-metrics-to-track)
    - [Stop Frequency](#stop-frequency)
    - [Time to Resolution](#time-to-resolution)
    - [Root Cause Categories](#root-cause-categories)
    - [Recurrence Rate](#recurrence-rate)
  - [🎯 PHASE-SPECIFIC STOPPING RULES](#-phase-specific-stopping-rules)
    - [Phase 2 (Current): Lean Optimization](#phase-2-current-lean-optimization)
    - [Phase 3 (Planned): Technical Debt](#phase-3-planned-technical-debt)
    - [Phase 4 (Future): Production Hardening](#phase-4-future-production-hardening)
  - [🔧 AUTOMATION HOOKS](#-automation-hooks)
    - [Pre-Commit Hooks](#pre-commit-hooks)
    - [CI Pipeline Gates](#ci-pipeline-gates)
    - [Deployment Gates](#deployment-gates)
  - [📚 LEARNING FROM STOPS](#-learning-from-stops)
    - [Post-Stop Review Template](#post-stop-review-template)
  - [🏆 SUCCESS CRITERIA](#-success-criteria)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 🛑 ANDON STOPPING RULES - When to STOP and Fix

**Purpose**: Define clear criteria for when development must STOP to address quality issues.

---

## 🔴 CATEGORY 1: IMMEDIATE STOP (Red Alert)

**Rule**: STOP all work immediately. Team swarms the problem.

### 1.1 Test Failures
```
TRIGGER: ANY test failure in ANY environment
ACTION:
  1. STOP current work
  2. Notify team immediately
  3. Root cause analysis within 30 minutes
  4. Fix before any new commits

RATIONALE:
- Broken tests indicate broken functionality
- Cost of defects increases 10x downstream
- Team velocity depends on stable foundation

EXCEPTION: None - zero tolerance for test failures
```

### 1.2 Critical/High Security Vulnerabilities
```
TRIGGER: CVE with severity >= 7.0 (CVSS score)
ACTION:
  1. STOP feature development
  2. Security team assessment
  3. Patch/upgrade within 24 hours
  4. Security audit of affected code
  5. Deploy fix to all environments

RATIONALE:
- Security breaches have catastrophic business impact
- Regulatory compliance requirements
- Customer trust is paramount

EXCEPTION: None for Critical (9.0+), rare for High with documented mitigation
```

### 1.3 Production Deployment Failures
```
TRIGGER: Deployment fails validation checks
ACTION:
  1. STOP deployment immediately
  2. Rollback to last known good state
  3. Root cause analysis
  4. Fix validation issues
  5. Re-verify before retry

RATIONALE:
- Failed deployments cause downtime
- Customer impact is immediate
- Deployment pipeline integrity critical

EXCEPTION: None - deployment quality is non-negotiable
```

### 1.4 Performance Regression >10%
```
TRIGGER: Any core operation >10% slower than baseline
ACTION:
  1. STOP merging performance-impacting PRs
  2. Profile the regression
  3. Identify root cause commit
  4. Revert or fix within sprint
  5. Add performance test to prevent recurrence

RATIONALE:
- User experience degradation compounds
- Performance debt is expensive to fix later
- Maintains competitive advantage

EXCEPTION: Acceptable if documented tradeoff (e.g., +10% time for +50% reliability)
```

### 1.5 Build Time >5 Seconds (Critical Threshold)
```
TRIGGER: Release build exceeds 5 seconds
ACTION:
  1. STOP adding new features
  2. Profile build process
  3. Identify bottleneck (deps, macros, compilation units)
  4. Optimize before proceeding
  5. Consider incremental compilation improvements

RATIONALE:
- Developer productivity suffers with slow builds
- CI/CD pipeline efficiency critical
- Fast feedback loops drive quality

EXCEPTION: Temporarily acceptable if major refactoring in progress (<1 week)
```

---

## 🟡 CATEGORY 2: PAUSE AND INVESTIGATE (Yellow Alert)

**Rule**: Pause non-critical work. Address before next milestone.

### 2.1 Compiler Warnings >3
```
TRIGGER: Clippy or rustc warnings exceed 3
ACTION:
  1. Pause new feature work
  2. Review and fix warnings
  3. Determine if warnings indicate design issues
  4. Fix before PR merge

RATIONALE:
- Warnings indicate code quality issues
- "Broken windows" theory - small issues compound
- Clean builds enable better tooling

TIMELINE: Fix within current sprint
EXCEPTION: Acceptable during rapid prototyping (must fix before merge)
```

### 2.2 Binary Size >5MB
```
TRIGGER: Release binary exceeds 5MB
ACTION:
  1. Investigate size increase
  2. Profile binary composition (cargo bloat)
  3. Remove unused dependencies
  4. Consider feature flags for optional code
  5. Optimize before release

RATIONALE:
- Large binaries slow distribution
- Indicates potential dependency bloat
- Embedded/CLI tools need to stay lean

TIMELINE: Investigate within 3 days
EXCEPTION: Acceptable if large dependency is critical (must document)
```

### 2.3 Test Coverage <90% (Critical Paths)
```
TRIGGER: Coverage of critical code paths drops below 90%
ACTION:
  1. Identify uncovered critical paths
  2. Write tests for gaps
  3. Review if code is truly critical
  4. Add coverage tracking to CI

RATIONALE:
- Critical paths must be tested
- Regression prevention
- Confidence in refactoring

TIMELINE: Add tests before next release
EXCEPTION: Acceptable for experimental features (must be flagged)
```

### 2.4 Low Severity Security Issues (CVSS 3.0-6.9)
```
TRIGGER: Low/Medium CVE in dependencies
ACTION:
  1. Assess actual risk to our use case
  2. Check for available patches
  3. Plan upgrade in next sprint
  4. Document mitigation if no patch available

RATIONALE:
- Stay ahead of security issues
- Easier to fix early than later
- Demonstrates security commitment

TIMELINE: Patch within 2 weeks
EXCEPTION: Acceptable if CVE not applicable to our usage (document why)
```

### 2.5 Unmaintained Dependencies
```
TRIGGER: Dependency unmaintained >1 year OR security advisories
ACTION:
  1. Assess criticality of dependency
  2. Search for maintained alternatives
  3. Plan migration timeline
  4. Document decision to keep/replace

RATIONALE:
- Unmaintained code accumulates vulnerabilities
- Future upgrades become harder
- Shows technical debt awareness

TIMELINE: Plan replacement in next quarter
EXCEPTION: Acceptable if no alternative AND code is stable AND no CVEs
```

---

## 🟢 CATEGORY 3: MONITOR AND IMPROVE (Green Alert)

**Rule**: Continue work. Track for continuous improvement.

### 3.1 Build Time 3-5 Seconds
```
TRIGGER: Build time approaching upper limit
ACTION:
  1. Monitor trend
  2. Identify optimization opportunities
  3. Plan improvements in Kaizen backlog

RATIONALE: Stay ahead of potential issues
TIMELINE: Address in next optimization sprint
```

### 3.2 Test Count Growth
```
TRIGGER: Adding new features without tests
ACTION:
  1. Review test coverage
  2. Ensure new code has tests
  3. Maintain >90% coverage on critical paths

RATIONALE: Prevent technical debt accumulation
TIMELINE: Ongoing vigilance
```

### 3.3 Dependency Count Growth
```
TRIGGER: Dependencies increasing without review
ACTION:
  1. Audit new dependencies
  2. Ensure genuine value add
  3. Consider bundling related deps

RATIONALE: Keep dependency tree lean
TIMELINE: Review quarterly
```

---

## 📊 DECISION MATRIX

| Severity | Impact | Response Time | Escalation | Team Action |
|----------|--------|---------------|------------|-------------|
| **Critical** | Production down | Immediate | CTO/CEO | All hands |
| **High** | Feature broken | <30 min | Engineering Lead | Swarm |
| **Medium** | Quality degraded | <2 hours | Team Lead | Pair on fix |
| **Low** | Minor issue | <1 day | Developer | Individual |
| **Info** | FYI | Next sprint | None | Backlog |

---

## 🚨 ANDON CORD PULLING PROTOCOL

### How to Pull the Andon Cord

1. **Identify**: Recognize a quality issue matching stop criteria
2. **Alert**: Immediately notify team (Slack #andon channel)
3. **Stop**: Cease current work if Red/Yellow category
4. **Swarm**: Team assembles to analyze problem
5. **Root Cause**: Use 5 Whys to find true cause
6. **Countermeasure**: Implement fix, not workaround
7. **Prevent**: Add test/check to prevent recurrence
8. **Resume**: Continue work after fix verified

### Cultural Expectations

✅ **DO:**
- Pull the cord without hesitation
- Thank people who pull the cord
- Learn from every stop
- Share learnings widely
- Celebrate quality wins

❌ **DON'T:**
- Shame or blame for pulling cord
- Hide quality issues
- Work around problems
- Rush fixes without root cause
- Repeat same mistakes

---

## 📈 METRICS TO TRACK

### Stop Frequency
```
Target: 1-2 stops per sprint (shows active quality monitoring)
Too Few (<1/sprint): May not be catching issues
Too Many (>5/sprint): Process/quality issues upstream
```

### Time to Resolution
```
Red Alerts: <1 hour average
Yellow Alerts: <1 day average
Green Alerts: <1 week average
```

### Root Cause Categories
```
Track: Code, Tests, Dependencies, Performance, Security
Goal: Identify patterns and prevent systematically
```

### Recurrence Rate
```
Target: <5% recurrence of same root cause
Indicates: Effectiveness of countermeasures
```

---

## 🎯 PHASE-SPECIFIC STOPPING RULES

### Phase 2 (Current): Lean Optimization
```
FOCUS: Performance, test quality, security
PRIMARY STOPS: Test failures, performance regression, security
RELAXED: Dependency warnings (planned for Phase 3)
```

### Phase 3 (Planned): Technical Debt
```
FOCUS: Dependency cleanup, unsafe code reduction
PRIMARY STOPS: Unmaintained deps, unsafe proliferation
ENHANCED: Security scanning, debt metrics
```

### Phase 4 (Future): Production Hardening
```
FOCUS: Zero unsafe code, production monitoring
PRIMARY STOPS: Any unsafe addition, deployment issues
ENHANCED: Observability, error budgets
```

---

## 🔧 AUTOMATION HOOKS

### Pre-Commit Hooks
```bash
# Run before every commit
1. cargo clippy --all-targets --all-features -- -D warnings
2. cargo test --all
3. cargo fmt --check
4. cargo audit --deny warnings
```

### CI Pipeline Gates
```yaml
Quality Gates (All must pass):
  - Build time < 5s
  - Zero test failures
  - Zero clippy warnings
  - Zero critical/high CVEs
  - Binary size < 5MB
  - Performance benchmarks within 10% of baseline
```

### Deployment Gates
```yaml
Production Deployment (All must pass):
  - All CI quality gates
  - Manual security review
  - Performance validation
  - Rollback plan documented
  - Monitoring configured
```

---

## 📚 LEARNING FROM STOPS

### Post-Stop Review Template

```markdown
## Andon Stop Review - [Date]

**Trigger**: [What caused the stop?]
**Category**: [Red/Yellow/Green]
**Time to Detection**: [How long before caught?]
**Time to Resolution**: [How long to fix?]

**Root Cause**:
- Why 1: [Surface cause]
- Why 2: [Deeper cause]
- Why 3: [Process cause]
- Why 4: [System cause]
- Why 5: [Root cause]

**Countermeasure**:
- Immediate fix: [What was fixed]
- Prevention: [How to prevent recurrence]
- Process update: [What process changed]

**Lessons Learned**:
- [Key insight 1]
- [Key insight 2]
- [Action items]

**Status**: [Resolved/Monitoring/Recurring]
```

---

## 🏆 SUCCESS CRITERIA

**Effective Andon System:**
- ✅ Issues caught early (before production)
- ✅ Fast resolution times (<1 hour for Red)
- ✅ Low recurrence rate (<5%)
- ✅ Team empowered to stop work
- ✅ Continuous improvement visible
- ✅ Quality trends improving

**Team Culture:**
- ✅ No fear of pulling the cord
- ✅ Celebration of quality
- ✅ Learning from failures
- ✅ Shared ownership
- ✅ Pride in craftsmanship

---

**Remember**: Stopping to fix quality issues NOW is always faster and cheaper than fixing them later in production.

**The Andon Cord is Your Friend**: It prevents small issues from becoming big disasters.
