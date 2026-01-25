<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Incident Response Plan (v6.0.0)](#incident-response-plan-v600)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Incident Severity Levels](#incident-severity-levels)
    - [P0 - Critical (Response: Immediate)](#p0---critical-response-immediate)
    - [P1 - High (Response: Urgent)](#p1---high-response-urgent)
    - [P2 - Medium (Response: Standard)](#p2---medium-response-standard)
    - [P3 - Low (Response: Normal)](#p3---low-response-normal)
  - [Detection and Analysis](#detection-and-analysis)
    - [Detection Methods](#detection-methods)
    - [Initial Analysis](#initial-analysis)
  - [Containment Strategy](#containment-strategy)
    - [Immediate Containment (P0/P1)](#immediate-containment-p0p1)
    - [Long-term Containment (P2/P3)](#long-term-containment-p2p3)
  - [Eradication and Recovery](#eradication-and-recovery)
    - [Eradication Process](#eradication-process)
    - [Recovery Process](#recovery-process)
  - [Post-Incident Review](#post-incident-review)
    - [Review Meeting (Within 1 Week)](#review-meeting-within-1-week)
    - [Root Cause Analysis (5 Whys)](#root-cause-analysis-5-whys)
    - [Incident Report Template](#incident-report-template)
  - [Communication Plan](#communication-plan)
    - [Internal Communication](#internal-communication)
    - [External Communication](#external-communication)
  - [Roles and Responsibilities](#roles-and-responsibilities)
    - [Incident Commander (IC)](#incident-commander-ic)
    - [Security Engineer](#security-engineer)
    - [DevOps Engineer](#devops-engineer)
    - [Communications Lead](#communications-lead)
  - [Incident Response Tools](#incident-response-tools)
    - [Runbooks](#runbooks)
    - [Scripts](#scripts)
    - [Checklists](#checklists)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Incident Response Plan (v6.0.0)

## Overview

This document defines the incident response procedures for ggen v6.0.0, covering detection, analysis, containment, eradication, recovery, and post-incident review.

**Last Updated**: 2026-01-24
**Version**: 6.0.0
**Audience**: Security Team, Maintainers, Incident Responders

---

## Table of Contents

1. [Incident Severity Levels](#incident-severity-levels)
2. [Detection and Analysis](#detection-and-analysis)
3. [Containment Strategy](#containment-strategy)
4. [Eradication and Recovery](#eradication-and-recovery)
5. [Post-Incident Review](#post-incident-review)
6. [Communication Plan](#communication-plan)
7. [Roles and Responsibilities](#roles-and-responsibilities)
8. [Incident Response Tools](#incident-response-tools)

---

## Incident Severity Levels

### P0 - Critical (Response: Immediate)

**Definition**: Active exploitation, data breach, or critical vulnerability

**Examples**:
- Remote code execution (RCE) in production
- Active data breach (credentials, secrets leaked)
- Widespread service outage affecting all users
- Zero-day vulnerability being actively exploited

**Response Time**:
- Acknowledge: 15 minutes
- Initial assessment: 1 hour
- Containment: 2 hours
- Fix: 24 hours
- Public disclosure: After patch deployment

**Team**: All hands on deck, escalate to leadership

---

### P1 - High (Response: Urgent)

**Definition**: Serious vulnerability or potential for significant impact

**Examples**:
- Authentication bypass discovered
- SPARQL injection vulnerability
- Path traversal enabling arbitrary file read
- Unpatched high-severity CVE in dependency

**Response Time**:
- Acknowledge: 1 hour
- Initial assessment: 4 hours
- Containment: 1 day
- Fix: 1 week
- Public disclosure: After patch deployment

**Team**: Security team + on-call engineers

---

### P2 - Medium (Response: Standard)

**Definition**: Moderate vulnerability with limited impact

**Examples**:
- Information disclosure (non-sensitive data)
- Denial of service requiring extreme inputs
- Logic errors in non-critical paths
- Missing rate limiting on endpoints

**Response Time**:
- Acknowledge: 4 hours
- Initial assessment: 1 day
- Containment: 3 days
- Fix: 2 weeks
- Public disclosure: Next release notes

**Team**: Security team

---

### P3 - Low (Response: Normal)

**Definition**: Minor issue with negligible impact

**Examples**:
- Cosmetic security warnings
- Edge case vulnerabilities
- Documentation gaps
- Non-critical dependency updates

**Response Time**:
- Acknowledge: 1 day
- Initial assessment: 1 week
- Containment: N/A
- Fix: Next release
- Public disclosure: Next release notes

**Team**: Maintainers

---

## Detection and Analysis

### Detection Methods

**1. Automated Monitoring**

```bash
# Log monitoring (security events)
tail -f .ggen/audit/*.json | jq 'select(.event_type == "security_violation")'

# Rate limit violations
tail -f .ggen/audit/*.json | jq 'select(.event_type == "rate_limit_exceeded")'

# Anomalous patterns
tail -f .ggen/audit/*.json | jq 'select(.suspicious == true)'
```

**2. User Reports**

- Email: `sean@chatmangpt.com` (Subject: `[SECURITY]`)
- GitHub Security Advisories (private reporting)
- Community reports (Discord, forums)

**3. Security Scanning**

- `cargo audit` (nightly CI)
- Dependabot alerts (GitHub)
- Third-party security audits (quarterly)

**4. Threat Intelligence**

- RustSec Advisory Database
- CVE feeds
- Security mailing lists

### Initial Analysis

**Triage Questions**:
1. What is the vulnerability?
2. What is the severity (P0-P3)?
3. Is it being actively exploited?
4. What versions are affected?
5. What is the attack vector?
6. What is the potential impact?

**Analysis Checklist**:
- [ ] Reproduce vulnerability in test environment
- [ ] Identify affected versions
- [ ] Assess exploitability
- [ ] Determine scope of impact
- [ ] Check logs for evidence of exploitation
- [ ] Assign severity level

---

## Containment Strategy

### Immediate Containment (P0/P1)

**Goals**:
- Stop ongoing exploitation
- Prevent further damage
- Preserve evidence

**Actions**:

**1. Disable Vulnerable Feature**

```rust
// Add feature flag to disable vulnerable code
#[cfg(not(feature = "disable_vulnerable_feature"))]
fn vulnerable_function() {
    // ...
}

// Deploy with:
cargo build --release --no-default-features
```

**2. Rate Limit Affected Endpoints**

```toml
# Emergency rate limiting
[rate_limit]
max_requests_per_minute = 10  # Reduce from 60
max_concurrent_generations = 1  # Reduce from 10
```

**3. Notify Affected Users**

```bash
# Send email to all registered users
./scripts/notify_security_incident.sh \
  --severity=P0 \
  --message="Security incident detected. Please update immediately."
```

**4. Preserve Evidence**

```bash
# Copy logs to secure location
cp -r .ggen/audit /secure/incident-$(date +%Y%m%d)/

# Preserve receipts
cp -r .ggen/receipts /secure/incident-$(date +%Y%m%d)/

# Snapshot system state
tar czf /secure/incident-$(date +%Y%m%d)/system-snapshot.tar.gz \
  .ggen/ Cargo.lock Cargo.toml
```

### Long-term Containment (P2/P3)

**Goals**:
- Develop permanent fix
- Prevent similar vulnerabilities
- Update documentation

**Actions**:
- Develop patch (see [Eradication](#eradication-and-recovery))
- Update security tests
- Review related code for similar issues
- Update security documentation

---

## Eradication and Recovery

### Eradication Process

**1. Develop Fix**

```rust
// Example: Fix path traversal vulnerability

// ❌ Before (vulnerable)
fn load_file(path: &str) -> Result<String, Error> {
    let full_path = PathBuf::from(path);
    fs::read_to_string(full_path)
}

// ✅ After (fixed)
use ggen_core::security::SafePath;

fn load_file(path: &str) -> Result<String, Error> {
    let safe_path = SafePath::new(path)?
        .within_directory(Path::new("templates"))?;
    fs::read_to_string(safe_path.as_path())
}
```

**2. Write Regression Test**

```rust
/// Regression test for CVE-2026-XXXX (path traversal)
#[test]
fn test_cve_2026_xxxx_path_traversal() {
    // Should reject traversal attempts
    let result = load_file("../../../etc/passwd");
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("path traversal"));
}
```

**3. Validate Fix**

```bash
# Run security tests
cargo make test-security

# Run regression tests
cargo test test_cve_2026

# Run penetration tests
./scripts/pen_test.sh --vulnerability=path-traversal
```

**4. Update Dependencies (if applicable)**

```bash
# Update vulnerable dependency
cargo update oxigraph

# Verify no vulnerabilities
cargo audit --deny warnings

# Test with new dependency version
cargo make test
```

### Recovery Process

**1. Release Patch**

```bash
# Bump version
./scripts/bump_version.sh --type=patch --reason="Security fix"

# Build release
cargo make release-validate

# Tag release
git tag -s v6.0.1 -m "Security patch for CVE-2026-XXXX"

# Publish to crates.io
cargo publish
```

**2. Deploy to Production (if applicable)**

```bash
# Deploy patch
./scripts/deploy.sh --version=v6.0.1 --emergency

# Verify deployment
./scripts/verify_deployment.sh --check=security

# Monitor for issues
./scripts/monitor.sh --duration=1h
```

**3. Notify Users**

```markdown
## Security Advisory: CVE-2026-XXXX (Path Traversal)

**Severity**: High (P1)
**Affected Versions**: 6.0.0
**Fixed Version**: 6.0.1

### Description

A path traversal vulnerability was discovered in ggen v6.0.0 that allows
arbitrary file read when processing templates.

### Impact

An attacker could read arbitrary files on the system by crafting a
malicious template path.

### Remediation

Update to ggen v6.0.1 immediately:

    cargo update ggen

### Workaround

If you cannot update immediately, restrict template directory permissions:

    chmod 700 templates/

### References

- CVE-2026-XXXX
- GitHub Advisory: GHSA-XXXX-XXXX-XXXX

### Credits

Discovered by: [Researcher Name] (with permission)

### Timeline

- 2026-01-24: Vulnerability reported
- 2026-01-24: Acknowledged and confirmed
- 2026-01-25: Patch developed and tested
- 2026-01-25: v6.0.1 released
- 2026-01-26: Public disclosure
```

---

## Post-Incident Review

### Review Meeting (Within 1 Week)

**Attendees**:
- Security team
- Engineering leads
- Product management
- DevOps

**Agenda**:
1. Timeline review
2. What went well?
3. What went poorly?
4. Root cause analysis (5 Whys)
5. Action items

### Root Cause Analysis (5 Whys)

**Example**:

**Problem**: Path traversal vulnerability in template loading

1. **Why did the vulnerability occur?**
   - Because we didn't validate user-provided paths

2. **Why didn't we validate paths?**
   - Because SafePath wasn't implemented yet

3. **Why wasn't SafePath implemented?**
   - Because it wasn't prioritized in the roadmap

4. **Why wasn't it prioritized?**
   - Because we didn't have a security threat model

5. **Why didn't we have a threat model?**
   - Because security review wasn't part of the design process

**Root Cause**: Lack of security review in design process

**Corrective Actions**:
1. Implement mandatory security review for all features
2. Add security checkpoints to roadmap
3. Train developers on threat modeling
4. Create security design templates

### Incident Report Template

```markdown
# Incident Report: [CVE-ID or Internal ID]

## Summary

[Brief description of the incident]

## Timeline

- 2026-01-24 10:00 UTC: Vulnerability reported
- 2026-01-24 10:15 UTC: Acknowledged
- 2026-01-24 11:00 UTC: Initial assessment complete
- 2026-01-24 13:00 UTC: Containment measures deployed
- 2026-01-25 09:00 UTC: Patch developed
- 2026-01-25 15:00 UTC: Patch released (v6.0.1)
- 2026-01-26 09:00 UTC: Public disclosure

## Root Cause

[5 Whys analysis]

## Impact

- Severity: [P0/P1/P2/P3]
- Affected versions: [...]
- Users affected: [...]
- Data compromised: [None/...]

## Mitigation

- Short-term: [...]
- Long-term: [...]

## Lessons Learned

**What went well**:
- [...]

**What went poorly**:
- [...]

**Action items**:
- [ ] [Action 1] (Owner: [...], Due: [...])
- [ ] [Action 2] (Owner: [...], Due: [...])

## References

- CVE: [...]
- GitHub Advisory: [...]
- Pull Request: [...]
```

---

## Communication Plan

### Internal Communication

**Channels**:
- Security Slack channel (immediate)
- Email (security team, within 1 hour)
- All-hands meeting (P0/P1 incidents, within 1 day)

**Template**:
```
🚨 SECURITY INCIDENT [P0/P1/P2/P3]

Incident ID: SEC-2026-001
Severity: P1
Status: Investigating

Summary:
[Brief description]

Actions taken:
- [...]

Next steps:
- [...]

Assigned to: [Name]
Expected resolution: [Date/Time]
```

### External Communication

**Channels**:
- Email to affected users
- GitHub Security Advisory
- Release notes
- Blog post (P0/P1)
- Twitter/social media (P0)

**Timeline**:
- P0: Immediate (after containment)
- P1: Within 24 hours (after patch)
- P2: Next release
- P3: Next release notes

**Template**:
```markdown
## Security Advisory: [Title]

**Date**: 2026-01-26
**Severity**: High
**CVE**: CVE-2026-XXXX

### Summary

[Brief, non-technical description]

### Impact

[Who is affected, what is the risk]

### Remediation

[How to fix, upgrade instructions]

### Workaround

[If patch not available yet]

### Timeline

[Disclosure timeline]

### Credits

[Researcher acknowledgment]

### Contact

For questions: security@ggen.dev
```

---

## Roles and Responsibilities

### Incident Commander (IC)

**Responsibilities**:
- Coordinate response
- Make decisions
- Communicate status
- Escalate if needed

**On-call rotation**: Weekly

### Security Engineer

**Responsibilities**:
- Analyze vulnerability
- Develop patch
- Write tests
- Review code

### DevOps Engineer

**Responsibilities**:
- Deploy patches
- Monitor systems
- Preserve evidence
- Restore services

### Communications Lead

**Responsibilities**:
- Draft advisories
- Notify users
- Update documentation
- Manage public disclosure

---

## Incident Response Tools

### Runbooks

**Location**: `/docs/security/runbooks/`

- `path_traversal.md` - Path traversal response
- `sparql_injection.md` - SPARQL injection response
- `dos_attack.md` - Denial of service response
- `data_breach.md` - Data breach response

### Scripts

**Location**: `/scripts/incident_response/`

```bash
# Disable feature
./scripts/incident_response/disable_feature.sh --feature=templates

# Emergency rate limiting
./scripts/incident_response/emergency_ratelimit.sh --limit=10

# Preserve evidence
./scripts/incident_response/preserve_evidence.sh --incident=SEC-2026-001

# Notify users
./scripts/incident_response/notify_users.sh --severity=P1
```

### Checklists

**P0 Response Checklist**:
- [ ] Acknowledge within 15 minutes
- [ ] Activate incident response team
- [ ] Assess severity and impact
- [ ] Contain vulnerability (disable feature, rate limit)
- [ ] Preserve evidence (logs, receipts)
- [ ] Develop patch within 24 hours
- [ ] Test patch thoroughly
- [ ] Release emergency patch
- [ ] Notify all users
- [ ] Public disclosure after patch deployment
- [ ] Post-incident review within 1 week

---

## References

- [NIST Incident Response Guide](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-61r2.pdf)
- [SANS Incident Response Steps](https://www.sans.org/reading-room/whitepapers/incident/incident-handlers-handbook-33901)
- [Security Architecture](ARCHITECTURE.md)
- [Safe Coding Guidelines](SAFE_CODING.md)

---

**Last Updated**: 2026-01-24
**Next Review**: 2026-04-24
**On-Call**: See PagerDuty rotation
