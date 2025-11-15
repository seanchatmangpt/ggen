# Maturity Scoring Reference

Complete technical specification of the marketplace maturity matrix scoring system.

---

## Scoring Overview

**Total Score Range**: 0-100 points
**Number of Dimensions**: 6
**Number of Levels**: 4

### Score to Level Mapping

| Total Score | Level | Recommendation |
|------------|-------|-----------------|
| 0-40 | 🔴 **Experimental** | Research, prototyping only |
| 41-60 | 🟡 **Beta** | Testing, evaluation, non-critical systems |
| 61-80 | 🟢 **Production** | Live deployments, commercial use |
| 81-100 | 🟦 **Enterprise** | Mission-critical systems |

---

## Dimension Specifications

### 1. Documentation (0-20 points)

Evaluates completeness and quality of project documentation.

#### README (0-5 points)
**Score 5**: Comprehensive README with:
- Project description and purpose
- Installation instructions
- Quick-start example
- Link to full documentation
- License information

**Score 3**: Basic README with installation and examples

**Score 0**: Missing or incomplete README

#### API Documentation (0-5 points)
**Score 5**: Complete API documentation with:
- Parameter descriptions for all public functions
- Return type specifications
- Error handling documentation
- Example code snippets
- Generated from docstrings

**Score 3**: Partial API documentation

**Score 0**: No API documentation

#### Examples (0-5 points)
**Score 5**: Multiple runnable examples covering:
- Basic usage
- Common patterns
- Advanced features
- Error handling

**Score 3**: 1-2 basic examples

**Score 0**: No examples

#### Changelog (0-5 points)
**Score 5**: Detailed changelog with:
- Version history
- Breaking changes clearly marked
- Migration guides
- Feature descriptions

**Score 3**: Basic version log

**Score 0**: No changelog

**Total Documentation Score Calculation**:
```
documentation_score = readme + api_docs + examples + changelog
```

---

### 2. Testing (0-20 points)

Evaluates test coverage and testing practices.

#### Unit Tests (0-8 points)
Scoring based on test coverage percentage of codebase:

| Coverage | Points |
|----------|--------|
| 0% | 0 |
| 1-19% | 1 |
| 20-39% | 2 |
| 40-59% | 4 |
| 60-79% | 6 |
| 80-99% | 8 |
| 100% | 8 |

Requires `has_unit_tests = true`

#### Integration Tests (0-6 points)
**Score 6**: Has integration tests covering:
- Multiple components working together
- API boundaries
- System interactions
- External dependencies (when applicable)

**Score 0**: No integration tests

Requires `has_integration_tests = true`

#### End-to-End Tests (0-4 points)
**Score 4**: Has E2E tests covering:
- Complete user workflows
- Real-world scenarios
- Cross-component interactions

**Score 0**: No E2E tests

Requires `has_e2e_tests = true`

**Total Testing Score Calculation**:
```
testing_score = unit_tests + integration_tests + e2e_tests
```

Maximum: 8 + 6 + 4 = 18 possible (capped at 20 with coverage bonus)

---

### 3. Security (0-20 points)

Evaluates security posture and vulnerability management.

#### Vulnerability Scan (0-10 points)

| Known Vulnerabilities | Points |
|----------------------|--------|
| 0 | 10 |
| 1-2 | 8 |
| 3-5 | 6 |
| 6-10 | 4 |
| 11+ | 2 |

#### Dependency Audit (0-5 points)
**Score 5**: Regular dependency audits with:
- Automated scanning in CI/CD
- Documented vulnerability response process
- Dependency version pinning
- SLA for security patches

**Score 0**: No dependency auditing

Requires `has_dependency_audit = true`

#### Safe Code (0-5 points)

| Unsafe Code % | Points |
|---------------|--------|
| 0% | 5 |
| 0.01-1.99% | 4 |
| 2.00-4.99% | 3 |
| 5.00-9.99% | 2 |
| 10%+ | 1 |

**Total Security Score Calculation**:
```
security_score = vulnerability_scan + dependency_audit + safe_code
```

---

### 4. Performance (0-15 points)

Evaluates performance characteristics and optimization.

#### Benchmarks (0-8 points)
**Score 8**: Comprehensive benchmarks with:
- Performance baseline established
- Regression detection in CI
- Multiple scenarios tested
- Results tracked over time

**Score 4**: Basic benchmarks documented

**Score 0**: No benchmarks

Requires `has_benchmarks = true`

#### Optimization Documentation (0-4 points)
**Score 4**: Documentation covering:
- Performance characteristics
- Optimization recommendations
- Trade-offs explained
- Tuning parameters documented

**Score 0**: No optimization guidance

Requires `has_optimization_docs = true`

#### Determinism (0-3 points)
**Score 3**: Determinism verified with:
- Input-output guarantees tested
- Reproducibility confirmed
- Non-deterministic behavior documented

**Score 0**: Not verified

Requires `determinism_verified = true`

**Total Performance Score Calculation**:
```
performance_score = benchmarks + optimization + determinism
```

Maximum: 8 + 4 + 3 = 15

---

### 5. Adoption (0-15 points)

Evaluates real-world usage and community engagement.

#### Downloads (0-6 points)

| Total Downloads | Points |
|-----------------|--------|
| 0-10 | 0 |
| 11-50 | 1 |
| 51-100 | 2 |
| 101-500 | 3 |
| 501-1,000 | 4 |
| 1,001-5,000 | 5 |
| 5,000+ | 6 |

#### Academic Citations (0-5 points)

| Citations | Points |
|-----------|--------|
| 0 | 0 |
| 1-2 | 1 |
| 3-5 | 2 |
| 6-10 | 3 |
| 11-20 | 4 |
| 20+ | 5 |

#### Community (0-4 points)

| Active Contributors | Points |
|--------------------|--------|
| 0 | 0 |
| 1 | 1 |
| 2-3 | 2 |
| 4-5 | 3 |
| 6+ | 4 |

"Active" = contributed in last 90 days

**Total Adoption Score Calculation**:
```
adoption_score = downloads + citations + community
```

Maximum: 6 + 5 + 4 = 15

---

### 6. Maintenance (0-10 points)

Evaluates ongoing maintenance and responsiveness.

#### Release Cadence (0-5 points)

| Days Since Last Release | Points |
|------------------------|--------|
| 0-30 | 5 |
| 31-90 | 4 |
| 91-180 | 3 |
| 181-365 | 2 |
| 366-730 | 1 |
| 730+ | 0 |

#### Responsiveness (0-3 points)

| Avg Issue Response Time | Points |
|------------------------|--------|
| 0-24 hours | 3 |
| 24-48 hours | 2 |
| 48-72 hours | 1 |
| 72+ hours | 0 |

#### Active Maintenance (0-2 points)
**Score 2**: At least 1 active contributor (in last 90 days)
**Score 0**: No recent activity

**Total Maintenance Score Calculation**:
```
maintenance_score = release_cadence + responsiveness + active_maintenance
```

Maximum: 5 + 3 + 2 = 10

---

## Total Score Calculation

### Formula

```
total_score = documentation + testing + security + performance + adoption + maintenance
```

### Ranges

- **Minimum possible**: 0 (all zeros)
- **Maximum possible**: 100 (all perfect)

### Percentage Breakdown

```
documentation_pct = documentation / 20 * 100
testing_pct = testing / 20 * 100
security_pct = security / 20 * 100
performance_pct = performance / 15 * 100
adoption_pct = adoption / 15 * 100
maintenance_pct = maintenance / 10 * 100
```

---

## Level Assignment Logic

```python
def get_level(total_score: int) -> MaturityLevel:
    if total_score < 41:
        return MaturityLevel.Experimental
    elif total_score < 61:
        return MaturityLevel.Beta
    elif total_score < 81:
        return MaturityLevel.Production
    else:
        return MaturityLevel.Enterprise
```

---

## Scoring Examples

### Example 1: Mature Production Package

```
Documentation:  18/20  (90%)  - Excellent
Testing:        16/20  (80%)  - Good
Security:       19/20  (95%)  - Excellent
Performance:    12/15  (80%)  - Good
Adoption:       12/15  (80%)  - Good
Maintenance:    8/10   (80%)  - Good
─────────────────────────────
Total:          85/100         → Enterprise Level ✓
```

### Example 2: Growing Project

```
Documentation:  12/20  (60%)  - Fair
Testing:        10/20  (50%)  - Fair
Security:       14/20  (70%)  - Good
Performance:    8/15   (53%)  - Fair
Adoption:       8/15   (53%)  - Fair
Maintenance:    3/10   (30%)  - Poor
─────────────────────────────
Total:          55/100         → Beta Level
```

### Example 3: Experimental/Prototype

```
Documentation:  5/20   (25%)  - Poor
Testing:        3/20   (15%)  - Poor
Security:       8/20   (40%)  - Fair
Performance:    2/15   (13%)  - Poor
Adoption:       1/15   (7%)   - Poor
Maintenance:    0/10   (0%)   - Poor
─────────────────────────────
Total:          19/100         → Experimental Level
```

---

## Automatic Assessment Data Sources

Scoring automatically pulls from:

| Dimension | Data Source |
|-----------|-------------|
| Documentation | README.md, docs/, CODE_OF_CONDUCT |
| Testing | test coverage reports, CI logs |
| Security | dependency scanning, vulnerability databases |
| Performance | benchmark results, profiling data |
| Adoption | download metrics, citations, contributors |
| Maintenance | git commit history, issue response times |

---

## Audit Trail

Each score includes metadata:

```json
{
  "assessment_id": "aud_20251115_abc123",
  "package_id": "io.ggen.example",
  "timestamp": "2025-11-15T10:30:00Z",
  "scores": {...},
  "data_sources": {
    "documentation": "README.md (2025-11-10), docs/ (2025-11-12)",
    "testing": "Coverage report 85% (2025-11-15)",
    "security": "Dependency scan (2025-11-14), Vulnerabilities: 0",
    "performance": "Benchmark run (2025-11-10)",
    "adoption": "Downloads: 5234, Citations: 8, Contributors: 3",
    "maintenance": "Last release: 15 days ago, Response time: 18 hours"
  },
  "next_assessment": "2025-11-22T10:30:00Z"
}
```

---

## Version History

- **2.7.0** (2025-11-15): Initial matrix specification, 6-dimension system
- Future: Expected refinements based on real-world usage patterns
