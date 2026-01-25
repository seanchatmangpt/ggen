# Case Study: [Feature/System Name] - RDF-First Transformation

> **Template Instructions**: Replace all `[bracketed sections]` with actual content. Delete this instruction block when creating a real case study.

## Executive Summary

**Project**: [Project name and brief description]

**Timeline**: [Start date] to [End date] ([Duration])

**Team Size**: [Number] developers

**Outcome**: [1-2 sentence summary of results]

**Key Metrics**:
- [Metric 1]: [Before] → [After] ([% change])
- [Metric 2]: [Before] → [After] ([% change])
- [Metric 3]: [Before] → [After] ([% change])

---

## Table of Contents

1. [Background](#background)
2. [The Traditional Approach (Before)](#the-traditional-approach-before)
3. [Pain Points](#pain-points)
4. [The Transformation Process](#the-transformation-process)
5. [RDF-First Solution (After)](#rdf-first-solution-after)
6. [Implementation Details](#implementation-details)
7. [Lessons Learned](#lessons-learned)
8. [Metrics and Evidence](#metrics-and-evidence)
9. [Recommendations](#recommendations)

---

## Background

### Context

[Describe the project context, business domain, and why this feature/system was needed]

**Business Requirements**:
- [Requirement 1]
- [Requirement 2]
- [Requirement 3]

**Technical Constraints**:
- [Constraint 1]
- [Constraint 2]
- [Constraint 3]

**Stakeholders**:
- [Stakeholder 1]: [Role and expectations]
- [Stakeholder 2]: [Role and expectations]
- [Stakeholder 3]: [Role and expectations]

### Initial Approach

[Describe the initial approach taken - was it code-first? What methodology was used?]

---

## The Traditional Approach (Before)

### Requirements Gathering

**Method**: [How requirements were gathered - e.g., Word documents, Jira tickets, verbal discussions]

**Example Requirement**:
```
[Paste an example requirement in its original format]

Example:
"Users should be able to log in with their email and password.
The system should validate credentials and return a session token.
Sessions should expire after 60 minutes of inactivity."
```

**Problems with Format**:
- [Problem 1, e.g., "No formal structure"]
- [Problem 2, e.g., "Missing edge cases"]
- [Problem 3, e.g., "Ambiguous language"]

### Implementation Process

**Workflow**:
```
[Describe the traditional workflow]

Example:
1. PM writes requirement in Jira
2. Engineer reads Jira ticket
3. Engineer interprets requirement (subjective)
4. Engineer writes code based on interpretation
5. Engineer writes tests (may not match requirement)
6. Engineer updates docs (if time permits)
7. Code review (reviewer may interpret differently)
8. Merge and hope for the best
```

**Code Example (Traditional)**:
```[language]
[Paste representative code snippet from traditional approach]

Example (Python):
def login(email: str, password: str) -> dict:
    # Implementation based on engineer's interpretation
    user = User.find_by_email(email)
    if user and user.check_password(password):
        token = generate_token(user)
        return {"token": token, "expires_in": 3600}
    raise AuthenticationError("Invalid credentials")
```

**Test Example (Traditional)**:
```[language]
[Paste representative test from traditional approach]

Example (Python):
def test_login():
    response = login("test@example.com", "password123")
    assert "token" in response
    # Test passes, but does it match the requirement?
```

**Documentation (Traditional)**:
```markdown
[Paste representative documentation]

Example:
# Authentication

Users can log in with email and password.

## Endpoints

POST /auth/login

Request:
{
  "email": "string",
  "password": "string"
}

Response:
{
  "token": "string"
}
```

---

## Pain Points

### 1. [Pain Point Name, e.g., "Spec/Code Drift"]

**Description**: [Detailed description of the pain point]

**Example Incident**:
```
[Describe a specific incident that illustrates this pain point]

Example:
"On June 15, 2025, a customer reported that sessions were expiring
after 30 minutes, not 60 minutes as documented. Investigation revealed:
- Requirement doc said 60 minutes
- Code implementation used 30 minutes
- API docs said 60 minutes
- Tests checked for 30 minutes

The code had been changed months ago without updating requirements or docs."
```

**Impact**:
- [Impact 1, e.g., "Customer trust damaged"]
- [Impact 2, e.g., "2 days engineer time to investigate"]
- [Impact 3, e.g., "Emergency patch required"]

**Frequency**: [How often this occurred, e.g., "3-4 times per quarter"]

### 2. [Pain Point Name]

[Repeat structure for each pain point - aim for 3-5 total pain points]

**Description**: [...]

**Example Incident**: [...]

**Impact**: [...]

**Frequency**: [...]

### 3. [Pain Point Name]

[...]

---

## The Transformation Process

### Decision to Adopt RDF-First

**Trigger**: [What prompted the decision to try RDF-first?]

**Champions**: [Who advocated for the change?]

**Resistance**: [What concerns were raised?]

**Pilot Scope**: [What was chosen for the pilot project?]

### Phase 1: Learning and Setup ([Duration])

**Activities**:
- [Activity 1, e.g., "Team read EVENT_HORIZON.md guide"]
- [Activity 2, e.g., "Attended ggen workshop"]
- [Activity 3, e.g., "Set up ggen tooling in CI/CD"]

**Challenges**:
- [Challenge 1]
- [Challenge 2]

**Outcome**: [What was achieved by end of Phase 1]

### Phase 2: First Ontology ([Duration])

**Feature Selected**: [Which feature was converted to RDF-first]

**Ontology Design**:
```turtle
[Paste excerpt of the first ontology created]

Example:
@prefix sk: <http://github.com/github/spec-kit#> .
@prefix : <http://example.com/auth#> .

:LoginFeature a sk:Feature ;
    sk:featureName "User Authentication" ;
    sk:hasUserStory :us-001, :us-002 .

:us-001 a sk:UserStory ;
    sk:storyIndex 1 ;
    sk:title "User can log in with email and password" ;
    sk:priority "P1" ;
    sk:description "As a user, I want to log in with my email and password so that I can access my account" ;
    sk:hasAcceptanceScenario :us-001-as-001, :us-001-as-002 .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:scenarioIndex 1 ;
    sk:given "User has email 'test@example.com' with password 'Secret123'" ;
    sk:when "User submits login form with valid credentials" ;
    sk:then "System returns status 200 and JWT token with 60 minute expiry" .

:us-001-as-002 a sk:AcceptanceScenario ;
    sk:scenarioIndex 2 ;
    sk:given "User has been logged in for 60 minutes" ;
    sk:when "User attempts to access protected resource" ;
    sk:then "System returns status 401 Unauthorized" .
```

**Challenges**:
- [Challenge 1, e.g., "Learning Turtle syntax"]
- [Challenge 2, e.g., "Designing SHACL shapes"]

**Outcome**: [What was achieved - e.g., "First ontology validated and committed"]

### Phase 3: Template Creation ([Duration])

**Templates Developed**:
- [Template 1]: [Purpose]
- [Template 2]: [Purpose]

**Example Template**:
```tera
[Paste excerpt of a Tera template]

Example:
{# Generate Rust authentication handler #}
{% for scenario in acceptance_scenarios %}
#[test]
fn test_{{ scenario.id | snake_case }}() {
    // {{ scenario.given }}
    {{ scenario.given | generate_test_setup }}

    // {{ scenario.when }}
    let response = {{ scenario.when | generate_action }};

    // {{ scenario.then }}
    {{ scenario.then | generate_assertion }}
}
{% endfor %}
```

**Challenges**:
- [Challenge 1]
- [Challenge 2]

**Outcome**: [What was achieved]

### Phase 4: First Generation ([Duration])

**Command Used**:
```bash
ggen sync --audit true
```

**Artifacts Generated**:
- [Artifact 1]: [File path and description]
- [Artifact 2]: [File path and description]
- [Artifact 3]: [File path and description]

**Receipt Evidence**:
```json
[Paste excerpt of receipt JSON]

Example:
{
  "execution_id": "20260115T143022Z-a3f8c9d1",
  "manifest_hash": "sha256:abc123...",
  "files_generated": [
    {
      "path": "src/auth/login.rs",
      "hash": "sha256:def456...",
      "size_bytes": 2048
    }
  ],
  "timings": {
    "total": "1342μs"
  }
}
```

**Challenges**:
- [Challenge 1]
- [Challenge 2]

**Outcome**: [What was achieved]

### Phase 5: Validation and Iteration ([Duration])

**Validation Activities**:
- [Activity 1, e.g., "Ran cargo make test"]
- [Activity 2, e.g., "Manual QA testing"]
- [Activity 3, e.g., "Stakeholder review"]

**Issues Found**:
- [Issue 1]: [How it was resolved]
- [Issue 2]: [How it was resolved]

**Iterations**:
- **Iteration 1**: [What was changed in ontology and why]
- **Iteration 2**: [What was changed in ontology and why]

**Outcome**: [What was achieved - e.g., "Feature ready for production"]

### Phase 6: Team Rollout ([Duration])

**Training**:
- [Training activity 1]
- [Training activity 2]

**Adoption**:
- [Number] developers trained
- [Number] features converted to RDF-first
- [Number] weeks to full adoption

**Challenges**:
- [Challenge 1]
- [Challenge 2]

**Outcome**: [What was achieved - e.g., "Entire team using RDF-first"]

---

## RDF-First Solution (After)

### Ontology-Driven Requirements

**Format**: Turtle/RDF ontology

**Example Ontology**:
```turtle
[Paste complete ontology for the feature]

Example:
@prefix sk: <http://github.com/github/spec-kit#> .
@prefix : <http://example.com/auth#> .

:LoginFeature a sk:Feature ;
    sk:featureName "User Authentication" ;
    sk:featureBranch "feature/authentication" ;
    sk:status "production" ;
    sk:hasUserStory :us-001, :us-002, :us-003 .

# User Story 1: Login with email/password
:us-001 a sk:UserStory ;
    sk:storyIndex 1 ;
    sk:title "User can log in with email and password" ;
    sk:priority "P1" ;
    sk:description """As a user, I want to log in with my email and password
                      so that I can access my account securely.""" ;
    sk:priorityRationale "Core authentication mechanism required for MVP" ;
    sk:independentTest "User can complete full login workflow end-to-end" ;
    sk:hasAcceptanceScenario :us-001-as-001, :us-001-as-002, :us-001-as-003 .

# Acceptance Scenarios for US-001
:us-001-as-001 a sk:AcceptanceScenario ;
    sk:scenarioIndex 1 ;
    sk:given "User has email 'test@example.com' with password 'Secret123'" ;
    sk:when "User submits login form with valid credentials" ;
    sk:then "System returns HTTP 200 with JWT token and 60 minute expiry" .

:us-001-as-002 a sk:AcceptanceScenario ;
    sk:scenarioIndex 2 ;
    sk:given "User has email 'test@example.com' with password 'WrongPassword'" ;
    sk:when "User submits login form with invalid password" ;
    sk:then "System returns HTTP 401 with error message 'Invalid credentials'" .

:us-001-as-003 a sk:AcceptanceScenario ;
    sk:scenarioIndex 3 ;
    sk:given "User has been logged in for 60 minutes" ;
    sk:when "User attempts to access protected resource /api/profile" ;
    sk:then "System returns HTTP 401 with error message 'Session expired'" .

# [Additional user stories...]
```

**Benefits of RDF Format**:
- [Benefit 1, e.g., "Machine-readable via SPARQL"]
- [Benefit 2, e.g., "SHACL validation catches errors"]
- [Benefit 3, e.g., "Formal semantics prevent ambiguity"]

### Generated Artifacts

**Code (Rust)**:
```rust
[Paste generated Rust code]

Example:
// Generated from ontology: :us-001
// DO NOT EDIT - Regenerate with `ggen sync`

use serde::{Deserialize, Serialize};
use crate::error::AuthError;

#[derive(Debug, Deserialize)]
pub struct LoginRequest {
    pub email: String,
    pub password: String,
}

#[derive(Debug, Serialize)]
pub struct LoginResponse {
    pub token: String,
    pub expires_in: u32,
}

pub async fn login(req: LoginRequest) -> Result<LoginResponse, AuthError> {
    // Generated from :us-001-as-001 and :us-001-as-002
    let user = User::find_by_email(&req.email)
        .await?
        .ok_or(AuthError::InvalidCredentials)?;

    if !user.verify_password(&req.password)? {
        return Err(AuthError::InvalidCredentials);
    }

    let token = generate_jwt(&user, Duration::from_secs(3600))?;

    Ok(LoginResponse {
        token,
        expires_in: 3600, // Generated from :us-001-as-001 (60 minutes)
    })
}
```

**Tests (Rust)**:
```rust
[Paste generated tests]

Example:
// Generated from ontology acceptance scenarios
// DO NOT EDIT - Regenerate with `ggen sync`

#[cfg(test)]
mod tests {
    use super::*;

    // Generated from :us-001-as-001
    #[tokio::test]
    async fn test_us_001_as_001_login_with_valid_credentials() {
        // Arrange: "User has email 'test@example.com' with password 'Secret123'"
        let user = create_test_user("test@example.com", "Secret123").await;
        let req = LoginRequest {
            email: "test@example.com".to_string(),
            password: "Secret123".to_string(),
        };

        // Act: "User submits login form with valid credentials"
        let response = login(req).await.unwrap();

        // Assert: "System returns HTTP 200 with JWT token and 60 minute expiry"
        assert!(response.token.len() > 0);
        assert_eq!(response.expires_in, 3600); // 60 minutes
    }

    // Generated from :us-001-as-002
    #[tokio::test]
    async fn test_us_001_as_002_login_with_invalid_password() {
        // Arrange: "User has email 'test@example.com' with password 'WrongPassword'"
        let user = create_test_user("test@example.com", "Secret123").await;
        let req = LoginRequest {
            email: "test@example.com".to_string(),
            password: "WrongPassword".to_string(),
        };

        // Act: "User submits login form with invalid password"
        let result = login(req).await;

        // Assert: "System returns HTTP 401 with error message 'Invalid credentials'"
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), AuthError::InvalidCredentials);
    }

    // Generated from :us-001-as-003
    #[tokio::test]
    async fn test_us_001_as_003_session_expires_after_60_minutes() {
        // Arrange: "User has been logged in for 60 minutes"
        let token = create_expired_token(Duration::from_secs(3600)).await;

        // Act: "User attempts to access protected resource /api/profile"
        let result = verify_token(&token).await;

        // Assert: "System returns HTTP 401 with error message 'Session expired'"
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), AuthError::SessionExpired);
    }
}
```

**Documentation (Markdown)**:
```markdown
[Paste generated documentation]

Example:
# User Authentication

Generated from ontology: `auth.ttl`
Last updated: 2026-01-15T14:30:22Z

## Feature Overview

User Authentication enables users to securely log in and access protected resources.

## User Stories

### US-001: User can log in with email and password

**Priority**: P1 (Critical for MVP launch)

**Description**: As a user, I want to log in with my email and password so that I can access my account securely.

**Acceptance Scenarios**:

#### Scenario 1: Login with valid credentials
- **Given**: User has email 'test@example.com' with password 'Secret123'
- **When**: User submits login form with valid credentials
- **Then**: System returns HTTP 200 with JWT token and 60 minute expiry

#### Scenario 2: Login with invalid password
- **Given**: User has email 'test@example.com' with password 'WrongPassword'
- **When**: User submits login form with invalid password
- **Then**: System returns HTTP 401 with error message 'Invalid credentials'

#### Scenario 3: Session expires after 60 minutes
- **Given**: User has been logged in for 60 minutes
- **When**: User attempts to access protected resource /api/profile
- **Then**: System returns HTTP 401 with error message 'Session expired'

[...]
```

### Workflow Comparison

**Before (Code-First)**:
```
PM writes Jira ticket (30 min)
  ↓
Engineer reads and interprets (15 min)
  ↓
Engineer writes code (2 hours)
  ↓
Engineer writes tests (1 hour)
  ↓
Engineer updates docs (30 min if time)
  ↓
Code review (30 min)
  ↓
Fix review comments (30 min)
  ↓
Merge
  ↓
TOTAL: ~5 hours, high drift risk
```

**After (RDF-First)**:
```
PM/Engineer writes ontology (1 hour)
  ↓
SHACL validation (automated, <1 second)
  ↓
ggen sync (automated, <5 seconds)
  ↓
Code + Tests + Docs generated
  ↓
Review ontology only (20 min)
  ↓
Merge
  ↓
TOTAL: ~1.5 hours, zero drift
```

**Time Savings**: 70% reduction (5 hours → 1.5 hours)

---

## Implementation Details

### Ontology Structure

[Describe how the ontology is organized - which .ttl files, what each contains]

**Files**:
- `.specify/specs/[NNN]-[feature]/feature.ttl`: [Description]
- `.specify/specs/[NNN]-[feature]/entities.ttl`: [Description]
- `.specify/specs/[NNN]-[feature]/plan.ttl`: [Description]

**SHACL Shapes**:
```turtle
[Paste relevant SHACL shapes that validate the ontology]

Example:
:UserStoryShape a sh:NodeShape ;
    sh:targetClass sk:UserStory ;
    sh:property [
        sh:path sk:priority ;
        sh:in ("P1" "P2" "P3") ;
        sh:message "Priority must be exactly P1, P2, or P3" ;
    ] ;
    sh:property [
        sh:path sk:hasAcceptanceScenario ;
        sh:minCount 1 ;
        sh:message "User story must have at least one acceptance scenario" ;
    ] .
```

### Template Design

[Describe the Tera templates created and their purpose]

**Template: [name].tera**
- **Purpose**: [What it generates]
- **Input**: [SPARQL query results]
- **Output**: [File type and location]

**Example Excerpt**:
```tera
[Paste key sections of the template]
```

### CI/CD Integration

[Describe how RDF-first was integrated into CI/CD pipeline]

**Pipeline Steps**:
```yaml
[Paste relevant CI/CD configuration]

Example (.github/workflows/validate.yml):
name: Validate Ontology

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install ggen
        run: cargo install ggen
      - name: Validate ontology
        run: ggen sync --validate_only true
      - name: Generate artifacts
        run: ggen sync --audit true
      - name: Upload receipts
        uses: actions/upload-artifact@v2
        with:
          name: generation-receipts
          path: .ggen/receipts/
```

---

## Lessons Learned

### What Went Well

**1. [Success 1]**

[Describe what worked well and why]

**Evidence**: [Metrics, quotes, or specific examples]

**2. [Success 2]**

[...]

**3. [Success 3]**

[...]

### What Was Challenging

**1. [Challenge 1]**

**Description**: [What was difficult]

**Root Cause**: [Why it was difficult]

**Resolution**: [How it was overcome]

**Lesson**: [What was learned for next time]

**2. [Challenge 2]**

[...]

**3. [Challenge 3]**

[...]

### What We Would Do Differently

**1. [Improvement 1]**

[Describe what you would do differently knowing what you know now]

**2. [Improvement 2]**

[...]

**3. [Improvement 3]**

[...]

### Key Insights

**1. [Insight 1]**

[Describe a key realization or "aha moment"]

**2. [Insight 2]**

[...]

**3. [Insight 3]**

[...]

---

## Metrics and Evidence

### Development Velocity

| Metric | Before (Code-First) | After (RDF-First) | Improvement |
|--------|---------------------|-------------------|-------------|
| Time to implement feature | [X hours] | [Y hours] | [Z%] faster |
| Time to write tests | [X hours] | [Y hours] (generated) | [Z%] faster |
| Time to update docs | [X hours] | [Y hours] (generated) | [Z%] faster |
| Code review time | [X min] | [Y min] | [Z%] faster |
| **Total feature time** | **[X hours]** | **[Y hours]** | **[Z%] faster** |

### Quality Metrics

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Spec/code drift incidents | [X per month] | [Y per month] | [Z%] reduction |
| Bugs related to unclear requirements | [X per month] | [Y per month] | [Z%] reduction |
| Documentation accuracy | [X%] | [Y%] | [+Z%] |
| Test coverage | [X%] | [Y%] | [+Z%] |

### Team Metrics

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Onboarding time (days to first commit) | [X days] | [Y days] | [Z%] faster |
| Knowledge silos (features only one person understands) | [X features] | [Y features] | [Z%] reduction |
| Time to understand feature (query vs read code) | [X min] | [Y min] | [Z%] faster |

### Evidence

**Receipt Example**:
```json
[Paste actual receipt from production use]
```

**SPARQL Query Example**:
```sparql
[Paste actual SPARQL query used to debug or understand system]

Example:
# Find all high-priority user stories related to authentication
PREFIX sk: <http://github.com/github/spec-kit#>

SELECT ?title ?scenarios
WHERE {
    ?story a sk:UserStory ;
           sk:title ?title ;
           sk:priority "P1" ;
           sk:relatedTo :authentication .

    {
        SELECT ?story (COUNT(?scenario) as ?scenarios)
        WHERE {
            ?story sk:hasAcceptanceScenario ?scenario .
        }
        GROUP BY ?story
    }
}
ORDER BY DESC(?scenarios)
```

**Testimonials**:
> "[Quote from team member about their experience]"
> — [Name], [Role]

> "[Another quote]"
> — [Name], [Role]

---

## Recommendations

### For Teams Considering RDF-First

**1. [Recommendation 1]**

[Detailed recommendation based on your experience]

**2. [Recommendation 2]**

[...]

**3. [Recommendation 3]**

[...]

### For Teams Already Using RDF-First

**1. [Recommendation 1]**

[Recommendations for advanced usage]

**2. [Recommendation 2]**

[...]

**3. [Recommendation 3]**

[...]

### Red Flags to Watch For

**1. [Red Flag 1]**

[Warning sign that indicates a problem]

**How to Address**: [Solution]

**2. [Red Flag 2]**

[...]

**3. [Red Flag 3]**

[...]

---

## Conclusion

[Summarize the case study - what was the journey, what was learned, what is the current state]

**Key Takeaways**:
1. [Takeaway 1]
2. [Takeaway 2]
3. [Takeaway 3]

**Would we do it again?**: [Yes/No and why]

**Advice for others**: [Final piece of advice]

---

## Appendix

### Ontology Files

**Location**: [Path to ontology files in repository]

**Key Files**:
- [File 1]: [Link and description]
- [File 2]: [Link and description]

### Generated Artifacts

**Location**: [Path to generated artifacts]

**Key Files**:
- [File 1]: [Link and description]
- [File 2]: [Link and description]

### Receipts

**Location**: `.ggen/receipts/`

**Sample Receipt**: [Link to representative receipt file]

### References

- [Reference 1]: [Link and description]
- [Reference 2]: [Link and description]
- [Reference 3]: [Link and description]

---

**Document Status**: [Draft/Review/Published]
**Case Study Date**: [Date of case study creation]
**Authors**: [Names and roles]
**Last Updated**: [Date]
**Version**: [Version number]
