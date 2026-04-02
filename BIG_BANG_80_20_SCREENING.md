# BIG BANG 80/20 Screening Gate (ggen v6)

**Purpose**: Prevent Seth/David-like execution failures by enforcing specification closure and forcing real execution discipline before users can initialize a ggen project.

## The Problem: Seth's Three-Month Detour

Sean asked: **"Can you find me one ontology link?"** (5-minute task: Google "schema.org")

Seth's response: Build a 100-page custom ontology (3 months, zero market validation)

**Why this matters**: Seth failed the execution litmus test. He couldn't:
- Find an existing solution (used Google to search for 3 minutes instead of building)
- Accept "good enough" (custom > standard)
- Execute on simple requests (5-minute task → 3-month detour)
- Validate with real users (built in isolation)

ggen v6 screens for this pattern **before project initialization**.

## The Solution: Five Screening Questions

Before `ggen init`, users must answer yes to 5 questions:

### Question 1: Real User Data
**"Do you have real user data (CSV/JSON)?"**
- Not promised. Actual files.
- If building a feature: Do you have beta users' data?
- **Why**: Prevents narratives in place of artifacts

### Question 2: Standard Ontology
**"Can you find ONE existing standard ontology for your domain?"**
- schema.org, FOAF, Dublin Core, SKOS - should take 5 minutes
- If it takes 3 months, you're building custom (Seth's mistake)
- **Why**: Enforces "use before building" discipline

### Question 3: Problem Articulation
**"Can you explain your problem in ONE sentence?"**
- No 100-page documents
- Just the core job-to-be-done
- **Why**: Clarity forces ruthless focus

### Question 4: Market Signal
**"Has anyone (not friends, not co-founders) committed to this?"**
- Email list, signed beta contract, payment
- PROOF, not enthusiasm
- **Why**: External validation forces reality-checking

### Question 5: Validation Speed
**"Can you validate with 10 real users in 48 hours?"**
- How will you know if it works?
- If no: What's your validation plan?
- **Why**: Forces speed and user-driven iteration

## Screening Gate Implementation

### File: `startup.sh` (Interactive Gate)

```bash
#!/bin/bash
# Implements BIG BANG 80/20 screening before project initialization

echo "🚀 ggen v6: BIG BANG 80/20 Screening Gate"
echo "Before initializing, you must answer 5 questions about execution readiness."
echo "If you answer NO to any, stop and talk to Sean."

# Q1: Real User Data
read -r q1
if [[ "$q1" != "yes" ]]; then
    echo "❌ STOP. You need real data to validate with. Build MVP first, use ggen after."
    exit 1
fi

# Q2: Standard Ontology
read -r q2
if [[ "$q2" != "yes" ]]; then
    echo "❌ STOP. You're about to build a custom ontology (Seth's mistake)."
    exit 1
fi

# ... (Q3, Q4, Q5)
```

### File: `ggen.toml` (Configuration Gate)

```toml
[ontology]
# REQUIRED: Use standard ontologies only (BIG BANG 80/20 gate)
# Approved: schema.org, FOAF, Dublin Core, SKOS, Big Five
standard_only = true

# Example: Using schema.org for e-commerce domain
# [[ontology.pack]]
# name = "schema-org"
# version = "^3.13.0"
# namespace = "https://schema.org/"
```

Comments in `ggen.toml` list the 5 screening questions and remind users:
> "If you answered NO to any of these, stop. Talk to Sean before proceeding."

### File: `domain.ttl` (Standard-First Ontology)

Uses `schema.org` instead of custom namespace:

```turtle
@prefix schema: <https://schema.org/> .

# Example: Using schema.org Person and properties
# This is a real, standard vocabulary used by Google, Microsoft, Yahoo, Yandex
schema:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "Schema.org Person type - standard vocabulary for person data" .

# NEXT STEPS:
# 1. Load your actual CSV/JSON user data
# 2. Validate with 10 real users (not friends)
# 3. Only extend schema.org if needed (stay standard-first)
```

Embedded comment references Seth's failure:
> "Why? Because Seth built 3-month custom ontology for what schema.org does in 5 minutes."

## Standard Ontologies Module

### File: `validation/standard_ontologies.rs` (New)

**StandardOntology enum** - Approved ontologies:
- **schema.org** (v3.13+) - Largest vocabulary for web structured data
- **FOAF** - Social networks, personal profiles
- **Dublin Core** (v1.1) - Metadata and document properties
- **SKOS** - Thesauri, controlled vocabularies
- **Big Five Traits** - Personality modeling (OCEAN framework)

**StandardOntologyValidator** - Validation logic:
```rust
pub fn validate_config_ontologies(
    ontology_namespaces: &[String],
) -> Result<Vec<StandardOntology>> {
    // Rejects custom ontologies
    // Returns error with Seth-focused message:
    // "Custom ontologies delay execution. Seth built a 100-page custom
    //  ontology instead of using schema.org in 5 minutes. Use standards first."
}
```

**OntologyScreeningConfig** - Two modes:
- `big_bang_80_20()` - Maximum strictness (no custom ontologies)
- `permissive()` - Allow custom if user has execution history

## How This Prevents Seth/David Failures

| Pattern | Screening Question | Gate | Prevents |
|---------|-------------------|------|----------|
| **Seth's 3-month custom ontology** | Q2: Can you find standard? | Rejects custom namespaces | Wasting months building what schema.org does in 5 min |
| **Seth's zero market validation** | Q4: Market signal? Q5: User validation? | Requires proof + plan | Building in isolation; discovering no one cares |
| **Seth's vague problem statement** | Q3: One sentence? | Requires clarity | 100-page documents hiding unclear thinking |
| **Seth/David's narrative-only progress** | Q1: Real data? | Requires artifacts | Substituting promises for evidence |
| **David's outdated startup questions** | All 5 questions | Forces execution discipline | Using 2015 frameworks in 2026 post-human context |

## User Flow: Before and After

### Without Screening (Seth's Path)
```
1. ggen init
2. Create custom ontology (3 months)
3. Build code generators (2 months)
4. Discover no users want it (too late)
5. Blame "communication" (narrative, not analysis)
```

### With Screening (ggen v6 Path)
```
1. User runs: ggen init
2. Screen: "Do you have real user data?" NO → STOP. Build MVP first.
3. User builds MVP with 10 beta users
4. User runs: ggen init (now has real data)
5. Screen: "Can you find standard ontology?" NO → STOP. Research 1 hour.
6. User chooses schema.org (5 min)
7. Load CSV data → ggen sync → Generate code
8. Validate with 10 real users (48 hours)
9. Iterate or ship
```

**Result**: 1-2 weeks instead of 3+ months. Execution-driven, not research-driven.

## Speed Targets

```
Data upload:         1 hour
Ontology selection:  1 hour (use standard, don't build custom)
Template creation:   2-4 hours
First user validation: 24 hours
Total to MVP:        2-4 days (vs 3+ months)
```

## Configuration Examples

### Example 1: E-commerce with schema.org

```toml
[ontology]
standard_only = true

[[ontology.pack]]
name = "schema-org"
version = "^3.13.0"
namespace = "https://schema.org/"
```

### Example 2: Social Network with FOAF

```toml
[ontology]
standard_only = true

[[ontology.pack]]
name = "foaf"
version = "~0.1.0"
namespace = "http://xmlns.com/foaf/0.1/"
```

### Example 3: Metadata with Dublin Core + schema.org

```toml
[ontology]
standard_only = true

[[ontology.pack]]
name = "dublin-core"
version = "^1.1.0"
namespace = "http://purl.org/dc/elements/1.1/"

[[ontology.pack]]
name = "schema-org"
version = "^3.13.0"
namespace = "https://schema.org/"
```

## Error Messages

When user tries to use custom ontology:

```
Error: Ontology 'https://custom.example.com/ontology#' is not standard-approved.

Approved options:
  • schema.org (https://schema.org/)
  • FOAF (http://xmlns.com/foaf/0.1/)
  • Dublin Core (http://purl.org/dc/elements/1.1/)
  • SKOS (http://www.w3.org/2004/02/skos/core#)
  • Big Five Traits (https://ggen.io/ontologies/big-five#)

Why? Custom ontologies delay execution. Seth built a 100-page custom
ontology instead of using schema.org in 5 minutes. Use standards first,
prove market fit, then extend if needed.
```

## Commit

```
feat: Implement BIG BANG 80/20 ontology screening gate to prevent Seth patterns

Add standard ontology validation module that enforces specification closure by:
1. Rejecting custom ontologies (force schema.org, FOAF, Dublin Core, SKOS, Big Five)
2. Requiring real user data before project initialization
3. Screening for execution capability via 5 litmus test questions
4. Documenting why Seth's 3-month custom ontology was wrong path

This is ggen-paas v6 implementation: force discipline at initialization.
```

**Commit Hash**: `6d4eccf` (See git history for full diff)

## Next Steps (v6 → v7)

These are ggen-paas features (v7), not v6:

1. **Auto-inference** (`ggen infer`): Derive ontology from CSV data, not hand-write
2. **User data upload**: Web UI to upload CSV/JSON with validation
3. **Template marketplace**: Browse + install vetted templates (not build custom)
4. **Validation runner**: 48-hour validation sprint with real users
5. **Market feedback loop**: Collect email commitments before shipping

For v6: **Screening gate is sufficient to prevent Seth patterns.**

## References

- `CONVERSATION_ANALYSIS.md` - Full PlantUML analysis of Seth/David/Sean conversation
- `GENAI_STUDY.md` - Genai package integration for actual LLM calls
- `CLAUDE.md` - ggen Constitutional Rules and BIG BANG 80/20 principle
