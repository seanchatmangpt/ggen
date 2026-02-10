# The Fundamental Regime Split: SELECT/DO vs CONSTRUCT

**Version**: 1.0.0
**Last Updated**: 2026-02-09
**Status**: Canonical Reference

## Overview

Software development operates under two fundamentally incompatible paradigms, which we term **regimes**. Understanding this split is critical to manufacturing-grade code generation. These regimes cannot be mixed without catastrophic consequences to determinism, reproducibility, and verification.

```
SELECT/DO Regime          CONSTRUCT Regime
─────────────────────────────────────────────
Negotiation            ←→  Closed Ontology
Discretion             ←→  Determinism
Dialogue               ←→  Declarations
Hedging                ←→  Proofs
Gradualism             ←→  Atomicity
```

## The SELECT/DO Regime

### Core Characteristics

The SELECT/DO regime is the dominant paradigm in modern software development. It is characterized by:

1. **Negotiation**: Requirements emerge through dialogue
2. **Discretion**: Implementation details are left to developer judgment
3. **Interpretive Dialogue**: Ambiguity is resolved through conversation
4. **Hedging**: Uncertainty is managed through approximation and iteration
5. **Gradualism**: Progress is incremental and partial

### Code Example: SELECT/DO in Action

```rust
// Traditional development process (SELECT/DO)
struct UserRequest {
    description: String,  // Ambiguous natural language
    priority: Option<Priority>,  // Negotiable
    deadline: Option<DateTime>,  // Flexible
}

impl UserRequest {
    fn interpret(&self) -> Result<Implementation, NeedsMoreInfo> {
        // Developer discretion applied here
        match self.priority {
            Some(Priority::High) => {
                // Maybe we should optimize?
                // Perhaps we need to discuss trade-offs?
                self.negotiate_approach()
            }
            None => {
                // What did they really mean?
                Err(NeedsMoreInfo::PriorityUnclear)
            }
        }
    }

    fn negotiate_approach(&self) -> Result<Implementation, NeedsMoreInfo> {
        // Interpretive dialogue occurs here
        // Multiple iterations expected
        // Partial solutions acceptable
        todo!("Will discuss with stakeholders")
    }
}
```

### Real-World SELECT/DO Pattern

```bash
# Typical development cycle (SELECT/DO)
$ git commit -m "WIP: initial implementation"
$ git commit -m "fixes based on review comments"
$ git commit -m "addressing edge cases"
$ git commit -m "performance improvements"
$ git commit -m "final cleanup"

# Each commit is a negotiation checkpoint
# Requirements clarify through dialogue
# Partial progress is the norm
```

### When SELECT/DO Is Appropriate

- **Exploratory development**: Problem space is unknown
- **Human-centric systems**: Requirements emerge from user feedback
- **Creative work**: Solutions require human intuition
- **Prototype phase**: Speed of learning exceeds need for rigor

### SELECT/DO Trade-offs

**Benefits**:
- Flexible adaptation to changing requirements
- Human creativity and judgment applied
- Natural for human cognitive processes
- Handles ambiguity gracefully

**Costs**:
- Non-deterministic outcomes
- Difficult to reproduce
- No proof of correctness
- Cannot be automated reliably
- High communication overhead

## The CONSTRUCT Regime

### Core Characteristics

The CONSTRUCT regime is the manufacturing paradigm. It is characterized by:

1. **Closed Ontology**: All valid states are enumerable and type-checkable
2. **Refusal Determinism**: Invalid inputs are rejected deterministically
3. **No Partials**: Operations are atomic (complete or nothing)
4. **Proofs/Receipts**: Every operation produces cryptographic evidence
5. **Replay**: Perfect reproducibility from specifications

### Code Example: CONSTRUCT in Action

```rust
// Specification-driven generation (CONSTRUCT)
use serde::{Deserialize, Serialize};
use sha2::{Sha256, Digest};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ClosedSpec {
    // Every field has exactly one valid interpretation
    target_type: TargetType,  // Enum with finite cases
    constraints: Vec<Constraint>,  // All constraints type-checked
    schema_hash: Hash,  // Cryptographic binding
}

#[derive(Debug, Clone, Copy)]
enum TargetType {
    RustStruct,
    RustEnum,
    RustTrait,
    // Closed set - no "Other" case
}

impl ClosedSpec {
    fn validate(&self) -> Result<(), SpecificationError> {
        // Deterministic validation
        // No human judgment required
        // Same input = same output, always

        if self.schema_hash != self.compute_hash() {
            return Err(SpecificationError::HashMismatch);
        }

        for constraint in &self.constraints {
            constraint.check()?;  // Deterministic check
        }

        Ok(())
    }

    fn compute_hash(&self) -> Hash {
        let mut hasher = Sha256::new();
        hasher.update(serde_json::to_vec(&self.target_type).unwrap());
        hasher.update(serde_json::to_vec(&self.constraints).unwrap());
        Hash(hasher.finalize().into())
    }

    fn construct(&self) -> Result<GeneratedCode, ConstructError> {
        // Atomic operation: complete or nothing
        // No partial generation
        // No "we'll fix it later"

        self.validate()?;

        let code = match self.target_type {
            TargetType::RustStruct => self.construct_struct()?,
            TargetType::RustEnum => self.construct_enum()?,
            TargetType::RustTrait => self.construct_trait()?,
        };

        // Generate cryptographic receipt
        let receipt = Receipt {
            spec_hash: self.schema_hash,
            code_hash: code.hash(),
            timestamp: SystemTime::now(),
            generator_version: VERSION,
        };

        Ok(GeneratedCode { code, receipt })
    }

    fn construct_struct(&self) -> Result<String, ConstructError> {
        // Every decision is deterministic
        // No human discretion
        // Reproducible across time and space
        todo!("Deterministic code generation")
    }
}

#[derive(Debug, Clone, Serialize)]
struct Receipt {
    spec_hash: Hash,
    code_hash: Hash,
    timestamp: SystemTime,
    generator_version: &'static str,
}

impl Receipt {
    fn verify(&self, spec: &ClosedSpec, code: &str) -> bool {
        // Anyone can verify the proof
        // No trust required
        self.spec_hash == spec.schema_hash
            && self.code_hash == Hash::from_string(code)
    }
}
```

### Real-World CONSTRUCT Pattern

```bash
# Specification-driven workflow (CONSTRUCT)
$ cat feature.ttl
@prefix : <http://example.org/schema#> .
:UserService a :RustStruct ;
    :field [ :name "id" ; :type :Uuid ] ;
    :field [ :name "name" ; :type :String ] ;
    :derives :Debug, :Clone, :Serialize .

$ ggen sync --audit true
[Receipt] Spec hash: 0x7f3a9b2c...
[Receipt] Generated: crates/core/src/user.rs
[Receipt] Code hash: 0x4d2e1a9f...
[Receipt] Timestamp: 2026-02-09T15:42:00Z
[Receipt] Reproducible: YES
✓ Generation complete - receipt saved to .ggen/receipts/

$ ggen replay --receipt .ggen/receipts/user-2026-02-09.json
✓ Replay successful - hashes match
✓ Determinism verified
```

### When CONSTRUCT Is Required

- **Manufacturing-grade systems**: Zero tolerance for ambiguity
- **Safety-critical code**: Formal verification required
- **Automated pipelines**: Human judgment not available
- **Regulatory compliance**: Audit trail mandatory
- **Mass production**: Same specification must produce identical outputs

### CONSTRUCT Trade-offs

**Benefits**:
- Deterministic and reproducible
- Formally verifiable
- Fully automatable
- Cryptographic proof of correctness
- Zero ambiguity

**Costs**:
- Requires complete specification upfront
- Less flexible to requirement changes
- Closed ontology must be maintained
- Cannot handle truly novel problems
- Higher upfront design cost

## Why the Split Matters for Manufacturing

### Manufacturing Paradigm Requirements

Modern manufacturing (physical or digital) requires:

1. **Repeatability**: Same specification → Same output
2. **Traceability**: Cryptographic receipts for every operation
3. **Quality Gates**: Deterministic pass/fail criteria
4. **Zero Defects**: No partial failures in production
5. **Automation**: Human judgment removed from production line

### The Impossibility of Mixing Regimes

**Theorem**: SELECT/DO and CONSTRUCT regimes are fundamentally incompatible.

**Proof by Contradiction**:

Assume we can mix regimes. Then:

```rust
// Hypothetical mixed regime (INVALID)
fn mixed_workflow(spec: Spec) -> Result<Code, Error> {
    let partial = spec.construct()?;  // CONSTRUCT: deterministic

    // Now we "improve" it with human judgment
    let improved = negotiate_improvements(partial)?;  // SELECT/DO: non-deterministic

    Ok(improved)
}

// Problem 1: Lost Determinism
// spec → partial is deterministic
// partial → improved is non-deterministic
// Therefore: spec → improved is non-deterministic
// CONTRADICTION: Violates CONSTRUCT requirement

// Problem 2: Lost Proof
// Receipt proves: spec → partial
// Receipt CANNOT prove: spec → improved (non-deterministic step)
// CONTRADICTION: Cannot provide cryptographic guarantee

// Problem 3: Lost Replay
// ggen replay --receipt r.json
// Can reproduce: spec → partial ✓
// Cannot reproduce: partial → improved ✗ (requires human negotiation)
// CONTRADICTION: Violates reproducibility requirement
```

### Concrete Failure Modes

#### Failure Mode 1: The "Almost Works" Trap

```rust
// SELECT/DO leaks into CONSTRUCT
fn generate_with_fixups(spec: &Spec) -> Code {
    let code = spec.construct();  // Deterministic generation

    // "Just a quick manual fix..."
    manual_fix_edge_case(&code);  // ← REGIME VIOLATION

    // Now the receipt is a lie:
    // Receipt says: "Generated from spec with hash 0x7f3a9b2c"
    // Reality: "Generated from spec, THEN manually modified"
    // Result: Cannot replay, cannot verify, cannot trust
}
```

#### Failure Mode 2: The "Flexible Schema" Trap

```rust
// Trying to add SELECT/DO flexibility to closed ontology
#[derive(Deserialize)]
struct FlexibleSpec {
    target_type: TargetType,
    constraints: Vec<Constraint>,
    extra_fields: HashMap<String, Value>,  // ← REGIME VIOLATION
    //                                          Opens the ontology
}

// Now we have:
// - Some fields are deterministic (target_type, constraints)
// - Some fields are interpretive (extra_fields)
// Result: Cannot reason about validity, cannot type-check completely
```

#### Failure Mode 3: The "Gradual Migration" Trap

```rust
// Attempting partial CONSTRUCT adoption
impl Project {
    fn build(&self) -> Result<Artifact, Error> {
        // Phase 1: Use CONSTRUCT for core
        let core = self.specs.construct_core()?;  // Deterministic

        // Phase 2: Use SELECT/DO for "complex" parts
        let complex = self.iterate_on_complex_parts()?;  // Non-deterministic

        // Phase 3: Combine
        combine(core, complex)  // ← REGIME VIOLATION
        //                          Which regime's guarantees apply?
    }
}

// Problems:
// - Cannot verify the combined artifact
// - Cannot replay the build
// - Cannot determine which parts failed and why
```

## Practical Examples by Domain

### Example 1: REST API Generation

#### SELECT/DO Approach

```rust
// Developer writes code, iterates based on feedback
#[derive(Serialize, Deserialize)]
struct User {
    id: Uuid,
    name: String,
    // "Should we add email? Let's discuss..."
}

#[get("/users/{id}")]
async fn get_user(id: Path<Uuid>) -> Result<Json<User>> {
    // "How should we handle not found? 404 or 204?"
    // "Do we need rate limiting? Let's add it later..."
    todo!("Implement after design review")
}
```

#### CONSTRUCT Approach

```turtle
# Specification defines everything deterministically
@prefix api: <http://example.org/api#> .

:UserEndpoint a api:RestEndpoint ;
    api:path "/users/{id}" ;
    api:method api:GET ;
    api:pathParam [ api:name "id" ; api:type xsd:string ] ;
    api:response [
        api:status 200 ;
        api:body :User
    ] ;
    api:response [
        api:status 404 ;
        api:body :NotFoundError
    ] .

:User a api:Schema ;
    api:field [ api:name "id" ; api:type api:Uuid ; api:required true ] ;
    api:field [ api:name "name" ; api:type xsd:string ; api:required true ] ;
    api:field [ api:name "email" ; api:type xsd:string ; api:required true ] .
```

```bash
$ ggen sync --spec api.ttl
✓ Generated: crates/api/src/endpoints/user.rs
✓ Generated: crates/api/src/models/user.rs
✓ Generated: crates/api/tests/user_endpoint_test.rs
[Receipt] All files cryptographically signed
```

### Example 2: Database Schema Migration

#### SELECT/DO Approach

```sql
-- Migration written manually, reviewed, tested
-- File: migrations/20260209_add_users.sql
CREATE TABLE users (
    id UUID PRIMARY KEY,
    name VARCHAR(255) NOT NULL
    -- "Do we need an index on name?"
    -- "What about soft deletes?"
    -- "Should we add created_at now or later?"
);

-- File: migrations/20260210_add_email.sql
ALTER TABLE users ADD COLUMN email VARCHAR(255);
-- "Oops, forgot NOT NULL constraint"

-- File: migrations/20260211_fix_email.sql
ALTER TABLE users ALTER COLUMN email SET NOT NULL;
-- "Wait, existing rows have NULL emails..."
```

#### CONSTRUCT Approach

```turtle
# Complete schema specification
@prefix db: <http://example.org/db#> .

:UsersTable a db:Table ;
    db:column [ db:name "id" ; db:type db:Uuid ; db:primaryKey true ] ;
    db:column [ db:name "name" ; db:type db:VarChar ; db:length 255 ; db:notNull true ] ;
    db:column [ db:name "email" ; db:type db:VarChar ; db:length 255 ; db:notNull true ] ;
    db:index [ db:columns ("email") ; db:unique true ] ;
    db:timestamp [ db:created_at true ; db:updated_at true ] .
```

```bash
$ ggen sync --spec schema.ttl --validate
✓ Schema validation passed
✓ Generated: migrations/20260209_create_schema.sql
✓ Generated: crates/db/src/models/user.rs
✓ Generated: crates/db/tests/user_model_test.rs
[Receipt] Schema hash: 0x9c4f2a1b
[Receipt] All constraints enforced
```

### Example 3: Type-Safe Configuration

#### SELECT/DO Approach

```rust
// Config evolves through trial and error
#[derive(Deserialize)]
struct Config {
    database_url: String,
    redis_url: Option<String>,  // "Maybe we need Redis?"
    feature_flags: HashMap<String, bool>,  // "Flexible!"
    // "We'll add validation later..."
}

impl Config {
    fn load() -> Result<Self, ConfigError> {
        let config: Config = toml::from_str(&fs::read_to_string("config.toml")?)?;
        // "Should we validate URLs? Let's see if it breaks first..."
        Ok(config)
    }
}
```

#### CONSTRUCT Approach

```turtle
# Configuration schema with validation rules
@prefix cfg: <http://example.org/config#> .

:AppConfig a cfg:ConfigSchema ;
    cfg:field [
        cfg:name "database_url" ;
        cfg:type cfg:Url ;
        cfg:required true ;
        cfg:validation [ cfg:scheme "postgresql" ]
    ] ;
    cfg:field [
        cfg:name "redis_url" ;
        cfg:type cfg:Url ;
        cfg:required true ;
        cfg:validation [ cfg:scheme "redis" ]
    ] ;
    cfg:field [
        cfg:name "feature_flags" ;
        cfg:type cfg:EnumSet ;
        cfg:values ( "feature_a" "feature_b" "feature_c" )  # Closed set
    ] .
```

```bash
$ ggen sync --spec config.ttl
✓ Generated: crates/config/src/app_config.rs
✓ Generated: crates/config/src/validation.rs
✓ Generated: crates/config/tests/config_test.rs

$ cargo build
✓ Type-safe configuration with validation
✓ Invalid configs rejected at compile time
✓ No runtime surprises
```

## Decision Framework: Which Regime?

Use this decision tree to select the appropriate regime:

```
Is the problem space fully specified?
├─ YES → Can you enumerate all valid states?
│  ├─ YES → Do you need reproducibility?
│  │  ├─ YES → Use CONSTRUCT
│  │  └─ NO → Consider SELECT/DO (but why not have proofs?)
│  └─ NO → Use SELECT/DO (or improve specification)
└─ NO → Use SELECT/DO (for now)
       └─ Once specified, migrate to CONSTRUCT
```

### Migration Path: SELECT/DO → CONSTRUCT

```rust
// Phase 1: SELECT/DO (exploration)
fn prototype_feature() -> Code {
    // Rapid iteration, human judgment
    // Learn what the problem really is
}

// Phase 2: Extract Specification
fn document_learned_requirements() -> Specification {
    // Capture what was learned in formal spec
    // Define closed ontology
}

// Phase 3: CONSTRUCT (production)
fn generate_from_spec(spec: &Specification) -> Code {
    // Deterministic generation
    // Cryptographic receipts
    // Perfect replay
}

// Phase 4: Retire SELECT/DO Code
fn deprecate_prototype() {
    // Remove hand-written code
    // Spec is now the source of truth
}
```

## Validation Checklist

### For SELECT/DO Regime

- [ ] Problem space is truly underspecified
- [ ] Human judgment is genuinely required
- [ ] Iterative refinement is acceptable
- [ ] Non-determinism is tolerable
- [ ] This is temporary (prototype phase)

### For CONSTRUCT Regime

- [ ] Complete specification exists
- [ ] All valid states are enumerable
- [ ] Deterministic validation possible
- [ ] Cryptographic receipts generated
- [ ] Perfect replay verified
- [ ] No manual intervention in pipeline

### For Regime Purity

- [ ] No mixing of paradigms in single workflow
- [ ] Clear boundaries between regimes
- [ ] Migration path from SELECT/DO to CONSTRUCT defined
- [ ] No "just this once" manual fixes to generated code
- [ ] Receipts accurately reflect actual process

## Common Misconceptions

### Misconception 1: "CONSTRUCT is just SELECT/DO with better docs"

**FALSE**. The regimes differ fundamentally:

```rust
// SELECT/DO with good documentation
/// Get a user by ID
///
/// # Arguments
/// * `id` - The user ID
///
/// # Returns
/// The user if found, or an error
fn get_user(id: Uuid) -> Result<User, Error> {
    // Still requires human judgment
    // Still non-deterministic
    // Still no replay
}

// CONSTRUCT
// Specification IS the documentation
// Specification IS the source code
// Specification GENERATES the implementation
```

### Misconception 2: "We can be 'mostly CONSTRUCT' with some flexibility"

**FALSE**. CONSTRUCT requires complete closure:

```rust
// This is NOT CONSTRUCT (ontology is open)
enum TargetType {
    RustStruct,
    RustEnum,
    Other(String),  // ← Opens the ontology
}

// This IS CONSTRUCT (ontology is closed)
enum TargetType {
    RustStruct,
    RustEnum,
    // No "Other" case - finite enumeration
}
```

### Misconception 3: "Manual fixes to generated code are OK if documented"

**FALSE**. Manual fixes break the proof chain:

```
Specification → [Generate] → Code → [Manual Fix] → Deployed Code
                ^^^^^^^^^^^^        ^^^^^^^^^^^^^
                 Provable            Breaks Proof

Receipt proves: Specification → Code
Receipt CANNOT prove: Specification → Deployed Code
```

## Summary

The SELECT/DO and CONSTRUCT regimes represent fundamentally different approaches to software development:

| Aspect | SELECT/DO | CONSTRUCT |
|--------|-----------|-----------|
| **Decision Making** | Negotiation | Determinism |
| **Flexibility** | High | Zero (by design) |
| **Reproducibility** | None | Perfect |
| **Verification** | Informal | Cryptographic |
| **Automation** | Partial | Complete |
| **Appropriate For** | Exploration | Production |

**Key Insight**: These regimes cannot be mixed. Every workflow must choose one regime and maintain that choice throughout the pipeline.

**For ggen**: We operate in the CONSTRUCT regime. This means:
- Specifications are complete and closed
- Generation is deterministic
- Every output has a cryptographic receipt
- Perfect replay is guaranteed
- No manual intervention in production pipeline

## Further Reading

- [02-manufacturing-paradigm.md](./02-manufacturing-paradigm.md) - How CONSTRUCT enables software manufacturing
- [03-ontology-closure.md](./03-ontology-closure.md) - Techniques for closing ontologies
- [04-receipt-verification.md](./04-receipt-verification.md) - Cryptographic proof systems

## References

- ggen Source Code: `/home/user/ggen/crates/`
- CLAUDE.md: `/home/user/ggen/CLAUDE.md`
- Rules: `/home/user/ggen/.claude/rules/`

---

**Document Hash**: `sha256:TBD`
**Generated**: 2026-02-09
**Regime**: CONSTRUCT (this document is specification-driven)
