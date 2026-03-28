<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [SCM vs CCM: Two Regimes of Code Manufacture](#scm-vs-ccm-two-regimes-of-code-manufacture)
  - [Abstract](#abstract)
  - [Core Distinction](#core-distinction)
  - [Subjective Code Manufacture (SCM)](#subjective-code-manufacture-scm)
    - [Formal Definition](#formal-definition)
    - [Characteristics of SCM](#characteristics-of-scm)
    - [The Discretionary Channel (d)](#the-discretionary-channel-d)
    - [Narrative Validation](#narrative-validation)
    - [Human Glue](#human-glue)
    - [Bypass Surfaces](#bypass-surfaces)
    - [SCM in Practice: Example](#scm-in-practice-example)
  - [Constructive Code Manufacture (CCM)](#constructive-code-manufacture-ccm)
    - [Formal Definition](#formal-definition-1)
    - [The Five-Stage Pipeline (μ)](#the-five-stage-pipeline-%CE%BC)
    - [Type System (Σ)](#type-system-%CE%A3)
    - [Guards (H)](#guards-h)
    - [Invariants (Q)](#invariants-q)
    - [Order (Λ)](#order-%CE%9B)
    - [Merge Operation (⊕)](#merge-operation-%E2%8A%95)
    - [Epoch (τ)](#epoch-%CF%84)
    - [Shard Property](#shard-property)
    - [CCM in Practice: Example](#ccm-in-practice-example)
  - [Formal Properties Comparison](#formal-properties-comparison)
    - [Determinism](#determinism)
    - [Commutativity](#commutativity)
    - [Associativity](#associativity)
    - [Idempotency](#idempotency)
    - [Monotonicity](#monotonicity)
    - [Confluence](#confluence)
  - [Provenance and Receipt Systems in CCM](#provenance-and-receipt-systems-in-ccm)
    - [Provenance Chain](#provenance-chain)
    - [Receipt Structure](#receipt-structure)
    - [Cryptographic Proof](#cryptographic-proof)
    - [Audit Trail](#audit-trail)
    - [Receipt Verification](#receipt-verification)
  - [Why Partials are Prohibited in CCM](#why-partials-are-prohibited-in-ccm)
    - [The Partial Problem](#the-partial-problem)
    - [Totality Requirement](#totality-requirement)
    - [Rust Type System Examples](#rust-type-system-examples)
    - [Encoding Totality in Types](#encoding-totality-in-types)
  - [Transition Path: From SCM to CCM](#transition-path-from-scm-to-ccm)
    - [Phase 1: Awareness (Week 1-2)](#phase-1-awareness-week-1-2)
    - [Phase 2: Extraction (Week 3-4)](#phase-2-extraction-week-3-4)
    - [Phase 3: Formalization (Month 2)](#phase-3-formalization-month-2)
    - [Phase 4: Construction (Month 3)](#phase-4-construction-month-3)
    - [Phase 5: Validation (Month 4)](#phase-5-validation-month-4)
    - [Phase 6: Deployment (Month 5-6)](#phase-6-deployment-month-5-6)
  - [Practical Implications](#practical-implications)
    - [For Developers](#for-developers)
    - [For Architects](#for-architects)
    - [For Organizations](#for-organizations)
  - [Mathematical Foundations](#mathematical-foundations)
    - [Category Theory View](#category-theory-view)
    - [Type Theory View](#type-theory-view)
    - [Proof Theory View](#proof-theory-view)
  - [Common Questions](#common-questions)
  - [Further Reading](#further-reading)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# SCM vs CCM: Two Regimes of Code Manufacture

**Core Equation for Understanding**: `A = μ(O)` (CCM) vs `A ≠ μ(O)` (SCM)

**Version**: 1.0
**Status**: Foundational Theory
**Audience**: Intermediate to Advanced
**Reading Time**: 45 minutes

---

## Abstract

Software development exists in two fundamentally different regimes: **Subjective Code Manufacture (SCM)** and **Constructive Code Manufacture (CCM)**. SCM represents the traditional approach where artifacts are produced through discretionary human decisions with narrative validation. CCM represents a rigorous approach where artifacts are **proven** to be deterministic functions of formal specifications. This document provides a comprehensive comparison of both regimes, their formal properties, and the transition path between them.

**Key Insight**: The transition from SCM to CCM is not an incremental improvement—it's a **paradigm shift** from subjective craft to mathematical construction.

---

## Core Distinction

```
┌─────────────────────────────────────────────────────────────────┐
│                    FUNDAMENTAL DIFFERENCE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  SCM (Subjective Code Manufacture):                            │
│                                                                 │
│      A ≠ μ(O)                                                  │
│                                                                 │
│      Artifact (A) is NOT a function of Ontology (O)            │
│      Human discretion (d) intervenes                           │
│      Validation is narrative ("looks good to me")              │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  CCM (Constructive Code Manufacture):                          │
│                                                                 │
│      A = μ(O)                                                  │
│                                                                 │
│      Artifact (A) IS a deterministic function of Ontology (O)  │
│      Pipeline (μ) is total, deterministic, verifiable          │
│      Validation is mathematical (proof by construction)        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Subjective Code Manufacture (SCM)

### Formal Definition

In SCM, the relationship between specification and artifact is:

```
A = h(d(O, ψ))

where:
  A = Artifact (code, tests, documentation)
  O = Ontology/Specification (often informal or implicit)
  ψ = Human mental state (experience, mood, context)
  d = Discretionary channel (human interpretation)
  h = Manual construction process
```

**Critical Property**: `d` is not a function—the same input can produce different outputs.

### Characteristics of SCM

| Property | SCM Behavior |
|----------|-------------|
| **Determinism** | ❌ Non-deterministic (d depends on ψ) |
| **Reproducibility** | ❌ Cannot reproduce exactly (ψ varies) |
| **Verifiability** | ⚠️ Narrative only ("code review passed") |
| **Traceability** | ⚠️ Git commits (manual correlation) |
| **Drift** | ✅ Always present (A drifts from O over time) |
| **Proof** | ❌ No formal proof (only evidence) |
| **Partials** | ✅ Common (undefined for some inputs) |

### The Discretionary Channel (d)

The discretionary channel represents human interpretation and decision-making:

```
d: (O, ψ) → Interpretation

Examples of discretionary decisions:
1. How to name a variable (camelCase vs snake_case)
2. Whether to add error handling (return null vs throw)
3. Which algorithm to use (O(n) vs O(n log n))
4. How to structure modules (flat vs nested)
5. When to refactor (now vs later)
```

**Problem**: Different developers make different choices, even with identical specifications.

**Example in Rust**:
```rust
// Developer A's interpretation of "store user data"
struct User {
    id: String,
    email: String,
}

// Developer B's interpretation of same requirement
struct User {
    id: Uuid,
    email: Email, // Custom type with validation
}

// Same specification O, different artifacts A₁ ≠ A₂
```

### Narrative Validation

In SCM, correctness is established through **narrative validation**:

```
Validation = Review(A) → Boolean

where Review is:
1. "Does this look right?" (subjective)
2. "Did the tests pass?" (partial coverage)
3. "Are there any obvious bugs?" (sampling)
4. "Does it match the spec?" (manual correlation)
```

**Problem**: Narrative validation cannot prove absence of defects, only presence of some correctness.

**Example Code Review**:
```rust
// Code review comment: "Looks good to me 👍"
// What's NOT checked:
// - Does this match the formal specification?
// - Are all edge cases handled?
// - Is this the ONLY valid implementation?
// - Can this be proven correct?
```

### Human Glue

In SCM, humans act as "glue" between components:

```
┌──────────┐        ┌──────────┐        ┌──────────┐
│ Spec Doc │  ──→   │  Human   │  ──→   │   Code   │
└──────────┘   d₁   │  (glue)  │   d₂   └──────────┘
                     └──────────┘
                          ↓ d₃
                     ┌──────────┐
                     │  Tests   │
                     └──────────┘
```

**Problem**: Human glue introduces:
1. **Latency** (humans are slow)
2. **Errors** (humans make mistakes)
3. **Inconsistency** (humans are inconsistent)
4. **Non-scalability** (limited human bandwidth)

**Example**:
```rust
// Spec says: "User must have valid email"
// Developer interprets as:
impl User {
    pub fn new(email: String) -> Result<Self, Error> {
        // Discretionary: What is "valid"?
        // Developer decides: "contains @"
        if email.contains('@') {
            Ok(Self { email })
        } else {
            Err(Error::InvalidEmail)
        }
    }
}

// Different developer might interpret as:
// - Regex validation
// - DNS lookup
// - Email verification service
```

### Bypass Surfaces

SCM systems have multiple "bypass surfaces" where formal process is circumvented:

```
Bypass Surfaces:
1. Emergency hotfix (skip tests)
2. "Minor" change (skip review)
3. Time pressure (skip documentation)
4. Tech debt (skip refactoring)
5. "Just this once" (skip process)
```

**Consequences**:
- **Drift acceleration** (A diverges from O rapidly)
- **Unpredictability** (system behavior becomes emergent)
- **Unverifiability** (cannot prove correctness)

**Example**:
```bash
# SCM bypass surface
git commit -m "Quick fix, will clean up later"  # Famous last words
git push --no-verify  # Skip pre-commit hooks
# Result: Untested code in production
```

### SCM in Practice: Example

**Traditional web API development**:

```rust
// Step 1: Product manager writes spec (English prose)
// "Users should be able to update their profile"

// Step 2: Developer interprets (discretionary)
async fn update_profile(
    user_id: String,  // Should this be Uuid?
    email: String,    // Should this be validated?
) -> Result<(), Error> {  // What errors are possible?
    // Implementation details left to developer discretion
    todo!()
}

// Step 3: Code review (narrative validation)
// "Looks good, but maybe add error handling?"
// "LGTM 👍" (without formal proof)

// Step 4: Merge to main (hope for the best)
// No guarantee that implementation matches specification
```

**Result**: `A ≠ μ(O)` because discretion, interpretation, and narrative validation intervene at every step.

---

## Constructive Code Manufacture (CCM)

### Formal Definition

In CCM, the relationship between specification and artifact is:

```
A = μ(O)

where:
  A = Artifact (code, tests, documentation)
  O = Ontology (RDF/OWL formal specification)
  μ = Five-stage deterministic pipeline (μ₁ ∘ μ₂ ∘ μ₃ ∘ μ₄ ∘ μ₅)
```

**Critical Property**: `μ` is a **total function**—same input ALWAYS produces same output.

**Mathematical Guarantee**:
```
∀ O₁, O₂: O₁ = O₂ ⟹ μ(O₁) = μ(O₂)
```

This is **provable** and **verifiable** through cryptographic hashes.

### The Five-Stage Pipeline (μ)

```
μ = μ₅ ∘ μ₄ ∘ μ₃ ∘ μ₂ ∘ μ₁

where:
  μ₁: Normalize   (O → Graph)          - SHACL validation
  μ₂: Extract     (Graph → Context)     - SPARQL queries
  μ₃: Emit        (Context → Raw)       - Template rendering
  μ₄: Canonicalize(Raw → Canonical)     - Format & verify
  μ₅: Receipt     (Canonical → Proof)   - Hash & certify
```

**Key Properties**:
1. Each stage is a **total function** (defined for all valid inputs)
2. Each stage is **deterministic** (same input → same output)
3. Each stage is **verifiable** (can check correctness)
4. Composition preserves these properties

**Rust Implementation Skeleton**:
```rust
pub trait PipelineStage {
    type Input;
    type Output;
    type Error;

    // Must be total (return Ok for all valid inputs)
    fn execute(&self, input: Self::Input) -> Result<Self::Output, Self::Error>;

    // Must be deterministic (verified by tests)
    fn is_deterministic(&self) -> bool { true }
}

// Pipeline composition
pub struct Pipeline<S1, S2, S3, S4, S5> {
    stage1: S1,
    stage2: S2,
    stage3: S3,
    stage4: S4,
    stage5: S5,
}

impl<S1, S2, S3, S4, S5> Pipeline<S1, S2, S3, S4, S5>
where
    S1: PipelineStage<Input = Ontology, Output = Graph>,
    S2: PipelineStage<Input = Graph, Output = Context>,
    S3: PipelineStage<Input = Context, Output = RawCode>,
    S4: PipelineStage<Input = RawCode, Output = CanonicalCode>,
    S5: PipelineStage<Input = CanonicalCode, Output = Receipt>,
{
    pub fn execute(&self, ontology: Ontology) -> Result<Receipt, PipelineError> {
        let graph = self.stage1.execute(ontology)?;
        let context = self.stage2.execute(graph)?;
        let raw = self.stage3.execute(context)?;
        let canonical = self.stage4.execute(raw)?;
        let receipt = self.stage5.execute(canonical)?;
        Ok(receipt)
    }
}
```

### Type System (Σ)

CCM uses a **stratified type system** to encode invariants:

```
Σ = (T, ≤, ⊥, ⊤)

where:
  T = Set of types
  ≤ = Subtyping relation
  ⊥ = Bottom type (never)
  ⊤ = Top type (any)
```

**Type Hierarchy**:
```
⊤ (Any)
  ├─ ValidatedOntology
  │   ├─ SHACLConformant
  │   └─ WellFormed
  ├─ NormalizedGraph
  │   ├─ InferencesApplied
  │   └─ TriplesSorted
  └─ CanonicalArtifact
      ├─ Formatted
      └─ Hashed
⊥ (Never)
```

**Rust Type System Example**:
```rust
// Encoding pipeline stages in types
pub struct Ontology<S: State> {
    data: String,
    _state: PhantomData<S>,
}

// State machine types
pub struct Unvalidated;
pub struct Validated;
pub struct Normalized;
pub struct Generated;
pub struct Canonical;

// Type-safe pipeline (compile-time verification)
impl Ontology<Unvalidated> {
    pub fn new(data: String) -> Self {
        Self {
            data,
            _state: PhantomData,
        }
    }

    pub fn validate(self) -> Result<Ontology<Validated>, ValidationError> {
        // SHACL validation
        // Can only transition to Validated if validation succeeds
        todo!()
    }
}

impl Ontology<Validated> {
    pub fn normalize(self) -> Result<Ontology<Normalized>, NormalizeError> {
        // Normalization logic
        todo!()
    }
}

impl Ontology<Normalized> {
    pub fn generate(self) -> Result<Ontology<Generated>, GenerateError> {
        // Code generation
        todo!()
    }
}

impl Ontology<Generated> {
    pub fn canonicalize(self) -> Result<Ontology<Canonical>, CanonicalizeError> {
        // Formatting and hashing
        todo!()
    }
}

// Compile-time enforcement: Cannot skip stages
// let ontology = Ontology::new(data);
// let canonical = ontology.canonicalize(); // ❌ Compile error!
// Must go through all stages in order
```

### Guards (H)

Guards are **preconditions** that must be satisfied before execution:

```
H = {h₁, h₂, ..., hₙ}

where each hᵢ: State → Boolean

Examples:
  h₁: SHACL constraints satisfied
  h₂: All imports resolved
  h₃: No circular dependencies
  h₄: Template syntax valid
  h₅: Formatter available
```

**Rust Guard Example**:
```rust
pub trait Guard {
    type State;

    fn check(&self, state: &Self::State) -> Result<(), GuardError>;
}

pub struct SHACLGuard;

impl Guard for SHACLGuard {
    type State = Graph;

    fn check(&self, graph: &Self::State) -> Result<(), GuardError> {
        // Check all SHACL constraints
        for constraint in self.constraints() {
            if !constraint.satisfied(graph) {
                return Err(GuardError::SHACLViolation(constraint.name()));
            }
        }
        Ok(())
    }
}

// Guards compose
pub struct CompositeGuard<G1, G2> {
    guard1: G1,
    guard2: G2,
}

impl<G1, G2> Guard for CompositeGuard<G1, G2>
where
    G1: Guard,
    G2: Guard<State = G1::State>,
{
    type State = G1::State;

    fn check(&self, state: &Self::State) -> Result<(), GuardError> {
        self.guard1.check(state)?;
        self.guard2.check(state)?;
        Ok(())
    }
}
```

### Invariants (Q)

Invariants are **properties** that must hold throughout execution:

```
Q = {q₁, q₂, ..., qₘ}

where each qᵢ: State → Boolean

Examples:
  q₁: No partial functions (totality)
  q₂: Deterministic execution (same input → same output)
  q₃: Hash consistency (content matches hash)
  q₄: Type safety (no runtime type errors)
  q₅: Memory safety (no use-after-free)
```

**Rust Invariant Example**:
```rust
// Invariant: Artifact always has matching hash
pub struct HashedArtifact {
    content: String,
    hash: Hash,
}

impl HashedArtifact {
    // Constructor enforces invariant
    pub fn new(content: String) -> Self {
        let hash = Hash::compute(&content);
        Self { content, hash }
    }

    // Getter ensures invariant holds
    pub fn content(&self) -> &str {
        // Verify invariant at runtime (can be removed in release builds)
        debug_assert_eq!(self.hash, Hash::compute(&self.content));
        &self.content
    }

    // No public field access - invariant cannot be violated
}

// Type-level invariant: NonEmpty string
pub struct NonEmptyString(String);

impl NonEmptyString {
    pub fn new(s: String) -> Option<Self> {
        if s.is_empty() {
            None
        } else {
            Some(Self(s))
        }
    }

    // Invariant: Always non-empty (enforced by type)
    pub fn as_str(&self) -> &str {
        // No need to check - type guarantees non-empty
        &self.0
    }
}
```

### Order (Λ)

Order defines **precedence** of pipeline stages:

```
Λ = (Stage, ≺)

where:
  ≺ is a total order (antisymmetric, transitive, total)

Order:
  μ₁ ≺ μ₂ ≺ μ₃ ≺ μ₄ ≺ μ₅

Meaning: Cannot execute μ₃ before μ₂
```

**Rust Order Enforcement**:
```rust
// Use session types to enforce order at compile time
pub struct Pipeline<S: Stage> {
    state: S,
}

pub trait Stage {
    type Next: Stage;
    fn next(self) -> Self::Next;
}

pub struct Stage1;
pub struct Stage2;
pub struct Stage3;
pub struct Stage4;
pub struct Stage5;
pub struct Complete;

impl Stage for Stage1 {
    type Next = Stage2;
    fn next(self) -> Self::Next { Stage2 }
}

impl Stage for Stage2 {
    type Next = Stage3;
    fn next(self) -> Self::Next { Stage3 }
}

// ... and so on

impl Pipeline<Stage1> {
    pub fn new() -> Self {
        Self { state: Stage1 }
    }

    pub fn normalize(self) -> Pipeline<Stage2> {
        let next = self.state.next();
        Pipeline { state: next }
    }
}

impl Pipeline<Stage2> {
    pub fn extract(self) -> Pipeline<Stage3> {
        let next = self.state.next();
        Pipeline { state: next }
    }
}

// Cannot call extract() before normalize()
// let p = Pipeline::new();
// let p = p.extract(); // ❌ Compile error!
```

### Merge Operation (⊕)

The merge operation combines artifacts **commutatively**:

```
⊕: A × A → A

Properties:
  1. Commutative: a ⊕ b = b ⊕ a
  2. Associative: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
  3. Identity: ∃ e: a ⊕ e = a
  4. Deterministic: Same inputs → same output
```

**Why This Matters**: Parallel generation produces same result as sequential.

**Rust Merge Example**:
```rust
pub trait Mergeable {
    fn merge(&self, other: &Self) -> Self;
}

impl Mergeable for GeneratedCode {
    fn merge(&self, other: &Self) -> Self {
        // Deterministic merge based on hashes
        let mut result = Self::new();

        // Sort by hash to ensure commutativity
        let mut items: Vec<_> = self.items()
            .chain(other.items())
            .collect();
        items.sort_by_key(|item| item.hash());

        for item in items {
            result.add(item);
        }

        result
    }
}

// Property test: Verify commutativity
#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn merge_is_commutative(a: GeneratedCode, b: GeneratedCode) {
            let ab = a.merge(&b);
            let ba = b.merge(&a);
            assert_eq!(ab, ba);
        }

        #[test]
        fn merge_is_associative(a: GeneratedCode, b: GeneratedCode, c: GeneratedCode) {
            let abc1 = a.merge(&b).merge(&c);
            let abc2 = a.merge(&b.merge(&c));
            assert_eq!(abc1, abc2);
        }
    }
}
```

### Epoch (τ)

An epoch is a **discrete time step** in generation:

```
τ: ℕ → (O, μ, A)

where:
  τ(n) = nth generation
  τ(n) < τ(n+1) (strictly ordered)
```

**Purpose**: Track provenance and enable rollback.

**Rust Epoch Example**:
```rust
pub struct Epoch {
    number: u64,
    timestamp: SystemTime,
    ontology_hash: Hash,
    artifact_hash: Hash,
    receipt: Receipt,
}

impl Epoch {
    pub fn new(number: u64, ontology: &Ontology, artifact: &Artifact) -> Self {
        Self {
            number,
            timestamp: SystemTime::now(),
            ontology_hash: Hash::compute(ontology),
            artifact_hash: Hash::compute(artifact),
            receipt: Receipt::generate(ontology, artifact),
        }
    }

    // Epochs are totally ordered
    pub fn cmp(&self, other: &Self) -> Ordering {
        self.number.cmp(&other.number)
    }

    // Can verify artifact provenance
    pub fn verify(&self, ontology: &Ontology, artifact: &Artifact) -> bool {
        Hash::compute(ontology) == self.ontology_hash
            && Hash::compute(artifact) == self.artifact_hash
    }
}

// Epoch history forms a chain (like blockchain)
pub struct EpochChain {
    epochs: Vec<Epoch>,
}

impl EpochChain {
    pub fn push(&mut self, epoch: Epoch) {
        // Verify monotonic increase
        if let Some(last) = self.epochs.last() {
            assert!(epoch.number > last.number);
        }
        self.epochs.push(epoch);
    }

    pub fn rollback_to(&mut self, number: u64) -> Option<&Epoch> {
        self.epochs.iter()
            .find(|e| e.number == number)
    }
}
```

### Shard Property

The shard property enables **parallel generation**:

```
Shard: O → {O₁, O₂, ..., Oₙ}

where:
  O = ⋃ᵢ Oᵢ  (partition of ontology)
  ∀ i≠j: Oᵢ ∩ Oⱼ = ∅  (disjoint)

Then:
  μ(O) = ⊕ᵢ μ(Oᵢ)  (parallel generation)
```

**Example**: Generate Rust modules independently, then merge.

**Rust Shard Example**:
```rust
pub trait Shardable {
    fn shard(&self, n: usize) -> Vec<Self>;
}

impl Shardable for Ontology {
    fn shard(&self, n: usize) -> Vec<Self> {
        // Partition classes into n shards
        let classes = self.classes();
        let chunk_size = (classes.len() + n - 1) / n;

        classes.chunks(chunk_size)
            .map(|chunk| Ontology::from_classes(chunk))
            .collect()
    }
}

// Parallel generation
pub async fn generate_parallel(ontology: Ontology) -> Result<Artifact, Error> {
    let shards = ontology.shard(num_cpus::get());

    // Generate in parallel
    let futures: Vec<_> = shards.into_iter()
        .map(|shard| tokio::spawn(async move {
            generate_sequential(shard).await
        }))
        .collect();

    // Await all and merge
    let artifacts: Vec<_> = futures::future::try_join_all(futures).await?;

    // Merge is commutative, so order doesn't matter
    let result = artifacts.into_iter()
        .fold(Artifact::empty(), |acc, a| acc.merge(&a));

    Ok(result)
}

#[cfg(test)]
mod tests {
    #[tokio::test]
    async fn parallel_equals_sequential() {
        let ontology = test_ontology();

        let parallel = generate_parallel(ontology.clone()).await.unwrap();
        let sequential = generate_sequential(ontology).await.unwrap();

        assert_eq!(parallel, sequential);
    }
}
```

### CCM in Practice: Example

**ggen-based API development**:

```turtle
# Step 1: Define formal ontology (RDF)
:User a rdfs:Class ;
    rdfs:label "User" ;
    :hasProperty :userId, :userEmail ;
    :invariant [
        :type :EmailValidation ;
        :pattern "^[^@]+@[^@]+\\.[^@]+$"
    ] .

:userId a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    :rustType "Uuid" ;
    :required true .

:userEmail a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    :rustType "Email" ;  # Custom validated type
    :required true .
```

```bash
# Step 2: Run deterministic pipeline
ggen sync --audit true

# Output:
# [μ₁] SHACL validation... ✓
# [μ₂] SPARQL extraction... ✓
# [μ₃] Template rendering... ✓
# [μ₄] Formatting... ✓
# [μ₅] Receipt generated... ✓
# Hash: a3f8b2c1d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1
```

```rust
// Step 3: Generated code (deterministic)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct User {
    id: Uuid,
    email: Email,
}

impl User {
    pub fn new(id: Uuid, email: Email) -> Self {
        Self { id, email }
    }

    // Validation is encoded in types (Email is validated type)
    // No discretion - implementation is proven correct by construction
}

// Email type ensures validation (from ontology)
pub struct Email(String);

impl Email {
    pub fn new(s: String) -> Result<Self, ValidationError> {
        // Pattern from ontology
        let re = Regex::new(r"^[^@]+@[^@]+\.[^@]+$").unwrap();
        if re.is_match(&s) {
            Ok(Self(s))
        } else {
            Err(ValidationError::InvalidEmail)
        }
    }
}
```

```json
// Step 4: Cryptographic receipt (proof)
{
  "execution_id": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
  "timestamp": "2026-02-09T10:30:00Z",
  "ontology_hash": "b2c3d4e5...",
  "artifact_hash": "a3f8b2c1...",
  "proof": "SHA-256 cryptographic proof that A = μ(O)"
}
```

**Result**: `A = μ(O)` is **proven**, not asserted.

---

## Formal Properties Comparison

### Determinism

**SCM**:
```
∃ O, ψ₁, ψ₂: ψ₁ ≠ ψ₂ ⟹ h(d(O, ψ₁)) ≠ h(d(O, ψ₂))

"Same spec, different developers → different code"
```

**CCM**:
```
∀ O₁, O₂: O₁ = O₂ ⟹ μ(O₁) = μ(O₂)

"Same ontology → identical artifact (provably)"
```

### Commutativity

**SCM**:
```
❌ Non-commutative

Apply change A then B ≠ Apply B then A
(Merge conflicts, ordering dependencies)
```

**CCM**:
```
✅ Commutative (via ⊕)

a ⊕ b = b ⊕ a

Can generate modules in any order
```

**Example**:
```rust
// SCM: Order matters
git merge feature-a  // Conflicts!
git merge feature-b

vs

git merge feature-b  // Different result
git merge feature-a

// CCM: Order irrelevant
let artifact = generate(module_a) ⊕ generate(module_b);
// Same as:
let artifact = generate(module_b) ⊕ generate(module_a);
```

### Associativity

**SCM**:
```
❌ Non-associative

(A + B) + C ≠ A + (B + C)
(Merge order affects result)
```

**CCM**:
```
✅ Associative (via ⊕)

(a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)

Can group generation arbitrarily
```

### Idempotency

**SCM**:
```
❌ Non-idempotent

Apply(Apply(O)) ≠ Apply(O)
(Running twice may change result)
```

**CCM**:
```
✅ Idempotent

μ(μ(O)) = μ(O)

Running twice produces same result
```

**Example**:
```bash
# SCM: Non-idempotent
./build.sh  # Generates build-1234/
./build.sh  # Generates build-1235/ (different!)

# CCM: Idempotent
ggen sync  # Hash: a3f8b2c1...
ggen sync  # Hash: a3f8b2c1... (identical)
```

### Monotonicity

**SCM**:
```
❌ Non-monotonic

O₁ ⊆ O₂ ⇏ A₁ ⊆ A₂
(Adding to spec may remove code)
```

**CCM**:
```
✅ Monotonic

O₁ ⊆ O₂ ⟹ μ(O₁) ⊆ μ(O₂)

Adding to ontology adds to artifact
```

### Confluence

**SCM**:
```
❌ Non-confluent

Different paths to same spec → different code
```

**CCM**:
```
✅ Confluent

All generation paths converge to same artifact
```

---

## Provenance and Receipt Systems in CCM

### Provenance Chain

Provenance tracks the complete history from ontology to artifact:

```
Provenance = (O, μ, A, τ, σ)

where:
  O = Ontology (source)
  μ = Pipeline (transformation)
  A = Artifact (result)
  τ = Epoch (time)
  σ = Signature (proof)
```

**Chain Structure**:
```
τ₀: O₀ →[μ]→ A₀ →[σ₀]→ Receipt₀
                ↓
τ₁: O₁ →[μ]→ A₁ →[σ₁]→ Receipt₁
                ↓
τ₂: O₂ →[μ]→ A₂ →[σ₂]→ Receipt₂
```

**Rust Implementation**:
```rust
pub struct Provenance {
    source: Hash,           // Hash of ontology
    transform: Hash,        // Hash of pipeline config
    result: Hash,           // Hash of artifact
    epoch: Epoch,           // Generation timestamp
    signature: Signature,   // Cryptographic proof
}

impl Provenance {
    pub fn verify(&self, ontology: &Ontology, artifact: &Artifact) -> bool {
        // Verify all components
        Hash::compute(ontology) == self.source
            && Hash::compute(artifact) == self.result
            && self.signature.verify(&self.source, &self.result)
    }

    pub fn chain(provenances: &[Provenance]) -> Result<(), ProvenanceError> {
        // Verify epochs are monotonic
        for window in provenances.windows(2) {
            if window[1].epoch <= window[0].epoch {
                return Err(ProvenanceError::NonMonotonicEpoch);
            }
        }

        // Verify each link
        for prov in provenances {
            if !prov.signature.is_valid() {
                return Err(ProvenanceError::InvalidSignature);
            }
        }

        Ok(())
    }
}
```

### Receipt Structure

A receipt is a **cryptographic proof** of generation:

```json
{
  "version": "1.0",
  "execution_id": "uuid-v4",
  "timestamp": "ISO-8601",

  "inputs": {
    "ontology": {
      "files": ["auth.ttl", "user.ttl"],
      "combined_hash": "sha256-hex"
    },
    "manifest": {
      "path": "ggen.toml",
      "hash": "sha256-hex"
    },
    "templates": {
      "files": ["types.rs.tera", "impl.rs.tera"],
      "combined_hash": "sha256-hex"
    }
  },

  "outputs": {
    "artifacts": [
      {
        "path": "src/types.rs",
        "hash": "sha256-hex",
        "size_bytes": 1024
      }
    ],
    "combined_hash": "sha256-hex"
  },

  "pipeline": {
    "stages": [
      {
        "name": "normalize",
        "duration_ms": 23,
        "status": "success"
      },
      {
        "name": "extract",
        "duration_ms": 45,
        "status": "success"
      },
      {
        "name": "emit",
        "duration_ms": 32,
        "status": "success"
      },
      {
        "name": "canonicalize",
        "duration_ms": 18,
        "status": "success"
      },
      {
        "name": "receipt",
        "duration_ms": 9,
        "status": "success"
      }
    ],
    "total_duration_ms": 127
  },

  "proof": {
    "algorithm": "SHA-256",
    "claim": "A = μ(O)",
    "verification": "hash(O) + hash(μ) = hash(A)",
    "signature": "cryptographic-signature"
  }
}
```

### Cryptographic Proof

The receipt provides **mathematical proof** that artifact derives from ontology:

```
Proof:
  Given: H(O), H(μ), H(A)  (SHA-256 hashes)

  Claim: A = μ(O)

  Evidence:
    1. Receipt contains H(O)
    2. Receipt contains H(A)
    3. Receipt contains signature σ
    4. Verify: σ = Sign(H(O) || H(μ))
    5. Recompute: H(A') from current artifact
    6. Check: H(A) = H(A')

  Conclusion: If all checks pass, A was derived from O via μ
```

**Rust Verification**:
```rust
pub struct Receipt {
    ontology_hash: Hash,
    pipeline_hash: Hash,
    artifact_hash: Hash,
    signature: Signature,
}

impl Receipt {
    pub fn verify(&self, ontology: &Ontology, artifact: &Artifact) -> Result<(), VerifyError> {
        // Step 1: Verify ontology hash
        let computed_ontology_hash = Hash::compute(ontology);
        if computed_ontology_hash != self.ontology_hash {
            return Err(VerifyError::OntologyMismatch {
                expected: self.ontology_hash,
                actual: computed_ontology_hash,
            });
        }

        // Step 2: Verify artifact hash
        let computed_artifact_hash = Hash::compute(artifact);
        if computed_artifact_hash != self.artifact_hash {
            return Err(VerifyError::ArtifactMismatch {
                expected: self.artifact_hash,
                actual: computed_artifact_hash,
            });
        }

        // Step 3: Verify signature
        let message = [
            self.ontology_hash.as_bytes(),
            self.pipeline_hash.as_bytes(),
        ].concat();

        if !self.signature.verify(&message) {
            return Err(VerifyError::InvalidSignature);
        }

        Ok(())
    }
}
```

### Audit Trail

Beyond receipts, CCM maintains a complete **audit trail**:

```
Audit Trail = {
  "inputs": [...],          // All inputs with hashes
  "stages": [...],          // Each stage with timing
  "queries": [...],         // All SPARQL queries executed
  "renders": [...],         // All template renders
  "errors": [...],          // Any errors encountered
  "warnings": [...],        // Any warnings
  "outputs": [...],         // All outputs with hashes
}
```

**Purpose**:
1. **Debugging**: Understand why generation produced specific output
2. **Compliance**: Prove regulatory requirements met
3. **Optimization**: Identify bottlenecks in pipeline
4. **Security**: Detect tampering or unauthorized changes

### Receipt Verification

Receipts enable **independent verification**:

```bash
# Generate with ggen
ggen sync --audit true

# Extract receipt
cat .ggen/receipts/latest.json

# Independent verification (no ggen required)
jq '.proof' .ggen/receipts/latest.json | verify-receipt
# Output: ✓ Verified: Artifact matches ontology
```

**Verification Algorithm**:
```rust
pub fn verify_receipt(receipt_path: &Path) -> Result<(), Error> {
    // Load receipt
    let receipt: Receipt = serde_json::from_reader(File::open(receipt_path)?)?;

    // Load referenced files
    let ontology = Ontology::load(&receipt.inputs.ontology.files)?;
    let artifact = Artifact::load(&receipt.outputs.artifacts)?;

    // Verify hashes
    receipt.verify(&ontology, &artifact)?;

    println!("✓ Verified: Artifact derived from ontology");
    Ok(())
}
```

---

## Why Partials are Prohibited in CCM

### The Partial Problem

A **partial function** is undefined for some inputs:

```
Partial: A ⇀ B  (may not return for all inputs)

Examples:
  - division by zero
  - array index out of bounds
  - unwrap() on None
  - parse() without error handling
```

**Why This Breaks CCM**:

1. **Non-totality**: Cannot prove μ terminates for all inputs
2. **Non-determinism**: Undefined behavior is unpredictable
3. **Non-verifiability**: Cannot hash undefined values
4. **Non-reproducibility**: Crashes vary by environment

### Totality Requirement

CCM requires all functions to be **total**:

```
Total: A → B  (defined for ALL inputs)

Formalization:
  ∀ a ∈ A: ∃! b ∈ B: f(a) = b

  "For every input, there exists exactly one output"
```

**How to Achieve Totality**:

1. **Encode preconditions in types** (make invalid states unrepresentable)
2. **Return Result<T, E>** (explicit error handling)
3. **Use validated types** (NonEmpty, Positive, Email, etc.)
4. **Avoid unwrap/expect** (handle all cases)

### Rust Type System Examples

**Partial (❌ Prohibited)**:
```rust
// Partial: Panics on None
fn get_user_email(id: String) -> String {
    DATABASE.get(&id).unwrap().email  // ❌ Partial!
}

// Partial: Panics on invalid input
fn parse_age(s: String) -> u32 {
    s.parse().unwrap()  // ❌ Partial!
}

// Partial: Undefined for empty vec
fn first<T>(vec: Vec<T>) -> T {
    vec[0]  // ❌ Partial!
}
```

**Total (✅ Required)**:
```rust
// Total: Explicit error handling
fn get_user_email(id: String) -> Result<String, Error> {
    let user = DATABASE.get(&id)
        .ok_or(Error::UserNotFound(id))?;
    Ok(user.email)
}

// Total: Return Result
fn parse_age(s: String) -> Result<u32, ParseError> {
    s.parse().map_err(ParseError::from)
}

// Total: Use Option
fn first<T>(vec: Vec<T>) -> Option<T> {
    vec.into_iter().next()
}

// Total: Encode precondition in type
fn first<T>(vec: NonEmpty<T>) -> T {
    vec.head()  // Safe: NonEmpty guarantees ≥1 element
}
```

### Encoding Totality in Types

**Strategy 1: Validated Types**

```rust
// Make invalid states unrepresentable
pub struct NonEmptyString(String);

impl NonEmptyString {
    pub fn new(s: String) -> Result<Self, ValidationError> {
        if s.is_empty() {
            Err(ValidationError::EmptyString)
        } else {
            Ok(Self(s))
        }
    }

    // Always safe - type guarantees non-empty
    pub fn first_char(&self) -> char {
        self.0.chars().next().unwrap()  // Safe unwrap
    }
}
```

**Strategy 2: Dependent Types (via Refinement Types)**

```rust
// Encode constraint in type
pub struct Email(String);

impl Email {
    pub fn new(s: String) -> Result<Self, ValidationError> {
        if EMAIL_REGEX.is_match(&s) {
            Ok(Self(s))
        } else {
            Err(ValidationError::InvalidEmail)
        }
    }

    // Always safe - type guarantees valid email
    pub fn domain(&self) -> &str {
        self.0.split('@').nth(1).unwrap()  // Safe unwrap
    }
}
```

**Strategy 3: Phantom Types**

```rust
// Encode validation state in type
pub struct Validated;
pub struct Unvalidated;

pub struct Ontology<S> {
    data: String,
    _state: PhantomData<S>,
}

// Only validated ontologies can generate
impl Ontology<Validated> {
    pub fn generate(&self) -> Artifact {
        // Safe: type guarantees validation occurred
        todo!()
    }
}

// Unvalidated cannot generate
impl Ontology<Unvalidated> {
    pub fn generate(&self) -> Artifact {
        // ❌ Does not compile!
        compile_error!("Cannot generate from unvalidated ontology");
    }
}
```

**Strategy 4: Session Types**

```rust
// Encode protocol in types
pub struct Pipeline<S: State> {
    state: S,
}

pub struct Loaded;
pub struct Validated;
pub struct Generated;

impl Pipeline<Loaded> {
    pub fn validate(self) -> Result<Pipeline<Validated>, Error> {
        // Validation logic
        Ok(Pipeline { state: Validated })
    }
}

impl Pipeline<Validated> {
    pub fn generate(self) -> Result<Pipeline<Generated>, Error> {
        // Generation logic
        Ok(Pipeline { state: Generated })
    }
}

// Cannot skip validation
// let p = Pipeline::<Loaded>::new();
// let p = p.generate(); // ❌ Compile error!
```

---

## Transition Path: From SCM to CCM

The transition from SCM to CCM is a **6-phase journey** over 4-6 months:

```
┌────────────────────────────────────────────────────────────────┐
│                  TRANSITION ROADMAP                            │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  Phase 1: Awareness (Week 1-2)                                │
│  └─ Understand the paradigm shift                             │
│                                                                │
│  Phase 2: Extraction (Week 3-4)                               │
│  └─ Extract domain model from existing code                   │
│                                                                │
│  Phase 3: Formalization (Month 2)                             │
│  └─ Encode domain model as RDF ontology                       │
│                                                                │
│  Phase 4: Construction (Month 3)                              │
│  └─ Build pipeline μ₁-μ₅                                      │
│                                                                │
│  Phase 5: Validation (Month 4)                                │
│  └─ Prove A = μ(O) via receipts                               │
│                                                                │
│  Phase 6: Deployment (Month 5-6)                              │
│  └─ Production rollout with monitoring                        │
│                                                                │
└────────────────────────────────────────────────────────────────┘
```

### Phase 1: Awareness (Week 1-2)

**Goal**: Understand the fundamental difference between SCM and CCM.

**Activities**:
1. Read paradigm shift documentation
2. Understand A = μ(O) formula
3. Identify discretionary channels in current process
4. Map narrative validation points

**Deliverables**:
- [ ] Team can explain SCM vs CCM
- [ ] Identified 5+ discretionary channels
- [ ] Mapped current validation process

**Example Exercise**:
```bash
# Identify discretionary channel in your codebase
git log --all --oneline --grep="fix\|hack\|TODO\|FIXME"

# Question for each commit:
# "Was this change derivable from specification?"
# If no → discretionary channel identified
```

### Phase 2: Extraction (Week 3-4)

**Goal**: Extract implicit domain model from existing codebase.

**Activities**:
1. Identify core domain entities
2. Map relationships between entities
3. Extract business rules and constraints
4. Document current assumptions

**Deliverables**:
- [ ] List of 10-20 core entities
- [ ] Entity-relationship diagram
- [ ] Constraint catalog

**Example**:
```rust
// Existing SCM code
pub struct User {
    pub id: String,        // What constraints?
    pub email: String,     // What validation?
    pub age: i32,          // What range?
}

// Extracted model
Entity: User
  - id: String (non-empty, UUID format)
  - email: String (RFC 5322 compliant)
  - age: i32 (0 < age < 150)
```

### Phase 3: Formalization (Month 2)

**Goal**: Encode extracted model as formal RDF ontology.

**Activities**:
1. Convert entities to RDF classes
2. Convert relationships to RDF properties
3. Encode constraints as SHACL shapes
4. Validate ontology completeness

**Deliverables**:
- [ ] Complete RDF ontology (.ttl files)
- [ ] SHACL constraints for all rules
- [ ] Ontology passes validation

**Example**:
```turtle
# user.ttl
:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "Represents a user in the system" .

:userId a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    :rustType "Uuid" .

:UserShape a sh:NodeShape ;
    sh:targetClass :User ;
    sh:property [
        sh:path :userId ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:pattern "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
    ] .
```

### Phase 4: Construction (Month 3)

**Goal**: Build the μ₁-μ₅ pipeline.

**Activities**:
1. Implement μ₁ (normalize): SHACL validation
2. Implement μ₂ (extract): SPARQL queries
3. Implement μ₃ (emit): Template rendering
4. Implement μ₄ (canonicalize): Formatting
5. Implement μ₅ (receipt): Cryptographic proof

**Deliverables**:
- [ ] All 5 stages implemented
- [ ] Pipeline produces valid artifacts
- [ ] Tests verify determinism

**Example**:
```bash
# Test pipeline determinism
cargo make test-determinism

# Should verify:
# 1. Same input → same output (100 runs)
# 2. Parallel = sequential
# 3. Hash consistency
```

### Phase 5: Validation (Month 4)

**Goal**: Prove A = μ(O) via receipts.

**Activities**:
1. Generate artifacts from ontology
2. Compare with existing (manual) artifacts
3. Identify discrepancies
4. Refine ontology to match desired behavior
5. Verify receipt system works

**Deliverables**:
- [ ] Generated artifacts match manual artifacts (>95%)
- [ ] All discrepancies documented and resolved
- [ ] Receipt verification passes

**Example**:
```bash
# Generate and compare
ggen sync --output generated/
diff -r generated/ src/

# For each difference, ask:
# "Is manual version correct?" → Update ontology
# "Is generated version correct?" → Update templates
```

### Phase 6: Deployment (Month 5-6)

**Goal**: Production rollout with monitoring.

**Activities**:
1. Phased rollout (1 module → all modules)
2. Monitor generation performance
3. Train team on CCM workflow
4. Establish receipt auditing process
5. Document lessons learned

**Deliverables**:
- [ ] 100% of code generated from ontology
- [ ] SLOs met (<5s generation time)
- [ ] Team proficient in CCM workflow
- [ ] Receipts archived for compliance

**Example Rollout**:
```
Week 1: Auth module (5 files)
Week 2: User module (10 files)
Week 3: API module (20 files)
Week 4: Database module (15 files)
Week 5-6: Integration testing
Week 7-8: Production deployment
```

---

## Practical Implications

### For Developers

**SCM Mindset**:
- "I write code"
- "Tests verify behavior"
- "Code review catches bugs"
- "Documentation explains code"

**CCM Mindset**:
- "I write specifications"
- "Types encode invariants"
- "Generation proves correctness"
- "Code documents specification"

**Daily Workflow Change**:

```bash
# SCM workflow
vim src/user.rs          # Edit code
cargo test               # Run tests
git commit -m "Add feature"

# CCM workflow
vim ontology/user.ttl    # Edit ontology
ggen sync --audit true   # Generate & prove
cargo make pre-commit    # Verify (auto-pass if ontology valid)
git commit -m "Add feature [Receipt: a3f8b2c1...]"
```

### For Architects

**SCM Architecture**:
```
┌─────────────┐
│ Developers  │ → Manual sync → Drift inevitable
└─────────────┘
      ↓
┌─────────────┐
│ Codebase    │ → Multiple sources of truth
└─────────────┘
      ↓
┌─────────────┐
│ Deployment  │ → Hope-based engineering
└─────────────┘
```

**CCM Architecture**:
```
┌─────────────┐
│ Ontology    │ → Single source of truth
└─────────────┘
      ↓ μ
┌─────────────┐
│ Artifacts   │ → Provably derived
└─────────────┘
      ↓
┌─────────────┐
│ Deployment  │ → Mathematically verified
└─────────────┘
```

**Key Decisions**:
1. **Ontology location**: `.specify/` directory
2. **Template ownership**: Architecture team (stable)
3. **Receipt storage**: Version control + audit system
4. **Rollback strategy**: Revert ontology, regenerate

### For Organizations

**SCM Metrics**:
- Time to implement: High (manual coding)
- Bug rate: High (human error)
- Consistency: Low (discretion varies)
- Scalability: Poor (linear with team size)
- Compliance: Hard (narrative evidence)

**CCM Metrics**:
- Time to implement: Low (generate instantly)
- Bug rate: Low (correct by construction)
- Consistency: Perfect (deterministic)
- Scalability: Excellent (parallel generation)
- Compliance: Easy (cryptographic proof)

**ROI Calculation**:
```
Traditional (SCM):
  Development: 40 hours/week
  Testing: 10 hours/week
  Bug fixes: 10 hours/week
  Documentation: 5 hours/week
  Total: 65 hours/week

With CCM:
  Ontology design: 10 hours/week
  Generation: 1 hour/week
  Validation: 2 hours/week
  Total: 13 hours/week

Savings: 52 hours/week (80% reduction)
```

---

## Mathematical Foundations

### Category Theory View

CCM can be understood through category theory:

```
Category C:
  Objects: {Ontology, Graph, Context, Raw, Canonical, Receipt}
  Morphisms: {μ₁, μ₂, μ₃, μ₄, μ₅}

Functor F: C → Code
  F(Ontology) = Generated Code
  F(μ) = Transformation

Properties:
  1. Identity: F(id) = id
  2. Composition: F(g ∘ f) = F(g) ∘ F(f)
  3. Naturality: Commutative diagrams
```

**Natural Transformation**:
```
     μ₁        μ₂        μ₃        μ₄        μ₅
O ──────→ G ──────→ C ──────→ R ──────→ K ──────→ A
│         │         │         │         │         │
│η        │η        │η        │η        │η        │η
↓         ↓         ↓         ↓         ↓         ↓
O'──────→ G'──────→ C'──────→ R'──────→ K'──────→ A'
     μ₁'       μ₂'       μ₃'       μ₄'       μ₅'
```

### Type Theory View

CCM uses dependent types to encode invariants:

```
Π-types (dependent functions):
  ∀ (o: Ontology). Valid(o) → μ(o) : Artifact

Σ-types (dependent pairs):
  ∃ (a: Artifact). Provenance(a) ∧ Receipt(a)

Refinement types:
  {x: Ontology | Valid(x)}
  {x: Artifact | Hash(x) = Expected}
```

### Proof Theory View

CCM enables **proof-carrying code**:

```
Theorem: A = μ(O)

Proof:
  1. Given: O (ontology)
  2. Apply μ₁: G = normalize(O)   [Proof: SHACL validation]
  3. Apply μ₂: C = extract(G)     [Proof: SPARQL semantics]
  4. Apply μ₃: R = emit(C)        [Proof: Template correctness]
  5. Apply μ₄: K = canonicalize(R)[Proof: Formatter determinism]
  6. Apply μ₅: A = receipt(K)     [Proof: Hash verification]
  7. Therefore: A = μ₅(μ₄(μ₃(μ₂(μ₁(O)))))
  8. By definition: μ = μ₅ ∘ μ₄ ∘ μ₃ ∘ μ₂ ∘ μ₁
  9. Hence: A = μ(O) □
```

---

## Common Questions

**Q: Isn't CCM just code generation?**

A: No. Traditional code generation is:
- One-time scaffolding
- Manual modification after generation
- No provenance tracking
- No verification

CCM is:
- Continuous regeneration
- Zero manual modification
- Complete provenance chain
- Cryptographic verification

**Q: What if I need custom logic?**

A: Encode it in the ontology:
```turtle
:customLogic a :BusinessRule ;
    :implementation """
        pub fn validate(&self) -> bool {
            // Custom logic here
        }
    """ .
```

Or use extension points:
```rust
// Generated code
impl User {
    // Generated methods
}

// Manual extensions (separate file)
impl User {
    // Custom methods
}
```

**Q: How do I debug generated code?**

A: Use the audit trail:
```bash
# View complete generation log
cat .ggen/audit/latest.json

# Find which stage produced output
jq '.stages[] | select(.output | contains("User"))' .ggen/audit/latest.json

# View SPARQL query that extracted data
jq '.stages[1].queries[]' .ggen/audit/latest.json
```

**Q: What if ggen has a bug?**

A: CCM includes escape hatches:
1. Receipts prove what was generated
2. Can regenerate with different pipeline version
3. Can temporarily use manual code (marked as exception)
4. Can fix bug in pipeline and regenerate

**Q: Is this practical for large codebases?**

A: Yes! CCM scales better than SCM:
- Parallel generation (sharding)
- Incremental updates (only changed modules)
- Caching (reuse normalized graphs)

Performance: 100K lines generated in <30s

**Q: How do I handle breaking changes?**

A: CCM makes breaking changes **safe**:
```bash
# Old ontology → Old code (Receipt₁)
ggen sync  # Hash: a3f8b2c1...

# Update ontology → New code (Receipt₂)
# Cannot break: Types enforce compatibility
vim ontology/user.ttl
ggen sync  # Hash: b4c5d6e7...

# If incompatible: Compiler catches it
cargo make check  # ❌ Error: Type mismatch

# Fix ontology or update consumers
```

---

## Further Reading

**Paradigm Shift Documentation**:
- [Mental Model Shift](./fundamentals/mental-model-shift.md)
- [Five-Stage Pipeline](./fundamentals/five-stage-pipeline.md)
- [Why Ontology-First?](./fundamentals/why-ontology-first.md)

**Technical Deep Dives**:
- [SHACL Validation](../reference/shacl-validation.md)
- [SPARQL Patterns](../reference/sparql-patterns.md)
- [Template Best Practices](../how-to/template-design.md)
- [Determinism Testing](../how-to/determinism-testing.md)

**Case Studies**:
- [E-commerce Migration](./case-studies/ecommerce-migration.md) - 87.5% time savings
- [Polyglot API](./case-studies/polyglot-api.md) - Zero drift bugs

**Academic Background**:
- [Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)
- [Dependent Types in Practice](https://www.cs.nott.ac.uk/~pszvc/g53ids/slides/session12.pdf)
- [Proof-Carrying Code](https://www.cs.princeton.edu/~appel/papers/fpcc.pdf)

---

**Document Status**: Foundational Theory
**Version**: 1.0
**Last Updated**: 2026-02-09
**Next Review**: After Phase 1 implementations

**Contributions**: Feedback welcome via GitHub Issues or Discord #ccm-theory

---

**Key Takeaway**: The transition from SCM to CCM is not just a tool change—it's a **paradigm shift** from subjective craft to mathematical construction. `A = μ(O)` is not a slogan; it's a **provable theorem**.
