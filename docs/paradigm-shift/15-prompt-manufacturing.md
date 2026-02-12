<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Prompt Manufacturing: Compiling Prompts from Ontology](#prompt-manufacturing-compiling-prompts-from-ontology)
  - [TL;DR](#tldr)
  - [Table of Contents](#table-of-contents)
  - [The Problem: Prompt Drift](#the-problem-prompt-drift)
    - [Traditional Prompt Engineering](#traditional-prompt-engineering)
  - [The Solution: Prompts as Target Language](#the-solution-prompts-as-target-language)
    - [The Paradigm Shift](#the-paradigm-shift)
    - [Prompts as Compilation Target](#prompts-as-compilation-target)
  - [Prompt IR is CONSTRUCT-Shaped](#prompt-ir-is-construct-shaped)
    - [The No Moving Parts Principle](#the-no-moving-parts-principle)
    - [Traditional Template Approach (Moving Parts)](#traditional-template-approach-moving-parts)
    - [ggen CONSTRUCT Approach (No Moving Parts)](#ggen-construct-approach-no-moving-parts)
  - [Deterministic Prompt Emission](#deterministic-prompt-emission)
    - [The Determinism Guarantee](#the-determinism-guarantee)
    - [Concrete Example: Bond Extraction Prompts](#concrete-example-bond-extraction-prompts)
  - [LLMs as Replaceable Stations](#llms-as-replaceable-stations)
    - [The Manufacturing Analogy](#the-manufacturing-analogy)
    - [LLM as CPU Analogy](#llm-as-cpu-analogy)
    - [Practical Example: LLM Swapping](#practical-example-llm-swapping)
  - [Prompt Drift Elimination](#prompt-drift-elimination)
    - [The Drift Problem](#the-drift-problem)
    - [ggen Solution: Prompts are Derived, Not Written](#ggen-solution-prompts-are-derived-not-written)
    - [Example: Preventing Drift](#example-preventing-drift)
  - [Designing Prompt Ontologies](#designing-prompt-ontologies)
    - [Core Principles](#core-principles)
    - [Prompt Ontology Structure](#prompt-ontology-structure)
    - [CONSTRUCT Rules for Prompt Generation](#construct-rules-for-prompt-generation)
    - [Template for Prompt Emission](#template-for-prompt-emission)
  - [Complete Example: Narrative to Compiled Prompt](#complete-example-narrative-to-compiled-prompt)
    - [Step 1: Narrative Requirement](#step-1-narrative-requirement)
    - [Step 2: Formalize as OWL Ontology](#step-2-formalize-as-owl-ontology)
    - [Step 3: Generate Prompt IR (μ₁ + μ₂)](#step-3-generate-prompt-ir-%CE%BC%E2%82%81--%CE%BC%E2%82%82)
    - [Step 4: Emit Final Prompt (μ₃)](#step-4-emit-final-prompt-%CE%BC%E2%82%83)
    - [Step 5: Verify Determinism](#step-5-verify-determinism)
    - [Step 6: Use in Production](#step-6-use-in-production)
  - [Integration with ggen Pipeline](#integration-with-ggen-pipeline)
    - [The Five-Stage Pipeline (μ₁-μ₅)](#the-five-stage-pipeline-%CE%BC%E2%82%81-%CE%BC%E2%82%85)
    - [Example: ggen.toml Configuration](#example-ggentoml-configuration)
    - [CLI Integration](#cli-integration)
  - [Performance and Verification](#performance-and-verification)
    - [Performance Metrics](#performance-metrics)
    - [Verification Checklist](#verification-checklist)
    - [Test Suite](#test-suite)
  - [Summary](#summary)
  - [Further Reading](#further-reading)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Prompt Manufacturing: Compiling Prompts from Ontology

**Version**: 1.0.0
**Last Updated**: 2026-02-09
**Status**: Canonical Reference
**Reading Time**: 35 minutes | **Difficulty**: Advanced

---

## TL;DR

**Traditional Approach**: Hand-craft prompts → LLM → hope for consistent output → manual validation → prompt drift over time

**ggen Prompt Manufacturing**: Ontology → CONSTRUCT IR → Deterministic Prompt Emission → LLM (replaceable) → Guaranteed Valid Output

**Result**: Prompts become a target language. LLMs become replaceable manufacturing stations driven by compiled prompt IR.

---

## Table of Contents

1. [The Problem: Prompt Drift](#the-problem-prompt-drift)
2. [The Solution: Prompts as Target Language](#the-solution-prompts-as-target-language)
3. [Prompt IR is CONSTRUCT-Shaped](#prompt-ir-is-construct-shaped)
4. [Deterministic Prompt Emission](#deterministic-prompt-emission)
5. [LLMs as Replaceable Stations](#llms-as-replaceable-stations)
6. [Prompt Drift Elimination](#prompt-drift-elimination)
7. [Designing Prompt Ontologies](#designing-prompt-ontologies)
8. [Complete Example: Narrative to Compiled Prompt](#complete-example-narrative-to-compiled-prompt)
9. [Integration with ggen Pipeline](#integration-with-ggen-pipeline)
10. [Performance and Verification](#performance-and-verification)

---

## The Problem: Prompt Drift

### Traditional Prompt Engineering

```
Week 1: "Extract the bond ISIN from this document"
  → Works 80% of the time

Week 2: "Extract the bond ISIN (12 characters, format XX...) from this document"
  → Works 85% of the time

Week 3: "Extract the bond ISIN. CRITICAL: Must be exactly 12 characters..."
  → Works 87% of the time

Week 4: "You are a financial data extraction system. Extract the bond ISIN..."
  → Works 85% of the time (regressed!)

Week 5: Engineer leaves, new engineer rewrites prompts
  → Everything breaks
```

**Problems**:
1. **Non-deterministic**: Same input → different prompts → different results
2. **No version control**: Prompt "improvements" can regress
3. **No audit trail**: Cannot prove what prompt produced what output
4. **Tribal knowledge**: Prompt engineering becomes dark art
5. **Drift**: Prompts diverge from original requirements
6. **Brittle**: Small wording changes → large behavior changes

**The core issue**: Prompts are treated as **mutable strings**, not **compiled artifacts**.

---

## The Solution: Prompts as Target Language

### The Paradigm Shift

In ggen, **prompts are not strings you write—they're artifacts you compile**.

```
Source Code (C)         →  Compiler  →  Machine Code (x86)
Domain Ontology (RDF)   →  μ₁-μ₅     →  Prompt Code (LLM IR)
```

**Key insight**: Just as you don't hand-write assembly, you shouldn't hand-write prompts. Instead:

1. **Define the domain ontology** (what is a Bond? what constraints?)
2. **Declare the extraction task** (extract Bond from text)
3. **Compiler generates the prompt** (deterministically)
4. **LLM executes the prompt** (like a CPU executes machine code)

### Prompts as Compilation Target

```rust
// Traditional: Prompts as strings
const PROMPT: &str = "Extract the bond ISIN...";  // Hand-written, brittle

// ggen: Prompts as compiled IR
struct PromptIR {
    task: ExtractionTask,           // What to extract
    constraints: ConstraintSet,     // What validations
    domain_context: OntologyRef,    // Semantic grounding
    examples: Vec<Example>,         // Few-shot learning
}

// Generated deterministically from ontology
impl From<&OWLClass> for PromptIR {
    fn from(owl_class: &OWLClass) -> Self {
        // Compilation happens here
        Self::compile(owl_class)
    }
}
```

**Benefits**:
- **Prompts are reproducible**: Same ontology → same prompt IR → same LLM behavior
- **Prompts have versions**: Ontology version = prompt version
- **Prompts have receipts**: Cryptographic proof of generation
- **Prompts are testable**: Test ontology, not strings
- **Prompts don't drift**: Ontology is source of truth

---

## Prompt IR is CONSTRUCT-Shaped

### The No Moving Parts Principle

From [04-no-moving-parts.md](./04-no-moving-parts.md), we know:

> "CONSTRUCT queries emit pre-shaped intermediate representations. Templates become pure fold operations. No moving parts = determinism."

**This applies to prompts too.**

### Traditional Template Approach (Moving Parts)

```python
# Template recovers structure at runtime (non-deterministic)
prompt_template = """
Extract bond information from the document.

{% for field in fields %}  {# Runtime iteration over hash set #}
  {% if field.required %}  {# Runtime conditional #}
    - {{ field.name }} (required): {{ field.description }}
  {% endif %}
{% endfor %}
"""
```

**Problems**:
- `fields` iteration order is non-deterministic (hash set)
- Template makes runtime decisions (`if field.required`)
- Same ontology can produce different prompts (ordering changes)

### ggen CONSTRUCT Approach (No Moving Parts)

```sparql
# Stage μ₁: CONSTRUCT shapes the Prompt IR upfront
CONSTRUCT {
    :PromptInstruction_{{ ?field }} a :RequiredFieldInstruction ;
        :fieldName ?fieldLabel ;
        :fieldType ?rangeLabel ;
        :constraint ?constraintDesc ;
        :order ?fieldOrder .
}
WHERE {
    ?class a owl:Class ;
        rdfs:label "Bond" .
    ?field rdfs:domain ?class ;
        rdfs:label ?fieldLabel ;
        rdfs:range ?range .
    ?range rdfs:label ?rangeLabel .

    # Extract constraints
    OPTIONAL {
        ?class rdfs:subClassOf ?restriction .
        ?restriction owl:onProperty ?field ;
                     owl:minCardinality ?minCard .
        BIND(CONCAT("Required (min: ", STR(?minCard), ")") as ?constraintDesc)
    }

    # Deterministic ordering
    BIND(xsd:integer(REPLACE(STR(?field), ".*#field_", "")) as ?fieldOrder)
}
ORDER BY ?fieldOrder  # ← DETERMINISTIC
```

**Result**: Prompt IR is **pre-shaped, pre-ordered, pre-filtered**. Template just folds to text.

```tera
{# Stage μ₃: Pure fold (no decisions, no traversal) #}
Extract bond information from the document.

{% for instruction in prompt_instructions | sort(attribute="order") %}
- {{ instruction.fieldName }} ({{ instruction.fieldType }}): {{ instruction.constraint }}
{% endfor %}

All fields must satisfy constraints. Invalid data will be rejected.
```

**Key difference**:
- CONSTRUCT phase does **all structure recovery**
- Template phase does **zero decision making**
- Same ontology → same IR → same prompt (byte-for-byte)

---

## Deterministic Prompt Emission

### The Determinism Guarantee

**Theorem**: Given fixed ontology O and fixed templates T, ggen produces identical prompts P.

**Proof**:

```
P = emit(extract(O, T))

Where:
  extract(O, T) = CONSTRUCT queries on O using rules in T
  emit(IR)      = Fold IR to text via Tera templates

Since:
1. CONSTRUCT with ORDER BY is deterministic (SPARQL semantics)
2. RDF graph canonicalization ensures stable serialization
3. Tera fold over ordered arrays is pure (no I/O, no time, no random)

∴ P is deterministic. ∎
```

### Concrete Example: Bond Extraction Prompts

**Input Ontology** (`bond.ttl`):

```turtle
@prefix : <http://example.com/bond#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:Bond a owl:Class ;
    rdfs:label "Bond" ;
    rdfs:comment "A debt security" .

:hasISIN a owl:DatatypeProperty ;
    rdfs:domain :Bond ;
    rdfs:range xsd:string ;
    rdfs:label "ISIN" .

:hasCouponRate a owl:DatatypeProperty ;
    rdfs:domain :Bond ;
    rdfs:range xsd:decimal ;
    rdfs:label "Coupon Rate" .

# Constraints
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:cardinality 1
] , [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:allValuesFrom [
        owl:onDatatype xsd:string ;
        owl:withRestrictions ([ xsd:length 12 ])
    ]
] .
```

**CONSTRUCT Query** (μ₁ Normalization):

```sparql
CONSTRUCT {
    :BondPrompt a :PromptSpec ;
        :taskDescription "Extract structured bond data from financial documents" ;
        :hasInstruction :Instruction_ISIN , :Instruction_CouponRate .

    :Instruction_ISIN a :FieldInstruction ;
        :fieldName "isin" ;
        :fieldType "string" ;
        :required true ;
        :constraint "Exactly 12 characters" ;
        :order 1 .

    :Instruction_CouponRate a :FieldInstruction ;
        :fieldName "coupon_rate" ;
        :fieldType "decimal" ;
        :required false ;
        :constraint "Between 0 and 20" ;
        :order 2 .
}
WHERE {
    # Query extracts field instructions from ontology
    # ORDER BY ensures deterministic field ordering
}
ORDER BY ?fieldName
```

**Generated Prompt IR** (JSON from SELECT):

```json
{
  "task_description": "Extract structured bond data from financial documents",
  "instructions": [
    {
      "field_name": "isin",
      "field_type": "string",
      "required": true,
      "constraint": "Exactly 12 characters",
      "order": 1
    },
    {
      "field_name": "coupon_rate",
      "field_type": "decimal",
      "required": false,
      "constraint": "Between 0 and 20",
      "order": 2
    }
  ]
}
```

**Emitted Prompt** (μ₃ Fold):

```
You are a financial data extraction system following FIBO standards.

Task: Extract structured bond data from financial documents

REQUIRED fields:
- isin (string): Exactly 12 characters
- coupon_rate (decimal): Between 0 and 20

Return JSON with this exact structure:
{
    "isin": "US0378331005",
    "coupon_rate": 4.5
}

CRITICAL: All constraints must be satisfied. Invalid data will be rejected.
```

**Verification**:

```bash
# Generate prompt from ontology
ggen prompt compile bond.ttl --output bond_prompt.txt

# Compute hash
sha256sum bond_prompt.txt
# Output: 7f3a9b2c4d1e8f5a... (deterministic)

# Regenerate a month later
ggen prompt compile bond.ttl --output bond_prompt_v2.txt

# Hashes match
sha256sum bond_prompt_v2.txt
# Output: 7f3a9b2c4d1e8f5a... (identical!)
```

**Same ontology → same prompt (byte-for-byte)**

---

## LLMs as Replaceable Stations

### The Manufacturing Analogy

Traditional software treats LLMs like artisans:

```
[Hand-crafted Prompt] → [Artisan LLM] → [Unpredictable Output]
                         (GPT-4, Claude, etc.)
                         Cannot be replaced without retraining
```

ggen treats LLMs like **interchangeable manufacturing stations**:

```
[Compiled Prompt IR] → [Station LLM] → [Validated Output]
                       (Any LLM that can execute IR)
                       Swap stations without recompilation
```

### LLM as CPU Analogy

```
Assembly Code      →  CPU (Intel, AMD, ARM)  →  Machine Execution
Prompt IR          →  LLM (GPT, Claude, LLaMA)  →  Prompt Execution
```

**Key properties**:

| Aspect | CPUs | LLMs (in ggen) |
|--------|------|----------------|
| **Input** | Machine code | Compiled prompt IR |
| **Execution** | Deterministic | Constraint-guided |
| **Output** | Register state | Validated JSON |
| **Replaceable** | Yes (same ISA) | Yes (same IR format) |
| **Verification** | Test suite | Constraint calculus |

### Practical Example: LLM Swapping

**Scenario**: Start with GPT-4, switch to Claude Opus, no code changes

```rust
// Define the extraction task (compiled from ontology)
let prompt_ir = PromptIR::from_ontology("bond.ttl")?;

// Execution is LLM-agnostic
let llm_client: Box<dyn LLMClient> = if use_openai {
    Box::new(OpenAIClient::new(api_key))
} else {
    Box::new(ClaudeClient::new(api_key))
};

// Execute compiled prompt on ANY LLM
let result = llm_client.execute(&prompt_ir).await?;

// Validation is the same regardless of LLM
let valid_bond = ConstraintSet::validate(result)?;
```

**Benefits**:
1. **Vendor independence**: Not locked to OpenAI or Anthropic
2. **A/B testing**: Compare LLM performance with same prompts
3. **Cost optimization**: Route to cheapest LLM that meets SLOs
4. **Fallback strategies**: If GPT-4 down, use Claude automatically
5. **Local models**: Swap in Ollama/Llama for privacy-sensitive tasks

**The key**: Prompt IR decouples task specification from LLM execution.

---

## Prompt Drift Elimination

### The Drift Problem

**Traditional approach**: Prompts are strings stored in code

```python
# prompt.py (Week 1)
BOND_EXTRACTION_PROMPT = "Extract the bond ISIN"

# prompt.py (Week 3, after "improvements")
BOND_EXTRACTION_PROMPT = "Extract the bond ISIN (12 chars)"

# prompt.py (Week 6, after team change)
BOND_EXTRACTION_PROMPT = "You are a financial expert. Extract ISIN..."
```

**Drift symptoms**:
- Original requirement: "Extract ISIN"
- Actual prompt: Unrecognizable after 10 iterations
- No audit trail of changes
- No way to recover original semantics

### ggen Solution: Prompts are Derived, Not Written

```
Source of Truth:       bond.ttl (ontology)
    ↓ (μ₁ CONSTRUCT)
Prompt IR:             bond_prompt_ir.json (generated)
    ↓ (μ₃ Emit)
Executable Prompt:     bond_prompt.txt (generated)
    ↓ (μ₄ Format)
Final Prompt:          bond_prompt_formatted.txt (generated)
```

**Every prompt is:**
1. **Generated from ontology** (not hand-written)
2. **Versioned with ontology** (git tracks ontology, not prompts)
3. **Reproducible** (re-run pipeline → same prompt)
4. **Auditable** (receipt proves generation)

### Example: Preventing Drift

**Month 1**: Initial requirement

```turtle
# bond_v1.ttl
:hasISIN a owl:DatatypeProperty ;
    rdfs:label "ISIN" .
```

```bash
ggen prompt compile bond_v1.ttl
# Generated prompt: "Extract ISIN"
```

**Month 3**: Add constraint

```turtle
# bond_v2.ttl (changed)
:hasISIN a owl:DatatypeProperty ;
    rdfs:label "ISIN" .

:Bond rdfs:subClassOf [
    owl:onProperty :hasISIN ;
    owl:allValuesFrom [
        owl:withRestrictions ([ xsd:length 12 ])
    ]
] .
```

```bash
ggen prompt compile bond_v2.ttl
# Generated prompt: "Extract ISIN (exactly 12 characters)"
```

**Month 6**: Team change, new engineer

```bash
# New engineer doesn't touch prompts, only ontology
git log bond.ttl
# Sees full history of requirement changes

ggen prompt compile bond_v2.ttl
# Same prompt as Month 3 (no drift!)
```

**Drift is impossible because**:
1. Prompts are not edited, they're generated
2. Changes go through ontology (versioned, reviewed)
3. Pipeline is deterministic (same input → same output)
4. Receipts prove provenance

---

## Designing Prompt Ontologies

### Core Principles

1. **Separation of concerns**: Domain model vs. prompt template
2. **Constraint-first**: Constraints drive prompt instructions
3. **Compositional**: Prompts compose from reusable patterns
4. **Semantic grounding**: Link to standard ontologies (FIBO, Schema.org)

### Prompt Ontology Structure

```turtle
@prefix prompt: <http://ggen.ai/prompt#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# ============================================================================
# PROMPT SPECIFICATION CLASS
# ============================================================================

prompt:PromptSpec a owl:Class ;
    rdfs:label "Prompt Specification" ;
    rdfs:comment """
    A compiled prompt specification derived from domain ontology.

    Consists of:
    - Task description (what to extract)
    - Field instructions (what fields with what constraints)
    - Examples (few-shot learning)
    - Validation rules (constraint enforcement)
    """ .

# ============================================================================
# CORE PROPERTIES
# ============================================================================

prompt:taskDescription a owl:DatatypeProperty ;
    rdfs:domain prompt:PromptSpec ;
    rdfs:range xsd:string ;
    rdfs:label "task description" ;
    rdfs:comment "High-level description of extraction task" .

prompt:hasInstruction a owl:ObjectProperty ;
    rdfs:domain prompt:PromptSpec ;
    rdfs:range prompt:FieldInstruction ;
    rdfs:label "has instruction" .

prompt:hasExample a owl:ObjectProperty ;
    rdfs:domain prompt:PromptSpec ;
    rdfs:range prompt:Example ;
    rdfs:label "has example" .

# ============================================================================
# FIELD INSTRUCTION
# ============================================================================

prompt:FieldInstruction a owl:Class ;
    rdfs:label "Field Instruction" ;
    rdfs:comment "Instruction for extracting a single field" .

prompt:fieldName a owl:DatatypeProperty ;
    rdfs:domain prompt:FieldInstruction ;
    rdfs:range xsd:string .

prompt:fieldType a owl:DatatypeProperty ;
    rdfs:domain prompt:FieldInstruction ;
    rdfs:range xsd:string .

prompt:required a owl:DatatypeProperty ;
    rdfs:domain prompt:FieldInstruction ;
    rdfs:range xsd:boolean .

prompt:constraint a owl:DatatypeProperty ;
    rdfs:domain prompt:FieldInstruction ;
    rdfs:range xsd:string .

prompt:order a owl:DatatypeProperty ;
    rdfs:domain prompt:FieldInstruction ;
    rdfs:range xsd:integer .

# ============================================================================
# EXAMPLE (Few-Shot Learning)
# ============================================================================

prompt:Example a owl:Class ;
    rdfs:label "Prompt Example" ;
    rdfs:comment "Example input/output pair for few-shot learning" .

prompt:exampleInput a owl:DatatypeProperty ;
    rdfs:domain prompt:Example ;
    rdfs:range xsd:string .

prompt:exampleOutput a owl:DatatypeProperty ;
    rdfs:domain prompt:Example ;
    rdfs:range xsd:string .
```

### CONSTRUCT Rules for Prompt Generation

```sparql
# Rule 1: Generate task description from class
CONSTRUCT {
    :PromptSpec_{{ ?class }} a prompt:PromptSpec ;
        prompt:taskDescription ?taskDesc .
}
WHERE {
    ?class a owl:Class ;
           rdfs:label ?classLabel ;
           rdfs:comment ?classComment .
    BIND(CONCAT("Extract structured ", ?classLabel, " data from text. ", ?classComment) as ?taskDesc)
}

# Rule 2: Generate field instructions from properties
CONSTRUCT {
    :PromptSpec_{{ ?class }} prompt:hasInstruction :Instruction_{{ ?prop }} .

    :Instruction_{{ ?prop }} a prompt:FieldInstruction ;
        prompt:fieldName ?fieldName ;
        prompt:fieldType ?fieldType ;
        prompt:required ?isRequired ;
        prompt:constraint ?constraintDesc ;
        prompt:order ?fieldOrder .
}
WHERE {
    ?prop rdfs:domain ?class ;
          rdfs:label ?fieldName ;
          rdfs:range ?range .

    # Extract field type
    BIND(REPLACE(STR(?range), ".*#", "") as ?fieldType)

    # Check if required (has minCardinality >= 1)
    OPTIONAL {
        ?class rdfs:subClassOf ?restriction .
        ?restriction owl:onProperty ?prop ;
                     owl:minCardinality ?minCard .
        FILTER(?minCard >= 1)
    }
    BIND(BOUND(?minCard) as ?isRequired)

    # Extract constraint description
    OPTIONAL {
        ?class rdfs:subClassOf ?restriction2 .
        ?restriction2 owl:onProperty ?prop ;
                      owl:allValuesFrom ?datatype .
        ?datatype owl:withRestrictions ?restrictionList .
        # Parse restrictions (length, pattern, min/max)
    }
    BIND(COALESCE(?constraintDesc, "No constraints") as ?finalConstraint)

    # Deterministic ordering
    BIND(xsd:integer(REPLACE(STR(?prop), ".*#", "")) as ?fieldOrder)
}
ORDER BY ?class ?fieldOrder
```

### Template for Prompt Emission

```tera
{# templates/prompt.tera #}
{# Stage μ₃: Fold prompt IR to text #}

You are a {{ domain }} data extraction system following {{ standard }} standards.

Task: {{ task_description }}

{% if required_fields | length > 0 %}
REQUIRED fields:
{% for field in required_fields | sort(attribute="order") %}
- {{ field.field_name }} ({{ field.field_type }}): {{ field.constraint }}
{% endfor %}
{% endif %}

{% if optional_fields | length > 0 %}
OPTIONAL fields:
{% for field in optional_fields | sort(attribute="order") %}
- {{ field.field_name }} ({{ field.field_type }}): {{ field.constraint }}
{% endfor %}
{% endif %}

Return JSON with this exact structure:
{
{% for field in all_fields | sort(attribute="order") %}
    "{{ field.field_name }}": {{ field.example_value }}{% if not loop.last %},{% endif %}
{% endfor %}
}

{% if examples | length > 0 %}
Examples:
{% for example in examples %}
Input: {{ example.input }}
Output: {{ example.output }}
{% endfor %}
{% endif %}

CRITICAL: All constraints must be satisfied. Invalid data will be rejected.
```

---

## Complete Example: Narrative to Compiled Prompt

### Step 1: Narrative Requirement

**Business analyst writes**:

```
We need to extract bond information from financial documents.

Required fields:
- ISIN (international securities ID, 12 characters)
- Coupon rate (percentage between 0 and 20)
- Maturity date (ISO date format)
- Face value (positive number)

Optional fields:
- Issuer name
```

### Step 2: Formalize as OWL Ontology

**Domain expert creates** `bond_extraction.ttl`:

```turtle
@prefix : <http://example.com/bond#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Domain model
:Bond a owl:Class ;
    rdfs:label "Bond" ;
    rdfs:comment "A debt security issued by an organization" .

# Properties
:hasISIN a owl:DatatypeProperty ;
    rdfs:domain :Bond ;
    rdfs:range xsd:string ;
    rdfs:label "ISIN" ;
    rdfs:comment "International Securities Identification Number" .

:hasCouponRate a owl:DatatypeProperty ;
    rdfs:domain :Bond ;
    rdfs:range xsd:decimal ;
    rdfs:label "Coupon Rate" .

:hasMaturityDate a owl:DatatypeProperty ;
    rdfs:domain :Bond ;
    rdfs:range xsd:date ;
    rdfs:label "Maturity Date" .

:hasFaceValue a owl:DatatypeProperty ;
    rdfs:domain :Bond ;
    rdfs:range xsd:decimal ;
    rdfs:label "Face Value" .

:hasIssuerName a owl:DatatypeProperty ;
    rdfs:domain :Bond ;
    rdfs:range xsd:string ;
    rdfs:label "Issuer Name" .

# Constraints (ISIN: required, exactly 12 characters)
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:cardinality 1  # Required
] , [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:allValuesFrom [
        owl:onDatatype xsd:string ;
        owl:withRestrictions (
            [ xsd:length 12 ]
            [ xsd:pattern "^[A-Z]{2}[A-Z0-9]{9}[0-9]$" ]
        )
    ]
] .

# Constraints (Coupon rate: required, 0-20%)
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasCouponRate ;
    owl:cardinality 1  # Required
] , [
    a owl:Restriction ;
    owl:onProperty :hasCouponRate ;
    owl:allValuesFrom [
        owl:onDatatype xsd:decimal ;
        owl:withRestrictions (
            [ xsd:minInclusive 0.0 ]
            [ xsd:maxInclusive 20.0 ]
        )
    ]
] .

# Constraints (Maturity date: required)
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasMaturityDate ;
    owl:cardinality 1
] .

# Constraints (Face value: required, positive)
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasFaceValue ;
    owl:cardinality 1
] , [
    a owl:Restriction ;
    owl:onProperty :hasFaceValue ;
    owl:allValuesFrom [
        owl:onDatatype xsd:decimal ;
        owl:withRestrictions (
            [ xsd:minExclusive 0.0 ]
        )
    ]
] .

# Issuer name is optional (no cardinality constraint)
```

### Step 3: Generate Prompt IR (μ₁ + μ₂)

```bash
ggen prompt compile bond_extraction.ttl --output-ir bond_ir.json
```

**Generated IR** (`bond_ir.json`):

```json
{
  "spec_id": "bond_extraction_v1",
  "task_description": "Extract structured Bond data from text. A debt security issued by an organization",
  "domain": "financial",
  "standard": "custom",
  "instructions": [
    {
      "field_name": "isin",
      "field_type": "string",
      "required": true,
      "constraint": "Exactly 12 characters matching pattern ^[A-Z]{2}[A-Z0-9]{9}[0-9]$",
      "semantic_type": "http://example.com/bond#hasISIN",
      "order": 1
    },
    {
      "field_name": "coupon_rate",
      "field_type": "decimal",
      "required": true,
      "constraint": "Between 0.0 and 20.0 (inclusive)",
      "semantic_type": "http://example.com/bond#hasCouponRate",
      "order": 2
    },
    {
      "field_name": "maturity_date",
      "field_type": "date",
      "required": true,
      "constraint": "ISO 8601 date format (YYYY-MM-DD)",
      "semantic_type": "http://example.com/bond#hasMaturityDate",
      "order": 3
    },
    {
      "field_name": "face_value",
      "field_type": "decimal",
      "required": true,
      "constraint": "Positive number (> 0.0)",
      "semantic_type": "http://example.com/bond#hasFaceValue",
      "order": 4
    },
    {
      "field_name": "issuer_name",
      "field_type": "string",
      "required": false,
      "constraint": "None",
      "semantic_type": "http://example.com/bond#hasIssuerName",
      "order": 5
    }
  ],
  "examples": []
}
```

### Step 4: Emit Final Prompt (μ₃)

```bash
ggen prompt emit bond_ir.json --output bond_prompt.txt
```

**Generated Prompt** (`bond_prompt.txt`):

```
You are a financial data extraction system following custom standards.

Task: Extract structured Bond data from text. A debt security issued by an organization

REQUIRED fields:
- isin (string): Exactly 12 characters matching pattern ^[A-Z]{2}[A-Z0-9]{9}[0-9]$
- coupon_rate (decimal): Between 0.0 and 20.0 (inclusive)
- maturity_date (date): ISO 8601 date format (YYYY-MM-DD)
- face_value (decimal): Positive number (> 0.0)

OPTIONAL fields:
- issuer_name (string): None

Return JSON with this exact structure:
{
    "isin": "US0378331005",
    "coupon_rate": 4.5,
    "maturity_date": "2030-06-15",
    "face_value": 1000.00,
    "issuer_name": "Apple Inc."
}

CRITICAL: All constraints must be satisfied. Invalid data will be rejected.
```

### Step 5: Verify Determinism

```bash
# Generate hash of prompt
sha256sum bond_prompt.txt
# Output: a7f3c9b2d4e8f1a5...

# Regenerate from same ontology
ggen prompt compile bond_extraction.ttl --output bond_prompt_v2.txt

# Compare hashes
sha256sum bond_prompt_v2.txt
# Output: a7f3c9b2d4e8f1a5... (identical!)

# Prompts are byte-for-byte identical
diff bond_prompt.txt bond_prompt_v2.txt
# Output: (no differences)
```

### Step 6: Use in Production

```rust
use ggen_ai::prompt::PromptIR;
use ggen_ai::llm::LLMClient;
use ggen_ai::dspy::{Forward, ConstraintSet};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Load compiled prompt IR
    let prompt_ir = PromptIR::from_file("bond_ir.json")?;

    // Initialize LLM client (any LLM works)
    let llm = LLMClient::from_env()?;

    // Execute prompt on document
    let document = r#"
        Apple Inc. issued a 10-year bond with ISIN US0378331005.
        The bond has a face value of $1,000 and pays a 4.5% coupon rate.
        Maturity date is June 15, 2030.
    "#;

    let result = llm.execute(&prompt_ir, document).await?;

    // Validate against constraints (automatically)
    let valid_bond = ConstraintSet::from_ir(&prompt_ir).validate(result)?;

    println!("Extracted bond: {:#?}", valid_bond);
    // Output:
    // {
    //     "isin": "US0378331005",
    //     "coupon_rate": 4.5,
    //     "maturity_date": "2030-06-15",
    //     "face_value": 1000.0,
    //     "issuer_name": null
    // }

    Ok(())
}
```

**Key observations**:
1. **Prompt was never hand-written** - compiled from ontology
2. **Prompt is deterministic** - same ontology → same prompt
3. **Prompt is versioned** - ontology version = prompt version
4. **Prompt is validated** - constraints enforced automatically
5. **LLM is replaceable** - any LLM can execute same IR

---

## Integration with ggen Pipeline

### The Five-Stage Pipeline (μ₁-μ₅)

Prompt manufacturing integrates seamlessly with ggen's existing pipeline:

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROMPT MANUFACTURING PIPELINE                 │
└─────────────────────────────────────────────────────────────────┘

┌──────────────────┐
│  μ₁: Normalize   │  Input: OWL ontology
│                  │  CONSTRUCT: Extract classes, properties, restrictions
│  CONSTRUCT Rules │  Output: Prompt IR graph (RDF)
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│  μ₂: Extract     │  Input: Prompt IR graph
│                  │  SELECT: Flatten to JSON bindings
│  SELECT Queries  │  Output: Prompt IR (JSON)
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│  μ₃: Emit        │  Input: Prompt IR (JSON)
│                  │  Tera fold: IR → prompt text
│  Tera Templates  │  Output: Raw prompt
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│  μ₄: Format      │  Input: Raw prompt
│                  │  Format: Apply LLM-specific formatting
│  LLM Adapters    │  Output: Formatted prompt
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│  μ₅: Receipt     │  Input: Formatted prompt
│                  │  Hash: Generate cryptographic receipt
│  Receipt Gen     │  Output: Prompt + receipt
└──────────────────┘
```

### Example: ggen.toml Configuration

```toml
[prompt]
# Enable prompt manufacturing
enabled = true

# Source ontology
ontology = ".specify/bond_extraction.ttl"

# Target class to extract
target_class = "http://example.com/bond#Bond"

# Template for prompt emission
template = "templates/financial_extraction.tera"

# LLM-specific formatting
llm_format = "openai"  # or "anthropic", "ollama", etc.

# Output paths
output_ir = "generated/prompts/bond_ir.json"
output_prompt = "generated/prompts/bond_prompt.txt"
output_receipt = "generated/receipts/bond_prompt_receipt.json"

[prompt.construct_rules]
# Custom CONSTRUCT rules (optional)
task_description = "sparql/prompt_task.rq"
field_instructions = "sparql/prompt_fields.rq"
examples = "sparql/prompt_examples.rq"

[prompt.validation]
# Constraint validation
enabled = true
fail_on_violation = true
max_retries = 3
```

### CLI Integration

```bash
# Compile prompt from ontology
ggen prompt compile .specify/bond_extraction.ttl

# Output:
# [Receipt] μ₁ Normalize: ✓
#   - Loaded: 25 triples
#   - Classes: 1 (Bond)
#   - Properties: 5 (hasISIN, hasCouponRate, ...)
#   - Restrictions: 8
#   - Duration: 0.2s
#
# [Receipt] μ₂ Extract: ✓
#   - SELECT queries: 3 (task, fields, examples)
#   - Bindings: 5 field instructions
#   - Duration: 0.1s
#
# [Receipt] μ₃ Emit: ✓
#   - Template: financial_extraction.tera
#   - Prompt length: 487 characters
#   - Duration: 0.05s
#
# [Receipt] μ₄ Format: ✓
#   - LLM format: openai
#   - Formatted length: 521 characters
#   - Duration: 0.01s
#
# [Receipt] μ₅ Receipt: ✓
#   - Prompt hash: a7f3c9b2d4e8f1a5...
#   - Receipt saved: generated/receipts/bond_prompt_receipt.json
#   - Duration: 0.01s
#
# [Receipt] Total: ✓ 0.37s (generation) + 0.0s (validation) = 0.37s

# Validate prompt reproduces deterministically
ggen prompt verify generated/receipts/bond_prompt_receipt.json

# Output:
# ✓ Prompt hash matches receipt
# ✓ Regeneration produces identical prompt
# ✓ All constraints preserved
# ✓ Verification successful
```

---

## Performance and Verification

### Performance Metrics

**Target SLOs**:

| Stage | Target | Actual |
|-------|--------|--------|
| μ₁ Normalize | <1s | 0.2s |
| μ₂ Extract | <0.5s | 0.1s |
| μ₃ Emit | <0.2s | 0.05s |
| μ₄ Format | <0.1s | 0.01s |
| μ₅ Receipt | <0.1s | 0.01s |
| **Total** | **<2s** | **0.37s** |

**Benchmark**: Generate prompts for 100 different ontologies

```bash
cargo make bench --bench prompt_generation

# Results:
# prompt_compile/small_ontology    time:   [0.35s 0.37s 0.39s]
# prompt_compile/medium_ontology   time:   [0.82s 0.85s 0.88s]
# prompt_compile/large_ontology    time:   [2.1s  2.3s  2.5s]
#
# All within SLO targets (<2s for small/medium, <5s for large)
```

### Verification Checklist

- [ ] **Determinism**: Same ontology → same prompt (byte-for-byte)
- [ ] **Completeness**: All properties from ontology represented in prompt
- [ ] **Constraint preservation**: All OWL restrictions → prompt instructions
- [ ] **Ordering**: Field order is deterministic (ORDER BY in CONSTRUCT)
- [ ] **Formatting**: Prompt is valid for target LLM
- [ ] **Receipt**: Cryptographic hash verifies generation
- [ ] **Reproducibility**: Can regenerate prompt from receipt
- [ ] **Performance**: Generation completes within SLO (<2s)

### Test Suite

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prompt_compilation_deterministic() {
        // Arrange: Same ontology compiled twice
        let ontology = load_test_ontology("bond.ttl");

        // Act: Compile twice
        let prompt1 = compile_prompt(&ontology).unwrap();
        let prompt2 = compile_prompt(&ontology).unwrap();

        // Assert: Byte-for-byte identical
        assert_eq!(prompt1, prompt2);
        assert_eq!(hash(&prompt1), hash(&prompt2));
    }

    #[test]
    fn test_all_constraints_preserved() {
        // Arrange: Ontology with 8 constraints
        let ontology = load_test_ontology("bond_complex.ttl");
        let constraints = extract_constraints(&ontology);
        assert_eq!(constraints.len(), 8);

        // Act: Compile prompt
        let prompt_ir = compile_prompt_ir(&ontology).unwrap();

        // Assert: All constraints in IR
        let ir_constraints = prompt_ir.extract_constraints();
        assert_eq!(ir_constraints.len(), 8);

        for constraint in &constraints {
            assert!(ir_constraints.contains(constraint),
                    "Constraint {:?} not preserved", constraint);
        }
    }

    #[test]
    fn test_llm_format_compatibility() {
        // Arrange: Compiled prompt IR
        let prompt_ir = compile_prompt_ir(&load_test_ontology("bond.ttl")).unwrap();

        // Act: Format for different LLMs
        let openai_prompt = format_for_llm(&prompt_ir, LLMFormat::OpenAI).unwrap();
        let claude_prompt = format_for_llm(&prompt_ir, LLMFormat::Claude).unwrap();
        let ollama_prompt = format_for_llm(&prompt_ir, LLMFormat::Ollama).unwrap();

        // Assert: All formats valid
        assert!(validate_prompt(&openai_prompt, LLMFormat::OpenAI));
        assert!(validate_prompt(&claude_prompt, LLMFormat::Claude));
        assert!(validate_prompt(&ollama_prompt, LLMFormat::Ollama));
    }
}
```

---

## Summary

**Prompt manufacturing transforms prompts from mutable strings to compiled artifacts:**

| Aspect | Traditional Prompts | ggen Prompt Manufacturing |
|--------|---------------------|---------------------------|
| **Authoring** | Hand-written strings | Compiled from ontology |
| **Storage** | Code files | Generated (gitignored) |
| **Versioning** | String diffs | Ontology versions |
| **Reproducibility** | None | Perfect (deterministic) |
| **Drift** | Common | Impossible |
| **LLM Coupling** | Tight | Loose (replaceable) |
| **Validation** | Manual | Automatic (constraint calculus) |
| **Audit Trail** | None | Cryptographic receipts |

**Core equation**:

```
Prompt = μ(Ontology)

Where μ is the five-stage pipeline (μ₁-μ₅)
Same Ontology → Same Prompt (always)
```

**Key benefits**:

1. **Prompts are reproducible**: Same input → same output
2. **Prompts don't drift**: Source of truth is ontology, not strings
3. **LLMs are replaceable**: Prompt IR decouples task from execution
4. **Constraints are enforced**: Validation is automatic, not manual
5. **Audit trail exists**: Receipts prove generation

**Philosophy**:

> "You don't hand-write assembly. You shouldn't hand-write prompts. Compile them from specifications."

---

## Further Reading

- [01-regime-split.md](./01-regime-split.md) - CONSTRUCT vs SELECT/DO paradigms
- [04-no-moving-parts.md](./04-no-moving-parts.md) - IR architecture and determinism
- [10-packet-discipline.md](./10-packet-discipline.md) - Type-safe work orders
- [LLM-Construct Tutorial](/home/user/ggen/docs/tutorials/LLM_CONSTRUCT_TUTORIAL.md) - Complete implementation guide
- [LLM-Construct Implementation](/home/user/ggen/docs/LLM_CONSTRUCT_IMPLEMENTATION.md) - Technical deep dive

---

**Document Hash**: `sha256:TBD`
**Generated**: 2026-02-09
**Regime**: CONSTRUCT (this document is specification-driven)
