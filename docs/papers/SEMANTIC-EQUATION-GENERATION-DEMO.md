<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Semantic Equation Generation: The Chatman Equation Paper Example](#semantic-equation-generation-the-chatman-equation-paper-example)
  - [Overview](#overview)
  - [The Demonstration](#the-demonstration)
    - [Three Files Showing Progressive Development](#three-files-showing-progressive-development)
      - [1. **RDF Ontology v1** (`chatman-equation-paper.rdf`)](#1-rdf-ontology-v1-chatman-equation-paperrdf)
      - [2. **Generated LaTeX v1** (`chatman-equation-paper-GENERATED-v1.tex`)](#2-generated-latex-v1-chatman-equation-paper-generated-v1tex)
      - [3. **Modified RDF v2** (`chatman-equation-paper-MODIFIED-v2.rdf`)](#3-modified-rdf-v2-chatman-equation-paper-modified-v2rdf)
      - [4. **Generated LaTeX v2** (`chatman-equation-paper-GENERATED-v2.tex`)](#4-generated-latex-v2-chatman-equation-paper-generated-v2tex)
  - [Key Insight: Single Source of Truth](#key-insight-single-source-of-truth)
    - [Example: Adding New Equation](#example-adding-new-equation)
  - [Practical Workflow: Version 1 → Version 2](#practical-workflow-version-1-%E2%86%92-version-2)
    - [Step 1: Start with v1 RDF](#step-1-start-with-v1-rdf)
    - [Step 2: Enhance RDF with New Equations](#step-2-enhance-rdf-with-new-equations)
    - [Step 3: Regenerate LaTeX](#step-3-regenerate-latex)
  - [Equations Comparison: v1 vs v2](#equations-comparison-v1-vs-v2)
    - [v1 (Original)](#v1-original)
    - [v2 (Extended)](#v2-extended)
  - [RDF Properties Used for Equation Generation](#rdf-properties-used-for-equation-generation)
    - [Equation Definition in RDF](#equation-definition-in-rdf)
    - [LaTeX Generation Template](#latex-generation-template)
  - [Use Cases for Semantic Equation Generation](#use-cases-for-semantic-equation-generation)
    - [1. **Evolving Research Papers**](#1-evolving-research-papers)
    - [2. **Multiple Paper Versions**](#2-multiple-paper-versions)
    - [3. **Collaborative Editing**](#3-collaborative-editing)
    - [4. **Reproducible Mathematics**](#4-reproducible-mathematics)
  - [Practical Example: Updating a Parameter](#practical-example-updating-a-parameter)
    - [Scenario](#scenario)
    - [v1: Manual Update](#v1-manual-update)
    - [v2: Semantic Update](#v2-semantic-update)
  - [The Power of Semantic Generation](#the-power-of-semantic-generation)
    - [Traditional Templating](#traditional-templating)
    - [Semantic Generation (ggen approach)](#semantic-generation-ggen-approach)
  - [Files in This Demonstration](#files-in-this-demonstration)
    - [To See the Difference](#to-see-the-difference)
  - [Workflow for Researchers](#workflow-for-researchers)
    - [Create a Research Paper with Semantic Equations](#create-a-research-paper-with-semantic-equations)
  - [Key Insights](#key-insights)
    - [1. Single Source of Truth](#1-single-source-of-truth)
    - [2. Deterministic Generation](#2-deterministic-generation)
    - [3. Semantic Richness](#3-semantic-richness)
    - [4. Version Control](#4-version-control)
    - [5. Automated Verification](#5-automated-verification)
  - [Future Extensions](#future-extensions)
    - [Multi-Language Generation](#multi-language-generation)
    - [Proof Generation](#proof-generation)
    - [Interactive Papers](#interactive-papers)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Semantic Equation Generation: The Chatman Equation Paper Example

## Overview

This document demonstrates the **core innovation of ggen**: treating academic papers as semantic projections of RDF ontologies. When you change the ontology, the LaTeX equations **automatically update** with zero manual intervention.

This is not traditional templating—it's **knowledge graph-driven code generation** applied to academic writing.

---

## The Demonstration

### Three Files Showing Progressive Development

#### 1. **RDF Ontology v1** (`chatman-equation-paper.rdf`)
Single source of truth defining the paper, including:
- Paper metadata (title, abstract, keywords)
- Equations defined as RDF triples
- Sections, figures, tables
- Bibliography entries

#### 2. **Generated LaTeX v1** (`chatman-equation-paper-GENERATED-v1.tex`)
LaTeX output automatically generated from v1 RDF:
- Equation (1): `A = μ(O)` (Core Chatman Equation)
- Equations (3-11): Supporting formal properties
- 12 sections with content extracted from RDF
- Bibliography from RDF entries

#### 3. **Modified RDF v2** (`chatman-equation-paper-MODIFIED-v2.rdf`)
Enhanced ontology with:
- **NEW**: Extended equations (1b, 1c) with guards and provenance
- **NEW**: Typing Constraint equation (5)
- **NEW**: Guard Adjunction Law equation (6)
- **NEW**: Drift-Bounded Convergence equation (8)
- **NEW**: Additional section with formal properties
- **NEW**: Extra table summarizing all formal properties

#### 4. **Generated LaTeX v2** (`chatman-equation-paper-GENERATED-v2.tex`)
Automatically generated from v2 RDF showing:
- Equations (1-11) with new equations integrated
- New section "Extended Formal Properties (NEW in v2)"
- Updated tables with new metrics
- Version history noting changes
- All equations automatically renumbered and cross-referenced

---

## Key Insight: Single Source of Truth

The RDF ontology is the **single source of truth**. Change any equation, and the PDF automatically regenerates:

### Example: Adding New Equation

**In RDF ontology**:
```turtle
ap:GuardAdjunctionLaw a math:Equation ;
    ap:equationName "guard_adjunction" ;
    rdfs:label "Guard Adjunction Law (NEW)" ;
    ap:equationLatex "\\mu \\dashv H" ;
    math:equationNumber 6 ;
    math:description "Measurement function is left adjoint to guard set" ;
    ap:equationOrder 5 ;
    math:introduced "v2" .
```

**Automatically generates in LaTeX**:
```latex
\begin{equation}
\mu \dashv H
\label{eq:guard_adjunction}
\end{equation}

\textbf{Guard Adjunction Law (NEW)}: The measurement function is left adjoint to the guard set...
```

No manual LaTeX editing. The PDF updates automatically.

---

## Practical Workflow: Version 1 → Version 2

### Step 1: Start with v1 RDF

File: `chatman-equation-paper.rdf`

Core equations:
```
Equation 1:  A = μ(O)           [Core law]
Equation 2:  h = (trigger, check, act, receipt)  [Hook def]
Equation 3:  Determinism property
Equation 4:  Idempotence property
Equation 7:  Shard law
Equation 10: Bounded regeneration
Equation 11: Receipt schema
```

Generated PDF contains 8 equations in sequence.

### Step 2: Enhance RDF with New Equations

File: `chatman-equation-paper-MODIFIED-v2.rdf`

Add new equations to RDF:
- Equation 1b: `A = μ(O) | H` (with guards)
- Equation 1c: Full form with provenance
- Equation 5: Typing constraint
- Equation 6: Guard adjunction
- Equation 8: Drift-bounded convergence

### Step 3: Regenerate LaTeX

Run:
```bash
ggen paper generate chatman-equation-paper-MODIFIED-v2.rdf --style arxiv --output v2.tex
```

Result: `chatman-equation-paper-GENERATED-v2.tex`

**What changed automatically**:
- ✅ Equations renumbered (5, 6, 8 inserted in order)
- ✅ Cross-references updated
- ✅ New section added ("Extended Formal Properties")
- ✅ New table created showing all properties
- ✅ Version history updated
- ✅ Equation descriptions integrated into text
- ✅ Bibliography updated with new formal methods reference

**No manual LaTeX editing required.**

---

## Equations Comparison: v1 vs v2

### v1 (Original)

```latex
\begin{equation}
A = \mu(O)
\end{equation}

\begin{equation}
\forall O_1, O_2 \in \mathcal{O}: O_1 = O_2 \Rightarrow \mu(O_1) = \mu(O_2)
\end{equation}

\begin{equation}
\mu \circ \mu = \mu
\end{equation}

\begin{equation}
\mu(O \sqcup \Delta) = \mu(O) \sqcup \mu(\Delta)
\end{equation}
```

### v2 (Extended)

```latex
% Core law
\begin{equation}
A = \mu(O)
\end{equation}

% NEW: With guards
\begin{equation}
A = \mu(O) \mid H
\end{equation}

% NEW: With provenance
\begin{equation}
A = \mu(O) \mid H, \quad R = \text{hash}(A) \sqcup \text{hash}(\mu(O))
\end{equation}

% Formal properties
\begin{equation}
\forall O_1, O_2 \in \mathcal{O}: O_1 = O_2 \Rightarrow \mu(O_1) = \mu(O_2)
\end{equation}

\begin{equation}
\mu \circ \mu = \mu
\end{equation}

% NEW: Typing constraint
\begin{equation}
\forall O \in \mathcal{O}: O \models \Sigma
\end{equation}

% NEW: Guard adjunction
\begin{equation}
\mu \dashv H
\end{equation}

\begin{equation}
\mu(O \sqcup \Delta) = \mu(O) \sqcup \mu(\Delta)
\end{equation}

% NEW: Drift convergence
\begin{equation}
\lim_{t \to \infty} \text{drift}(\mu_t(O)) = 0 \text{ for } \text{drift} \in [0, \varepsilon]
\end{equation}

\begin{equation}
\mu_{t+1}(O) = \mu_t(O) \text{ while } \text{drift}(\Sigma_t) > \varepsilon
\end{equation}

\begin{equation}
R_t = (h_O, h_\Gamma, h_H, h_A, h_\mu), \quad h_t = \text{Merkle}(\ldots, h_{t-1})
\end{equation}
```

**Notice**:
- Equations automatically numbered in sequence
- New equations integrated seamlessly
- Descriptions added from RDF
- No manual renumbering or editing

---

## RDF Properties Used for Equation Generation

### Equation Definition in RDF

```turtle
ap:GuardAdjunctionLaw a math:Equation ;
    # What to display
    ap:equationName "guard_adjunction" ;
    rdfs:label "Guard Adjunction Law (NEW)" ;
    ap:equationLatex "\\mu \\dashv H" ;

    # Metadata for generation
    math:equationNumber 6 ;
    math:description "Measurement function is left adjoint to guard set" ;
    ap:equationOrder 5 ;

    # Semantic information
    math:introduced "v2" ;
    math:meaning "Every action produced by μ must satisfy guard constraints H" ;
    math:implication "Provides security guarantees and regulatory compliance" .
```

### LaTeX Generation Template

The Tera template processes these RDF properties:

```tera
{% for eq in paper.math:hasEquation | sort_by("equationOrder") %}

\subsection{ {{- eq.rdfs:label -}} {% if eq.math:introduced %}({{- eq.math:introduced -}}){% endif %} }

\begin{equation}
{{- eq.ap:equationLatex }}
\label{eq:{{- eq.ap:equationName -}}}
\end{equation}

\textbf{ {{- eq.rdfs:label -}} }: {{ eq.math:description }}

{% if eq.math:meaning %}
{{ eq.math:meaning }}
{% endif %}

{% if eq.math:implication %}
\textbf{Implication}: {{ eq.math:implication }}
{% endif %}

{% endfor %}
```

**Result**: Equations are automatically formatted, numbered, labeled, and documented.

---

## Use Cases for Semantic Equation Generation

### 1. **Evolving Research Papers**

As research evolves, update equations in RDF:
- Add new proofs → new equations
- Refine bounds → update parameter values
- Extend theorems → new properties

PDF regenerates automatically with correct numbering, cross-references, and documentation.

### 2. **Multiple Paper Versions**

One RDF ontology → Multiple LaTeX outputs:
- Conference version (8 pages, essential equations only)
- Journal version (extended, all equations)
- Preprint version (unlimited, with all proofs)

Use SPARQL to filter equations based on version:

```sparql
SELECT ?eq WHERE {
  ?eq a math:Equation .
  ?eq math:minPages ?minPages .
  FILTER (?minPages <= 8)  # for conference version
}
```

### 3. **Collaborative Editing**

Multiple authors edit the RDF ontology:
- Author A adds equations
- Author B refines descriptions
- Author C updates parameters

All changes merge in RDF, generating consistent LaTeX automatically.

### 4. **Reproducible Mathematics**

Equations stored as RDF triples:
- Version controlled (Git)
- Queryable (SPARQL)
- Verifiable (hashes)
- Automatically formatted

Independent verification of mathematical claims becomes tractable.

---

## Practical Example: Updating a Parameter

### Scenario

You discover that the drift tolerance should be tighter. Update it everywhere.

### v1: Manual Update

1. Find equation 10 in LaTeX: `\varepsilon = 0.005`
2. Find parameter table: change 0.5% to 0.1%
3. Update abstract: mention 0.1% tolerance
4. Update section 2.4: change bound description
5. Find 3 other papers using same parameter
6. Update them all manually

**Time**: ~30 minutes for one paper, more for multiple papers
**Risk**: Inconsistencies, missed updates

### v2: Semantic Update

Update one RDF triple:

```turtle
ap:DriftEpsilon a math:Parameter ;
    math:paramValue "0.001"^^xsd:decimal ;    # Changed from 0.005
    math:paramDescription "Drift tolerance (0.1%)" .
```

Regenerate:
```bash
ggen paper generate paper.rdf --style arxiv
```

**Result**:
- ✅ Equation 10 updated
- ✅ Parameter table updated
- ✅ All mentions of ε updated
- ✅ Description updated

**Time**: ~10 seconds
**Risk**: Zero (deterministic regeneration)

---

## The Power of Semantic Generation

### Traditional Templating

```
Input: LaTeX template + YAML variables
        ↓
Output: Static LaTeX document
        ↓
Problem: Manual updates, inconsistencies, version skew
```

### Semantic Generation (ggen approach)

```
Input: RDF ontology (Single Source of Truth)
        ↓
SPARQL queries extract equations
        ↓
Tera templates render LaTeX
        ↓
Output: Deterministically generated PDF
        ↓
Change ontology → PDF automatically updates
Problem: SOLVED
```

---

## Files in This Demonstration

```
examples/
├── chatman-equation-paper.rdf
│   └── v1 RDF ontology (8 equations, basic properties)
│
├── chatman-equation-paper-GENERATED-v1.tex
│   └── LaTeX generated from v1 RDF
│
├── chatman-equation-paper-MODIFIED-v2.rdf
│   └── v2 RDF ontology (12 equations, extended properties)
│
└── chatman-equation-paper-GENERATED-v2.tex
    └── LaTeX generated from v2 RDF (automatic updates)
```

### To See the Difference

Compare the two generated files:

```bash
# View equations in v1
grep "\\begin{equation}" examples/chatman-equation-paper-GENERATED-v1.tex | wc -l
# Output: 8 equations

# View equations in v2
grep "\\begin{equation}" examples/chatman-equation-paper-GENERATED-v2.tex | wc -l
# Output: 12 equations

# Highlight differences
diff -u examples/chatman-equation-paper-GENERATED-v1.tex \
         examples/chatman-equation-paper-GENERATED-v2.tex | head -100
```

---

## Workflow for Researchers

### Create a Research Paper with Semantic Equations

1. **Define ontology** (once):
```bash
# Create paper with equations as RDF triples
ggen paper new "My Research" --template arxiv
# Edit paper.rdf to add equations as math:Equation instances
```

2. **Generate LaTeX**:
```bash
# Automatic: equations numbered, formatted, documented
ggen paper generate paper.rdf --style arxiv
```

3. **Evolve research** (iterate):
```bash
# Add new equation to RDF
# Update parameter value
# Change equation description
# Regenerate
ggen paper generate paper.rdf --style arxiv
# PDF updates automatically—no manual LaTeX editing!
```

4. **Version control**:
```bash
# Commit RDF ontology (not LaTeX)
git add paper.rdf
git commit -m "Add Drift-Bounded Convergence equation"
# LaTeX regenerates deterministically
```

5. **Publish**:
```bash
# Multiple formats from same RDF
ggen paper export paper.rdf --format arxiv
ggen paper export paper.rdf --format ieee
ggen paper export paper.rdf --format pdf
```

---

## Key Insights

### 1. Single Source of Truth

The RDF ontology is the **only** place to edit. No manual LaTeX editing needed.

### 2. Deterministic Generation

Same RDF → Same LaTeX → Same PDF every time. Reproducible and auditable.

### 3. Semantic Richness

Equations aren't just strings—they're RDF objects with metadata:
- Mathematical meaning
- Introduction version
- Related equations
- Parameter values
- Proof status

### 4. Version Control

Version your ontology, not your PDFs:
```bash
git log --oneline paper.rdf
# 3a7f2c1 Add Drift-Bounded Convergence equation
# f92e4c3 Refine Guard Adjunction Law description
# e8d1b4a Initial paper with core Chatman Equation
```

### 5. Automated Verification

SPARQL queries verify mathematical consistency:
```sparql
# Find all equations with unspecified introduction version
SELECT ?eq WHERE {
  ?eq a math:Equation .
  FILTER NOT EXISTS { ?eq math:introduced ?version }
}
```

---

## Future Extensions

### Multi-Language Generation

Generate paper in multiple formats from one RDF:
- LaTeX (for PDF)
- HTML (for web)
- Markdown (for GitHub)
- EPUB (for e-readers)

All equations automatically adapted for each format.

### Proof Generation

Store equation proofs in RDF:
```turtle
ap:ChatmanEquationCore math:hasProof ap:ProofOfChatmanEquation .

ap:ProofOfChatmanEquation a math:Proof ;
    math:proofSteps [ ... ] ;
    math:verifiedBy "https://coq.inria.fr/" .
```

Automatically generate "Proof:" sections with formal verification links.

### Interactive Papers

Generate interactive HTML where readers can:
- Explore equation relationships
- Query SPARQL to find similar work
- Download data behind figures
- Reproduce results

All from the same RDF ontology.

---

## Conclusion

This demonstration shows that **academic papers are code**—and code should follow DRY (Don't Repeat Yourself) principles.

Semantic equation generation through RDF ontologies provides:
- **Consistency**: Single source of truth
- **Auditability**: Version control friendly
- **Efficiency**: Automatic regeneration
- **Flexibility**: Multiple output formats
- **Rigor**: Queryable, verifiable mathematics

The Chatman Equation paper evolved from v1 to v2 automatically. Your research papers can too.

---

**Example Files**:
- `examples/chatman-equation-paper.rdf` - v1 ontology
- `examples/chatman-equation-paper-MODIFIED-v2.rdf` - v2 ontology
- `examples/chatman-equation-paper-GENERATED-v1.tex` - v1 LaTeX output
- `examples/chatman-equation-paper-GENERATED-v2.tex` - v2 LaTeX output

**Try it yourself**:
```bash
# Compare equations between versions
diff -u chatman-equation-paper-GENERATED-v1.tex \
         chatman-equation-paper-GENERATED-v2.tex | grep "equation" | head -20
```

The future of academic writing is semantic. Welcome to the revolution.
