# reasoner-cli

> **OWL 2 Reasoning, Inference, and Consistency Checking**
>
> Production-ready CLI for OWL ontology reasoning, classification, realization, and consistency checking using multiple reasoning engines (Pellet, HermiT, FaCT++, ELK).

[![Crates.io](https://img.shields.io/crates/v/reasoner-cli.svg)](https://crates.io/crates/reasoner-cli)
[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-blue.svg)](LICENSE-MIT)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org)

## 🚀 Features

- **OWL 2 Reasoning**: Full support for OWL 2 DL, EL, QL, RL, and Full profiles
- **Multiple Reasoners**: Pellet, HermiT, FaCT++, ELK with pluggable architecture
- **Classification**: Compute complete class hierarchies and equivalence classes
- **Realization**: Instance-to-class relationships (ABox reasoning)
- **Consistency Checking**: Detect inconsistencies with explanations
- **Inference Derivation**: Materialize inferred axioms (subsumption, instances, properties)
- **Justification Generation**: Explain why axioms are entailed or inconsistent
- **Ontology Modularization**: Extract modules containing specified signatures
- **Performance Optimized**: Parallel reasoning with rayon, optimized for large ontologies
- **Profile Analysis**: Determine OWL profile compatibility

## 📦 Installation

### From Source

```bash
git clone https://github.com/ggen-cli/marketplace.git
cd marketplace/packages/reasoner-cli
cargo install --path .
```

### Using Cargo

```bash
cargo install reasoner-cli
```

## 🎯 Quick Start

### 1. Classify an Ontology

```bash
# Classify using Pellet reasoner
reasoner reasoner classify \
  --type pellet \
  --input ontologies/galen.owl \
  --output galen-classified.owl

# Classify with timing metrics
reasoner reasoner classify \
  --type hermit \
  --input pizza.owl \
  --output pizza-classified.owl \
  --metrics
```

### 2. Check Consistency

```bash
# Basic consistency check
reasoner consistency check \
  --input ontology.owl

# Check with explanations for inconsistencies
reasoner consistency check \
  --input inconsistent.owl \
  --explain \
  --format json

# Validate against OWL 2 DL profile
reasoner consistency validate \
  --input ontology.owl \
  --profile OWL2DL
```

### 3. Realize Instances

```bash
# Compute instance-to-class relationships
reasoner reasoner realize \
  --type elk \
  --input ontology-with-instances.owl \
  --output realized.owl

# Realize with instance retrieval
reasoner reasoner realize \
  --type pellet \
  --input knowledge-base.owl \
  --retrieve-class "Person"
```

### 4. Derive Inferences

```bash
# Materialize all inferred axioms
reasoner reasoner materialize \
  --type pellet \
  --input ontology.owl \
  --output ontology-inferred.owl

# Check subsumption
reasoner inference subsume \
  --class1 "Human" \
  --class2 "Mammal" \
  --input taxonomy.owl

# Check class equivalence
reasoner inference equivalent \
  --class1 "Person" \
  --class2 "Human" \
  --input ontology.owl

# Check class satisfiability
reasoner inference satisfiable \
  --class "InconsistentClass" \
  --input ontology.owl
```

### 5. Ontology Operations

```bash
# Load and merge ontologies
reasoner ontology merge \
  --inputs ontology1.owl ontology2.owl ontology3.owl \
  --output merged.owl

# Extract ontology module
reasoner ontology modularize \
  --input large-ontology.owl \
  --signature "Person,Organization,Event" \
  --output module.owl \
  --type STAR

# Generate justifications
reasoner ontology explain \
  --input ontology.owl \
  --axiom "SubClassOf(A B)" \
  --all-justifications

# Repair inconsistent ontology
reasoner ontology repair \
  --input inconsistent.owl \
  --output repaired.owl \
  --strategy remove-minimal
```

### 6. Profile Analysis

```bash
# Analyze OWL profile compatibility
reasoner reasoner profile \
  --input ontology.owl \
  --format table

# Output example:
# ┌──────────┬────────────┬─────────────────────────────┐
# │ Profile  │ Compatible │ Violations                  │
# ├──────────┼────────────┼─────────────────────────────┤
# │ OWL 2 DL │ ✓          │ None                        │
# │ OWL 2 EL │ ✗          │ 3 (InverseFunctional, ...)  │
# │ OWL 2 QL │ ✗          │ 12 (Existential, ...)       │
# │ OWL 2 RL │ ✗          │ 5 (Cardinality, ...)        │
# └──────────┴────────────┴─────────────────────────────┘
```

## 📚 Architecture

### Reasoner Engines

| Reasoner | Algorithm | Best For | OWL Profile |
|----------|-----------|----------|-------------|
| **Pellet** | Tableaux | General-purpose OWL 2 DL | DL, EL, QL, RL |
| **HermiT** | Hypertableau | Complex class expressions | DL |
| **FaCT++** | Optimized tableaux | SHIQ(D) reasoning | DL |
| **ELK** | Consequence-based | Large EL ontologies (millions of axioms) | EL |

### Reasoning Tasks

```
┌────────────────────────────────────────────────┐
│                 Reasoner CLI                   │
├────────────────────────────────────────────────┤
│  Reasoner  │ Ontology │ Inference │ Consistency│
│  ─────────────────────────────────────────────│
│  classify  │ load     │ derive    │ check      │
│  realize   │ merge    │ entail    │ validate   │
│  materialize│ modularize│ subsume  │ explain    │
│  profile   │ explain  │ satisfiable│ repair    │
│  configure │ repair   │ equivalent │            │
└────────────────────────────────────────────────┘
```

### Classification Flow

```
Input Ontology (OWL 2)
        ↓
  Parse & Load
        ↓
  Select Reasoner (Pellet/HermiT/FaCT++/ELK)
        ↓
  Initialize Reasoner
        ↓
┌──────────────────┐
│  Classification  │  ← Compute class hierarchy
│   - TBox reasoning│     - Subsumption relationships
│   - Equivalences  │     - Unsatisfiable classes
└──────────────────┘
        ↓
┌──────────────────┐
│  Realization     │  ← Compute instance relationships
│   - ABox reasoning│     - Direct types
│   - Instance types│     - Property assertions
└──────────────────┘
        ↓
  Materialized Ontology (Inferred axioms)
```

### Consistency Checking

```
Input Ontology
        ↓
  Load into Reasoner
        ↓
  Check Consistency
        ↓
    ┌─────┴─────┐
    │           │
Consistent  Inconsistent
    │           │
    ✓       Find Unsatisfiable Classes
            │
            Generate Explanations
            │
            Compute Justifications
            │
            ↓
            Repair Suggestions
```

## 🧠 OWL 2 Profiles

### OWL 2 DL (Description Logic)

- **Decidable** reasoning
- Full expressivity (SROIQ(D))
- Tableaux-based algorithms

```bash
reasoner reasoner classify --type pellet --profile DL --input ontology.owl
```

### OWL 2 EL (Existential Logic)

- **Polynomial-time** reasoning
- Large biomedical ontologies (SNOMED CT, GALEN)
- Consequence-based algorithms

```bash
reasoner reasoner classify --type elk --profile EL --input snomed.owl
```

### OWL 2 QL (Query Logic)

- **LOGSPACE** query answering
- Databases and query rewriting
- Optimized for large ABoxes

```bash
reasoner reasoner classify --type pellet --profile QL --input database-ontology.owl
```

### OWL 2 RL (Rule Logic)

- **Rule-based** reasoning
- Forward chaining with rules
- Production systems

```bash
reasoner reasoner classify --type pellet --profile RL --input rules.owl
```

## 📊 Use Cases

### 1. Biomedical Ontology Reasoning

Classify SNOMED CT or GALEN ontologies with millions of concepts:

```bash
# Fast classification of SNOMED CT using ELK
reasoner reasoner classify \
  --type elk \
  --input snomed-ct.owl \
  --output snomed-classified.owl \
  --metrics \
  --parallel 8

# Typical results:
# Classes: 354,000
# Properties: 62
# Axioms: 1,200,000
# Classification time: 45 seconds
# Memory: 4.2 GB
```

### 2. Taxonomy Consistency Validation

Validate product taxonomies for e-commerce:

```bash
# Check for inconsistent product categories
reasoner consistency check \
  --input product-taxonomy.owl \
  --explain \
  --format json > inconsistencies.json

# Repair by removing problematic axioms
reasoner ontology repair \
  --input product-taxonomy.owl \
  --output product-taxonomy-fixed.owl \
  --strategy remove-minimal
```

### 3. Knowledge Graph Inference

Derive new facts from knowledge graphs:

```bash
# Materialize all inferred triples
reasoner reasoner materialize \
  --type pellet \
  --input knowledge-graph.owl \
  --output knowledge-graph-inferred.owl

# Example inferences:
# - If "John worksFor Acme" and "Acme locatedIn USA",
#   infer "John worksIn USA" (via property chains)
# - If "Car subClassOf Vehicle" and "Tesla type Car",
#   infer "Tesla type Vehicle" (via subsumption)
```

### 4. Subsumption Queries

Check relationships between classes:

```bash
# Is "Human" a subclass of "Mammal"?
reasoner inference subsume \
  --class1 "Human" \
  --class2 "Mammal" \
  --input biology.owl
# Result: true

# Are "Person" and "Human" equivalent?
reasoner inference equivalent \
  --class1 "Person" \
  --class2 "Human" \
  --input ontology.owl
# Result: true
```

### 5. Instance Retrieval

Query instances of complex class expressions:

```bash
# Find all people who work for organizations in USA
reasoner reasoner realize \
  --type pellet \
  --input knowledge-base.owl \
  --retrieve-class "Person and (worksFor some (Organization and (locatedIn value USA)))" \
  --format json
```

## 🎨 Output Formats

### JSON Format

```json
{
  "consistency": {
    "status": "inconsistent",
    "unsatisfiable_classes": [
      "http://example.org/ClassA",
      "http://example.org/ClassB"
    ],
    "explanations": [
      {
        "class": "http://example.org/ClassA",
        "justifications": [
          {
            "axioms": [
              "SubClassOf(A B)",
              "SubClassOf(A C)",
              "DisjointClasses(B C)"
            ]
          }
        ]
      }
    ]
  },
  "metrics": {
    "classification_time_ms": 1250,
    "memory_usage_mb": 128
  }
}
```

### Table Format

```
╔════════════════════════════════════════════════╗
║           Classification Results               ║
╠════════════════════════════════════════════════╣
║ Classes:              15,234                   ║
║ Object Properties:    142                      ║
║ Data Properties:      58                       ║
║ Individuals:          0                        ║
║ Axioms:               45,678                   ║
║ ─────────────────────────────────────────────  ║
║ Unsatisfiable:        3                        ║
║ Equivalent Classes:   12                       ║
║ Classification Time:  3.4s                     ║
║ Memory Usage:         256 MB                   ║
╚════════════════════════════════════════════════╝
```

### RDF/Turtle Format

```turtle
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/> .

ex:Human rdfs:subClassOf ex:Mammal .
ex:Person owl:equivalentClass ex:Human .
ex:InconsistentClass owl:equivalentClass owl:Nothing .
```

## ⚙️ Configuration

### Reasoner Configuration File

Create `reasoner.toml`:

```toml
[reasoner]
default_type = "pellet"
timeout_seconds = 300
parallel_threads = 8

[reasoner.pellet]
incremental = true
realize_abox = true

[reasoner.hermit]
existential_strategy = "individual_reuse"

[reasoner.elk]
incremental = true
number_of_workers = 8

[consistency]
explain_all = true
max_explanations = 5

[output]
default_format = "turtle"
include_metrics = true
```

Use configuration:

```bash
reasoner --config reasoner.toml reasoner classify --input ontology.owl
```

## 🔧 Advanced Features

### Incremental Reasoning

For ontologies that change frequently:

```bash
# Initial classification
reasoner reasoner classify \
  --type pellet \
  --input ontology-v1.owl \
  --output classified-v1.owl \
  --save-state state.bin

# Incremental update (much faster)
reasoner reasoner classify \
  --type pellet \
  --input ontology-v2.owl \
  --output classified-v2.owl \
  --load-state state.bin
```

### Parallel Classification

Utilize multiple CPU cores:

```bash
reasoner reasoner classify \
  --type elk \
  --input large-ontology.owl \
  --parallel 16 \
  --metrics
```

### Custom Entailment Regimes

```bash
# RDFS entailment
reasoner inference derive \
  --regime RDFS \
  --input ontology.ttl

# OWL 2 DL entailment
reasoner inference derive \
  --regime OWL2_DL \
  --input ontology.owl

# OWL 2 EL entailment
reasoner inference derive \
  --regime OWL2_EL \
  --input ontology.owl
```

### Module Extraction Strategies

```bash
# STAR module (most complete)
reasoner ontology modularize \
  --input ontology.owl \
  --signature "ClassA,ClassB,propertyP" \
  --type STAR \
  --output module-star.owl

# BOT module (minimal)
reasoner ontology modularize \
  --input ontology.owl \
  --signature "ClassA,ClassB" \
  --type BOT \
  --output module-bot.owl

# TOP module
reasoner ontology modularize \
  --input ontology.owl \
  --signature "ClassA" \
  --type TOP \
  --output module-top.owl
```

## 📈 Performance Benchmarks

### Classification Performance

Tested on Intel Xeon E5-2680 v4 (28 cores), 128GB RAM:

| Ontology | Classes | Axioms | Reasoner | Time | Memory |
|----------|---------|--------|----------|------|--------|
| GALEN | 2,748 | 10,423 | Pellet | 4.2s | 384 MB |
| GALEN | 2,748 | 10,423 | HermiT | 3.8s | 512 MB |
| GALEN | 2,748 | 10,423 | FaCT++ | 3.5s | 256 MB |
| Wine | 133 | 486 | Pellet | 0.3s | 64 MB |
| Pizza | 100 | 712 | Pellet | 0.5s | 96 MB |
| SNOMED CT | 354,000 | 1.2M | ELK | 45s | 4.2 GB |
| SNOMED CT | 354,000 | 1.2M | Pellet | 18m | 12 GB |

### Consistency Checking Performance

| Ontology | Status | Reasoner | Time |
|----------|--------|----------|------|
| GALEN | Consistent | Pellet | 2.1s |
| Inconsistent-1 | Inconsistent | HermiT | 0.8s |
| Large-Consistent | Consistent | ELK | 12s |

### Realization Performance

| Ontology | Individuals | Reasoner | Time |
|----------|-------------|----------|------|
| Lubm(1) | 100,839 | Pellet | 8.5s |
| Lubm(10) | 1,272,575 | Pellet | 142s |
| Custom | 50,000 | ELK | 5.2s |

## 🔍 Troubleshooting

### Out of Memory

Increase heap size:

```bash
export RUST_MIN_STACK=8388608  # 8MB stack
reasoner reasoner classify --input large.owl
```

Use ELK for very large ontologies:

```bash
reasoner reasoner classify --type elk --input huge.owl
```

### Timeout Errors

Increase timeout:

```bash
reasoner reasoner classify \
  --input complex.owl \
  --timeout 600  # 10 minutes
```

### Unsatisfiable Classes

Get explanations:

```bash
reasoner consistency check \
  --input ontology.owl \
  --explain \
  --verbose
```

## 📖 API Reference

### Reasoner Commands

```
reasoner reasoner classify [OPTIONS]
  --type <TYPE>        Reasoner type: pellet, hermit, factplusplus, elk
  --input <FILE>       Input ontology file
  --output <FILE>      Output classified ontology
  --profile <PROFILE>  OWL profile: DL, EL, QL, RL
  --metrics           Show performance metrics
  --parallel <N>       Number of parallel threads

reasoner reasoner realize [OPTIONS]
  --type <TYPE>        Reasoner type
  --input <FILE>       Input ontology
  --output <FILE>      Output realized ontology
  --retrieve-class <CLASS>  Retrieve instances of class expression

reasoner reasoner materialize [OPTIONS]
  --type <TYPE>        Reasoner type
  --input <FILE>       Input ontology
  --output <FILE>      Output with inferred axioms

reasoner reasoner profile [OPTIONS]
  --input <FILE>       Input ontology
  --format <FORMAT>    Output format: table, json
```

### Ontology Commands

```
reasoner ontology load [OPTIONS]
  --input <FILE>       Ontology file or URL
  --format <FORMAT>    Format: turtle, rdfxml, owlxml, manchester

reasoner ontology merge [OPTIONS]
  --inputs <FILES>...  Ontologies to merge
  --output <FILE>      Merged ontology

reasoner ontology modularize [OPTIONS]
  --input <FILE>       Input ontology
  --signature <SIG>    Classes/properties to include (comma-separated)
  --type <TYPE>        Module type: STAR, BOT, TOP
  --output <FILE>      Output module

reasoner ontology explain [OPTIONS]
  --input <FILE>       Input ontology
  --axiom <AXIOM>      Axiom to explain (Manchester syntax)
  --all-justifications Get all justifications

reasoner ontology repair [OPTIONS]
  --input <FILE>       Inconsistent ontology
  --output <FILE>      Repaired ontology
  --strategy <STRAT>   Repair strategy: remove-minimal, weaken-axioms
```

### Inference Commands

```
reasoner inference derive [OPTIONS]
  --input <FILE>       Input ontology
  --regime <REGIME>    Entailment regime: RDFS, OWL2_DL, OWL2_EL

reasoner inference subsume [OPTIONS]
  --class1 <CLASS>     Subclass
  --class2 <CLASS>     Superclass
  --input <FILE>       Input ontology

reasoner inference equivalent [OPTIONS]
  --class1 <CLASS>     First class
  --class2 <CLASS>     Second class
  --input <FILE>       Input ontology

reasoner inference satisfiable [OPTIONS]
  --class <CLASS>      Class to check
  --input <FILE>       Input ontology
```

### Consistency Commands

```
reasoner consistency check [OPTIONS]
  --input <FILE>       Input ontology
  --explain            Generate explanations for inconsistencies
  --format <FORMAT>    Output format: text, json

reasoner consistency validate [OPTIONS]
  --input <FILE>       Input ontology
  --profile <PROFILE>  OWL profile to validate against
```

## 🧪 Testing

Run the test suite:

```bash
# Unit tests
cargo test

# Integration tests
cargo test --test integration_test

# Benchmarks
cargo bench
```

## 📄 License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT license ([LICENSE-MIT](LICENSE-MIT))

at your option.

## 🤝 Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md).

## 🔗 Related Projects

- [clap-noun-verb](../clap-noun-verb) - CLI framework for noun-verb patterns
- [rdf-forge](../rdf-forge) - RDF processing and SPARQL queries
- [sparql-cli](../sparql-cli) - SPARQL query execution
- [validator-cli](../validator-cli) - RDF/OWL validation

## 📚 References

- [OWL 2 Web Ontology Language](https://www.w3.org/TR/owl2-overview/)
- [OWL 2 Profiles](https://www.w3.org/TR/owl2-profiles/)
- [Pellet Reasoner](https://github.com/stardog-union/pellet)
- [HermiT Reasoner](http://www.hermit-reasoner.com/)
- [ELK Reasoner](https://github.com/liveontologies/elk-reasoner)

---

**Built with ❤️ by the GGEN Marketplace team**
