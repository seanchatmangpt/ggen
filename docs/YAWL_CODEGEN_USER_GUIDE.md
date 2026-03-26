# YAWL Codegen User Guide

Complete user documentation for ggen-yawl: YAWL workflow and Spring Boot application generation from industry ontologies.

**Version**: 0.1.0
**Last Updated**: 2026-03-26

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Configuration](#configuration)
4. [Usage Examples](#usage-examples)
5. [Output Structure](#output-structure)
6. [Customization](#customization)
7. [Troubleshooting](#troubleshooting)

---

## Installation

### Prerequisites

- **Rust** 1.91.1+
- **Cargo** (comes with Rust)
- **Java 17+** (for generated code compilation)
- **Maven 3.8+** (optional, for Maven integration)

### Add as Dependency

Add `ggen-yawl` to your `Cargo.toml`:

```toml
[dependencies]
ggen-yawl = "0.1"
```

### Via ggen CLI

The easiest way is through the ggen command-line tool:

```bash
# Install/update ggen
cargo install ggen-cli

# Create new YAWL generation project
ggen new --type yawl my-workflow-project
cd my-workflow-project
```

---

## Quick Start

### 5-Minute Workflow Generation

#### Step 1: Create Ontology File

Create `schema/domain.ttl` with your business model:

```turtle
@prefix fibo: <https://spec.edmcouncil.org/fibo/ontology/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Define workflow entity
fibo:LoanApplication a owl:Class ;
    rdfs:label "Loan Application" ;
    rdfs:comment "Customer loan application process" ;
    fibo:hasStatus fibo:LoanStatus .

fibo:LoanStatus a owl:Class ;
    owl:oneOf (fibo:StatusPending fibo:StatusApproved fibo:StatusRejected) .

# Define tasks as subclasses
fibo:CreditCheck a owl:Class ;
    rdfs:subClassOf fibo:LoanApplication ;
    rdfs:label "Credit Check" .

fibo:IdentityVerification a owl:Class ;
    rdfs:subClassOf fibo:LoanApplication ;
    rdfs:label "Identity Verification" .
```

#### Step 2: Generate Workflow

```bash
# Generate YAWL XML workflow
ggen yawl generate --ontology schema/domain.ttl --output-dir .ggen/yawl/

# Validate output
ggen yawl validate --file .ggen/yawl/workflow.yawl.xml
```

#### Step 3: Generate Spring Boot Application

```bash
# Generate Java code (Rules 3-10)
ggen yawl codegen --ontology schema/domain.ttl --output-dir src/main/java/

# File structure created:
# src/main/java/com/example/
# ├── entity/          (Rule 3: JPA Entities)
# ├── repository/      (Rule 4: Repositories)
# ├── dto/             (Rule 5: DTOs)
# ├── controller/      (Rule 6: REST Controllers)
# ├── service/         (Rule 8: Services)
# ├── hbm/             (Rule 9: Hibernate Mappings)
# ├── serializer/      (Rule 10: Jackson Serializers)
# └── enums/           (Rule 7: Enums)
```

#### Step 4: Compile and Run

```bash
# Compile generated Java
mvn compile

# Run tests
mvn test

# Start application
mvn spring-boot:run
```

---

## Configuration

### Command-Line Options

#### `ggen yawl generate`

Generate YAWL XML workflows from ontology:

```bash
ggen yawl generate [OPTIONS]
```

**Options**:

| Option | Default | Description |
|--------|---------|-------------|
| `--ontology <PATH>` | `schema/domain.ttl` | Path to RDF ontology file |
| `--output-dir <PATH>` | `.ggen/yawl/` | Output directory for YAWL XML |
| `--format <FORMAT>` | `xml` | Output format: `xml` or `erlang` |
| `--validate` | `true` | Validate generated YAWL XML |
| `--watch` | `false` | Watch mode: regenerate on file changes |
| `--timeout <MS>` | `30000` | Generation timeout in milliseconds |
| `--verbose` | `false` | Show detailed progress output |

**Exit Codes**:
- `0` - Success
- `1` - Ontology load error
- `3` - Generation error

**Example**:

```bash
ggen yawl generate \
  --ontology schema/fibo-loan.ttl \
  --output-dir .ggen/yawl/ \
  --format xml \
  --validate true \
  --timeout 60000 \
  --verbose
```

#### `ggen yawl codegen`

Generate Spring Boot Java code from ontology:

```bash
ggen yawl codegen [OPTIONS]
```

**Options**:

| Option | Default | Description |
|--------|---------|-------------|
| `--ontology <PATH>` | `schema/domain.ttl` | Path to RDF ontology file |
| `--output-dir <PATH>` | `src/main/java/` | Output directory for Java code |
| `--package <PKG>` | `com.example.yawl` | Java package base name |
| `--rules <RULES>` | `3,4,5,6,7,8,9,10` | Comma-separated rule numbers to execute |
| `--format-code` | `true` | Auto-format generated code |
| `--validate` | `true` | Validate Java syntax |
| `--verbose` | `false` | Show detailed progress output |

**Example**:

```bash
ggen yawl codegen \
  --ontology schema/domain.ttl \
  --output-dir src/main/java/ \
  --package com.mycompany.workflow \
  --rules 3,4,5,6,7,8,9,10 \
  --format-code true
```

#### `ggen yawl validate`

Validate YAWL XML workflow:

```bash
ggen yawl validate [OPTIONS]
```

**Options**:

| Option | Default | Description |
|--------|---------|-------------|
| `--file <PATH>` | `.ggen/yawl/*.yawl.xml` | YAWL file to validate |
| `--strict` | `false` | Enable strict validation |
| `--verbose` | `false` | Show detailed validation output |

**Exit Codes**:
- `0` - Valid
- `2` - Validation error

#### `ggen yawl deploy`

Deploy YAWL workflow to gen_yawl engine:

```bash
ggen yawl deploy [OPTIONS]
```

**Options**:

| Option | Default | Description |
|--------|---------|-------------|
| `--workflow <PATH>` | `.ggen/yawl/*.yawl.xml` | Workflow file to deploy |
| `--target <PATH>` | `vendors/gen_yawl/` | gen_yawl installation path |
| `--restart` | `false` | Restart gen_yawl after deployment |
| `--compile` | `false` | Compile Erlang modules |
| `--verbose` | `false` | Show detailed output |

**Exit Codes**:
- `0` - Deployed
- `4` - Deployment error

#### `ggen yawl watch`

Watch ontology for changes and auto-regenerate:

```bash
ggen yawl watch [OPTIONS]
```

**Options**:

| Option | Default | Description |
|--------|---------|-------------|
| `--ontology <PATH>` | `schema/domain.ttl` | Ontology file to watch |
| `--output-dir <PATH>` | `.ggen/yawl/` | Output directory |
| `--debounce <MS>` | `500` | Debounce delay in milliseconds |
| `--verbose` | `false` | Show detailed output |

---

## Usage Examples

### Example 1: FIBO Loan Approval Workflow

**Ontology** (`schema/loan.ttl`):

```turtle
@prefix fibo: <https://spec.edmcouncil.org/fibo/ontology/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

fibo:LoanApplication a owl:Class ;
    rdfs:label "Loan Application" .

fibo:CreditCheck a owl:Class ;
    rdfs:subClassOf fibo:LoanApplication ;
    rdfs:label "Credit Check" .

fibo:ApprovalReview a owl:Class ;
    rdfs:subClassOf fibo:LoanApplication ;
    rdfs:label "Approval Review" .
```

**Generation**:

```bash
ggen yawl generate --ontology schema/loan.ttl
```

**Generated YAWL XML**:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<specification>
  <workflow name="LoanApplication">
    <task id="t1" name="Credit Check"/>
    <task id="t2" name="Approval Review"/>
    <flow source="input" target="t1"/>
    <flow source="t1" target="t2"/>
    <flow source="t2" target="output"/>
  </workflow>
</specification>
```

### Example 2: HL7 FHIR Patient Workflow

**Ontology** (`schema/hl7-patient.ttl`):

```turtle
@prefix hl7: <http://hl7.org/fhir/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

hl7:PatientRegistration a owl:Class ;
    rdfs:label "Patient Registration" .

hl7:VerifyInsurance a owl:Class ;
    rdfs:subClassOf hl7:PatientRegistration ;
    rdfs:label "Verify Insurance" .

hl7:AssignProvider a owl:Class ;
    rdfs:subClassOf hl7:PatientRegistration ;
    rdfs:label "Assign Provider" .
```

**Generation**:

```bash
ggen yawl generate \
  --ontology schema/hl7-patient.ttl \
  --output-dir .ggen/hl7-workflow/
```

### Example 3: Development Workflow with Watch Mode

```bash
# Terminal 1: Watch for changes
ggen yawl watch \
  --ontology schema/domain.ttl \
  --debounce 500 \
  --verbose

# Terminal 2: Edit ontology and changes auto-regenerate in Terminal 1
vim schema/domain.ttl
```

### Example 4: Full Spring Boot Application Generation

```bash
# 1. Generate YAWL workflow
ggen yawl generate --ontology schema/domain.ttl

# 2. Generate Java code
ggen yawl codegen \
  --ontology schema/domain.ttl \
  --output-dir src/main/java/ \
  --package com.mycompany.workflow

# 3. Check generated files
ls -la src/main/java/com/mycompany/workflow/
#
# Result:
# entity/
# ├── LoanApplication.java
# ├── CreditCheck.java
# └── ...
# repository/
# ├── LoanApplicationRepository.java
# ├── CreditCheckRepository.java
# └── ...
# service/
# ├── LoanApplicationService.java
# ├── CreditCheckService.java
# └── ...
# controller/
# ├── LoanApplicationController.java
# ├── CreditCheckController.java
# └── ...

# 4. Compile
mvn clean compile

# 5. Run tests
mvn test

# 6. Start app
mvn spring-boot:run
```

---

## Output Structure

### YAWL XML Output

Generated in `.ggen/yawl/workflow.yawl.xml`:

```
.ggen/yawl/
├── workflow.yawl.xml          # Main workflow definition
├── workflow.yawl.erlang        # Optional Erlang implementation
└── validation_report.json      # Validation results
```

### Java Code Output

Generated in `src/main/java/com/example/yawl/`:

```
src/main/java/com/example/yawl/
├── entity/                     # Rule 3: JPA Entities
│   ├── YWorkItem.java
│   ├── YTask.java
│   └── YNet.java
├── repository/                 # Rule 4: Spring Data Repositories
│   ├── YWorkItemRepository.java
│   ├── YTaskRepository.java
│   └── YNetRepository.java
├── dto/                        # Rule 5: Data Transfer Objects
│   ├── YWorkItemDTO.java
│   ├── YTaskDTO.java
│   └── YNetDTO.java
├── controller/                 # Rule 6: REST Controllers
│   ├── YWorkItemController.java
│   ├── YTaskController.java
│   └── YNetController.java
├── service/                    # Rule 8: Business Services
│   ├── YWorkItemService.java
│   ├── YTaskService.java
│   └── YNetService.java
├── enums/                      # Rule 7: Java Enums
│   ├── WorkItemStatus.java
│   ├── TaskType.java
│   └── NetStatus.java
├── hbm/                        # Rule 9: Hibernate HBM Mappings
│   ├── YWorkItem.hbm.xml
│   ├── YTask.hbm.xml
│   └── YNet.hbm.xml
└── serializer/                 # Rule 10: Jackson Serializers
    ├── YWorkItemSerializer.java
    ├── YTaskSerializer.java
    └── YNetSerializer.java
```

### File Naming Conventions

| Layer | Pattern | Example |
|-------|---------|---------|
| Entity | `{ClassName}.java` | `LoanApplication.java` |
| Repository | `{ClassName}Repository.java` | `LoanApplicationRepository.java` |
| DTO | `{ClassName}DTO.java` | `LoanApplicationDTO.java` |
| Controller | `{ClassName}Controller.java` | `LoanApplicationController.java` |
| Service | `{ClassName}Service.java` | `LoanApplicationService.java` |
| Enum | `{EnumName}.java` | `LoanStatus.java` |
| HBM Mapping | `{ClassName}.hbm.xml` | `LoanApplication.hbm.xml` |
| Serializer | `{ClassName}Serializer.java` | `LoanApplicationSerializer.java` |

---

## Customization

### Custom Ontology Namespace

Create ontology with custom namespace:

```turtle
@prefix myns: <http://mycompany.com/ontology/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

myns:Order a owl:Class ;
    rdfs:label "Order" .

myns:Payment a owl:Class ;
    rdfs:subClassOf myns:Order ;
    rdfs:label "Payment Processing" .
```

### Custom Template Directory

Provide custom Tera templates:

```bash
# Create custom templates
mkdir -p custom_templates/
cp -r $(ggen yawl templates) custom_templates/

# Edit custom_templates/workflow.yawl.tera
vim custom_templates/workflow.yawl.tera

# Generate with custom templates
ggen yawl generate \
  --ontology schema/domain.ttl \
  --template-dir custom_templates/
```

### Exclude Specific Rules

Generate subset of Java code:

```bash
# Only generate entities and repositories (Rules 3-4)
ggen yawl codegen \
  --ontology schema/domain.ttl \
  --rules 3,4

# Only generate REST controllers (Rule 6)
ggen yawl codegen \
  --ontology schema/domain.ttl \
  --rules 6
```

### Custom Package Structure

```bash
# Generate with custom base package
ggen yawl codegen \
  --ontology schema/domain.ttl \
  --package com.bank.lending.workflow \
  --output-dir src/main/java/

# Results in:
# src/main/java/com/bank/lending/workflow/
# ├── entity/
# ├── repository/
# ├── service/
# └── ...
```

---

## Troubleshooting

### Issue: "Ontology file not found"

**Error**:
```
Error: OntologyLoad("Failed to load ontology: file not found")
```

**Solution**:
```bash
# Verify file exists
ls -la schema/domain.ttl

# Use absolute path
ggen yawl generate --ontology /full/path/to/schema/domain.ttl
```

### Issue: "Invalid RDF syntax"

**Error**:
```
Error: OntologyLoad("Turtle parse error at line 5")
```

**Solution**:
```bash
# Validate Turtle syntax
ggen ontology validate schema/domain.ttl

# Common issues:
# - Missing semicolons in Turtle
# - Undefined namespace prefixes
# - Invalid URI format
```

### Issue: "Generation timeout"

**Error**:
```
Error: Timeout("Generation exceeded 30000ms")
```

**Solution**:
```bash
# Increase timeout
ggen yawl generate \
  --ontology schema/domain.ttl \
  --timeout 60000  # 60 seconds
```

### Issue: "SPARQL query error"

**Error**:
```
Error: Sparql("No results from CONSTRUCT query")
```

**Solution**:
- Ensure ontology uses expected namespace prefixes (yawl:, rdfs:, owl:)
- Check that classes are defined with `a owl:Class`
- Verify properties use `rdfs:subClassOf` or other expected properties

### Issue: "Java compilation error"

**Error**:
```
[ERROR] error: package com.example.yawl.entity does not exist
```

**Solution**:
```bash
# Ensure correct package path
ls -la src/main/java/com/example/yawl/entity/

# Regenerate if needed
ggen yawl codegen \
  --ontology schema/domain.ttl \
  --output-dir src/main/java/

# Then compile
mvn clean compile
```

### Issue: "Validation failure"

**Error**:
```
Error: Validation("YAWL XML validation failed")
```

**Solution**:
```bash
# Disable validation for debugging
ggen yawl generate \
  --ontology schema/domain.ttl \
  --validate false

# Check generated XML
cat .ggen/yawl/workflow.yawl.xml

# Run detailed validation
ggen yawl validate \
  --file .ggen/yawl/workflow.yawl.xml \
  --strict true \
  --verbose
```

### Performance Issue: Slow Generation

**Symptom**: Generation takes longer than expected

**Solution**:
```bash
# Use watch mode during development
ggen yawl watch --ontology schema/domain.ttl

# Generate only specific rules
ggen yawl codegen --rules 3,4,5  # Skip slow rules

# Use incremental generation
# (Caches previous results)
```

---

## Next Steps

- **Learn**: Read [YAWL_CODEGEN_ARCHITECTURE.md](./YAWL_CODEGEN_ARCHITECTURE.md) for technical details
- **Reference**: Check [YAWL_CODEGEN_API.md](./YAWL_CODEGEN_API.md) for API documentation
- **Rules**: Review individual rules in `docs/rules/` directory
- **Examples**: Explore examples in `crates/ggen-yawl/examples/`
- **Testing**: Run `cargo test -p ggen-yawl` to validate your setup

---

**For Support**:
- GitHub Issues: https://github.com/seanchatmangpt/ggen/issues
- Documentation: `/docs/`
- Examples: `/crates/ggen-yawl/examples/`
