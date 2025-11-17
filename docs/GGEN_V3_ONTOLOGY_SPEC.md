# ggen v3 Core Ontology Specification

**Status**: SPECIFICATION DRAFT
**Version**: 3.0.0-alpha
**Purpose**: Complete formal specification of `ggen_v3_core.ttl`

---

## Table of Contents

1. [Overview](#overview)
2. [Namespace & Prefixes](#namespace--prefixes)
3. [Entity Classes (Classes/Types)](#entity-classes)
4. [Properties & Relationships](#properties--relationships)
5. [Constraints & Validation Rules](#constraints--validation-rules)
6. [SPARQL Query Examples](#sparql-query-examples)
7. [Data Representation Guidelines](#data-representation-guidelines)

---

## Overview

The ggen v3 ontology is a **complete RDF representation** of ggen's architecture, CLI commands, marketplace system, type system, testing framework, and deployment targets.

### Design Principles

1. **Semantic Clarity**: Every entity is explicitly defined; no implicit assumptions
2. **Queryability**: If a question is about ggen, it should answerable via SPARQL
3. **Composability**: Ontologies can be merged (user + ggen_v3_core)
4. **Determinism**: Same ontology always produces identical code
5. **Traceability**: Every generated line traces back to ontology entity

### Key Dimensions

```
Ontology covers:
├── System Structure (Crates, Modules, Visibility, Exports)
├── Type System (Struct, Enum, Trait, Union; Field definitions)
├── CLI Commands (32 commands, arguments, flags, subcommands)
├── Marketplace (Packages, Guards, Validation, Scoring)
├── AI Integration (LLM Providers, Models, Streaming)
├── Testing (Unit, Integration, E2E, BDD test patterns)
├── Constraints (Range, Pattern, Enum, Unique, NotNull)
├── Lifecycle (Init, Setup, Generate, Test, Deploy phases)
├── Documentation (API Reference, Architecture, Tutorials)
└── Deployment (Docker, Kubernetes, Serverless targets)
```

---

## Namespace & Prefixes

```turtle
@prefix ggen:     <http://ggen.io/ontology/v3/ggen#> .
@prefix ggencli:  <http://ggen.io/ontology/v3/cli#> .
@prefix ggenai:   <http://ggen.io/ontology/v3/ai#> .
@prefix ggenmp:   <http://ggen.io/ontology/v3/marketplace#> .
@prefix xsd:      <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf:      <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs:     <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl:      <http://www.w3.org/2002/07/owl#> .
@prefix sh:       <http://www.w3.org/ns/shacl#> .
```

### Namespace Breakdown

- **ggen:** Core system, crates, modules, types, functions
- **ggencli:** CLI commands, arguments, flags, dispatching
- **ggenai:** LLM integration, AI providers, models
- **ggenmp:** Marketplace, packages, guards, validation, scoring
- **xsd:** XML Schema datatypes (string, integer, decimal, boolean, etc.)
- **sh:** SHACL constraints (minInclusive, maxInclusive, pattern, etc.)

---

## Entity Classes

### 1. System Structure Classes

#### **ggen:Crate**
Represents a Rust crate in the workspace.

```
Class: ggen:Crate
  ├── Properties:
  │   ├── ggen:name [xsd:string, 1..1]
  │   ├── ggen:version [xsd:string, 1..1]
  │   ├── ggen:description [xsd:string, 0..1]
  │   ├── ggen:modules [ggen:Module, 0..*]
  │   ├── ggen:dependencies [ggen:Crate, 0..*]
  │   ├── ggen:isPublic [xsd:boolean, 1..1, default: false]
  │   ├── ggen:license [xsd:string, 0..1, default: "MIT"]
  │   ├── ggen:maintainer [xsd:string, 0..1]
  │   ├── ggen:repository [xsd:anyURI, 0..1]
  │   ├── ggen:documentation [xsd:string, 0..1]
  │   └── ggen:createdDate [xsd:dateTime, 0..1]
  │
  └── Constraints:
      ├── name must match Rust naming conventions (lowercase, hyphens allowed)
      ├── version must be SemVer (e.g., "3.0.0")
      └── Cyclic dependencies are NOT allowed

Examples:
  ggen:CoreCrate a ggen:Crate ;
    ggen:name "ggen-core" ;
    ggen:version "3.0.0" ;
    ggen:description "Semantic projection engine for ontology-driven code generation" ;
    ggen:isPublic true ;
    ggen:modules ggen:GraphModule, ggen:LifecycleModule, ggen:TemplateModule, ggen:MergeModule, ggen:OntologyModule ;
    ggen:dependencies ggen:UtilsCrate ;
    ggen:license "MIT" .

  ggen:CliCrate a ggen:Crate ;
    ggen:name "ggen-cli" ;
    ggen:version "3.0.0" ;
    ggen:description "CLI interface for ggen" ;
    ggen:isPublic true ;
    ggen:modules ggen:CommandsModule, ggen:DispatchModule ;
    ggen:dependencies ggen:CoreCrate, ggen:DomainCrate, ggen:UtilsCrate ;
    ggen:license "MIT" .
```

#### **ggen:Module**
Represents a Rust module within a crate.

```
Class: ggen:Module
  ├── Properties:
  │   ├── ggen:name [xsd:string, 1..1]  # Module name in Rust code
  │   ├── ggen:path [xsd:string, 1..1]  # Relative path (e.g., "src/graph")
  │   ├── ggen:parent [ggen:Crate, 1..1]  # Containing crate
  │   ├── ggen:exports [ggen:Type | ggen:Function | ggen:Trait, 0..*]
  │   ├── ggen:internal [ggen:Type | ggen:Function, 0..*]  # Not re-exported
  │   ├── ggen:isPublic [xsd:boolean, 1..1, default: false]
  │   ├── ggen:documentation [xsd:string, 0..1]
  │   ├── ggen:tests [ggen:TestModule, 0..*]
  │   └── ggen:createdDate [xsd:dateTime, 0..1]
  │
  └── Constraints:
      ├── parent crate must exist
      ├── All exported items must be defined
      └── Module path must be unique within crate

Examples:
  ggen:GraphModule a ggen:Module ;
    ggen:name "graph" ;
    ggen:path "src/graph" ;
    ggen:parent ggen:CoreCrate ;
    ggen:isPublic true ;
    ggen:exports ggen:Graph, ggen:Query, ggen:SparqlResult ;
    ggen:internal ggen:GraphCache, ggen:QueryOptimizer ;
    ggen:documentation "RDF triple store operations and SPARQL query execution" ;
    ggen:tests ggen:GraphModuleTests .

  ggen:LifecycleModule a ggen:Module ;
    ggen:name "lifecycle" ;
    ggen:path "src/lifecycle" ;
    ggen:parent ggen:CoreCrate ;
    ggen:isPublic true ;
    ggen:exports ggen:Phase, ggen:Lifecycle ;
    ggen:documentation "Universal build system (Init→Setup→Generate→Test→Deploy)" .
```

#### **ggen:Type** (Abstract Base Class)
Base class for all types in the system.

```
Class: ggen:Type (abstract)
  ├── Properties:
  │   ├── ggen:name [xsd:string, 1..1]
  │   ├── ggen:module [ggen:Module, 1..1]  # Where type is defined
  │   ├── ggen:isPublic [xsd:boolean, 1..1, default: false]
  │   ├── ggen:documentation [xsd:string, 0..1]
  │   ├── ggen:constraints [ggen:Constraint, 0..*]
  │   ├── ggen:typeMapping [PropertyMapping, 0..*]  # Language-specific mappings
  │   ├── ggen:isDeprecated [xsd:boolean, 1..1, default: false]
  │   ├── ggen:deprecatedMessage [xsd:string, 0..1]
  │   └── ggen:createdDate [xsd:dateTime, 0..1]
  │
  ├── Subclasses:
  │   ├── ggen:Struct
  │   ├── ggen:Enum
  │   ├── ggen:Trait
  │   ├── ggen:Union
  │   └── ggen:Newtype
  │
  └── Constraints:
      ├── name must be UpperCamelCase in Rust
      ├── module must exist
      └── If deprecated, deprecatedMessage must be present
```

#### **ggen:Struct** (Subclass of Type)
Represents a Rust struct.

```
Class: ggen:Struct extends ggen:Type
  ├── Additional Properties:
  │   ├── ggen:fields [ggen:Field, 0..*]
  │   ├── ggen:derives [xsd:string, 0..*]  # e.g., ["Debug", "Clone", "Serialize"]
  │   ├── ggen:implements [ggen:Trait, 0..*]  # Traits this struct implements
  │   ├── ggen:generics [ggen:Generic, 0..*]  # Type parameters
  │   ├── ggen:visibility [Visibility, 1..1, default: pub]
  │   └── ggen:structKind [StructKind, 1..1, default: "named"]  # named | tuple | unit
  │
  └── Constraints:
      ├── All derives must be valid Rust derives
      ├── Trait implementations must exist
      └── Field names must be unique within struct

Example:
  ggen:Project a ggen:Struct ;
    ggen:name "Project" ;
    ggen:module ggen:DomainModule ;
    ggen:isPublic true ;
    ggen:documentation "Represents a ggen project with ontology and configuration" ;
    ggen:fields ggen:ProjectNameField, ggen:ProjectPathField, ggen:ProjectOntologyField ;
    ggen:derives "Debug", "Clone", "Serialize", "Deserialize" ;
    ggen:implements ggen:FromPathTrait ;
    ggen:visibility "pub" ;
    ggen:structKind "named" .
```

#### **ggen:Field**
Represents a field in a struct.

```
Class: ggen:Field
  ├── Properties:
  │   ├── ggen:name [xsd:string, 1..1]
  │   ├── ggen:type [XsdType | ggen:Type, 1..1]  # Field type
  │   ├── ggen:visibility [Visibility, 1..1, default: "pub"]
  │   ├── ggen:isRequired [xsd:boolean, 1..1, default: true]
  │   ├── ggen:defaultValue [xsd:string, 0..1]  # If not required
  │   ├── ggen:documentation [xsd:string, 0..1]
  │   ├── ggen:constraints [ggen:Constraint, 0..*]
  │   └── ggen:serde [SerdeConfig, 0..1]  # Serialization hints
  │
  └── Constraints:
      ├── name must match Rust naming (snake_case)
      ├── type must be defined
      └── If isRequired=false, defaultValue should exist

Example:
  ggen:ProjectNameField a ggen:Field ;
    ggen:name "name" ;
    ggen:type xsd:string ;
    ggen:visibility "pub" ;
    ggen:isRequired true ;
    ggen:constraints ggen:ProjectNameNotEmptyConstraint, ggen:ProjectNameValidCharsConstraint ;
    ggen:documentation "Project name (used in paths and identifiers)" .

  ggen:ProjectPathField a ggen:Field ;
    ggen:name "path" ;
    ggen:type xsd:string ;  # or could be PathBuf
    ggen:visibility "pub" ;
    ggen:isRequired true ;
    ggen:documentation "Absolute path to project root" .
```

#### **ggen:Enum** (Subclass of Type)
Represents a Rust enum.

```
Class: ggen:Enum extends ggen:Type
  ├── Additional Properties:
  │   ├── ggen:variants [ggen:EnumVariant, 1..*]
  │   ├── ggen:derives [xsd:string, 0..*]
  │   ├── ggen:generics [ggen:Generic, 0..*]
  │   └── ggen:visibility [Visibility, 1..1]
  │
  └── Constraints:
      ├── At least one variant must exist
      └── Variant names must be unique within enum

Example:
  ggen:MaturityLevel a ggen:Enum ;
    ggen:name "MaturityLevel" ;
    ggen:module ggen:MarketplaceModule ;
    ggen:isPublic true ;
    ggen:documentation "Production readiness level of a marketplace package" ;
    ggen:variants
      ggen:MaturityAlpha,
      ggen:MaturityBeta,
      ggen:MaturityStable,
      ggen:MaturityProduction,
      ggen:MaturityEnterprise ;
    ggen:derives "Debug", "Clone", "Copy", "PartialEq" ;
    ggen:visibility "pub" .

  ggen:MaturityAlpha a ggen:EnumVariant ;
    ggen:name "Alpha" ;
    ggen:documentation "Experimental, not for production use" .

  ggen:MaturityProduction a ggen:EnumVariant ;
    ggen:name "Production" ;
    ggen:documentation "Ready for production deployments" .
```

#### **ggen:Trait** (Subclass of Type)
Represents a Rust trait.

```
Class: ggen:Trait extends ggen:Type
  ├── Additional Properties:
  │   ├── ggen:methods [ggen:TraitMethod, 1..*]
  │   ├── ggen:bounds [ggen:GenericBound, 0..*]
  │   ├── ggen:associatedTypes [ggen:AssociatedType, 0..*]
  │   ├── ggen:supertraits [ggen:Trait, 0..*]
  │   └── ggen:visibility [Visibility, 1..1, default: "pub"]
  │
  └── Constraints:
      ├── At least one method must exist
      └── SuperTraits must be defined

Example:
  ggen:Guard a ggen:Trait ;
    ggen:name "Guard" ;
    ggen:module ggen:MarketplaceModule ;
    ggen:isPublic true ;
    ggen:documentation "Validation guard that checks packages against production readiness criteria" ;
    ggen:methods ggen:GuardCheckMethod, ggen:GuardNameMethod ;
    ggen:associated Types ggen:GuardErrorType ;
    ggen:visibility "pub" .

  ggen:GuardCheckMethod a ggen:TraitMethod ;
    ggen:name "check" ;
    ggen:parameters ggen:SelfParam, ggen:PackageParam ;
    ggen:returnType "Result<ValidationReceipt, GuardError>" ;
    ggen:documentation "Execute validation check on a package" .
```

### 2. CLI Command Classes

#### **ggencli:CliCommand**
Represents a CLI command in the ggen toolkit.

```
Class: ggencli:CliCommand
  ├── Properties:
  │   ├── ggencli:noun [xsd:string, 1..1]  # e.g., "project", "marketplace"
  │   ├── ggencli:verb [xsd:string, 1..1]  # e.g., "gen", "new", "search"
  │   ├── ggencli:description [xsd:string, 1..1]
  │   ├── ggencli:longDescription [xsd:string, 0..1]
  │   ├── ggencli:arguments [ggencli:Argument, 0..*]
  │   ├── ggencli:flags [ggencli:Flag, 0..*]
  │   ├── ggencli:subcommands [ggencli:CliCommand, 0..*]
  │   ├── ggencli:implementation [ggen:Module, 0..1]  # Module that implements
  │   ├── ggencli:examples [xsd:string, 0..*]  # Usage examples
  │   ├── ggencli:tests [ggen:TestCase, 0..*]
  │   ├── ggencli:outputFormat [xsd:string, 1..1, default: "text"]  # text|json|table
  │   ├── ggencli:aliases [xsd:string, 0..*]  # Alternative names
  │   ├── ggencli:isDeprecated [xsd:boolean, 1..1, default: false]
  │   └── ggencli:version [xsd:string, 0..1]  # When added
  │
  └── Constraints:
      ├── (noun, verb) must be unique
      ├── All arguments must be defined
      ├── Examples must be valid shell commands

Examples:
  ggencli:ProjectGenCommand a ggencli:CliCommand ;
    ggencli:noun "project" ;
    ggencli:verb "gen" ;
    ggencli:description "Generate code from ontology" ;
    ggencli:longDescription "Load ontology files and generate code for all configured projections" ;
    ggencli:arguments ggencli:ProjectGenPathArg ;
    ggencli:flags
      ggencli:OntologyFlag,
      ggencli:OutputFlag,
      ggencli:ForceFlag ;
    ggencli:examples
      "ggen project gen ." ,
      "ggen project gen . --ontology domain.ttl --output /tmp/out" ;
    ggencli:outputFormat "text" ;
    ggencli:implementation ggen:ProjectCommandModule ;
    ggencli:tests ggencli:ProjectGenTest1, ggencli:ProjectGenTest2 .

  ggencli:MarketplaceSearchCommand a ggencli:CliCommand ;
    ggencli:noun "marketplace" ;
    ggencli:verb "search" ;
    ggencli:description "Search marketplace for packages" ;
    ggencli:arguments ggencli:SearchQueryArg ;
    ggencli:flags
      ggencli:SectorFlag,
      ggencli:Only8020Flag,
      ggencli:LimitFlag ;
    ggencli:outputFormat "table" ;
    ggencli:examples
      "ggen marketplace search 'rust microservice'" ,
      "ggen marketplace search observability --sector observability --8020" .
```

#### **ggencli:Argument**
Represents a positional argument for a CLI command.

```
Class: ggencli:Argument
  ├── Properties:
  │   ├── ggencli:name [xsd:string, 1..1]
  │   ├── ggencli:type [ArgumentType, 1..1]  # String, Path, Integer, Enum, etc.
  │   ├── ggencli:description [xsd:string, 1..1]
  │   ├── ggencli:isRequired [xsd:boolean, 1..1, default: true]
  │   ├── ggencli:isVariadic [xsd:boolean, 1..1, default: false]
  │   ├── ggencli:defaultValue [xsd:string, 0..1]
  │   ├── ggencli:validation [ggen:Constraint, 0..*]
  │   └── ggencli:examples [xsd:string, 0..*]
  │
  └── Constraints:
      ├── type must be a valid ArgumentType
      ├── If isRequired=false, defaultValue should exist

Examples:
  ggencli:ProjectGenPathArg a ggencli:Argument ;
    ggencli:name "path" ;
    ggencli:type "Path" ;
    ggencli:description "Project root directory" ;
    ggencli:isRequired true ;
    ggencli:isVariadic false ;
    ggencli:examples ".", "/home/user/my-project" ;
    ggencli:validation ggencli:PathMustExistConstraint .

  ggencli:SearchQueryArg a ggencli:Argument ;
    ggencli:name "query" ;
    ggencli:type "String" ;
    ggencli:description "Search term (partial name or description match)" ;
    ggencli:isRequired false ;
    ggencli:defaultValue "*" .
```

#### **ggencli:Flag**
Represents a named flag/option for a CLI command.

```
Class: ggencli:Flag
  ├── Properties:
  │   ├── ggencli:longForm [xsd:string, 1..1]  # e.g., "--ontology"
  │   ├── ggencli:shortForm [xsd:string, 0..1]  # e.g., "-o"
  │   ├── ggencli:type [FlagType, 1..1]  # Boolean, String, Integer, Path
  │   ├── ggencli:description [xsd:string, 1..1]
  │   ├── ggencli:isRequired [xsd:boolean, 1..1, default: false]
  │   ├── ggencli:defaultValue [xsd:string, 0..1]
  │   ├── ggencli:validation [ggen:Constraint, 0..*]
  │   ├── ggencli:examples [xsd:string, 0..*]
  │   └── ggencli:aliases [xsd:string, 0..*]
  │
  └── Constraints:
      ├── longForm must start with "--"
      ├── shortForm (if present) must start with "-"

Examples:
  ggencli:OntologyFlag a ggencli:Flag ;
    ggencli:longForm "--ontology" ;
    ggencli:shortForm "-o" ;
    ggencli:type "Path" ;
    ggencli:description "Path to primary ontology file" ;
    ggencli:isRequired false ;
    ggencli:examples "--ontology domain.ttl", "-o /path/to/ontology.ttl" ;
    ggencli:validation ggencli:OntologyFileExistsConstraint .

  ggencli:ForceFlag a ggencli:Flag ;
    ggencli:longForm "--force" ;
    ggencli:shortForm "-f" ;
    ggencli:type "Boolean" ;
    ggencli:description "Force overwrite of existing files" ;
    ggencli:isRequired false ;
    ggencli:defaultValue "false" .

  ggencli:Only8020Flag a ggencli:Flag ;
    ggencli:longForm "--8020" ;
    ggencli:type "Boolean" ;
    ggencli:description "Show only 8020-certified packages" ;
    ggencli:defaultValue "false" .
```

### 3. Marketplace Classes

#### **ggenmp:Package**
Represents a marketplace package.

```
Class: ggenmp:Package
  ├── Properties:
  │   ├── ggenmp:name [xsd:string, 1..1]  # e.g., "sector-observability-8020"
  │   ├── ggenmp:version [xsd:string, 1..1]  # SemVer
  │   ├── ggenmp:namespace [xsd:string, 1..1]  # e.g., "io.ggen.observability"
  │   ├── ggenmp:description [xsd:string, 1..1]
  │   ├── ggenmp:author [xsd:string, 0..1]
  │   ├── ggenmp:license [xsd:string, 0..1]
  │   ├── ggenmp:repository [xsd:anyURI, 0..1]
  │   ├── ggenmp:sector [xsd:string, 0..1]  # healthcare|observability|microservice|etc
  │   ├── ggenmp:dependencies [ggenmp:Package, 0..*]
  │   ├── ggenmp:is8020 [xsd:boolean, 1..1, default: false]
  │   ├── ggenmp:is8020Certified [xsd:boolean, 1..1, default: false]
  │   ├── ggenmp:darkMatterReductionTarget [xsd:string, 0..1]
  │   ├── ggenmp:readinessScore [xsd:integer, 0..1]  # 0-100
  │   ├── ggenmp:maturityLevel [ggen:MaturityLevel, 0..1]
  │   ├── ggenmp:guards [ggen:Guard, 0..*]
  │   ├── ggenmp:validationReceipt [ggen:ValidationReceipt, 0..1]
  │   └── ggenmp:publishedDate [xsd:dateTime, 0..1]
  │
  └── Constraints:
      ├── name must be unique in namespace
      ├── version must be SemVer
      └── Dependencies must be resolvable

Example:
  ggenmp:SectorObservability8020Package a ggenmp:Package ;
    ggenmp:name "sector-observability-8020" ;
    ggenmp:namespace "io.ggen.observability" ;
    ggenmp:version "1.0.0" ;
    ggenmp:description "Complete observability stack: OTEL, Weaver, SLO, dashboards" ;
    ggenmp:author "ggen team" ;
    ggenmp:sector "observability" ;
    ggenmp:is8020 true ;
    ggenmp:is8020Certified true ;
    ggenmp:darkMatterReductionTarget "Eliminates ~70% of manual observability setup" ;
    ggenmp:guards ggen:Guard8020Coverage, ggen:GuardTelemetryComplete ;
    ggenmp:readinessScore 90 ;
    ggenmp:maturityLevel ggen:MaturityProduction .
```

#### **ggen:Guard** (detailed)
Validation rule for marketplace packages.

```
Class: ggen:Guard (abstract)
  ├── Properties:
  │   ├── ggen:name [xsd:string, 1..1]
  │   ├── ggen:description [xsd:string, 1..1]
  │   ├── ggen:appliesTo [GuardTarget, 1..*]  # Package | Type | Module
  │   ├── ggen:checks [ggen:Check, 1..*]
  │   ├── ggen:scoringRules [ggen:ScoringRule, 0..*]
  │   ├── ggen:threshold [xsd:integer, 0..1]  # Min points to pass (if scoring)
  │   ├── ggen:isBlocking [xsd:boolean, 1..1, default: true]
  │   └── ggen:documentation [xsd:string, 0..1]
  │
  ├── Subclasses:
  │   ├── ggen:Guard8020Coverage
  │   ├── ggen:GuardChatmanCompliant
  │   ├── ggen:GuardTelemetryComplete
  │   └── ggen:GuardPaperComplete
  │
  └── Constraints:
      ├── At least one check must exist
      └── All referenced checks must be defined

Examples:
  ggen:Guard8020Coverage a ggen:Guard ;
    ggen:name "Guard8020Coverage" ;
    ggen:description "Validates package meets 80/20 criteria" ;
    ggen:appliesTo "Package" ;
    ggen:checks
      ggen:Check8020OntologyPresent,
      ggen:Check8020ProjectionsComplete,
      ggen:Check8020TemplatesPresent,
      ggen:Check8020TestsPresent,
      ggen:Check8020DocsPresent,
      ggen:Check8020GuardsPresent,
      ggen:Check8020BundleIntegration ;
    ggen:threshold 5 ;  # 5/7 checks must pass
    ggen:isBlocking true .

  ggen:GuardTelemetryComplete a ggen:Guard ;
    ggen:name "GuardTelemetryComplete" ;
    ggen:description "Validates observability package is fully instrumented" ;
    ggen:appliesTo "Package" ;
    ggen:checks
      ggen:CheckOTelSpansPresent,
      ggen:CheckMetricsRegistered,
      ggen:CheckSLODefined,
      ggen:CheckDashboardsProvided ;
    ggen:threshold 3 ;  # 3/4 must pass
    ggen:isBlocking false .  # Warning only
```

#### **ggen:Check**
Individual validation check within a Guard.

```
Class: ggen:Check
  ├── Properties:
  │   ├── ggen:name [xsd:string, 1..1]
  │   ├── ggen:description [xsd:string, 1..1]
  │   ├── ggen:implementation [ggen:Module, 1..1]  # Code that runs check
  │   ├── ggen:isAutomatic [xsd:boolean, 1..1]  # Runnable without user input?
  │   ├── ggen:estimatedDuration [xsd:integer, 0..1]  # Seconds
  │   ├── ggen:pointsIfPass [xsd:integer, 1..1, default: 1]
  │   └── ggen:documentation [xsd:string, 0..1]
  │
  └── Constraints:
      ├── implementation module must exist
      └── pointsIfPass must be > 0

Example:
  ggen:Check8020OntologyPresent a ggen:Check ;
    ggen:name "Ontology present and valid RDF" ;
    ggen:description "Package includes a valid RDF ontology file" ;
    ggen:implementation ggen:MarketplaceValidationModule ;
    ggen:isAutomatic true ;
    ggen:estimatedDuration 2 ;
    ggen:pointsIfPass 1 .
```

### 4. AI Integration Classes

#### **ggenai:LlmProvider**
Represents an LLM provider (OpenAI, Anthropic, etc.).

```
Class: ggenai:LlmProvider
  ├── Properties:
  │   ├── ggenai:name [xsd:string, 1..1]  # "OpenAI", "Anthropic", "Ollama"
  │   ├── ggenai:endpoint [xsd:anyURI, 1..1]  # API endpoint
  │   ├── ggenai:authentication [AuthMethod, 1..1]  # ApiKey | OAuth2 | LocalhostOnly
  │   ├── ggenai:models [ggenai:LlmModel, 1..*]
  │   ├── ggenai:streaming [xsd:boolean, 1..1, default: false]
  │   ├── ggenai:documentation [xsd:anyURI, 0..1]
  │   ├── ggenai:costPer1kInputTokens [xsd:decimal, 0..1]
  │   ├── ggenai:costPer1kOutputTokens [xsd:decimal, 0..1]
  │   └── ggenai:requiredHeaders [xsd:string, 0..*]
  │
  └── Constraints:
      ├── At least one model must exist
      └── endpoint must be valid URI

Example:
  ggenai:OpenAIProvider a ggenai:LlmProvider ;
    ggenai:name "OpenAI" ;
    ggenai:endpoint <https://api.openai.com/v1> ;
    ggenai:authentication "ApiKey" ;
    ggenai:streaming true ;
    ggenai:models ggenai:GPT4o, ggenai:GPT4Turbo ;
    ggenai:costPer1kInputTokens 0.03 ;
    ggenai:costPer1kOutputTokens 0.06 .

  ggenai:OllamaProvider a ggenai:LlmProvider ;
    ggenai:name "Ollama" ;
    ggenai:endpoint <http://localhost:11434> ;
    ggenai:authentication "LocalhostOnly" ;
    ggenai:streaming true ;
    ggenai:models ggenai:Mistral, ggenai:Llama2 .
```

#### **ggenai:LlmModel**
Represents a specific LLM model.

```
Class: ggenai:LlmModel
  ├── Properties:
  │   ├── ggenai:name [xsd:string, 1..1]  # "gpt-4o", "claude-3-opus", "mistral"
  │   ├── ggenai:provider [ggenai:LlmProvider, 1..1]
  │   ├── ggenai:modelId [xsd:string, 1..1]  # Full model ID for API call
  │   ├── ggenai:contextWindow [xsd:integer, 1..1]  # Max tokens
  │   ├── ggenai:trainingDataCutoff [xsd:date, 0..1]
  │   ├── ggenai:isProgrammingSpecialized [xsd:boolean, 1..1, default: false]
  │   ├── ggenai:costPerMillionInputTokens [xsd:decimal, 0..1]
  │   ├── ggenai:costPerMillionOutputTokens [xsd:decimal, 0..1]
  │   └── ggenai:recommendedFor [xsd:string, 0..*]
  │
  └── Constraints:
      ├── contextWindow > 0
      └── costPerMillionInputTokens >= 0

Example:
  ggenai:GPT4o a ggenai:LlmModel ;
    ggenai:name "GPT-4o" ;
    ggenai:modelId "gpt-4o" ;
    ggenai:provider ggenai:OpenAIProvider ;
    ggenai:contextWindow 128000 ;
    ggenai:trainingDataCutoff "2024-04-01"^^xsd:date ;
    ggenai:isProgrammingSpecialized true ;
    ggenai:costPerMillionInputTokens 2.5 ;
    ggenai:costPerMillionOutputTokens 10.0 ;
    ggenai:recommandedFor "Code generation", "Architecture design" .
```

### 5. Constraint Classes

#### **ggen:Constraint** (Abstract)
Base class for all validation constraints.

```
Class: ggen:Constraint (abstract)
  ├── Properties:
  │   ├── ggen:name [xsd:string, 1..1]
  │   ├── ggen:description [xsd:string, 1..1]
  │   ├── ggen:target [Type | Field | Parameter, 1..1]
  │   ├── ggen:isMandatory [xsd:boolean, 1..1, default: true]
  │   └── ggen:documentation [xsd:string, 0..1]
  │
  ├── Subclasses:
  │   ├── ggen:RangeConstraint
  │   ├── ggen:PatternConstraint
  │   ├── ggen:EnumConstraint
  │   ├── ggen:UniqueConstraint
  │   ├── ggen:NotNullConstraint
  │   ├── ggen:CardinalityConstraint
  │   └── ggen:CustomConstraint
  │
  └── General Constraints:
      └── target must be a defined entity
```

---

## Properties & Relationships

### Core Properties

```
ggen:name
  Domain: Any entity
  Range: xsd:string
  Cardinality: 1..1
  Description: Identifier name for the entity

ggen:documentation
  Domain: Any entity
  Range: xsd:string (markdown format)
  Cardinality: 0..1
  Description: Human-readable documentation

ggen:modules
  Domain: ggen:Crate
  Range: ggen:Module
  Cardinality: 0..*
  InverseOf: ggen:parent
  Description: Modules contained in a crate

ggen:exports
  Domain: ggen:Module | ggen:Trait
  Range: ggen:Type | ggen:Function
  Cardinality: 0..*
  Description: Public items exported by module/trait

ggen:dependencies
  Domain: ggen:Crate | ggenmp:Package
  Range: ggen:Crate | ggenmp:Package
  Cardinality: 0..*
  Description: Direct dependencies

ggen:constraints
  Domain: ggen:Type | ggen:Field | ggencli:Parameter
  Range: ggen:Constraint
  Cardinality: 0..*
  Description: Validation constraints on entity
```

---

## SPARQL Query Examples

### System Structure Queries

```sparql
# Query 1: Get all crates in the system
SELECT ?crateName ?version ?isPublic WHERE {
  ?crate a ggen:Crate ;
         ggen:name ?crateName ;
         ggen:version ?version ;
         ggen:isPublic ?isPublic .
}
ORDER BY ?crateName

# Expected Result:
# crateName           | version  | isPublic
# ggen-ai            | 3.0.0    | true
# ggen-cli           | 3.0.0    | true
# ggen-core          | 3.0.0    | true
# ggen-domain        | 3.0.0    | true
# ggen-marketplace   | 3.0.0    | true
# ggen-utils         | 3.0.0    | false
```

```sparql
# Query 2: Get all public types in ggen-core crate
SELECT ?typeName ?typeKind ?module WHERE {
  ?crate ggen:name "ggen-core" ;
         ggen:modules ?module .
  ?type ggen:module ?module ;
        ggen:name ?typeName ;
        ggen:isPublic true .
  BIND(
    IF(EXISTS { ?type a ggen:Struct }, "Struct",
    IF(EXISTS { ?type a ggen:Enum }, "Enum",
    IF(EXISTS { ?type a ggen:Trait }, "Trait", "Other")))
    AS ?typeKind)
}
ORDER BY ?module ?typeName
```

```sparql
# Query 3: Get all async functions in ggen-cli
SELECT ?functionName ?module WHERE {
  ?crate ggen:name "ggen-cli" ;
         ggen:modules ?m .
  ?m ggen:name ?module ;
     ggen:exports | ggen:internal ?func .
  ?func a ggen:Function ;
        ggen:name ?functionName ;
        ggen:isAsync true .
}
```

### CLI Command Queries

```sparql
# Query 4: Get all CLI commands grouped by noun
SELECT ?noun (GROUP_CONCAT(?verb) as ?verbs) WHERE {
  ?cmd a ggencli:CliCommand ;
       ggencli:noun ?noun ;
       ggencli:verb ?verb .
}
GROUP BY ?noun
ORDER BY ?noun

# Expected Result (sample):
# noun           | verbs
# ai             | generate-ontology,chat,analyze,refactor-suggestion
# graph          | load,query,export,diff
# marketplace    | search,install,publish,info
# project        | new,gen,watch
```

```sparql
# Query 5: Get all flags used in CLI commands that start with "project"
SELECT ?cmdName ?longForm ?flagType WHERE {
  ?cmd a ggencli:CliCommand ;
       ggencli:noun "project" ;
       ggencli:verb ?verb ;
       ggencli:flags ?flag .
  BIND(CONCAT(?noun, " ", ?verb) AS ?cmdName)
  ?flag ggencli:longForm ?longForm ;
        ggencli:type ?flagType .
}
ORDER BY ?cmdName ?longForm
```

### Marketplace Queries

```sparql
# Query 6: Get all 8020-certified packages in "observability" sector
SELECT ?pkgName ?version ?readinessScore WHERE {
  ?pkg a ggenmp:Package ;
       ggenmp:name ?pkgName ;
       ggenmp:version ?version ;
       ggenmp:sector "observability" ;
       ggenmp:is8020Certified true ;
       ggenmp:readinessScore ?readinessScore .
}
ORDER BY DESC(?readinessScore)
```

```sparql
# Query 7: Calculate average readiness score by sector
SELECT ?sector (AVG(?score) as ?avgScore) (COUNT(?pkg) as ?pkgCount) WHERE {
  ?pkg a ggenmp:Package ;
       ggenmp:sector ?sector ;
       ggenmp:readinessScore ?score .
}
GROUP BY ?sector
ORDER BY DESC(?avgScore)
```

---

## Data Representation Guidelines

### 1. Date/Time Handling
```turtle
# Use xsd:dateTime for precise timestamps
ggen:EntityName ggen:createdDate "2025-11-17T14:30:00Z"^^xsd:dateTime .

# Use xsd:date for dates (no time)
ggenai:GPT4o ggenai:trainingDataCutoff "2024-04-01"^^xsd:date .
```

### 2. URI vs String
```turtle
# Use xsd:anyURI for URLs/endpoints
ggenai:OpenAIProvider ggenai:endpoint <https://api.openai.com/v1> .

# Use xsd:string for names, descriptions, etc.
ggen:CoreCrate ggen:name "ggen-core" ;
              ggen:description "Semantic projection engine..." .
```

### 3. Boolean Values
```turtle
# Always explicit boolean values
ggen:Crate ggen:isPublic true ;
          ggen:isDeprecated false .
```

### 4. Cardinality & Optionality
```turtle
# Required (1..1): Always present
ggen:Field ggen:name "fieldName" ;          # Required
          ggen:type xsd:string .            # Required

# Optional (0..1): May be absent
ggen:Field ggen:documentation "..." .       # Optional
          ggen:defaultValue "5" .           # Optional

# Multiple (0..*): Zero or more
ggen:Crate ggen:modules ggen:Module1, ggen:Module2, ggen:Module3 .

# One or more (1..*): At least one required
ggen:Enum ggen:variants ggen:Var1, ggen:Var2 .
```

### 5. Comments & Documentation
```turtle
# Comments in Turtle use "#"
# Descriptive comments help explain intent

# Formal documentation goes in ggen:documentation property
ggen:CoreCrate ggen:documentation """
Semantic projection engine for ontology-driven code generation.

Responsibilities:
- RDF/SPARQL operations (Oxigraph triple store)
- Template rendering (Tera engine)
- Delta detection & change analysis
- Three-way merge for generated + manual content
- Universal lifecycle management

Public APIs:
- Graph operations (load, query, export, diff)
- Template system (render, output)
- Lifecycle phases (init, setup, generate, test, deploy)
"""@en .
```

---

## Summary of Key Entities

| Entity | Purpose | Cardinality | Notes |
|--------|---------|-------------|-------|
| ggen:Crate | Rust crate | 1..* | Top-level organization unit |
| ggen:Module | Rust module | 1..* | Within crates |
| ggen:Type | Type (Struct/Enum/Trait) | 1..* | Domain model |
| ggen:Field | Struct field | 0..* | Type composition |
| ggencli:CliCommand | CLI command | 32 | All user-facing commands |
| ggencli:Argument | Positional arg | 0..* | Command parameters |
| ggencli:Flag | Named flag | 0..* | Command options |
| ggenmp:Package | Marketplace package | 1..* | Distributed code |
| ggen:Guard | Validation rule | 1..* | Quality gates |
| ggenai:LlmProvider | LLM API provider | 1..* | AI integration |
| ggenai:LlmModel | Specific model | 1..* | Inference targets |
| ggen:Constraint | Validation constraint | 0..* | Business rules |

---

**Document Version**: 1.0
**Created**: 2025-11-17
**Branch**: `claude/plan-ggen-v3-rewrite-01PyJAjvvvwdVWwD6wickodF`
**Next Step**: Week 1 of Phase 1 - Implement Ontology Domain Model
