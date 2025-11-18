//! HTF RDF Ontology Definition

use crate::models::*;

/// Base ontology namespace
pub const HTF_NS: &str = "http://thesis.local/htf#";
pub const SHACL_NS: &str = "http://www.w3.org/ns/shacl#";

/// Canonical Λ-ordering (total order chain)
pub fn lambda_order() -> Vec<ShardFamily> {
    vec![
        // Primordial structure: Problem → Gap → Claim → Intro
        ShardFamily::Problem,
        ShardFamily::Gap,
        ShardFamily::Claim,
        ShardFamily::Intro,
        // Method space
        ShardFamily::Method,
        ShardFamily::Context,
        ShardFamily::Voice,
        // Body of knowledge
        ShardFamily::Canon,
        ShardFamily::Field,
        ShardFamily::Artifact,
        // Evidence
        ShardFamily::Proof,
        ShardFamily::Paper,
        ShardFamily::Result,
        ShardFamily::Evaluation,
        // Interpretation
        ShardFamily::Objection,
        ShardFamily::Discussion,
        ShardFamily::Reply,
        // Pattern extraction
        ShardFamily::Pattern,
        ShardFamily::Theory,
        ShardFamily::Analysis,
        // Synthesis
        ShardFamily::Synthesis,
        ShardFamily::Insight,
        ShardFamily::Impact,
        ShardFamily::Design,
        ShardFamily::Ground,
        ShardFamily::Conclusion,
    ]
}

/// Get all Δ-families
pub fn all_families() -> Vec<ShardFamily> {
    vec![
        ShardFamily::Intro,
        ShardFamily::Method,
        ShardFamily::Result,
        ShardFamily::Discussion,
        ShardFamily::Paper,
        ShardFamily::Synthesis,
        ShardFamily::Claim,
        ShardFamily::Ground,
        ShardFamily::Proof,
        ShardFamily::Objection,
        ShardFamily::Reply,
        ShardFamily::Gap,
        ShardFamily::Design,
        ShardFamily::Evaluation,
        ShardFamily::Impact,
        ShardFamily::Context,
        ShardFamily::Canon,
        ShardFamily::Analysis,
        ShardFamily::Conclusion,
        ShardFamily::Problem,
        ShardFamily::Artifact,
        ShardFamily::Theory,
        ShardFamily::Field,
        ShardFamily::Voice,
        ShardFamily::Pattern,
        ShardFamily::Insight,
    ]
}

/// Core Q-invariants
pub fn core_invariants() -> Vec<Invariant> {
    vec![
        Invariant {
            name: "AllFamiliesCovered".to_string(),
            description: "All 26 Δ-families must be represented in the thesis".to_string(),
            constraint_type: ConstraintType::AllFamiliesCovered,
        },
        Invariant {
            name: "NoCyclicDependencies".to_string(),
            description: "Shard dependencies must form a DAG (no cycles)".to_string(),
            constraint_type: ConstraintType::NoCyclicDependencies,
        },
        Invariant {
            name: "TotalOrderPreserved".to_string(),
            description: "Λ-ordering must be respected by all shards".to_string(),
            constraint_type: ConstraintType::TotalOrderPreserved,
        },
        Invariant {
            name: "ContentNotEmpty".to_string(),
            description: "All shards must have non-empty content".to_string(),
            constraint_type: ConstraintType::ContentNotEmpty,
        },
        Invariant {
            name: "StatusConsistent".to_string(),
            description: "Shard status must follow valid transitions".to_string(),
            constraint_type: ConstraintType::StatusConsistent,
        },
    ]
}

/// TTL representation of HTF ontology
pub fn ontology_ttl() -> &'static str {
    r#"
@prefix : <http://thesis.local/htf#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# HTF Ontology: Hyper-Thesis Framework

:HTFOntology a owl:Ontology ;
    rdfs:label "Hyper-Thesis Framework Ontology"@en ;
    rdfs:comment "Unified μ-architecture for thesis research planning"@en .

# Classes: Δ-Shards
:DeltaShard a owl:Class ;
    rdfs:label "Δ-Shard"@en ;
    rdfs:comment "Atomic unit of thesis work"@en .

:ShardFamily a owl:Class ;
    rdfs:label "Shard Family"@en ;
    rdfs:comment "Category of Δ-Shard (26 types)"@en .

# Properties: Λ-Ordering
:lambdaOrder a rdf:Property ;
    rdfs:label "Λ-Order"@en ;
    rdfs:comment "Total order relation between shards"@en ;
    rdfs:domain :DeltaShard ;
    rdfs:range :DeltaShard .

# Properties: Π-Merge
:piMerge a rdf:Property ;
    rdfs:label "Π-Merge"@en ;
    rdfs:comment "Composition of multiple shards into thesis"@en ;
    rdfs:domain :DeltaShard .

# Properties: Γ-Globalization
:gammaGlobal a rdf:Property ;
    rdfs:label "Γ-Global"@en ;
    rdfs:comment "Globalization of shards into coherent whole"@en ;
    rdfs:domain :DeltaShard .

# Properties: Q-Invariants
:qInvariant a owl:Class ;
    rdfs:label "Q-Invariant"@en ;
    rdfs:comment "Constraint preserved across all layers"@en .

# Properties: τ-Evolution
:tauEvolution a rdf:Property ;
    rdfs:label "τ-Evolution"@en ;
    rdfs:comment "Draft-to-final transformation"@en ;
    rdfs:domain :DeltaShard ;
    rdfs:range xsd:string .

# Shard Families as Individuals
:IntroFamily a :ShardFamily ; rdfs:label "Intro"@en .
:MethodFamily a :ShardFamily ; rdfs:label "Method"@en .
:ResultFamily a :ShardFamily ; rdfs:label "Result"@en .
:DiscussionFamily a :ShardFamily ; rdfs:label "Discussion"@en .
:ProblemFamily a :ShardFamily ; rdfs:label "Problem"@en .
:GapFamily a :ShardFamily ; rdfs:label "Gap"@en .
:ArtifactFamily a :ShardFamily ; rdfs:label "Artifact"@en .
:ProofFamily a :ShardFamily ; rdfs:label "Proof"@en .
:TheoryFamily a :ShardFamily ; rdfs:label "Theory"@en .
:InsightFamily a :ShardFamily ; rdfs:label "Insight"@en .
"#
}
