//! HTF RDF Ontology Definition

use crate::models::*;

/// Base ontology namespace
#[allow(dead_code)]
pub const HTF_NS: &str = "http://thesis.local/htf#";
#[allow(dead_code)]
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
#[allow(dead_code)]
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

/// TTL representation of HTF ontology with all 26 families
#[allow(dead_code)]
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

# Classes
:DeltaShard a owl:Class ;
    rdfs:label "Δ-Shard"@en ;
    rdfs:comment "Atomic unit of thesis work"@en .

:ShardFamily a owl:Class ;
    rdfs:label "Shard Family"@en ;
    rdfs:comment "Category of Δ-Shard (26 types)"@en .

:qInvariant a owl:Class ;
    rdfs:label "Q-Invariant"@en ;
    rdfs:comment "Constraint preserved across all layers"@en .

# Properties
:id a rdf:Property ;
    rdfs:label "Identifier"@en ;
    rdfs:domain :ShardFamily ;
    rdfs:range xsd:string .

:name a rdf:Property ;
    rdfs:label "Name"@en ;
    rdfs:domain :ShardFamily ;
    rdfs:range xsd:string .

:lambdaOrder a rdf:Property ;
    rdfs:label "Λ-Order"@en ;
    rdfs:comment "Total order relation between shards"@en ;
    rdfs:domain :DeltaShard ;
    rdfs:range :DeltaShard .

:piMerge a rdf:Property ;
    rdfs:label "Π-Merge"@en ;
    rdfs:comment "Composition of multiple shards into thesis"@en ;
    rdfs:domain :DeltaShard .

:gammaGlobal a rdf:Property ;
    rdfs:label "Γ-Global"@en ;
    rdfs:comment "Globalization of shards into coherent whole"@en ;
    rdfs:domain :DeltaShard .

:tauEvolution a rdf:Property ;
    rdfs:label "τ-Evolution"@en ;
    rdfs:comment "Draft-to-final transformation"@en ;
    rdfs:domain :DeltaShard ;
    rdfs:range xsd:string .

# All 26 Δ-Shard Families (with id and name properties for SPARQL discovery)
:F01 a :ShardFamily ; :id "F01" ; :name "Problem" .
:F02 a :ShardFamily ; :id "F02" ; :name "Gap" .
:F03 a :ShardFamily ; :id "F03" ; :name "Claim" .
:F04 a :ShardFamily ; :id "F04" ; :name "Intro" .
:F05 a :ShardFamily ; :id "F05" ; :name "Method" .
:F06 a :ShardFamily ; :id "F06" ; :name "Context" .
:F07 a :ShardFamily ; :id "F07" ; :name "Voice" .
:F08 a :ShardFamily ; :id "F08" ; :name "Canon" .
:F09 a :ShardFamily ; :id "F09" ; :name "Field" .
:F10 a :ShardFamily ; :id "F10" ; :name "Artifact" .
:F11 a :ShardFamily ; :id "F11" ; :name "Proof" .
:F12 a :ShardFamily ; :id "F12" ; :name "Paper" .
:F13 a :ShardFamily ; :id "F13" ; :name "Result" .
:F14 a :ShardFamily ; :id "F14" ; :name "Evaluation" .
:F15 a :ShardFamily ; :id "F15" ; :name "Objection" .
:F16 a :ShardFamily ; :id "F16" ; :name "Discussion" .
:F17 a :ShardFamily ; :id "F17" ; :name "Reply" .
:F18 a :ShardFamily ; :id "F18" ; :name "Pattern" .
:F19 a :ShardFamily ; :id "F19" ; :name "Theory" .
:F20 a :ShardFamily ; :id "F20" ; :name "Analysis" .
:F21 a :ShardFamily ; :id "F21" ; :name "Synthesis" .
:F22 a :ShardFamily ; :id "F22" ; :name "Insight" .
:F23 a :ShardFamily ; :id "F23" ; :name "Impact" .
:F24 a :ShardFamily ; :id "F24" ; :name "Design" .
:F25 a :ShardFamily ; :id "F25" ; :name "Ground" .
:F26 a :ShardFamily ; :id "F26" ; :name "Conclusion" .
"#
}
