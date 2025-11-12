use anyhow::Result;
use reasoner_cli::{OWLProfile, Reasoner, ReasonerType};
use std::fs;

fn main() -> Result<()> {
    println!("=== Consistency Checking Example ===\n");

    // Create inconsistent ontology
    let inconsistent_ontology = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix : <http://example.org/robots#> .

# Define classes
:Person a owl:Class ;
    rdfs:subClassOf :Biological .
:Robot a owl:Class ;
    rdfs:subClassOf :NonBiological .
:Biological a owl:Class .
:NonBiological a owl:Class ;
    owl:complementOf :Biological .

# Inconsistent class: both biological and non-biological
:Cyborg a owl:Class ;
    rdfs:subClassOf :Person, :Robot .
"#;

    fs::write("/tmp/inconsistent.ttl", inconsistent_ontology)?;

    // Check inconsistent ontology
    println!("1. Checking inconsistent ontology...");
    let mut reasoner = Reasoner::new(ReasonerType::Pellet, OWLProfile::DL);
    reasoner.load_ontology("/tmp/inconsistent.ttl")?;

    let result = reasoner.check_consistency(true)?;
    println!("   Status: {:?}", result.status);
    println!(
        "   Unsatisfiable classes: {}",
        result.unsatisfiable_classes.len()
    );

    if !result.explanations.is_empty() {
        println!("\n   Explanations:");
        for explanation in &result.explanations {
            println!("     Class/Axiom: {}", explanation.class_or_axiom);
            for (i, justification) in explanation.justifications.iter().enumerate() {
                println!("       Justification {}:", i + 1);
                for axiom in &justification.axioms {
                    println!("         - {}", axiom);
                }
            }
        }
    }

    // Create consistent ontology
    let consistent_ontology = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix : <http://example.org/simple#> .

:Animal a owl:Class .
:Mammal a owl:Class ;
    rdfs:subClassOf :Animal .
:Dog a owl:Class ;
    rdfs:subClassOf :Mammal .
:Cat a owl:Class ;
    rdfs:subClassOf :Mammal .

:fido a :Dog .
:whiskers a :Cat .
"#;

    fs::write("/tmp/consistent.ttl", consistent_ontology)?;

    println!("\n2. Checking consistent ontology...");
    let mut reasoner2 = Reasoner::new(ReasonerType::Pellet, OWLProfile::DL);
    reasoner2.load_ontology("/tmp/consistent.ttl")?;

    let result2 = reasoner2.check_consistency(false)?;
    println!("   Status: {:?}", result2.status);
    println!(
        "   Unsatisfiable classes: {}",
        result2.unsatisfiable_classes.len()
    );

    if let Some(time) = result2.metrics.consistency_check_time_ms {
        println!("   Check time: {}ms", time);
    }

    println!("\n✓ Consistency checking complete!");

    Ok(())
}
