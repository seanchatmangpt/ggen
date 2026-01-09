//! Demo: Converting TTL Ontologies to DSPy Signatures
//!
//! This example demonstrates how to use the TTLToSignatureTranspiler to convert
//! RDF ontologies with SHACL shapes into DSPy-compatible signatures.
//!
//! Run with: cargo run --example ttl_to_signature_demo --package ggen-ai

use ggen_ai::codegen::TTLToSignatureTranspiler;
use oxigraph::store::Store;
use oxigraph::io::RdfFormat;
use std::io::BufReader;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create an RDF store
    let store = Store::new()?;

    // Example: Create a simple ontology in memory
    let ttl_content = r#"
@prefix : <http://example.com/thesis/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix cns: <http://cns.io/ontology#> .

# Thesis Abstract Expansion Signature
:ThesisAbstractExpansionShape
    a sh:NodeShape ;
    sh:targetClass :ThesisAbstractExpansion ;
    sh:property :CurrentAbstractProperty ;
    sh:property :TitleProperty ;
    sh:property :ResearchAreaProperty ;
    sh:property :ExpandedAbstractProperty ;
    sh:property :MotivationProperty .

:CurrentAbstractProperty
    sh:path :currentAbstract ;
    sh:datatype xsd:string ;
    rdfs:comment "Current thesis abstract from RDF ontology" .

:TitleProperty
    sh:path :title ;
    sh:datatype xsd:string ;
    rdfs:comment "Thesis title" .

:ResearchAreaProperty
    sh:path :researchArea ;
    sh:datatype xsd:string ;
    rdfs:comment "Research domain/field" .

:ExpandedAbstractProperty
    sh:path :expandedAbstract ;
    sh:datatype xsd:string ;
    cns:outputField "true" ;
    rdfs:comment "Enhanced abstract with research context, motivation, and contributions" .

:MotivationProperty
    sh:path :motivation ;
    sh:datatype xsd:string ;
    cns:outputField "true" ;
    rdfs:comment "Research motivation and problem statement (output)" .

# Section Content Suggestion Signature
:SectionContentSuggestionShape
    a sh:NodeShape ;
    sh:targetClass :SectionContentSuggestion ;
    sh:property :SectionTitleProperty ;
    sh:property :CurrentContentProperty ;
    sh:property :ChapterContextProperty ;
    sh:property :ContentSuggestionsProperty ;
    sh:property :RelatedConceptsProperty .

:SectionTitleProperty
    sh:path :sectionTitle ;
    sh:datatype xsd:string ;
    rdfs:comment "Current section title" .

:CurrentContentProperty
    sh:path :currentContent ;
    sh:datatype xsd:string ;
    rdfs:comment "Existing section content from RDF" .

:ChapterContextProperty
    sh:path :chapterContext ;
    sh:datatype xsd:string ;
    rdfs:comment "Chapter title and overall context" .

:ContentSuggestionsProperty
    sh:path :contentSuggestions ;
    sh:datatype xsd:string ;
    cns:outputField "true" ;
    rdfs:comment "Suggested additional content points (output)" .

:RelatedConceptsProperty
    sh:path :relatedConcepts ;
    sh:datatype xsd:string ;
    cns:outputField "true" ;
    rdfs:comment "Related concepts to explore (output)" .
    "#;

    // Load TTL content into the store
    println!("Loading TTL ontology...");
    let cursor = BufReader::new(ttl_content.as_bytes());
    store.load_from_reader(RdfFormat::Turtle, cursor)?;
    println!("✓ Ontology loaded successfully\n");

    // Create the transpiler
    let mut transpiler = TTLToSignatureTranspiler::new();

    // Find all classes with SHACL shapes
    println!("Finding classes with SHACL shapes...");
    let classes = transpiler.find_classes_with_shapes(&store)?;
    println!("✓ Found {} classes\n", classes.len());

    for class_iri in &classes {
        println!("  - {}", class_iri);
    }
    println!();

    // Build signatures
    println!("Building signatures...");
    let signatures = transpiler.build_signatures(&store)?;
    println!("✓ Generated {} signatures\n", signatures.len());

    // Display generated signatures
    for sig in &signatures {
        println!("═══════════════════════════════════════════════════");
        println!("Signature: {}", sig.name);
        println!("Description: {}", sig.description);
        println!("───────────────────────────────────────────────────");

        println!("\nInput Fields:");
        for input in &sig.inputs {
            println!("  • {} ({})", input.name(), input.type_annotation());
            println!("    └─ {}", input.desc());
        }

        println!("\nOutput Fields:");
        for output in &sig.outputs {
            println!("  • {} ({})", output.name(), output.type_annotation());
            println!("    └─ {}", output.desc());
        }
        println!();
    }

    println!("═══════════════════════════════════════════════════");
    println!("\nSummary:");
    println!("  Total Signatures: {}", signatures.len());
    println!("  Total Input Fields: {}", signatures.iter().map(|s| s.inputs.len()).sum::<usize>());
    println!("  Total Output Fields: {}", signatures.iter().map(|s| s.outputs.len()).sum::<usize>());

    // Demonstrate individual transpiler methods
    println!("\n═══════════════════════════════════════════════════\n");
    println!("Demonstrating Individual Methods:\n");

    // Name transformation examples
    println!("Name Transformations:");
    let names = vec![
        "MyPropertyName",
        "my-property-name",
        "CONSTANT_VALUE",
        "1stProperty",
    ];
    for name in names {
        let snake = transpiler.snake_case(name);
        println!("  {} → {}", name, snake);
    }

    println!("\nIRI Local Name Extraction:");
    let iris = vec![
        "http://example.com/ontology#ClassName",
        "http://example.com/ontology/ClassName",
        "SimpleClassName",
    ];
    for iri in iris {
        let local = transpiler.safe_local_name(iri);
        println!("  {} → {}", iri, local);
    }

    println!("\nDatatype Mapping:");
    let types = vec![
        "http://www.w3.org/2001/XMLSchema#string",
        "http://www.w3.org/2001/XMLSchema#integer",
        "http://www.w3.org/2001/XMLSchema#boolean",
        "http://www.w3.org/2001/XMLSchema#float",
    ];
    for xsd_type in types {
        let rust_type = transpiler.extract_datatype(xsd_type);
        println!("  {} → {}", xsd_type.split('#').last().unwrap_or("unknown"), rust_type);
    }

    println!("\n═══════════════════════════════════════════════════");
    println!("\nDemo completed successfully!");

    Ok(())
}
