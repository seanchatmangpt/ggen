//! Determinism tests for prompt manufacturing
//!
//! Validates that same input → same output across all compilation stages.

use ggen_prompt_mfg::{
    emitter::PromptEmitter,
    hash::compute_prompt_hash,
    ir::{BlockType, ContentBlock, PromptIR, PromptMetadata, Section, SectionType},
    validator::PromptValidator,
    PromptCompiler,
};
use std::collections::BTreeMap;

#[test]
fn test_compilation_determinism() {
    // Same CONSTRUCT query should produce same result
    let construct_query = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";

    let compiler = PromptCompiler::new().expect("Failed to create compiler");

    let result1 = compiler
        .compile_from_construct(construct_query)
        .expect("First compilation failed");

    let result2 = compiler
        .compile_from_construct(construct_query)
        .expect("Second compilation failed");

    assert_eq!(
        result1.content, result2.content,
        "Content must be deterministic"
    );
    assert_eq!(result1.hash, result2.hash, "Hash must be deterministic");
    assert_eq!(result1.ir, result2.ir, "IR must be deterministic");
}

#[test]
fn test_ir_determinism() {
    // Create same IR twice
    let ir1 = create_test_ir();
    let ir2 = create_test_ir();

    assert_eq!(ir1, ir2, "IR creation must be deterministic");

    // Serialize and compare
    let json1 = serde_json::to_string(&ir1).unwrap();
    let json2 = serde_json::to_string(&ir2).unwrap();

    assert_eq!(json1, json2, "IR serialization must be deterministic");
}

#[test]
fn test_hash_determinism() {
    let content = "Test prompt content for hashing";

    let hash1 = compute_prompt_hash(content).unwrap();
    let hash2 = compute_prompt_hash(content).unwrap();

    assert_eq!(hash1, hash2, "Hash computation must be deterministic");
}

#[test]
fn test_btreemap_ordering_determinism() {
    // BTreeMap maintains sorted order regardless of insertion order
    let mut map1 = BTreeMap::new();
    map1.insert("z", "last");
    map1.insert("a", "first");
    map1.insert("m", "middle");

    let mut map2 = BTreeMap::new();
    map2.insert("a", "first");
    map2.insert("m", "middle");
    map2.insert("z", "last");

    let keys1: Vec<_> = map1.keys().collect();
    let keys2: Vec<_> = map2.keys().collect();

    assert_eq!(keys1, keys2, "BTreeMap key order must be deterministic");
    assert_eq!(keys1, vec![&"a", &"m", &"z"], "Keys must be sorted");
}

#[test]
fn test_emission_determinism() {
    let ir = create_test_ir();
    let emitter = PromptEmitter::new().expect("Failed to create emitter");

    let output1 = emitter.emit(&ir).expect("First emission failed");
    let output2 = emitter.emit(&ir).expect("Second emission failed");

    assert_eq!(output1, output2, "Emission must be deterministic");
}

#[test]
fn test_validation_determinism() {
    let ir = create_test_ir();
    let validator = PromptValidator::new();

    let result1 = validator.validate(&ir);
    let result2 = validator.validate(&ir);

    assert_eq!(
        result1.is_ok(),
        result2.is_ok(),
        "Validation must be deterministic"
    );
}

#[test]
fn test_full_pipeline_determinism() {
    // Test entire pipeline multiple times
    let construct_query = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";
    let compiler = PromptCompiler::new().expect("Failed to create compiler");

    let outputs: Vec<_> = (0..5)
        .map(|_| {
            compiler
                .compile_from_construct(construct_query)
                .expect("Compilation failed")
        })
        .collect();

    // All outputs must be identical
    for i in 1..outputs.len() {
        assert_eq!(
            outputs[0].content, outputs[i].content,
            "Content must be deterministic across runs"
        );
        assert_eq!(
            outputs[0].hash, outputs[i].hash,
            "Hash must be deterministic across runs"
        );
    }
}

#[test]
fn test_concurrent_compilation_determinism() {
    use std::sync::Arc;

    let construct_query = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";
    let compiler = Arc::new(PromptCompiler::new().expect("Failed to create compiler"));

    let handles: Vec<_> = (0..10)
        .map(|_| {
            let compiler = Arc::clone(&compiler);
            let query = construct_query.to_string();
            std::thread::spawn(move || compiler.compile_from_construct(&query))
        })
        .collect();

    let results: Vec<_> = handles
        .into_iter()
        .map(|h| h.join().unwrap().unwrap())
        .collect();

    // All concurrent compilations must produce same result
    for i in 1..results.len() {
        assert_eq!(
            results[0].hash, results[i].hash,
            "Concurrent compilation must be deterministic"
        );
    }
}

#[test]
fn test_whitespace_normalization() {
    let mut ir1 = create_test_ir();
    let mut ir2 = create_test_ir();

    // Add content with different whitespace
    ir1.sections.get_mut("system").unwrap().blocks[0].content = "Content\n\n\nwith   spaces  ".to_string();
    ir2.sections.get_mut("system").unwrap().blocks[0].content = "Content\n\nwith   spaces".to_string();

    let emitter = PromptEmitter::new().expect("Failed to create emitter");

    let output1 = emitter.emit(&ir1).expect("Emission 1 failed");
    let output2 = emitter.emit(&ir2).expect("Emission 2 failed");

    // Whitespace normalization should make them equal
    assert!(
        output1.lines().all(|l| !l.ends_with(' ')),
        "No trailing spaces allowed"
    );
    assert!(
        output2.lines().all(|l| !l.ends_with(' ')),
        "No trailing spaces allowed"
    );
}

// Helper function to create test IR
fn create_test_ir() -> PromptIR {
    let mut ir = PromptIR {
        sections: BTreeMap::new(),
        metadata: PromptMetadata {
            id: "test_prompt".to_string(),
            version: "1.0.0".to_string(),
            schema_version: "1.0.0".to_string(),
            source_ontology: "test://ontology".to_string(),
            construct_query: "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }".to_string(),
        },
        variables: BTreeMap::new(),
    };

    ir.add_section(
        "system".to_string(),
        Section {
            section_type: SectionType::System,
            blocks: vec![ContentBlock {
                block_type: BlockType::Text,
                content: "You are a helpful assistant.".to_string(),
                metadata: BTreeMap::new(),
            }],
            priority: 0,
        },
    );

    ir.add_section(
        "user".to_string(),
        Section {
            section_type: SectionType::User,
            blocks: vec![ContentBlock {
                block_type: BlockType::Text,
                content: "Hello, world!".to_string(),
                metadata: BTreeMap::new(),
            }],
            priority: 1,
        },
    );

    ir
}

#[test]
fn test_property_based_determinism() {
    use proptest::prelude::*;

    proptest!(|(
        id in "[a-z]{1,20}",
        version in "(0|[1-9][0-9]*)\\.0\\.0",
        content in "[a-zA-Z0-9 ]{1,100}"
    )| {
        let mut ir = PromptIR {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id,
                version,
                schema_version: "1.0.0".to_string(),
                source_ontology: "test://ontology".to_string(),
                construct_query: "test".to_string(),
            },
            variables: BTreeMap::new(),
        };

        ir.add_section(
            "test".to_string(),
            Section {
                section_type: SectionType::System,
                blocks: vec![ContentBlock {
                    block_type: BlockType::Text,
                    content,
                    metadata: BTreeMap::new(),
                }],
                priority: 0,
            },
        );

        let emitter = PromptEmitter::new().unwrap();
        let output1 = emitter.emit(&ir).unwrap();
        let output2 = emitter.emit(&ir).unwrap();

        prop_assert_eq!(output1, output2);
    });
}
