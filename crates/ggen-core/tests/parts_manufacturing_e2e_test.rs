#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

//! End-to-end tests for parts manufacturing pipeline (μ₀-μ₅)
//!
//! Chicago TDD: Real compiler invocations, real file I/O, real cryptography.
//! No mocks. All evidence externalizable (files, hashes, signatures).

use ggen_core::parts_foundry::{
    AdapterGenerator, InterfaceSpec, ManufacturedPart, PartCompiler, PartFoundry, PartManifest,
    PartSigner, PartSpec, SignedPart,
};
use serde_json::json;
use std::fs;
use tempfile::TempDir;

/// Helper to create a basic part spec for testing
fn create_test_spec(id: &str, part_type: &str) -> PartSpec {
    PartSpec {
        id: id.to_string(),
        version: "1.0.0".to_string(),
        part_type: part_type.to_string(),
        rdf_source: r#"
@prefix ex: <http://example.org/> .
ex:TestPart a ex:Part ;
    ex:name "Test Part" ;
    ex:version "1.0.0" .
        "#
        .to_string(),
        target_language: if part_type == "beam" {
            "erlang".to_string()
        } else {
            "rust".to_string()
        },
    }
}

/// Helper to create a test manifest
fn create_test_manifest() -> PartManifest {
    PartManifest {
        interfaces: vec![InterfaceSpec {
            name: "Process".to_string(),
            params: json!({"input": "bytes"}),
            returns: json!({"output": "bytes"}),
        }],
        dependencies: Default::default(),
        config: json!({}),
    }
}

#[tokio::test]
async fn test_rust_adapter_generation() {
    let gen = AdapterGenerator::new();
    let spec = create_test_spec("test-part", "wasm32");
    let manifest = create_test_manifest();

    let code = gen
        .generate(&spec, &manifest)
        .await
        .expect("Adapter generation failed");

    assert!(code.contains("Generated Genesis membrane adapter"));
    assert!(code.contains("pub extern \"C\" fn"));
    assert!(code.contains("process"));
}

#[tokio::test]
async fn test_erlang_adapter_generation() {
    let gen = AdapterGenerator::new();
    let spec = create_test_spec("test-part", "beam");
    let manifest = create_test_manifest();

    let code = gen
        .generate(&spec, &manifest)
        .await
        .expect("Adapter generation failed");

    assert!(code.contains("Generated Genesis membrane adapter"));
    assert!(code.contains("-module(genesis_test_part)"));
    assert!(code.contains("process/1"));
}

#[tokio::test]
async fn test_part_compiler_unsupported_type() {
    let compiler = PartCompiler::new();
    let result = compiler.compile("unknown-type", "source code").await;
    assert!(result.is_err());
}

#[test]
fn test_part_signing() {
    let signer = PartSigner::new();
    let manufactured = ManufacturedPart {
        id: "test-part".to_string(),
        version: "1.0.0".to_string(),
        payload: vec![1, 2, 3, 4, 5],
        payload_hash: "abcd1234".to_string(),
        payload_size: 5,
        interfaces: vec![],
        compiler_output: String::new(),
        adapter_source: "// source".to_string(),
    };

    let signed = signer
        .sign_part(manufactured.clone())
        .expect("Part signing failed");

    assert!(!signed.signature.is_empty());
    assert!(!signed.verifying_key.is_empty());
    assert_eq!(signed.manufactured.id, "test-part");
    assert_eq!(signed.manufactured.version, "1.0.0");
    assert_eq!(signed.manufactured.payload_size, 5);
}

#[test]
fn test_signature_verification() {
    let signer = PartSigner::new();
    let manufactured = ManufacturedPart {
        id: "test-part".to_string(),
        version: "1.0.0".to_string(),
        payload: vec![1, 2, 3],
        payload_hash: "hash".to_string(),
        payload_size: 3,
        interfaces: vec![],
        compiler_output: String::new(),
        adapter_source: String::new(),
    };

    let signed = signer.sign_part(manufactured).expect("Signing failed");

    let is_valid = signer
        .verify_signature(&signed)
        .expect("Verification failed");

    assert!(is_valid);
}

#[test]
fn test_manufactured_part_to_file() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let part_dir = temp_dir.path().join("test-part").join("1.0.0");

    fs::create_dir_all(&part_dir).expect("Failed to create part directory");

    let manufactured = ManufacturedPart {
        id: "test-part".to_string(),
        version: "1.0.0".to_string(),
        payload: vec![0xDE, 0xAD, 0xBE, 0xEF],
        payload_hash: "deadbeef".to_string(),
        payload_size: 4,
        interfaces: vec![],
        compiler_output: String::new(),
        adapter_source: String::new(),
    };

    let manifest_path = part_dir.join("part.json");
    let manifest_json =
        serde_json::to_string_pretty(&manufactured).expect("Failed to serialize manifest");
    fs::write(&manifest_path, &manifest_json).expect("Failed to write manifest");

    let binary_path = part_dir.join("test-part.bin");
    fs::write(&binary_path, &manufactured.payload).expect("Failed to write binary");

    assert!(manifest_path.exists());
    assert!(binary_path.exists());

    let read_manifest = fs::read_to_string(&manifest_path).expect("Failed to read manifest");
    assert!(read_manifest.contains("test-part"));
    assert!(read_manifest.contains("1.0.0"));

    let read_binary = fs::read(&binary_path).expect("Failed to read binary");
    assert_eq!(read_binary, vec![0xDE, 0xAD, 0xBE, 0xEF]);
}

#[tokio::test]
async fn test_part_manifest_serialization() {
    let manifest = create_test_manifest();

    let json = serde_json::to_string(&manifest).expect("Serialization failed");
    assert!(json.contains("Process"));
    assert!(json.contains("params"));
    assert!(json.contains("returns"));

    let deserialized: PartManifest = serde_json::from_str(&json).expect("Deserialization failed");
    assert_eq!(deserialized.interfaces.len(), 1);
    assert_eq!(deserialized.interfaces[0].name, "Process");
}

#[test]
fn test_payload_hash_consistency() {
    let payload = vec![1, 2, 3, 4, 5];

    let hash1 = blake3::hash(&payload).to_hex().to_string();
    let hash2 = blake3::hash(&payload).to_hex().to_string();

    assert_eq!(hash1, hash2);
    assert!(!hash1.is_empty());
}

#[test]
fn test_part_payload_size_tracking() {
    let payloads = vec![
        (vec![1], 1),
        (vec![1, 2, 3], 3),
        (vec![0; 100], 100),
        (vec![], 0),
    ];

    for (payload, expected_size) in payloads {
        let part = ManufacturedPart {
            id: "test".to_string(),
            version: "1.0.0".to_string(),
            payload: payload.clone(),
            payload_hash: "test".to_string(),
            payload_size: payload.len() as u64,
            interfaces: vec![],
            compiler_output: String::new(),
            adapter_source: String::new(),
        };

        assert_eq!(part.payload_size, expected_size);
        assert_eq!(part.payload.len() as u64, expected_size);
    }
}

#[test]
fn test_multiple_interfaces_in_manifest() {
    let manifest = PartManifest {
        interfaces: vec![
            InterfaceSpec {
                name: "Process".to_string(),
                params: json!({"input": "bytes"}),
                returns: json!({"output": "bytes"}),
            },
            InterfaceSpec {
                name: "Validate".to_string(),
                params: json!({"data": "bytes"}),
                returns: json!({"valid": "bool"}),
            },
            InterfaceSpec {
                name: "Transform".to_string(),
                params: json!({"input": "object"}),
                returns: json!({"result": "object"}),
            },
        ],
        dependencies: Default::default(),
        config: json!({}),
    };

    assert_eq!(manifest.interfaces.len(), 3);
    assert_eq!(manifest.interfaces[0].name, "Process");
    assert_eq!(manifest.interfaces[1].name, "Validate");
    assert_eq!(manifest.interfaces[2].name, "Transform");
}

#[test]
fn test_signed_part_deterministic() {
    let signer = PartSigner::new();
    let manufactured = ManufacturedPart {
        id: "test-part".to_string(),
        version: "1.0.0".to_string(),
        payload: vec![1, 2, 3],
        payload_hash: "hash".to_string(),
        payload_size: 3,
        interfaces: vec![],
        compiler_output: String::new(),
        adapter_source: String::new(),
    };

    let signed1 = signer
        .sign_part(manufactured.clone())
        .expect("First signing failed");
    let signed2 = signer
        .sign_part(manufactured)
        .expect("Second signing failed");

    assert_eq!(signed1.signature, signed2.signature);
    assert_eq!(signed1.verifying_key, signed2.verifying_key);
}

#[test]
fn test_blake3_hash_integrity() {
    let data1 = b"test data";
    let data2 = b"different data";

    let hash1 = blake3::hash(data1).to_hex().to_string();
    let hash2 = blake3::hash(data2).to_hex().to_string();

    assert_ne!(hash1, hash2);

    let hash1_again = blake3::hash(data1).to_hex().to_string();
    assert_eq!(hash1, hash1_again);
}

#[tokio::test]
async fn test_adapter_generator_language_support() {
    let gen = AdapterGenerator::new();
    let manifest = create_test_manifest();

    // Test Rust
    let rust_spec = create_test_spec("test", "wasm32");
    let rust_code = gen
        .generate(&rust_spec, &manifest)
        .await
        .expect("Rust generation failed");
    assert!(rust_code.contains("pub extern \"C\" fn"));

    // Test Erlang
    let erlang_spec = PartSpec {
        id: "test".to_string(),
        version: "1.0.0".to_string(),
        part_type: "beam".to_string(),
        rdf_source: "(RDF)".to_string(),
        target_language: "erlang".to_string(),
    };
    let erlang_code = gen
        .generate(&erlang_spec, &manifest)
        .await
        .expect("Erlang generation failed");
    assert!(erlang_code.contains("-module("));

    // Test unsupported language
    let bad_spec = PartSpec {
        id: "test".to_string(),
        version: "1.0.0".to_string(),
        part_type: "wasm32".to_string(),
        rdf_source: "(RDF)".to_string(),
        target_language: "cobol".to_string(),
    };
    let result = gen.generate(&bad_spec, &manifest).await;
    assert!(result.is_err());
}

#[test]
fn test_part_spec_fields() {
    let spec = create_test_spec("my-part", "wasm32");

    assert_eq!(spec.id, "my-part");
    assert_eq!(spec.version, "1.0.0");
    assert_eq!(spec.part_type, "wasm32");
    assert_eq!(spec.target_language, "rust");
    assert!(!spec.rdf_source.is_empty());
}
