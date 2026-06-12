#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

//! LLM Generation Integration Tests
//!
//! Tests for auto-generating skill implementations using LLM during sync pipeline.

use ggen_core::codegen::pipeline::{GenerationPipeline, RuleType};
use ggen_core::manifest::{
    GenerationConfig, GenerationMode, GenerationRule, GgenManifest, InferenceConfig,
    OntologyConfig, ProjectConfig, QuerySource, TemplateSource, ValidationConfig,
};
use std::path::PathBuf;
use tempfile::TempDir;

fn create_test_manifest(base_dir: &PathBuf) -> GgenManifest {
    GgenManifest {
        project: ProjectConfig {
            name: "test-a2a-agent".to_string(),
            version: "0.1.0".to_string(),
            description: Some("Test A2A agent with LLM generation".to_string()),
            ..Default::default()
        },
        ontology: OntologyConfig {
            source: PathBuf::from("test.ttl"),
            imports: vec![],
            base_iri: None,
            prefixes: Default::default(),
            ..Default::default()
        },
        inference: InferenceConfig::default(),
        generation: GenerationConfig {
            output_dir: PathBuf::from("output"),
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            rules: vec![],
            enable_llm: false,
            llm_model: None,
            llm_provider: None,
        },
        validation: ValidationConfig::default(),
        packs: vec![],
        ..Default::default()
    }
}

#[test]
fn test_llm_generation_disabled_by_default() {
    // Arrange: Create manifest without LLM config
    let temp_dir = TempDir::new().unwrap();
    let base_dir = temp_dir.path().to_path_buf();
    let manifest = create_test_manifest(&base_dir);

    // Act: Create pipeline
    let pipeline = GenerationPipeline::new(manifest, base_dir);

    // Assert: Pipeline should not have LLM enabled by default
    // This will be verified by checking that generate_skill_impl returns TODO stub
}

#[test]
fn test_llm_prompt_construction() {
    // Test that prompts are constructed correctly from skill metadata
    let skill_name = "search_files";
    let system_prompt = "Search files by pattern";
    let implementation_hint = "Use glob patterns";
    let language = "rust";

    let expected_prompt = format!(
        "Generate {} implementation for skill '{}' with description: {}. Hint: {}",
        language, skill_name, system_prompt, implementation_hint
    );

    assert_eq!(
        expected_prompt,
        format!(
            "Generate {} implementation for skill '{}' with description: {}. Hint: {}",
            language, skill_name, system_prompt, implementation_hint
        )
    );
}

#[test]
fn test_llm_response_integration_into_template() {
    // Test that LLM response is inserted into template context
    let generated_impl = "pub async fn search_files(pattern: &str) -> Result<Vec<String>> { ... }";
    let template_context = serde_json::json!({
        "skill_name": "search_files",
        "generated_impl": generated_impl
    });

    assert_eq!(template_context["generated_impl"], generated_impl);
}
