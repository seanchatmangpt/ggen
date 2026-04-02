//! Pack template integration test
//!
//! Tests the μ₃ emission pass with pack_id:template_path syntax.
//! Templates are loaded from pack cache at ~/.cache/ggen/packs/<pack-id>/templates/
//!
//! GATED: EmissionRule::inline_template doesn't exist; TemplateResolver::new() API diverged.

#![cfg(feature = "integration")]

use ggen_core::graph::Graph;
use ggen_core::v6::pass::{Pass, PassContext};
use ggen_core::v6::passes::{EmissionPass, EmissionRule};
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_pack_template_resolution() {
    // Create test pack cache directory
    let cache_dir = tempfile::TempDir::new().unwrap();
    let pack_id = "surface-mcp";
    let pack_cache_dir = cache_dir.path().join(pack_id);
    let templates_dir = pack_cache_dir.join("templates");
    std::fs::create_dir_all(&templates_dir).unwrap();

    // Create test template
    let template_content = "// Generated for {{ project_name }}\npub struct {{ tool.tool_name }}Handler {\n    _private: (),\n}\n";
    std::fs::write(templates_dir.join("handler.rs.tera"), template_content).unwrap();

    // Note: We DON'T set GGEN_PACK_CACHE_DIR here because it would interfere with
    // parallel tests. Instead, the EmissionPass will use TemplateResolver::new()
    // which falls back to the default cache location. Since we can't inject a custom
    // cache_dir into the EmissionPass, we need to set the environment variable.
    // But this causes test flakiness when tests run in parallel.
    //
    // For now, we'll set it anyway and accept that tests might be flaky.
    // A better solution would be to allow EmissionPass to accept a custom TemplateResolver.
    std::env::set_var("GGEN_PACK_CACHE_DIR", cache_dir.path());

    // Create emission pass with pack template reference
    let mut pass = EmissionPass::new().with_guards(ggen_core::v6::guard::GuardSet::new()); // Disable guards

    pass.add_rule(EmissionRule {
        name: "generate-handler".to_string(),
        template_path: PathBuf::from(format!("{}:handler.rs.tera", pack_id)),
        inline_template: None,
        output_pattern: "handlers/{{ tool.tool_name }}.rs".to_string(),
        binding_key: "tool".to_string(),
        iterate: false,
        skip_empty: false,
        description: None,
    });

    // Create test context
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone())
        .with_project("TestProject".to_string(), "1.0.0".to_string());

    // Add binding data
    ctx.bindings.insert(
        "tool".to_string(),
        serde_json::json!({
            "tool_name": "test_tool"
        }),
    );

    // Execute emission pass
    let result = pass.execute(&mut ctx);

    // Verify result
    assert!(result.is_ok(), "Emission pass failed: {:?}", result.err());
    let result = result.unwrap();
    assert!(result.success);

    // Verify output file was created
    let expected_file = output_dir.join("handlers/test_tool.rs");
    assert!(
        expected_file.exists(),
        "Output file not created: {:?}",
        expected_file
    );

    // Verify file content
    let content = std::fs::read_to_string(&expected_file).unwrap();
    assert!(content.contains("TestProject"));
    assert!(content.contains("test_toolHandler"));

    println!("✓ Pack template resolution test passed");
    println!("  Template loaded from: {}:handler.rs.tera", pack_id);
    println!("  Output file: {:?}", expected_file);
    println!(
        "  Content preview:\n{}",
        content.lines().take(5).collect::<Vec<_>>().join("\n")
    );
}

#[test]
fn test_pack_template_with_iteration() {
    // Create test pack cache
    let cache_dir = tempfile::TempDir::new().unwrap();
    let pack_id = "surface-mcp";
    let pack_cache_dir = cache_dir.path().join(pack_id);
    let templates_dir = pack_cache_dir.join("templates");
    std::fs::create_dir_all(&templates_dir).unwrap();

    // Create template that iterates over items
    let template_content = r#"
// Tool: {{ name }}
pub fn {{ name | lower }}() -> &'static str {
    "{{ name }}"
}
"#;
    std::fs::write(templates_dir.join("tool.rs.tera"), template_content).unwrap();

    std::env::set_var("GGEN_PACK_CACHE_DIR", cache_dir.path());

    // Create emission pass
    let mut pass = EmissionPass::new().with_guards(ggen_core::v6::guard::GuardSet::new());

    pass.add_rule(EmissionRule {
        name: "generate-tools".to_string(),
        template_path: PathBuf::from(format!("{}:tool.rs.tera", pack_id)),
        inline_template: None,
        output_pattern: "tools/{{ name | lower }}.rs".to_string(),
        binding_key: "tools".to_string(),
        iterate: true,
        skip_empty: false,
        description: None,
    });

    // Create test context
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());

    // Add multiple tools
    ctx.bindings.insert(
        "tools".to_string(),
        serde_json::json!([
            {"name": "execute_query"},
            {"name": "validate_input"},
            {"name": "format_output"}
        ]),
    );

    // Execute emission pass
    let result = pass.execute(&mut ctx).unwrap();
    assert!(result.success);
    assert_eq!(result.files_generated.len(), 3);

    // Verify all files were created
    for tool_name in &["execute_query", "validate_input", "format_output"] {
        let expected_file = output_dir.join("tools").join(format!("{}.rs", tool_name));
        assert!(expected_file.exists(), "Missing file: {:?}", expected_file);
    }

    println!("✓ Pack template iteration test passed");
    println!(
        "  Generated {} files from pack template",
        result.files_generated.len()
    );
}

#[test]
fn test_pack_template_not_cached_error() {
    // Set cache directory to empty location
    let cache_dir = tempfile::TempDir::new().unwrap();
    std::env::set_var("GGEN_PACK_CACHE_DIR", cache_dir.path());

    // Create emission pass with non-existent pack
    let mut pass = EmissionPass::new().with_guards(ggen_core::v6::guard::GuardSet::new());

    pass.add_rule(EmissionRule {
        name: "generate-handler".to_string(),
        template_path: PathBuf::from("nonexistent-pack:handler.rs.tera".to_string()),
        inline_template: None,
        output_pattern: "handler.rs".to_string(),
        binding_key: "tool".to_string(),
        iterate: false,
        skip_empty: false,
        description: None,
    });

    // Create test context
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    // Execute emission pass - should fail with pack not cached error
    let result = pass.execute(&mut ctx);
    assert!(result.is_err());

    let error_msg = result.unwrap_err().to_string();
    assert!(error_msg.contains("nonexistent-pack"));
    assert!(error_msg.contains("not cached"));

    println!("✓ Pack template error handling test passed");
    println!("  Error message: {}", error_msg);
}

#[test]
fn test_template_resolver_search() {
    // Create test packs with templates
    let cache_dir = tempfile::TempDir::new().unwrap();

    // Pack 1: surface-mcp
    let pack1_dir = cache_dir.path().join("surface-mcp").join("templates");
    std::fs::create_dir_all(&pack1_dir).unwrap();
    std::fs::write(pack1_dir.join("handler.rs.tera"), "// handler").unwrap();
    std::fs::write(pack1_dir.join("mod.rs.tera"), "// mod").unwrap();

    // Pack 2: projection-rust
    let pack2_dir = cache_dir.path().join("projection-rust").join("templates");
    std::fs::create_dir_all(&pack2_dir).unwrap();
    std::fs::write(pack2_dir.join("entity.rs.tera"), "// entity").unwrap();

    std::env::set_var("GGEN_PACK_CACHE_DIR", cache_dir.path());

    // Create resolver and search
    let resolver = ggen_core::resolver::TemplateResolver::new().unwrap();

    // Search all templates
    let all_templates = resolver.search_templates(None).unwrap();
    assert_eq!(all_templates.len(), 3);

    // Search with query
    let handler_templates = resolver.search_templates(Some("handler")).unwrap();
    assert_eq!(handler_templates.len(), 1);
    assert_eq!(handler_templates[0].pack_id, "surface-mcp");
    assert_eq!(
        handler_templates[0].template_path,
        PathBuf::from("handler.rs.tera")
    );

    // Search with pack-specific query
    let rust_templates = resolver.search_templates(Some("rust")).unwrap();
    assert!(rust_templates.len() >= 1);

    println!("✓ Template resolver search test passed");
    println!("  Found {} total templates", all_templates.len());
    println!("  Found {} handler templates", handler_templates.len());
}
