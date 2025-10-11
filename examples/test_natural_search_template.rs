//! Test natural search template loading and rendering

use ggen_ai::prompts::PromptTemplateLoader;
use std::collections::HashMap;
use serde_json::Value;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing Natural Search Template System");
    println!("========================================\n");

    // Test 1: Create template loader
    println!("1. Creating PromptTemplateLoader...");
    let mut loader = PromptTemplateLoader::new()?;
    println!("   ✓ Template loader created successfully\n");

    // Test 2: Render natural search prompt
    println!("2. Rendering natural search prompt...");
    let test_query = "I need user authentication";
    let prompt = loader.render_natural_search(test_query)?;
    println!("   ✓ Prompt rendered successfully");
    println!("   Length: {} characters\n", prompt.len());

    // Test 3: Verify prompt contains expected elements
    println!("3. Verifying prompt content...");
    assert!(prompt.contains("natural language queries"), "Missing intro text");
    assert!(prompt.contains(test_query), "Missing user query");
    assert!(prompt.contains("JSON response"), "Missing JSON instruction");
    assert!(prompt.contains("interpretation"), "Missing interpretation field");
    assert!(prompt.contains("keywords"), "Missing keywords field");
    assert!(prompt.contains("packages"), "Missing packages field");
    println!("   ✓ All expected elements present\n");

    // Test 4: Show rendered prompt
    println!("4. Rendered Prompt Preview:");
    println!("---");
    println!("{}", &prompt[..500.min(prompt.len())]);
    println!("...");
    println!("---\n");

    // Test 5: Test other template methods
    println!("5. Testing SPARQL template rendering...");
    let sparql_prompt = loader.render_sparql_query(
        "Find all users",
        Some("User class defined"),
        vec![("ex".to_string(), "http://example.org/".to_string())],
        vec!["SELECT ?user WHERE { ?user a ex:User }".to_string()],
        vec!["Return user names".to_string()],
    )?;
    assert!(sparql_prompt.contains("SPARQL"), "Missing SPARQL reference");
    assert!(sparql_prompt.contains("Find all users"), "Missing query intent");
    println!("   ✓ SPARQL template rendered successfully\n");

    // Test 6: Test template generation prompt
    println!("6. Testing template generation prompt...");
    let template_prompt = loader.render_template_generation(
        "Generate a REST API template",
        vec!["Express.js example".to_string()],
        vec!["Include authentication".to_string()],
        Some("TypeScript"),
        Some("Express"),
    )?;
    assert!(template_prompt.contains("ggen template"), "Missing ggen reference");
    assert!(template_prompt.contains("TypeScript"), "Missing language");
    println!("   ✓ Template generation prompt rendered successfully\n");

    println!("✅ All tests passed!");
    println!("\nDogfooding Success! Using ggen templates to generate AI prompts.");

    Ok(())
}
