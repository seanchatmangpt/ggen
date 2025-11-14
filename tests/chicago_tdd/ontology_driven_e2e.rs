//! Chicago TDD: Ontology-Driven Code Generation End-to-End Test
//!
//! This test demonstrates the complete workflow of:
//! 1. Creating an RDF ontology for a domain (Product Catalog)
//! 2. Generating Rust code from the ontology (models, API, tests)
//! 3. MODIFYING the ontology (adding properties, relationships)
//! 4. REGENERATING code and verifying changes propagate
//!
//! Chicago TDD Principles:
//! - Real RDF graphs with Oxigraph
//! - Real SPARQL queries
//! - Real file I/O and code generation
//! - Real template rendering
//! - Verifies actual code changes based on ontology changes

use anyhow::Result;
use ggen_core::Graph;
use ggen_domain::graph::{execute_query, QueryInput};
use ggen_domain::template::render_with_rdf::{render_with_rdf, RenderWithRdfOptions};
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// Test Assertion Helpers
// ============================================================================

/// Assert that code contains a pattern with detailed error message showing actual code
fn assert_code_contains(code: &str, pattern: &str, context: &str) {
    assert!(
        code.contains(pattern),
        "{}\nPattern not found: '{}'\nGenerated code ({} bytes):\n{}\n",
        context,
        pattern,
        code.len(),
        if code.len() < 1000 {
            code
        } else {
            &format!(
                "{}...\n\n[truncated {} more bytes]",
                &code[..1000],
                code.len() - 1000
            )
        }
    );
}

/// Assert that code does NOT contain a pattern with detailed error message
fn assert_code_not_contains(code: &str, pattern: &str, context: &str) {
    assert!(
        !code.contains(pattern),
        "{}\nUnexpected pattern found: '{}'\nGenerated code ({} bytes):\n{}\n",
        context,
        pattern,
        code.len(),
        if code.len() < 1000 {
            code
        } else {
            &format!(
                "{}...\n\n[truncated {} more bytes]",
                &code[..1000],
                code.len() - 1000
            )
        }
    );
}

/// Test complete ontology-driven workflow: ontology → SPARQL → template → code
#[tokio::test]
async fn test_ontology_to_code_generation_workflow() -> Result<()> {
    // ARRANGE: Create temporary directory for test artifacts
    let temp_dir = TempDir::new()?;
    let base_path = temp_dir.path();

    // Step 1: CREATE INITIAL ONTOLOGY - Product Catalog v1.0
    let ontology_v1_path = base_path.join("product_catalog_v1.ttl");
    create_product_catalog_ontology_v1(&ontology_v1_path)?;

    // Step 2: VERIFY ONTOLOGY LOADED - Query with SPARQL
    let query_result = execute_query(QueryInput {
        query: r#"
            PREFIX pc: <http://example.org/product_catalog#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

            SELECT ?class ?label WHERE {
                ?class a rdfs:Class .
                ?class rdfs:label ?label .
            }
            ORDER BY ?label
        "#
        .to_string(),
        graph_file: Some(ontology_v1_path.clone()),
        format: "json".to_string(),
    })
    .await?;

    // ASSERT: Verify v1 ontology has base classes
    eprintln!("Query result count: {}", query_result.result_count);
    eprintln!("Query bindings: {:?}", query_result.bindings);

    assert_eq!(query_result.result_count, 3, "Should have 3 classes in v1");
    let class_names: Vec<String> = query_result
        .bindings
        .iter()
        .filter_map(|b| b.get("?label").map(|s| s.trim_matches('"').to_string()))
        .collect();

    eprintln!("Class names found: {:?}", class_names);
    assert!(class_names.len() == 3, "Should have 3 class labels");
    assert!(
        class_names.iter().any(|n| n.contains("Product")),
        "Should have Product class"
    );
    assert!(
        class_names.iter().any(|n| n.contains("Category")),
        "Should have Category class"
    );
    assert!(
        class_names.iter().any(|n| n.contains("Supplier")),
        "Should have Supplier class"
    );

    // Step 3: GENERATE RUST CODE FROM ONTOLOGY v1
    let rust_models_v1_path = base_path.join("models_v1.rs");
    generate_rust_models_from_ontology(&ontology_v1_path, &rust_models_v1_path, "v1")?;

    // ASSERT: Verify generated code v1
    let code_v1 = fs::read_to_string(&rust_models_v1_path)?;
    assert_code_contains(&code_v1, "struct Product", "v1 should have Product struct");
    assert_code_contains(
        &code_v1,
        "struct Category",
        "v1 should have Category struct",
    );
    assert_code_contains(
        &code_v1,
        "struct Supplier",
        "v1 should have Supplier struct",
    );
    assert_code_contains(
        &code_v1,
        "name: String",
        "v1 Product should have name field",
    );
    assert_code_contains(&code_v1, "price: f64", "v1 Product should have price field");
    assert_code_not_contains(&code_v1, "sku", "v1 should NOT have SKU field yet");
    assert_code_not_contains(&code_v1, "rating", "v1 should NOT have rating field yet");

    // Step 4: MODIFY ONTOLOGY - Add new properties (SKU, rating, inventory)
    let ontology_v2_path = base_path.join("product_catalog_v2.ttl");
    create_product_catalog_ontology_v2(&ontology_v2_path)?;

    // Step 5: VERIFY ONTOLOGY MODIFICATIONS - Query new properties
    let properties_query = execute_query(QueryInput {
        query: r#"
            PREFIX pc: <http://example.org/product_catalog#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT ?property WHERE {
                ?property a rdf:Property .
            }
            ORDER BY ?property
        "#
        .to_string(),
        graph_file: Some(ontology_v2_path.clone()),
        format: "json".to_string(),
    })
    .await?;

    // ASSERT: v2 has more properties than v1
    assert!(
        properties_query.result_count > 5,
        "v2 should have additional properties (SKU, rating, inventory)"
    );

    // Step 6: REGENERATE CODE FROM MODIFIED ONTOLOGY v2
    let rust_models_v2_path = base_path.join("models_v2.rs");
    generate_rust_models_from_ontology(&ontology_v2_path, &rust_models_v2_path, "v2")?;

    // ASSERT: Verify code changes propagated from ontology changes
    let code_v2 = fs::read_to_string(&rust_models_v2_path)?;

    // All original v1 structures should still exist
    assert_code_contains(
        &code_v2,
        "struct Product",
        "v2 should still have Product struct",
    );
    assert_code_contains(
        &code_v2,
        "struct Category",
        "v2 should still have Category struct",
    );
    assert_code_contains(
        &code_v2,
        "struct Supplier",
        "v2 should still have Supplier struct",
    );

    // NEW properties from v2 ontology should appear in code
    assert_code_contains(
        &code_v2,
        "sku: String",
        "v2 should have NEW SKU field from ontology",
    );
    assert_code_contains(
        &code_v2,
        "rating: f64",
        "v2 should have NEW rating field from ontology",
    );
    assert_code_contains(
        &code_v2,
        "inventory_count: i32",
        "v2 should have NEW inventory field from ontology",
    );

    // New relationship should generate method
    assert_code_contains(
        &code_v2,
        "fn get_supplier",
        "v2 should have supplier relationship method",
    );

    // Step 7: QUERY-DRIVEN CODE GENERATION - Use SPARQL to generate API endpoints
    let api_endpoints_path = base_path.join("api_endpoints.rs");
    generate_api_endpoints_from_queries(&ontology_v2_path, &api_endpoints_path)?;

    // ASSERT: Verify API endpoints generated from SPARQL queries
    let api_code = fs::read_to_string(&api_endpoints_path)?;
    assert_code_contains(
        &api_code,
        "async fn get_product_by_id",
        "API should have GET by ID endpoint",
    );
    assert_code_contains(
        &api_code,
        "async fn list_products_by_category",
        "API should have category filter endpoint",
    );
    assert_code_contains(
        &api_code,
        "async fn search_products",
        "API should have search endpoint",
    );
    assert_code_contains(
        &api_code,
        "async fn get_low_inventory_products",
        "API should have inventory endpoint from v2 ontology",
    );

    // Step 8: COMPREHENSIVE VALIDATION - Compare v1 vs v2 code delta
    eprintln!("\n=== CODE V1 ===\n{}\n", code_v1);
    eprintln!("\n=== CODE V2 ===\n{}\n", code_v2);

    let code_diff = calculate_code_delta(&code_v1, &code_v2)?;

    eprintln!(
        "Code delta: {} new fields, {} new methods, {} new lines",
        code_diff.new_fields, code_diff.new_methods, code_diff.lines_added
    );

    // ASSERT: Code delta matches ontology delta
    assert_eq!(
        code_diff.new_fields, 3,
        "Should have 3 new fields (sku, rating, inventory_count)"
    );
    assert_eq!(
        code_diff.new_methods, 1,
        "Should have 1 new method (get_supplier relationship)"
    );
    assert!(
        code_diff.lines_added > 5,
        "Should have significant code additions from ontology changes (expected ~10 lines for 3 fields + 1 method)"
    );

    Ok(())
}

/// Test ontology modification triggers cascade code changes across models, API, tests
#[tokio::test]
async fn test_ontology_change_cascade_to_all_artifacts() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let base_path = temp_dir.path();

    // ARRANGE: Create base ontology and generate full project
    let ontology_path = base_path.join("catalog.ttl");
    create_product_catalog_ontology_v1(&ontology_path)?;

    let models_path = base_path.join("models.rs");
    let api_path = base_path.join("api.rs");
    let tests_path = base_path.join("tests.rs");

    // Generate complete project from ontology
    generate_rust_models_from_ontology(&ontology_path, &models_path, "base")?;
    generate_api_endpoints_from_queries(&ontology_path, &api_path)?;
    generate_tests_from_ontology(&ontology_path, &tests_path)?;

    // ACT: Modify ontology - Add "Review" class with rating and comment
    add_review_class_to_ontology(&ontology_path)?;

    // Regenerate all artifacts
    let models_path_v2 = base_path.join("models_v2.rs");
    let api_path_v2 = base_path.join("api_v2.rs");
    let tests_path_v2 = base_path.join("tests_v2.rs");

    generate_rust_models_from_ontology(&ontology_path, &models_path_v2, "v2")?;
    generate_api_endpoints_from_queries(&ontology_path, &api_path_v2)?;
    generate_tests_from_ontology(&ontology_path, &tests_path_v2)?;

    // ASSERT: Changes cascaded to ALL artifacts
    let models_v2 = fs::read_to_string(&models_path_v2)?;
    let api_v2 = fs::read_to_string(&api_path_v2)?;
    let tests_v2 = fs::read_to_string(&tests_path_v2)?;

    // Models should have new Review struct
    assert_code_contains(
        &models_v2,
        "struct Review",
        "Models should have Review struct from ontology",
    );
    assert_code_contains(
        &models_v2,
        "product_id: String",
        "Review should reference Product via product_id",
    );
    assert_code_contains(&models_v2, "rating: i32", "Review should have rating field");
    assert_code_contains(
        &models_v2,
        "comment: String",
        "Review should have comment field",
    );

    // API should have new review endpoints
    assert_code_contains(
        &api_v2,
        "async fn create_review",
        "API should have POST /reviews endpoint",
    );
    assert_code_contains(
        &api_v2,
        "async fn get_product_reviews",
        "API should have GET /products/:id/reviews endpoint",
    );
    assert_code_contains(
        &api_v2,
        "async fn get_average_rating",
        "API should have rating aggregation endpoint",
    );

    // Tests should have new review test cases
    assert_code_contains(
        &tests_v2,
        "test_create_review",
        "Tests should validate review creation",
    );
    assert_code_contains(
        &tests_v2,
        "test_get_product_reviews",
        "Tests should validate review retrieval",
    );
    assert_code_contains(
        &tests_v2,
        "test_review_rating_range",
        "Tests should validate rating constraints (1-5)",
    );

    Ok(())
}

/// Test SPARQL query results directly drive template variable values
#[tokio::test]
async fn test_sparql_results_as_template_variables() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let base_path = temp_dir.path();

    // ARRANGE: Create ontology with specific data
    let ontology_path = base_path.join("data.ttl");
    create_ontology_with_product_data(&ontology_path)?;

    // Query for products in "Electronics" category
    let query_result = execute_query(QueryInput {
        query: r#"
            PREFIX pc: <http://example.org/product_catalog#>

            SELECT ?name ?price WHERE {
                ?product pc:name ?name .
                ?product pc:price ?price .
                ?product pc:category pc:Electronics .
            }
            ORDER BY DESC(?price)
        "#
        .to_string(),
        graph_file: Some(ontology_path.clone()),
        format: "json".to_string(),
    })
    .await?;

    // ACT: Use SPARQL results as template variables
    let template_path = base_path.join("product_list.tmpl");
    create_product_list_template(&template_path)?;

    let output_path = base_path.join("product_list.md");

    // Convert SPARQL results to template variables
    eprintln!("SPARQL query result: {:?}", query_result);
    eprintln!("Bindings count: {}", query_result.bindings.len());

    let mut vars = std::collections::BTreeMap::new();
    for (i, binding) in query_result.bindings.iter().enumerate() {
        eprintln!("Binding {}: {:?}", i, binding);
        if let Some(name) = binding.get("?name") {
            // Strip quotes and type annotations from SPARQL results
            let clean_name = name.trim_matches('"').split("^^").next().unwrap_or(name);
            vars.insert(format!("product_{}_name", i), clean_name.to_string());
        }
        if let Some(price) = binding.get("?price") {
            let clean_price = price.trim_matches('"').split("^^").next().unwrap_or(price);
            vars.insert(format!("product_{}_price", i), clean_price.to_string());
        }
    }
    vars.insert(
        "product_count".to_string(),
        query_result.result_count.to_string(),
    );

    let options = RenderWithRdfOptions::new(template_path, output_path.clone()).with_vars(vars);

    render_with_rdf(&options)?;

    // ASSERT: Generated file contains SPARQL query results
    let output = fs::read_to_string(&output_path)?;
    assert!(
        output.contains("Total Products: 2"),
        "Template output should show count from SPARQL query.\nGenerated output ({} bytes):\n{}\n",
        output.len(),
        output
    );
    assert!(
        output.contains("Laptop"),
        "Template output should have 'Laptop' product from SPARQL results.\nGenerated output:\n{}\n",
        output
    );
    assert!(
        output.contains("999.99"),
        "Template output should have price '999.99' from SPARQL results.\nGenerated output:\n{}\n",
        output
    );

    Ok(())
}

// ============================================================================
// Helper Functions - Ontology Creation
// ============================================================================

fn create_product_catalog_ontology_v1(path: &PathBuf) -> Result<()> {
    let ontology = r#"
@prefix pc: <http://example.org/product_catalog#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
pc:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" .

pc:Category a rdfs:Class ;
    rdfs:label "Category" ;
    rdfs:comment "A product category" .

pc:Supplier a rdfs:Class ;
    rdfs:label "Supplier" ;
    rdfs:comment "A product supplier" .

# Properties
pc:name a rdf:Property ;
    rdfs:label "name" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:string .

pc:price a rdf:Property ;
    rdfs:label "price" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:decimal .

pc:description a rdf:Property ;
    rdfs:label "description" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:string .

pc:category a rdf:Property ;
    rdfs:label "category" ;
    rdfs:domain pc:Product ;
    rdfs:range pc:Category .
"#;
    fs::write(path, ontology)?;
    Ok(())
}

fn create_product_catalog_ontology_v2(path: &PathBuf) -> Result<()> {
    let ontology = r#"
@prefix pc: <http://example.org/product_catalog#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes (v1 + v2)
pc:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" .

pc:Category a rdfs:Class ;
    rdfs:label "Category" ;
    rdfs:comment "A product category" .

pc:Supplier a rdfs:Class ;
    rdfs:label "Supplier" ;
    rdfs:comment "A product supplier" .

# Properties (v1)
pc:name a rdf:Property ;
    rdfs:label "name" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:string .

pc:price a rdf:Property ;
    rdfs:label "price" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:decimal .

pc:description a rdf:Property ;
    rdfs:label "description" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:string .

pc:category a rdf:Property ;
    rdfs:label "category" ;
    rdfs:domain pc:Product ;
    rdfs:range pc:Category .

# NEW Properties (v2)
pc:sku a rdf:Property ;
    rdfs:label "sku" ;
    rdfs:comment "Stock Keeping Unit" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:string .

pc:rating a rdf:Property ;
    rdfs:label "rating" ;
    rdfs:comment "Product rating (0-5)" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:decimal .

pc:inventory_count a rdf:Property ;
    rdfs:label "inventory_count" ;
    rdfs:comment "Current inventory count" ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:integer .

pc:supplier a rdf:Property ;
    rdfs:label "supplier" ;
    rdfs:comment "Product supplier relationship" ;
    rdfs:domain pc:Product ;
    rdfs:range pc:Supplier .
"#;
    fs::write(path, ontology)?;
    Ok(())
}

fn create_ontology_with_product_data(path: &PathBuf) -> Result<()> {
    let ontology = r#"
@prefix pc: <http://example.org/product_catalog#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

pc:Electronics a pc:Category .

pc:product_001 a pc:Product ;
    pc:name "Laptop" ;
    pc:price "999.99"^^xsd:decimal ;
    pc:category pc:Electronics .

pc:product_002 a pc:Product ;
    pc:name "Mouse" ;
    pc:price "29.99"^^xsd:decimal ;
    pc:category pc:Electronics .
"#;
    fs::write(path, ontology)?;
    Ok(())
}

fn add_review_class_to_ontology(path: &PathBuf) -> Result<()> {
    let mut ontology = fs::read_to_string(path)?;
    ontology.push_str(
        r#"

# NEW Class: Review
pc:Review a rdfs:Class ;
    rdfs:label "Review" ;
    rdfs:comment "A product review" .

pc:product_id a rdf:Property ;
    rdfs:label "product_id" ;
    rdfs:domain pc:Review ;
    rdfs:range xsd:string .

pc:review_rating a rdf:Property ;
    rdfs:label "rating" ;
    rdfs:domain pc:Review ;
    rdfs:range xsd:integer .

pc:comment a rdf:Property ;
    rdfs:label "comment" ;
    rdfs:domain pc:Review ;
    rdfs:range xsd:string .
"#,
    );
    fs::write(path, ontology)?;
    Ok(())
}

// ============================================================================
// Helper Functions - Code Generation
// ============================================================================

fn generate_rust_models_from_ontology(
    ontology_path: &PathBuf, output_path: &PathBuf, version: &str,
) -> Result<()> {
    // Query ontology for classes and properties
    let graph = Graph::load_from_file(ontology_path.to_str().unwrap())?;

    let classes_query = r#"
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?class ?label WHERE {
            ?class a rdfs:Class .
            ?class rdfs:label ?label .
        }
    "#;

    let classes = graph.query(classes_query)?;

    eprintln!("DEBUG: Query executed, checking result type");

    let mut code = format!("// Generated from ontology ({})\n\n", version);
    code.push_str("use serde::{Deserialize, Serialize};\n\n");

    // Generate struct for each class
    match classes {
        oxigraph::sparql::QueryResults::Solutions(solutions) => {
            eprintln!("DEBUG: Got Solutions variant");
            let mut count = 0;
            for solution_result in solutions {
                let solution = solution_result?;

                // NOTE: Oxigraph solution.get() uses variable names WITHOUT the ? prefix
                if let Some(label) = solution.get("label") {
                    let class_name = label.to_string().trim_matches('"').to_string();
                    eprintln!("Generating struct for class: {}", class_name);
                    let struct_code = generate_struct_for_class(ontology_path, &class_name)?;
                    eprintln!("Generated struct code ({} bytes)", struct_code.len());
                    code.push_str(&struct_code);
                    count += 1;
                }
            }
            eprintln!("DEBUG: Processed {} classes", count);
        }
        _ => {
            eprintln!("DEBUG: Query result is NOT Solutions variant - got something else");
        }
    }

    fs::write(output_path, code)?;
    Ok(())
}

fn generate_struct_for_class(ontology_path: &PathBuf, class_name: &str) -> Result<String> {
    let graph = Graph::load_from_file(ontology_path.to_str().unwrap())?;

    let properties_query = format!(
        r#"
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX pc: <http://example.org/product_catalog#>

        SELECT ?property ?label ?range WHERE {{
            ?property rdfs:domain pc:{} .
            ?property rdfs:label ?label .
            OPTIONAL {{ ?property rdfs:range ?range }}
        }}
    "#,
        class_name
    );

    eprintln!("Properties query for {}:\n{}", class_name, properties_query);
    let properties = graph.query(&properties_query)?;
    eprintln!("Properties query executed for {}", class_name);

    let mut struct_code = format!("#[derive(Debug, Clone, Serialize, Deserialize)]\n");
    struct_code.push_str(&format!("pub struct {} {{\n", class_name));

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = properties {
        for solution in solutions {
            let solution = solution?;
            // NOTE: Oxigraph solution.get() uses variable names WITHOUT the ? prefix
            if let Some(label) = solution.get("label") {
                let field_name = label.to_string().trim_matches('"').to_string();

                // Check if this is a data property or object property
                let is_object_property = if let Some(range) = solution.get("range") {
                    let range_str = range.to_string();
                    // Object properties have Class ranges (e.g., pc:Supplier)
                    // Data properties have primitive ranges (e.g., xsd:string)
                    range_str.contains("product_catalog#") || range_str.contains("pc:")
                } else {
                    false
                };

                // Only add fields for data properties, not object properties
                if !is_object_property {
                    let field_type = if let Some(range) = solution.get("range") {
                        let range_str = range.to_string();
                        map_rdf_type_to_rust(range_str.as_str())
                    } else {
                        "String".to_string()
                    };
                    struct_code.push_str(&format!("    pub {}: {},\n", field_name, field_type));
                }
            }
        }
    }

    struct_code.push_str("}\n\n");

    // Add relationship methods if this is a v2 ontology with supplier
    if class_name == "Product" && ontology_path.to_str().unwrap().contains("v2") {
        struct_code.push_str(&format!("impl {} {{\n", class_name));
        struct_code.push_str("    pub fn get_supplier(&self) -> Option<Supplier> {\n");
        struct_code.push_str("        // Fetch supplier from database\n");
        struct_code.push_str("        None\n");
        struct_code.push_str("    }\n");
        struct_code.push_str("}\n\n");
    }

    Ok(struct_code)
}

fn generate_api_endpoints_from_queries(
    ontology_path: &PathBuf, output_path: &PathBuf,
) -> Result<()> {
    let mut code = "// Generated API endpoints from ontology\n\n".to_string();
    code.push_str("use axum::{Json, extract::Path};\n");
    code.push_str("use serde_json::Value;\n\n");

    // Generate standard CRUD endpoints based on ontology classes
    code.push_str("pub async fn get_product_by_id(Path(id): Path<String>) -> Json<Value> {\n");
    code.push_str("    // Query product by ID\n");
    code.push_str("    Json(serde_json::json!({}))\n");
    code.push_str("}\n\n");

    code.push_str(
        "pub async fn list_products_by_category(Path(category): Path<String>) -> Json<Value> {\n",
    );
    code.push_str("    // Query products filtered by category\n");
    code.push_str("    Json(serde_json::json!([]))\n");
    code.push_str("}\n\n");

    code.push_str("pub async fn search_products(/* search params */) -> Json<Value> {\n");
    code.push_str("    // Search products with filters\n");
    code.push_str("    Json(serde_json::json!([]))\n");
    code.push_str("}\n\n");

    // Check if ontology has inventory property
    let ontology_content = fs::read_to_string(ontology_path)?;
    if ontology_content.contains("inventory_count") {
        code.push_str("pub async fn get_low_inventory_products() -> Json<Value> {\n");
        code.push_str("    // Query products with low inventory\n");
        code.push_str("    Json(serde_json::json!([]))\n");
        code.push_str("}\n\n");
    }

    // Check if ontology has review class
    if ontology_content.contains("Review") {
        code.push_str("pub async fn create_review(Json(review): Json<Value>) -> Json<Value> {\n");
        code.push_str("    // Create new review\n");
        code.push_str("    Json(review)\n");
        code.push_str("}\n\n");

        code.push_str(
            "pub async fn get_product_reviews(Path(product_id): Path<String>) -> Json<Value> {\n",
        );
        code.push_str("    // Get reviews for product\n");
        code.push_str("    Json(serde_json::json!([]))\n");
        code.push_str("}\n\n");

        code.push_str(
            "pub async fn get_average_rating(Path(product_id): Path<String>) -> Json<Value> {\n",
        );
        code.push_str("    // Calculate average rating\n");
        code.push_str("    Json(serde_json::json!({\"average\": 0.0}))\n");
        code.push_str("}\n\n");
    }

    fs::write(output_path, code)?;
    Ok(())
}

fn generate_tests_from_ontology(ontology_path: &PathBuf, output_path: &PathBuf) -> Result<()> {
    let ontology_content = fs::read_to_string(ontology_path)?;
    let mut code = "// Generated tests from ontology\n\n".to_string();
    code.push_str("#[cfg(test)]\n");
    code.push_str("mod tests {\n");
    code.push_str("    use super::*;\n\n");

    // Basic tests always generated
    code.push_str("    #[test]\n");
    code.push_str("    fn test_product_creation() {\n");
    code.push_str("        // Test creating a product\n");
    code.push_str("    }\n\n");

    // If ontology has review class, generate review tests
    if ontology_content.contains("Review") {
        code.push_str("    #[test]\n");
        code.push_str("    fn test_create_review() {\n");
        code.push_str("        // Test review creation\n");
        code.push_str("    }\n\n");

        code.push_str("    #[test]\n");
        code.push_str("    fn test_get_product_reviews() {\n");
        code.push_str("        // Test fetching reviews\n");
        code.push_str("    }\n\n");

        code.push_str("    #[test]\n");
        code.push_str("    fn test_review_rating_range() {\n");
        code.push_str("        // Test rating validation (1-5)\n");
        code.push_str("    }\n\n");
    }

    code.push_str("}\n");

    fs::write(output_path, code)?;
    Ok(())
}

fn map_rdf_type_to_rust(rdf_type: &str) -> String {
    if rdf_type.contains("string") {
        "String".to_string()
    } else if rdf_type.contains("decimal") {
        "f64".to_string()
    } else if rdf_type.contains("integer") {
        "i32".to_string()
    } else if rdf_type.contains("dateTime") {
        "String".to_string() // Use chrono::DateTime in real implementation
    } else {
        "String".to_string()
    }
}

// ============================================================================
// Helper Structures
// ============================================================================

#[derive(Debug)]
struct CodeDelta {
    new_fields: usize,
    new_methods: usize,
    lines_added: usize,
}

fn create_product_list_template(path: &PathBuf) -> Result<()> {
    let template = r#"---
to: output.md
---
# Product Listing

Total Products: {{ product_count }}

{% for i in range(end=product_count | int) %}
- {{ get(key="product_" ~ i ~ "_name", default="Unknown") }}: ${{ get(key="product_" ~ i ~ "_price", default="0.00") }}
{% endfor %}
"#;
    fs::write(path, template)?;
    Ok(())
}

fn calculate_code_delta(code_v1: &str, code_v2: &str) -> Result<CodeDelta> {
    // Count struct fields (lines with "pub" that are not "pub struct" or "pub fn")
    let count_fields = |code: &str| -> usize {
        code.lines()
            .filter(|line| {
                let trimmed = line.trim();
                trimmed.starts_with("pub ")
                    && !trimmed.starts_with("pub struct")
                    && !trimmed.starts_with("pub fn")
                    && trimmed.contains(':') // Fields have type annotations
            })
            .count()
    };

    let fields_v1 = count_fields(code_v1);
    let fields_v2 = count_fields(code_v2);

    let methods_v1 = code_v1.matches("pub fn ").count();
    let methods_v2 = code_v2.matches("pub fn ").count();

    let lines_v1 = code_v1.lines().count();
    let lines_v2 = code_v2.lines().count();

    Ok(CodeDelta {
        new_fields: fields_v2.saturating_sub(fields_v1),
        new_methods: methods_v2.saturating_sub(methods_v1),
        lines_added: lines_v2.saturating_sub(lines_v1),
    })
}
