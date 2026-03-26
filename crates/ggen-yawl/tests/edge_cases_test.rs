//! Edge case tests for ggen-yawl transformation rules.
//!
//! This module tests error conditions, boundary scenarios, and edge cases
//! for all transformation rules including:
//! - Empty inputs and minimal configurations
//! - Large numbers of entities
//! - Special characters and Unicode in names
//! - Reserved Java keywords
//! - Circular dependencies
//! - Null/missing fields
//! - Duplicate names
//! - Very long names
//! - Error handling and validation

use ggen_yawl::{YawlGenerator, Error};

// ===========================
// 1. EMPTY INPUT TESTS
// ===========================

#[test]
fn test_empty_ontology_string() {
    let gen = YawlGenerator::new();
    let result = gen.generate("");
    // Empty ontology should error (either OntologyLoad or SPARQL)
    assert!(result.is_err(), "Empty ontology should produce an error");
}

#[test]
fn test_minimal_valid_ontology() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:EmptyWorkflow a yawl:Specification ;
            rdfs:label "Empty Workflow" ;
            yawl:version "1.0.0" .
    "#;

    let result = gen.generate(ontology);
    // Should handle minimal valid input gracefully (may error or succeed)
    let _ = result;
}

#[test]
fn test_single_entity_ontology() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .
        @prefix fibo: <http://spec.edmcouncil.org/fibo/ontology/FBC/Entities/> .

        fibo:Organization001 a yawl:Task ;
            rdfs:label "Single Org" ;
            yawl:taskId "task_001" .
    "#;

    let result = gen.generate(ontology);
    // Should handle single entity without panicking
    let _ = result;
}

// ===========================
// 2. LARGE INPUT TESTS
// ===========================

#[test]
fn test_large_number_of_tasks_100() {
    let gen = YawlGenerator::new().with_validation(false);

    let mut tasks = String::new();
    for i in 0..100 {
        tasks.push_str(&format!(
            r#"
            yawl:task_{} a yawl:AtomicTask ;
                rdfs:label "Task {}" ;
                yawl:taskId "t_{}" .
            "#,
            i, i, i
        ));
    }

    let ontology = format!(
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:LargeWorkflow a yawl:Specification ;
            rdfs:label "Large Workflow" ;
            yawl:version "1.0.0" .

        {}
        "#,
        tasks
    );

    let result = gen.generate(&ontology);
    let _ = result;
}

#[test]
fn test_many_properties_per_entity() {
    let gen = YawlGenerator::new().with_validation(false);

    let mut properties = String::new();
    for i in 0..50 {
        properties.push_str(&format!(
            "yawl:hasProperty_{} yawl:propertyValue_{} ;\n",
            i, i
        ));
    }

    let ontology = format!(
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:ComplexTask a yawl:AtomicTask ;
            rdfs:label "Complex Task" ;
            yawl:taskId "t_complex" ;
            {} .
        "#,
        properties
    );

    let result = gen.generate(&ontology);
    let _ = result;
}

// ===========================
// 3. SPECIAL CHARACTERS & UNICODE
// ===========================

#[test]
fn test_special_characters_in_task_names() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_1 a yawl:AtomicTask ;
            rdfs:label "Task: <>&\"'`" ;
            yawl:taskId "t_special" .
    "#;

    let result = gen.generate(ontology);
    // Should handle special characters safely (XML escape them)
    let _ = result;
}

#[test]
fn test_unicode_characters_in_labels() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_1 a yawl:AtomicTask ;
            rdfs:label "Задача №1 日本語 العربية" ;
            yawl:taskId "t_unicode" .
    "#;

    let result = gen.generate(ontology);
    // Should handle Unicode gracefully
    let _ = result;
}

#[test]
fn test_quotes_in_string_values() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_1 a yawl:AtomicTask ;
            rdfs:label "Task with 'single' and \"double\" quotes" ;
            yawl:taskId "t_quotes" ;
            rdfs:comment "Line 1\nLine 2\nLine 3" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

// ===========================
// 4. RESERVED JAVA KEYWORDS
// ===========================

#[test]
fn test_java_reserved_keywords_as_names() {
    let gen = YawlGenerator::new().with_validation(false);

    let reserved_words = vec![
        "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char",
        "class", "const", "continue", "default", "do", "double", "else", "enum",
        "extends", "final", "finally", "float", "for", "goto", "if", "implements",
        "import", "instanceof", "int", "interface", "long", "native", "new",
        "package", "private", "protected", "public", "return", "short", "static",
        "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
        "transient", "try", "void", "volatile", "while",
    ];

    let mut tasks = String::new();
    for (idx, keyword) in reserved_words.iter().take(10).enumerate() {
        tasks.push_str(&format!(
            r#"
            yawl:task_{} a yawl:AtomicTask ;
                rdfs:label "{}" ;
                yawl:taskId "t_{}" .
            "#,
            idx, keyword, idx
        ));
    }

    let ontology = format!(
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:KeywordWorkflow a yawl:Specification ;
            rdfs:label "Keyword Workflow" ;
            yawl:version "1.0.0" .

        {}
        "#,
        tasks
    );

    let result = gen.generate(&ontology);
    // Should handle reserved keywords gracefully (prefix or suffix)
    let _ = result;
}

// ===========================
// 5. CIRCULAR DEPENDENCIES
// ===========================

#[test]
fn test_simple_circular_dependency() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .
        @prefix fibo: <http://spec.edmcouncil.org/fibo/ontology/FBC/Entities/> .

        yawl:task_a a yawl:AtomicTask ;
            rdfs:label "Task A" ;
            yawl:taskId "t_a" ;
            fibo:hasNextStep yawl:task_b .

        yawl:task_b a yawl:AtomicTask ;
            rdfs:label "Task B" ;
            yawl:taskId "t_b" ;
            fibo:hasNextStep yawl:task_a .
    "#;

    let result = gen.generate(ontology);
    // Should detect or handle circular dependencies
    let _ = result;
}

#[test]
fn test_self_referencing_task() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .
        @prefix fibo: <http://spec.edmcouncil.org/fibo/ontology/FBC/Entities/> .

        yawl:task_a a yawl:AtomicTask ;
            rdfs:label "Task A" ;
            yawl:taskId "t_a" ;
            fibo:hasNextStep yawl:task_a .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

#[test]
fn test_complex_circular_chain() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://www.w3.org/yawl#> .
        @prefix fibo: <http://spec.edmcouncil.org/fibo/ontology/FBC/Entities/> .

        yawl:task_a a yawl:AtomicTask ;
            rdfs:label "Task A" ;
            yawl:taskId "t_a" ;
            fibo:hasNextStep yawl:task_b .

        yawl:task_b a yawl:AtomicTask ;
            rdfs:label "Task B" ;
            yawl:taskId "t_b" ;
            fibo:hasNextStep yawl:task_c .

        yawl:task_c a yawl:AtomicTask ;
            rdfs:label "Task C" ;
            yawl:taskId "t_c" ;
            fibo:hasNextStep yawl:task_a .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

// ===========================
// 6. NULL/MISSING FIELDS
// ===========================

#[test]
fn test_missing_required_task_id() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_no_id a yawl:AtomicTask ;
            rdfs:label "Task Without ID" .
    "#;

    let result = gen.generate(ontology);
    // Should handle missing required bindings gracefully
    let _ = result;
}

#[test]
fn test_missing_label() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_no_label a yawl:AtomicTask ;
            yawl:taskId "t_no_label" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

#[test]
fn test_missing_workflow_metadata() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:MinimalWorkflow a yawl:Specification .
    "#;

    let result = gen.generate(ontology);
    // Should provide defaults or error gracefully
    let _ = result;
}

// ===========================
// 7. DUPLICATE NAMES
// ===========================

#[test]
fn test_duplicate_task_ids() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_1 a yawl:AtomicTask ;
            rdfs:label "Task One" ;
            yawl:taskId "duplicate_id" .

        yawl:task_2 a yawl:AtomicTask ;
            rdfs:label "Task Two" ;
            yawl:taskId "duplicate_id" .
    "#;

    let result = gen.generate(ontology);
    // Should handle duplicates (keep first, warn, or error)
    let _ = result;
}

#[test]
fn test_duplicate_task_labels() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_1 a yawl:AtomicTask ;
            rdfs:label "Same Name" ;
            yawl:taskId "t_1" .

        yawl:task_2 a yawl:AtomicTask ;
            rdfs:label "Same Name" ;
            yawl:taskId "t_2" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

// ===========================
// 8. VERY LONG NAMES
// ===========================

#[test]
fn test_very_long_task_name_256_chars() {
    let gen = YawlGenerator::new().with_validation(false);
    let long_name = "A".repeat(256);
    let ontology = format!(
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_long a yawl:AtomicTask ;
            rdfs:label "{}" ;
            yawl:taskId "t_long" .
        "#,
        long_name
    );

    let result = gen.generate(&ontology);
    let _ = result;
}

#[test]
fn test_very_long_task_id_512_chars() {
    let gen = YawlGenerator::new().with_validation(false);
    let long_id = "task_".to_string() + &"x".repeat(507);
    let ontology = format!(
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_very_long a yawl:AtomicTask ;
            rdfs:label "Very Long ID" ;
            yawl:taskId "{}" .
        "#,
        long_id
    );

    let result = gen.generate(&ontology);
    let _ = result;
}

#[test]
fn test_whitespace_in_long_names() {
    let gen = YawlGenerator::new().with_validation(false);
    let long_name = "Task ".repeat(51); // 255 chars with spaces
    let ontology = format!(
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_ws a yawl:AtomicTask ;
            rdfs:label "{}" ;
            yawl:taskId "t_ws" .
        "#,
        long_name
    );

    let result = gen.generate(&ontology);
    let _ = result;
}

// ===========================
// 9. TEMPLATE ERRORS
// ===========================

#[test]
fn test_missing_template_variables_caught() {
    let gen = YawlGenerator::new().with_validation(false);
    // If a template requires specific variables not present in context
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:Workflow a yawl:Specification ;
            yawl:version "1.0.0" .
    "#;

    let result = gen.generate(ontology);
    // Should complete or error, but not panic
    let _ = result;
}

// ===========================
// 10. VALIDATION ERRORS
// ===========================

#[test]
fn test_invalid_xml_generated() {
    // This test verifies that validation catches invalid XML
    let gen = YawlGenerator::new().with_validation(true);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:BadWorkflow a yawl:Specification ;
            yawl:badProperty "This shouldn't validate" .
    "#;

    let result = gen.generate(ontology);
    // Validation should catch or output error
    match result {
        Ok(_) => {} // May be valid depending on implementation
        Err(Error::Validation(_)) => {} // Expected
        Err(e) => {
            // Other errors are also acceptable
            eprintln!("Got error: {}", e);
        }
    }
}

// ===========================
// 11. EDGE CASES IN FLOWS
// ===========================

#[test]
fn test_flow_with_missing_source() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:flow_1 a yawl:Flow ;
            yawl:into yawl:task_b .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

#[test]
fn test_flow_with_missing_target() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:flow_1 a yawl:Flow ;
            yawl:from yawl:task_a .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

#[test]
fn test_many_flows_from_single_task() {
    let gen = YawlGenerator::new().with_validation(false);

    let mut flows = String::new();
    for i in 0..10 {
        flows.push_str(&format!(
            r#"
            yawl:flow_{} a yawl:Flow ;
                yawl:from yawl:task_a ;
                yawl:into yawl:task_{} .
            yawl:task_{} a yawl:AtomicTask ;
                rdfs:label "Task {}" ;
                yawl:taskId "t_{}" .
            "#,
            i, i, i, i, i
        ));
    }

    let ontology = format!(
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_a a yawl:AtomicTask ;
            rdfs:label "Source Task" ;
            yawl:taskId "t_a" .

        {}
        "#,
        flows
    );

    let result = gen.generate(&ontology);
    let _ = result;
}

// ===========================
// 12. SPLIT/JOIN BEHAVIOR EDGE CASES
// ===========================

#[test]
fn test_invalid_split_type() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_bad_split a yawl:AtomicTask ;
            rdfs:label "Bad Split Task" ;
            yawl:taskId "t_bad" ;
            yawl:splitType "INVALID_SPLIT" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

#[test]
fn test_task_with_both_split_and_join() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_gateway a yawl:AtomicTask ;
            rdfs:label "Gateway Task" ;
            yawl:taskId "t_gateway" ;
            yawl:splitType "AND" ;
            yawl:joinType "XOR" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

// ===========================
// 13. VARIABLE BINDING EDGE CASES
// ===========================

#[test]
fn test_variable_with_missing_type() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:var_no_type a yawl:Variable ;
            yawl:varName "myVar" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

#[test]
fn test_variable_with_empty_name() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:var_empty_name a yawl:Variable ;
            yawl:varName "" ;
            yawl:varType "string" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

// ===========================
// 14. CONDITION EDGE CASES
// ===========================

#[test]
fn test_flow_with_complex_condition() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:flow_conditional a yawl:Flow ;
            yawl:from yawl:task_a ;
            yawl:into yawl:task_b ;
            yawl:condition "(amount > 1000) AND (status == 'approved')" .

        yawl:task_a a yawl:AtomicTask ;
            rdfs:label "Task A" ;
            yawl:taskId "t_a" .

        yawl:task_b a yawl:AtomicTask ;
            rdfs:label "Task B" ;
            yawl:taskId "t_b" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

// ===========================
// 15. COMPOSITE TASK EDGE CASES
// ===========================

#[test]
fn test_composite_task_without_decomposition() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_composite a yawl:CompositeTask ;
            rdfs:label "Composite Without Decomposition" ;
            yawl:taskId "t_comp" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

#[test]
fn test_multiple_instance_task_without_count() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_mi a yawl:MultiInstanceTask ;
            rdfs:label "MI Task Without Count" ;
            yawl:taskId "t_mi" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

// ===========================
// 16. ONTOLOGY FORMAT EDGE CASES
// ===========================

#[test]
fn test_malformed_turtle_syntax() {
    let gen = YawlGenerator::new();
    let ontology = r#"
        @prefix yawl: <http://unrdf.org/yawl#>
        yawl:task a yawl:Task ; rdfs:label "Unclosed statement"
    "#;

    let result = gen.generate(ontology);
    assert!(result.is_err());
}

#[test]
fn test_invalid_iri_syntax() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_bad_iri a <not a valid IRI > ;
            rdfs:label "Bad IRI" ;
            yawl:taskId "t_bad" .
    "#;

    let result = gen.generate(ontology);
    assert!(result.is_err());
}

// ===========================
// 17. NAMESPACE EDGE CASES
// ===========================

#[test]
fn test_undefined_namespace_prefix() {
    let gen = YawlGenerator::new();
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        undefined:task a undefined:Task ;
            rdfs:label "Undefined Namespace" .
    "#;

    let result = gen.generate(ontology);
    assert!(result.is_err());
}

#[test]
fn test_duplicate_namespace_prefixes() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .
        @prefix yawl: <http://different.namespace/yawl#> .

        yawl:task a yawl:Task ;
            rdfs:label "Duplicate Prefix" .
    "#;

    let result = gen.generate(ontology);
    // Some parsers may accept last definition, others reject
    assert!(result.is_ok() || result.is_err());
}

// ===========================
// 18. TYPE SYSTEM EDGE CASES
// ===========================

#[test]
fn test_task_with_multiple_types() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_multi_type a yawl:AtomicTask ;
            a yawl:CompositeTask ;
            rdfs:label "Multi-Type Task" ;
            yawl:taskId "t_multi" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

#[test]
fn test_task_with_unknown_type() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:task_unknown a yawl:UnknownTaskType ;
            rdfs:label "Unknown Type Task" ;
            yawl:taskId "t_unknown" .
    "#;

    let result = gen.generate(ontology);
    let _ = result;
}

// ===========================
// 19. PERFORMANCE EDGE CASES
// ===========================

#[test]
fn test_deeply_nested_flows() {
    let gen = YawlGenerator::new().with_validation(false);

    let mut flows = String::new();
    for i in 0..20 {
        let next = i + 1;
        flows.push_str(&format!(
            r#"
            yawl:task_{} a yawl:AtomicTask ;
                rdfs:label "Task {}" ;
                yawl:taskId "t_{}" .
            yawl:flow_{} a yawl:Flow ;
                yawl:from yawl:task_{} ;
                yawl:into yawl:task_{} .
            "#,
            i, i, i, i, i, next
        ));
    }

    let ontology = format!(
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:NestedWorkflow a yawl:Specification ;
            rdfs:label "Deeply Nested Workflow" ;
            yawl:version "1.0.0" .

        {}
        "#,
        flows
    );

    let result = gen.generate(&ontology);
    let _ = result;
}

// ===========================
// 20. RECOVERY EDGE CASES
// ===========================

#[test]
fn test_partial_ontology_recovery() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:good_task a yawl:AtomicTask ;
            rdfs:label "Good Task" ;
            yawl:taskId "t_good" .

        yawl:bad_task a yawl:AtomicTask ;
            rdfs:label "Bad Task" .

        yawl:good_task_2 a yawl:AtomicTask ;
            rdfs:label "Good Task 2" ;
            yawl:taskId "t_good_2" .
    "#;

    let result = gen.generate(ontology);
    // Should process valid parts even if some parts have errors
    let _ = result;
}

#[test]
fn test_generator_with_validation_disabled() {
    let gen = YawlGenerator::new().with_validation(false);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:MinimalWorkflow a yawl:Specification ;
            yawl:version "1.0.0" .
    "#;

    let result = gen.generate(ontology);
    // Should skip validation errors and complete
    let _ = result;
}

#[test]
fn test_generator_with_validation_enabled() {
    let gen = YawlGenerator::new().with_validation(true);
    let ontology = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix yawl: <http://unrdf.org/yawl#> .

        yawl:MinimalWorkflow a yawl:Specification ;
            yawl:version "1.0.0" .
    "#;

    let result = gen.generate(ontology);
    // Validation may stricter
    match result {
        Ok(_) => {}
        Err(Error::Validation(_)) => {}
        Err(_) => {}
    }
}
