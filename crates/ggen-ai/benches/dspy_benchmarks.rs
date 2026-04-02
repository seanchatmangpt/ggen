//! Comprehensive DSPy Operations Benchmarks
//!
//! Benchmarks for core DSPy operations with SLO targets:
//! - Signature JSON Schema generation: <50ms target
//! - TTL to Signature transpilation: <500ms target
//! - Signature validation: <10ms target
//! - Field constraint evaluation: <5ms target
//!
//! Each benchmark includes small, medium, and large input scenarios
//! using realistic test data from existing fixtures.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_ai::codegen::TTLToSignatureTranspiler;
use ggen_ai::dspy::field::{FieldConstraints, InputField, OutputField};
use ggen_ai::dspy::signature::Signature;
use ggen_ai::dspy::SignatureValidator;
use oxigraph::io::RdfFormat;
use oxigraph::store::Store;
use serde_json::json;
use std::fs;
use std::path::Path;

// ============================================================================
// Test Fixture Helpers
// ============================================================================

const FIXTURES_DIR: &str = "crates/ggen-ai/tests/fixtures";

/// Load a TTL fixture into an RDF store
fn load_ttl_fixture(filename: &str) -> Store {
    let path = Path::new(FIXTURES_DIR).join(filename);
    let ttl_content = fs::read_to_string(&path)
        .unwrap_or_else(|_| panic!("Failed to read fixture file: {:?}", path));

    let store = Store::new().expect("Failed to create RDF store");
    let reader = std::io::Cursor::new(ttl_content);
    store
        .load_from_reader(RdfFormat::Turtle, reader)
        .unwrap_or_else(|_| panic!("Failed to load TTL from {}", filename));

    store
}

// ============================================================================
// Signature Creation Helpers (for consistent benchmarking)
// ============================================================================

/// Create a small signature (3 fields, minimal constraints)
fn create_small_signature() -> Signature {
    Signature::new("SmallSignature", "Small signature with 3 fields")
        .with_input(InputField::new("name", "User name", "String"))
        .with_input(InputField::new("email", "Email address", "String"))
        .with_input(InputField::new("age", "User age", "i32"))
}

/// Create a medium signature (10 fields, mixed types and constraints)
fn create_medium_signature() -> Signature {
    let mut sig = Signature::new(
        "MediumSignature",
        "Medium signature with 10 fields and constraints",
    );

    // Add input fields with various types and constraints
    sig = sig
        .with_input(
            InputField::new("username", "Username", "String")
                .with_min_length(3)
                .with_max_length(20)
                .required(true),
        )
        .with_input(
            InputField::new("email", "Email address", "String")
                .with_pattern(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$")
                .required(true),
        )
        .with_input(InputField::new("age", "User age", "i32").required(false))
        .with_input(
            InputField::new("status", "User status", "String")
                .with_enum_values(vec![
                    "active".to_string(),
                    "inactive".to_string(),
                    "suspended".to_string(),
                ])
                .required(true),
        )
        .with_input(
            InputField::new("tags", "User tags", "Vec<String>")
                .with_min_items(1)
                .with_max_items(10),
        )
        .with_input(InputField::new("score", "Numeric score", "f64"))
        .with_input(InputField::new("enabled", "Is enabled", "bool"))
        .with_input(InputField::new("metadata", "Metadata", "String"))
        .with_input(InputField::new("roles", "User roles", "Vec<String>").with_min_items(1))
        .with_input(InputField::new(
            "created_at",
            "Creation timestamp",
            "String",
        ));

    // Add output fields
    sig = sig
        .with_output(OutputField::new("result", "Processing result", "String"))
        .with_output(OutputField::new("confidence", "Confidence score", "f64"));

    sig
}

/// Create a large signature (30 fields, complex constraints)
fn create_large_signature() -> Signature {
    let mut sig = Signature::new(
        "LargeSignature",
        "Large signature with 30 fields and complex constraints",
    );

    // Add 20 input fields with various constraints
    for i in 1..=20 {
        let field_name = format!("field_{}", i);
        let field_desc = format!("Field number {}", i);

        let field = match i % 5 {
            0 => InputField::new(&field_name, &field_desc, "String")
                .with_min_length(1)
                .with_max_length(255)
                .required(i % 3 == 0),
            1 => InputField::new(&field_name, &field_desc, "i32").required(i % 4 == 0),
            2 => InputField::new(&field_name, &field_desc, "f64"),
            3 => InputField::new(&field_name, &field_desc, "bool"),
            4 => InputField::new(&field_name, &field_desc, "Vec<String>")
                .with_min_items(0)
                .with_max_items(100),
            _ => InputField::new(&field_name, &field_desc, "String"),
        };

        sig = sig.with_input(field);
    }

    // Add 10 output fields
    for i in 1..=10 {
        let field_name = format!("output_{}", i);
        let field_desc = format!("Output field {}", i);
        sig = sig.with_output(OutputField::new(&field_name, &field_desc, "String"));
    }

    sig
}

// ============================================================================
// Benchmark 1: JSON Schema Generation
// Target: <50ms for all sizes
// ============================================================================

fn benchmark_json_schema_generation_small(c: &mut Criterion) {
    let sig = create_small_signature();

    c.bench_function("json_schema/small_signature", |b| {
        b.iter(|| {
            let schema = black_box(&sig).as_json_schema();
            black_box(schema);
        })
    });
}

fn benchmark_json_schema_generation_medium(c: &mut Criterion) {
    let sig = create_medium_signature();

    c.bench_function("json_schema/medium_signature", |b| {
        b.iter(|| {
            let schema = black_box(&sig).as_json_schema();
            black_box(schema);
        })
    });
}

fn benchmark_json_schema_generation_large(c: &mut Criterion) {
    let sig = create_large_signature();

    c.bench_function("json_schema/large_signature", |b| {
        b.iter(|| {
            let schema = black_box(&sig).as_json_schema();
            black_box(schema);
        })
    });
}

fn benchmark_json_schema_with_constraints(c: &mut Criterion) {
    let mut group = c.benchmark_group("json_schema/with_constraints");

    for constraint_count in [1, 5, 10].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(constraint_count),
            constraint_count,
            |b, &count| {
                let mut sig = Signature::new("ConstrainedSig", "Signature with constraints");

                for i in 0..count {
                    let field =
                        InputField::new(&format!("field_{}", i), &format!("Field {}", i), "String")
                            .with_min_length(5)
                            .with_max_length(100)
                            .with_pattern("^[a-zA-Z0-9_]+$")
                            .with_enum_values(vec!["opt1".to_string(), "opt2".to_string()])
                            .required(true);

                    sig = sig.with_input(field);
                }

                b.iter(|| {
                    let schema = black_box(&sig).as_json_schema();
                    black_box(schema);
                })
            },
        );
    }
    group.finish();
}

// ============================================================================
// Benchmark 2: TTL to Signature Transpilation
// Target: <500ms for all sizes
// ============================================================================

fn benchmark_ttl_to_signature_simple(c: &mut Criterion) {
    let store = load_ttl_fixture("simple_shape.ttl");

    c.bench_function("ttl_transpilation/simple_shape", |b| {
        b.iter(|| {
            let mut transpiler = TTLToSignatureTranspiler::new();
            let signatures = transpiler.build_signatures(black_box(&store)).unwrap();
            black_box(signatures);
        })
    });
}

fn benchmark_ttl_to_signature_with_constraints(c: &mut Criterion) {
    let store = load_ttl_fixture("shape_with_constraints.ttl");

    c.bench_function("ttl_transpilation/with_constraints", |b| {
        b.iter(|| {
            let mut transpiler = TTLToSignatureTranspiler::new();
            let signatures = transpiler.build_signatures(black_box(&store)).unwrap();
            black_box(signatures);
        })
    });
}

fn benchmark_ttl_to_signature_with_datatypes(c: &mut Criterion) {
    let store = load_ttl_fixture("shape_with_datatypes.ttl");

    c.bench_function("ttl_transpilation/with_datatypes", |b| {
        b.iter(|| {
            let mut transpiler = TTLToSignatureTranspiler::new();
            let signatures = transpiler.build_signatures(black_box(&store)).unwrap();
            black_box(signatures);
        })
    });
}

fn benchmark_ttl_to_signature_multiple_classes(c: &mut Criterion) {
    let store = load_ttl_fixture("shape_with_multiple_classes.ttl");

    c.bench_function("ttl_transpilation/multiple_classes", |b| {
        b.iter(|| {
            let mut transpiler = TTLToSignatureTranspiler::new();
            let signatures = transpiler.build_signatures(black_box(&store)).unwrap();
            black_box(signatures);
        })
    });
}

fn benchmark_ttl_transpilation_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("ttl_transpilation/throughput");

    // Benchmark throughput for different fixture sizes
    let fixtures = vec![
        ("simple_shape.ttl", 1),
        ("shape_with_constraints.ttl", 1),
        ("shape_with_multiple_classes.ttl", 3),
    ];

    for (fixture_name, class_count) in fixtures {
        group.throughput(Throughput::Elements(class_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(fixture_name),
            &fixture_name,
            |b, &name| {
                let store = load_ttl_fixture(name);
                b.iter(|| {
                    let mut transpiler = TTLToSignatureTranspiler::new();
                    let signatures = transpiler.build_signatures(black_box(&store)).unwrap();
                    black_box(signatures);
                })
            },
        );
    }
    group.finish();
}

// ============================================================================
// Benchmark 3: Signature Validation
// Target: <10ms for all sizes
// ============================================================================

fn benchmark_validation_small_valid(c: &mut Criterion) {
    let sig = create_small_signature();
    let validator = SignatureValidator::new(sig);
    let input = json!({
        "name": "John Doe",
        "email": "john@example.com",
        "age": 30
    });

    c.bench_function("validation/small_valid", |b| {
        b.iter(|| {
            let result = black_box(&validator).validate(black_box(&input));
            black_box(result);
        })
    });
}

fn benchmark_validation_medium_valid(c: &mut Criterion) {
    let sig = create_medium_signature();
    let validator = SignatureValidator::new(sig);
    let input = json!({
        "username": "johndoe",
        "email": "john@example.com",
        "age": 30,
        "status": "active",
        "tags": ["user", "premium"],
        "score": 95.5,
        "enabled": true,
        "metadata": "{}",
        "roles": ["admin"],
        "created_at": "2024-01-01T00:00:00Z"
    });

    c.bench_function("validation/medium_valid", |b| {
        b.iter(|| {
            let result = black_box(&validator).validate(black_box(&input));
            black_box(result);
        })
    });
}

fn benchmark_validation_large_valid(c: &mut Criterion) {
    let sig = create_large_signature();
    let validator = SignatureValidator::new(sig);

    // Create valid input with all 20 fields
    let mut input_obj = serde_json::Map::new();
    for i in 1..=20 {
        let field_name = format!("field_{}", i);
        let value = match i % 5 {
            0 => json!("valid_string_value"),
            1 => json!(42),
            2 => json!(3.14),
            3 => json!(true),
            4 => json!(["item1", "item2"]),
            _ => json!("default"),
        };
        input_obj.insert(field_name, value);
    }
    let input = serde_json::Value::Object(input_obj);

    c.bench_function("validation/large_valid", |b| {
        b.iter(|| {
            let result = black_box(&validator).validate(black_box(&input));
            black_box(result);
        })
    });
}

fn benchmark_validation_with_errors(c: &mut Criterion) {
    let mut sig = Signature::new("ErrorTest", "Test validation errors");
    sig = sig
        .with_input(
            InputField::new("username", "Username", "String")
                .with_min_length(5)
                .required(true),
        )
        .with_input(
            InputField::new("email", "Email", "String")
                .with_pattern(r"^[a-z]+@[a-z]+\.[a-z]+$")
                .required(true),
        )
        .with_input(
            InputField::new("tags", "Tags", "Vec<String>")
                .with_min_items(1)
                .required(true),
        );

    let validator = SignatureValidator::new(sig);

    // Invalid input that will trigger multiple errors
    let invalid_input = json!({
        "username": "abc",  // Too short
        "email": "not-an-email",  // Invalid pattern
        "tags": []  // Too few items
    });

    c.bench_function("validation/with_errors", |b| {
        b.iter(|| {
            let result = black_box(&validator).validate(black_box(&invalid_input));
            black_box(result);
        })
    });
}

fn benchmark_validation_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("validation/throughput");

    let sig = create_medium_signature();
    let validator = SignatureValidator::new(sig);

    for input_count in [1, 10, 100].iter() {
        group.throughput(Throughput::Elements(*input_count));
        group.bench_with_input(
            BenchmarkId::from_parameter(input_count),
            input_count,
            |b, &count| {
                let input = json!({
                    "username": "johndoe",
                    "email": "john@example.com",
                    "age": 30,
                    "status": "active",
                    "tags": ["user"],
                    "score": 95.5,
                    "enabled": true,
                    "metadata": "{}",
                    "roles": ["admin"],
                    "created_at": "2024-01-01T00:00:00Z"
                });

                b.iter(|| {
                    for _ in 0..count {
                        let result = black_box(&validator).validate(black_box(&input));
                        black_box(result);
                    }
                })
            },
        );
    }
    group.finish();
}

// ============================================================================
// Benchmark 4: Field Constraint Evaluation
// Target: <5ms for all constraint types
// ============================================================================

fn benchmark_constraint_string_length(c: &mut Criterion) {
    let constraints = FieldConstraints::new().min_length(5).max_length(100);

    let valid_value = json!("This is a valid string");
    let too_short = json!("abc");
    let too_long = json!("a".repeat(150));

    let mut group = c.benchmark_group("constraints/string_length");

    group.bench_function("valid", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&valid_value));
            black_box(result);
        })
    });

    group.bench_function("too_short", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&too_short));
            black_box(result);
        })
    });

    group.bench_function("too_long", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&too_long));
            black_box(result);
        })
    });

    group.finish();
}

fn benchmark_constraint_array_items(c: &mut Criterion) {
    let constraints = FieldConstraints::new().min_items(1).max_items(10);

    let valid_value = json!(["item1", "item2", "item3"]);
    let too_few = json!([]);
    let too_many = json!(vec!["item"; 15]);

    let mut group = c.benchmark_group("constraints/array_items");

    group.bench_function("valid", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&valid_value));
            black_box(result);
        })
    });

    group.bench_function("too_few", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&too_few));
            black_box(result);
        })
    });

    group.bench_function("too_many", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&too_many));
            black_box(result);
        })
    });

    group.finish();
}

fn benchmark_constraint_pattern_matching(c: &mut Criterion) {
    let constraints =
        FieldConstraints::new().pattern(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$");

    let valid_email = json!("user@example.com");
    let invalid_email = json!("not-an-email");

    let mut group = c.benchmark_group("constraints/pattern_matching");

    group.bench_function("valid", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&valid_email));
            black_box(result);
        })
    });

    group.bench_function("invalid", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&invalid_email));
            black_box(result);
        })
    });

    group.finish();
}

fn benchmark_constraint_enum_validation(c: &mut Criterion) {
    let constraints = FieldConstraints::new().enum_values(vec![
        "active".to_string(),
        "inactive".to_string(),
        "suspended".to_string(),
        "pending".to_string(),
    ]);

    let valid_value = json!("active");
    let invalid_value = json!("unknown");

    let mut group = c.benchmark_group("constraints/enum_validation");

    group.bench_function("valid", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&valid_value));
            black_box(result);
        })
    });

    group.bench_function("invalid", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&invalid_value));
            black_box(result);
        })
    });

    group.finish();
}

fn benchmark_constraint_complex_combination(c: &mut Criterion) {
    let constraints = FieldConstraints::new()
        .required(true)
        .min_length(5)
        .max_length(50)
        .pattern(r"^[a-zA-Z0-9_]+$");

    let valid_value = json!("valid_username_123");
    let invalid_value = json!("abc@");

    let mut group = c.benchmark_group("constraints/complex_combination");

    group.bench_function("valid", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&valid_value));
            black_box(result);
        })
    });

    group.bench_function("invalid", |b| {
        b.iter(|| {
            let result = black_box(&constraints).is_satisfied(black_box(&invalid_value));
            black_box(result);
        })
    });

    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    json_schema_benches,
    benchmark_json_schema_generation_small,
    benchmark_json_schema_generation_medium,
    benchmark_json_schema_generation_large,
    benchmark_json_schema_with_constraints
);

criterion_group!(
    ttl_transpilation_benches,
    benchmark_ttl_to_signature_simple,
    benchmark_ttl_to_signature_with_constraints,
    benchmark_ttl_to_signature_with_datatypes,
    benchmark_ttl_to_signature_multiple_classes,
    benchmark_ttl_transpilation_throughput
);

criterion_group!(
    validation_benches,
    benchmark_validation_small_valid,
    benchmark_validation_medium_valid,
    benchmark_validation_large_valid,
    benchmark_validation_with_errors,
    benchmark_validation_throughput
);

criterion_group!(
    constraint_benches,
    benchmark_constraint_string_length,
    benchmark_constraint_array_items,
    benchmark_constraint_pattern_matching,
    benchmark_constraint_enum_validation,
    benchmark_constraint_complex_combination
);

criterion_main!(
    json_schema_benches,
    ttl_transpilation_benches,
    validation_benches,
    constraint_benches
);
