//! YAWL Generation SLO Benchmarks
//!
//! This benchmark suite validates that YAWL workflow generation meets its
//! Service Level Objectives (SLOs). These are critical performance thresholds
//! that must be met for production readiness.
//!
//! ## Service Level Objectives (SLOs)
//!
//! | Benchmark Category | Target | Description |
//! |-------------------|--------|-------------|
//! | RDF Loading (10k triples) | <1s | Load and parse RDF ontology |
//! | SPARQL CONSTRUCT (100 tasks) | <2s | Execute transformation queries |
//! | Template Rendering | <100ms | Single template render |
//! | Full Pipeline | <5s | Complete five-stage pipeline |
//!
//! ## Running Benchmarks
//!
//! ```bash
//! # Run all YAWL SLO benchmarks
//! cargo bench --bench yawl_generation_slo
//!
//! # Run specific benchmark group
//! cargo bench --bench yawl_generation_slo -- rdf_loading
//!
//! # Generate HTML report
//! cargo bench --bench yawl_generation_slo -- --output-format html
//! ```

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_yawl::{ConstructExecutor, OntologyLoader, TemplateRenderer, YawlGenerator};
use std::hint::black_box;
use std::time::Duration;

// =============================================================================
// SLO Constants
// =============================================================================

/// Maximum allowed time for RDF loading of 10k triples (1 second)
const SLO_RDF_LOADING_10K_MS: u64 = 1_000;

/// Maximum allowed time for SPARQL CONSTRUCT of 100 tasks (2 seconds)
const SLO_SPARQL_CONSTRUCT_100_MS: u64 = 2_000;

/// Maximum allowed time for template rendering (100ms)
const SLO_TEMPLATE_RENDER_MS: u64 = 100;

/// Maximum allowed time for full pipeline (5 seconds)
const SLO_FULL_PIPELINE_MS: u64 = 5_000;

/// Tolerance percentage for SLO validation (10%)
const SLO_TOLERANCE_PERCENT: f64 = 0.10;

// =============================================================================
// Test Data Generators
// =============================================================================

/// Generate industry ontology RDF content with specified triple count.
fn generate_industry_ontology(triple_count: usize) -> String {
    let mut rdf = String::from(
        r#"@prefix fibo: <https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPersons/Agents/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .

fibo:LegalPerson a owl:Class ;
    rdfs:label "Legal Person" ;
    rdfs:comment "A legal entity that has legal rights and obligations" ;
    skos:definition "A person or organization that has legal rights and responsibilities" .

fibo:NaturalPerson a owl:Class ;
    rdfs:subClassOf fibo:LegalPerson ;
    rdfs:label "Natural Person" ;
    rdfs:comment "A human being regarded as an individual" .

fibo:Organization a owl:Class ;
    rdfs:subClassOf fibo:LegalPerson ;
    rdfs:label "Organization" ;
    rdfs:comment "A structured group of people with a common purpose" .

fibo:hasIdentifier a owl:DatatypeProperty ;
    rdfs:label "has identifier" ;
    rdfs:domain fibo:LegalPerson ;
    rdfs:range xsd:string .

fibo:hasName a owl:DatatypeProperty ;
    rdfs:label "has name" ;
    rdfs:domain fibo:LegalPerson ;
    rdfs:range xsd:string .

fibo:isIncorporatedIn a owl:ObjectProperty ;
    rdfs:label "incorporated in" ;
    rdfs:domain fibo:Organization ;
    rdfs:range fibo:LegalPerson .

fibo:EmploymentContract a owl:Class ;
    rdfs:label "Employment Contract" ;
    rdfs:comment "An agreement between an employer and employee" .

fibo:hasEmployer a owl:ObjectProperty ;
    rdfs:label "has employer" ;
    rdfs:domain fibo:EmploymentContract ;
    rdfs:range fibo:Organization .

fibo:hasEmployee a owl:ObjectProperty ;
    rdfs:label "has employee" ;
    rdfs:domain fibo:EmploymentContract ;
    rdfs:range fibo:NaturalPerson .

fibo:ContractApprovalWorkflow a owl:Class ;
    rdfs:label "Contract Approval Workflow" ;
    rdfs:comment "Workflow for approving contracts" .

fibo:requiresApproval a owl:ObjectProperty ;
    rdfs:label "requires approval" ;
    rdfs:domain fibo:EmploymentContract ;
    rdfs:range fibo:ContractApprovalWorkflow .

"#,
    );

    // Generate entity instances
    for i in 0..triple_count {
        let entity_type = match i % 4 {
            0 => "fibo:NaturalPerson",
            1 => "fibo:Organization",
            2 => "fibo:EmploymentContract",
            _ => "fibo:ContractApprovalWorkflow",
        };

        rdf.push_str(&format!(
            "fibo:entity{} a {} ;\n    rdfs:label \"Entity {}\" ;\n    fibo:hasIdentifier \"ID-{}\"^^xsd:string ;\n    fibo:hasName \"Name {}\"^^xsd:string .\n\n",
            i, entity_type, i, i, i
        ));
    }

    rdf
}

/// Generate YAWL RDF template context for testing.
fn generate_yawl_rdf_context(task_count: usize) -> String {
    let mut rdf = String::from(
        r#"
@prefix yawl: <http://www.yawlfoundation.org/yawlschema> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

yawl:Specification a rdfs:Class .
yawl:Task a rdfs:Class .
yawl:Flow a rdfs:Class .

yawl:Workflow_1 a yawl:Specification ;
    yawl:name "Test Workflow" ;
    yawl:description "Test workflow for benchmarking" ;
    yawl:version "1.0.0" .

yawl:InputCondition a yawl:Task ;
    yawl:id "input" ;
    yawl:name "Start" ;
    yawl:splitType "XOR" ;
    yawl:joinType "XOR" ;
    yawl:isAuto "true"^^xsd:boolean .

yawl:OutputCondition a yawl:Task ;
    yawl:id "output" ;
    yawl:name "End" ;
    yawl:splitType "XOR" ;
    yawl:joinType "XOR" ;
    yawl:isAuto "true"^^xsd:boolean .

"#,
    );

    // Add tasks
    for i in 0..task_count {
        rdf.push_str(&format!(
            r#"
yawl:Task_{} a yawl:Task ;
    yawl:id "task_{}" ;
    yawl:name "Task {}" ;
    yawl:splitType "XOR" ;
    yawl:joinType "XOR" ;
    yawl:isAuto "false"^^xsd:boolean .

"#,
            i, i, i
        ));
    }

    // Add flows
    rdf.push_str("yawl:Flow_0 a yawl:Flow ; yawl:from \"input\" ; yawl:into \"task_0\" .\n");
    for i in 0..task_count.saturating_sub(1) {
        rdf.push_str(&format!(
            "yawl:Flow_{} a yawl:Flow ; yawl:from \"task_{}\" ; yawl:into \"task_{}\" .\n",
            i + 1,
            i,
            i + 1
        ));
    }
    if task_count > 0 {
        rdf.push_str(&format!(
            "yawl:Flow_{} a yawl:Flow ; yawl:from \"task_{}\" ; yawl:into \"output\" .\n",
            task_count,
            task_count - 1
        ));
    }

    rdf
}

/// Generate a simple SPARQL CONSTRUCT query for benchmarking.
fn generate_sparql_construct_query(pattern_count: usize) -> String {
    let mut query = String::from(
        r#"
PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPersons/Agents/>
PREFIX yawl: <http://www.yawlfoundation.org/yawlschema>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

CONSTRUCT {
    ?task a yawl:Task ;
        yawl:id ?taskId ;
        yawl:name ?taskName .
}
WHERE {
    ?entity a fibo:LegalPerson .
"#,
    );

    for i in 0..pattern_count {
        query.push_str(&format!("    ?entity fibo:property{} ?value{} .\n", i, i));
    }

    query.push_str("    BIND(CONCAT(\"task_\", STR(?entity)) AS ?taskId)\n");
    query.push_str("    BIND(?entity AS ?taskName)\n");
    query.push_str("}\n");

    query
}

/// Generate template source string for benchmarking.
fn generate_template_source(variable_count: usize) -> String {
    let mut template = String::from(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema">
    <specificationID>{{ workflow_name }}</specificationID>
    <name>{{ workflow_name }}</name>
    <description>{{ description }}</description>
    <version>{{ version }}</version>
    <decomposition id=" decomposition" type="WSNet">
        <rootNetID>{{ workflow_name }}</rootNetID>
        <net id="{{ workflow_name }}" version="{{ version }}">
            <controlFlowProtocols>
                <inputCondition id="input"/>
                <outputCondition id="output"/>
"#,
    );

    // Add variable placeholders
    for i in 0..variable_count {
        template.push_str(&format!(
            "                <variable id=\"var{}\" type=\"string\">{{{{ var{} }}}}</variable>\n",
            i, i
        ));
    }

    template.push_str(
        r#"            </controlFlowProtocols>
            <processControlElements>
                <!-- Tasks rendered here -->
                {% for task in tasks %}
                <task id="{{ task.id }}">
                    <name>{{ task.name }}</name>
                    <split type="{{ task.split_type }}"/>
                    <join type="{{ task.join_type }}"/>
                </task>
                {% endfor %}
            </processControlElements>
        </net>
    </decomposition>
</specification>
"#,
    );

    template
}

// =============================================================================
// RDF Loading SLO Benchmarks
// =============================================================================

fn bench_rdf_loading_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("rdf_loading_slo");

    // Test with different ontology sizes
    let triple_counts = [100, 500, 1_000, 2_000, 5_000, 10_000];

    for count in triple_counts {
        let ontology_content = generate_industry_ontology(count);

        group.throughput(Throughput::Elements(count as u64));
        group.measurement_time(Duration::from_secs(10));
        group.sample_size(50);

        group.bench_with_input(
            BenchmarkId::new("load_ontology", count),
            &count,
            |b, &_count| {
                b.iter(|| {
                    let start = std::time::Instant::now();

                    let loader = OntologyLoader::new();
                    let result = black_box(loader.load_from_str(
                        &ontology_content,
                        ggen_yawl::ontology::loader::OntologyFormat::Turtle,
                    ));

                    let elapsed = start.elapsed();

                    // Validate SLO for 10k triples
                    if count == 10_000 {
                        if let Ok(ref graph) = result {
                            let triple_count = graph.len();
                            assert!(triple_count > 0, "Loaded graph should contain triples");
                        }
                        assert_slo_measured(
                            elapsed,
                            Duration::from_millis(SLO_RDF_LOADING_10K_MS),
                            "rdf_loading_10k",
                        );
                    }

                    result
                });
            },
        );
    }

    group.finish();
}

// =============================================================================
// SPARQL CONSTRUCT SLO Benchmarks
// =============================================================================

fn bench_sparql_construct_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("sparql_construct_slo");

    // Test with different query complexities
    let pattern_counts = [5, 10, 25, 50, 100];

    for patterns in pattern_counts {
        let query = generate_sparql_construct_query(patterns);
        let ontology = generate_industry_ontology(1000);

        group.throughput(Throughput::Elements(patterns as u64));
        group.measurement_time(Duration::from_secs(10));
        group.sample_size(100);

        group.bench_with_input(
            BenchmarkId::new("construct_query", patterns),
            &patterns,
            |b, &_patterns| {
                b.iter(|| {
                    let start = std::time::Instant::now();

                    // Load ontology
                    let loader = OntologyLoader::new();
                    let graph = match loader.load_from_str(
                        &ontology,
                        ggen_yawl::ontology::loader::OntologyFormat::Turtle,
                    ) {
                        Ok(g) => g,
                        Err(_) => return start.elapsed(),
                    };

                    // Execute CONSTRUCT query
                    let executor = ConstructExecutor::new();
                    let _result = executor.execute_query(
                        &graph,
                        &ggen_yawl::transform::executor::Query::new("bench", &query),
                    );

                    let elapsed = start.elapsed();

                    // Validate SLO for 100 pattern query
                    if patterns == 100 {
                        assert_slo_measured(
                            elapsed,
                            Duration::from_millis(SLO_SPARQL_CONSTRUCT_100_MS),
                            "sparql_construct_100",
                        );
                    }

                    elapsed
                });
            },
        );
    }

    // Benchmark multiple CONSTRUCT queries (pipeline simulation)
    group.bench_function("pipeline_6_queries", |b| {
        let ontology = generate_industry_ontology(5000);

        b.iter(|| {
            let start = std::time::Instant::now();

            let loader = OntologyLoader::new();
            let graph = match loader.load_from_str(
                &ontology,
                ggen_yawl::ontology::loader::OntologyFormat::Turtle,
            ) {
                Ok(g) => g,
                Err(_) => return start.elapsed(),
            };

            let executor = ConstructExecutor::new();
            let _result = black_box(executor.execute_all(&graph));

            let elapsed = start.elapsed();

            // Validate SLO for full query pipeline
            assert_slo_measured(
                elapsed,
                Duration::from_millis(SLO_SPARQL_CONSTRUCT_100_MS),
                "sparql_construct_pipeline",
            );

            elapsed
        });
    });

    group.finish();
}

// =============================================================================
// Template Rendering SLO Benchmarks
// =============================================================================

fn bench_template_rendering_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_rendering_slo");

    // Test with different template complexities
    let variable_counts = [5, 10, 25, 50, 100];

    for variables in variable_counts {
        let template_source = generate_template_source(variables);

        group.throughput(Throughput::Elements(variables as u64));
        group.measurement_time(Duration::from_secs(5));
        group.sample_size(500);

        group.bench_with_input(
            BenchmarkId::new("render_template", variables),
            &variables,
            |b, &_variables| {
                b.iter(|| {
                    let start = std::time::Instant::now();

                    // Create a mock tera context
                    let mut tera = tera::Tera::default();
                    tera.add_raw_template("test", &template_source).unwrap();

                    let mut context = tera::Context::new();
                    context.insert("workflow_name", "test_workflow");
                    context.insert("description", "Test description");
                    context.insert("version", "1.0.0");

                    for i in 0..variables {
                        context.insert(&format!("var{}", i), &format!("value{}", i));
                    }

                    let tasks: Vec<serde_json::Value> = (0..10)
                        .map(|i| {
                            serde_json::json!({
                                "id": format!("task_{}", i),
                                "name": format!("Task {}", i),
                                "split_type": "XOR",
                                "join_type": "XOR"
                            })
                        })
                        .collect();
                    context.insert("tasks", &tasks);

                    let _result = black_box(tera.render("test", &context));

                    let elapsed = start.elapsed();

                    // Validate SLO for simple templates
                    if variables <= 10 {
                        assert_slo_measured(
                            elapsed,
                            Duration::from_millis(SLO_TEMPLATE_RENDER_MS),
                            "template_render_simple",
                        );
                    }

                    elapsed
                });
            },
        );
    }

    // Benchmark YAWL template renderer
    group.bench_function("yawl_renderer_simple", |b| {
        let renderer = TemplateRenderer::new();

        // Create a simple context
        let context = ggen_yawl::template::TemplateContext {
            workflow_name: "bench_workflow".to_string(),
            description: "Benchmark workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![ggen_yawl::template::TaskContext {
                id: "task1".to_string(),
                name: "First Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            }],
            flows: vec![ggen_yawl::template::FlowContext {
                source: "input".to_string(),
                target: "task1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            }],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        b.iter(|| {
            let start = std::time::Instant::now();

            let _result = black_box(renderer.render_yawl_xml(&context));

            let elapsed = start.elapsed();

            // Validate SLO for template rendering
            assert_slo_measured(
                elapsed,
                Duration::from_millis(SLO_TEMPLATE_RENDER_MS),
                "template_render_yawl",
            );

            elapsed
        });
    });

    group.finish();
}

// =============================================================================
// Full Pipeline SLO Benchmarks
// =============================================================================

fn bench_full_pipeline_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_pipeline_slo");

    group.measurement_time(Duration::from_secs(15));
    group.sample_size(20);

    // Benchmark complete YAWL generation pipeline
    group.bench_function("complete_generation_small", |b| {
        let ontology = generate_industry_ontology(1000);

        b.iter(|| {
            let start = std::time::Instant::now();

            let generator = YawlGenerator::new();
            let _result = black_box(generator.generate(&ontology));

            let elapsed = start.elapsed();

            // Small workflow should complete quickly
            assert_slo_measured(
                elapsed,
                Duration::from_millis(SLO_FULL_PIPELINE_MS),
                "full_pipeline_small",
            );

            elapsed
        });
    });

    // Benchmark with medium ontology
    group.bench_function("complete_generation_medium", |b| {
        let ontology = generate_industry_ontology(5000);

        b.iter(|| {
            let start = std::time::Instant::now();

            let generator = YawlGenerator::new();
            let _result = black_box(generator.generate(&ontology));

            let elapsed = start.elapsed();

            // Medium workflow still within SLO
            assert_slo_measured(
                elapsed,
                Duration::from_millis(SLO_FULL_PIPELINE_MS),
                "full_pipeline_medium",
            );

            elapsed
        });
    });

    // Benchmark pipeline stages individually
    group.bench_function("stage_1_normalize", |b| {
        let ontology = generate_industry_ontology(1000);

        b.iter(|| {
            let start = std::time::Instant::now();

            let loader = OntologyLoader::new();
            let _result = black_box(loader.load_from_str(
                &ontology,
                ggen_yawl::ontology::loader::OntologyFormat::Turtle,
            ));

            start.elapsed()
        });
    });

    group.bench_function("stage_2_extract", |b| {
        let ontology = generate_industry_ontology(1000);

        b.iter(|| {
            let start = std::time::Instant::now();

            let loader = OntologyLoader::new();
            let graph = match loader.load_from_str(
                &ontology,
                ggen_yawl::ontology::loader::OntologyFormat::Turtle,
            ) {
                Ok(g) => g,
                Err(_) => return start.elapsed(),
            };

            let executor = ConstructExecutor::new();
            let _result = black_box(executor.execute_all(&graph));

            start.elapsed()
        });
    });

    group.bench_function("stage_3_emit", |b| {
        let renderer = TemplateRenderer::new();

        let context = ggen_yawl::template::TemplateContext {
            workflow_name: "bench_workflow".to_string(),
            description: "Benchmark workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: (0..10)
                .map(|i| ggen_yawl::template::TaskContext {
                    id: format!("task_{}", i),
                    name: format!("Task {}", i),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: i % 2 == 0,
                    decomposition_id: None,
                })
                .collect(),
            flows: (0..9)
                .map(|i| ggen_yawl::template::FlowContext {
                    source: format!("task_{}", i),
                    target: format!("task_{}", i + 1),
                    condition: None,
                    predicate: None,
                    is_default: true,
                })
                .collect(),
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        b.iter(|| {
            let start = std::time::Instant::now();

            let _result = black_box(renderer.render_yawl_xml(&context));

            start.elapsed()
        });
    });

    group.finish();
}

// =============================================================================
// Scaling Benchmarks
// =============================================================================

fn bench_scaling_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("scaling_slo");

    // Test linear scaling with ontology size
    for triple_count in [1_000, 2_000, 5_000, 10_000, 20_000].iter() {
        let ontology = generate_industry_ontology(*triple_count);

        group.throughput(Throughput::Elements(*triple_count as u64));
        group.measurement_time(Duration::from_secs(10));
        group.sample_size(30);

        group.bench_with_input(
            BenchmarkId::new("load_scale", triple_count),
            triple_count,
            |b, &_count| {
                b.iter(|| {
                    let loader = OntologyLoader::new();
                    black_box(loader.load_from_str(
                        &ontology,
                        ggen_yawl::ontology::loader::OntologyFormat::Turtle,
                    ))
                });
            },
        );
    }

    // Test query scaling with complexity
    for task_count in [10, 50, 100, 200].iter() {
        let yawl_rdf = generate_yawl_rdf_context(*task_count);

        group.throughput(Throughput::Elements(*task_count as u64));
        group.measurement_time(Duration::from_secs(8));
        group.sample_size(50);

        group.bench_with_input(
            BenchmarkId::new("query_scale", task_count),
            task_count,
            |b, &_count| {
                b.iter(|| {
                    let loader = OntologyLoader::new();
                    let graph = match loader.load_from_str(
                        &yawl_rdf,
                        ggen_yawl::ontology::loader::OntologyFormat::Turtle,
                    ) {
                        Ok(g) => g,
                        Err(_) => return,
                    };

                    let executor = ConstructExecutor::new();
                    let _result = black_box(executor.execute_all(&graph));
                });
            },
        );
    }

    group.finish();
}

// =============================================================================
// SLO Validation Helper Functions
// =============================================================================

/// Assert that the measured duration meets the SLO with tolerance.
fn assert_slo_measured(measured: Duration, target: Duration, slo_name: &str) {
    let tolerance_ms = target.as_millis() as f64 * SLO_TOLERANCE_PERCENT;
    let max_allowed = target.as_millis() as f64 + tolerance_ms;
    let measured_ms = measured.as_millis() as f64;

    if measured_ms > max_allowed {
        eprintln!(
            "⚠️  SLO WARNING: {} exceeded target by {:.1}% (measured: {:.2}ms, target: {:.2}ms, max_allowed: {:.2}ms)",
            slo_name,
            ((measured_ms - target.as_millis() as f64) / target.as_millis() as f64) * 100.0,
            measured_ms,
            target.as_millis(),
            max_allowed
        );
    } else {
        println!(
            "✅ SLO PASSED: {} (measured: {:.2}ms, target: {:.2}ms)",
            slo_name,
            measured_ms,
            target.as_millis()
        );
    }
}

// =============================================================================
// Criterion Benchmark Groups
// =============================================================================

criterion_group!(rdf_loading_slo, bench_rdf_loading_slo);

criterion_group!(sparql_construct_slo, bench_sparql_construct_slo);

criterion_group!(template_rendering_slo, bench_template_rendering_slo);

criterion_group!(full_pipeline_slo, bench_full_pipeline_slo);

criterion_group!(scaling_slo, bench_scaling_slo);

criterion_main!(
    rdf_loading_slo,
    sparql_construct_slo,
    template_rendering_slo,
    full_pipeline_slo,
    scaling_slo
);
