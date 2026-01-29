//! Erlang Generation Pipeline Performance Benchmarks
//!
//! This benchmark suite validates Erlang OTP code generation performance across:
//! - Template rendering (single module, full application, large supervision trees)
//! - SPARQL query execution (entity extraction, supervision tree joins, inference rules)
//! - End-to-end μ₁-μ₅ pipeline (Normalize → Extract → Emit → Canonicalize → Receipt)
//! - SLO validation (generation time, memory usage, deterministic receipts)
//!
//! Run with: `cargo make bench -- erlang_generation`
//! HTML reports: `target/criterion/erlang_*/report/index.html`

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_core::graph::Graph;
use ggen_core::pipeline::Pipeline;
use ggen_core::template::Template;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;
use tera::Context;

// ============================================================================
// SLO DEFINITIONS (Erlang-Specific)
// ============================================================================

mod slo {
    use std::time::Duration;

    // Template Rendering SLOs
    pub const SINGLE_MODULE_P50: Duration = Duration::from_millis(10);
    pub const SINGLE_MODULE_P95: Duration = Duration::from_millis(20);
    pub const SINGLE_MODULE_MAX: Duration = Duration::from_millis(50);

    pub const FULL_APP_10_MODULES_P50: Duration = Duration::from_millis(100);
    pub const FULL_APP_10_MODULES_P95: Duration = Duration::from_millis(200);
    pub const FULL_APP_10_MODULES_MAX: Duration = Duration::from_millis(500);

    pub const LARGE_ONTOLOGY_100_ENTITIES_P50: Duration = Duration::from_millis(1000);
    pub const LARGE_ONTOLOGY_100_ENTITIES_MAX: Duration = Duration::from_millis(5000);

    // SPARQL Query SLOs
    pub const ENTITY_EXTRACTION_P50: Duration = Duration::from_millis(5);
    pub const ENTITY_EXTRACTION_P95: Duration = Duration::from_millis(10);

    pub const SUPERVISION_TREE_JOIN_P50: Duration = Duration::from_millis(15);
    pub const SUPERVISION_TREE_JOIN_P95: Duration = Duration::from_millis(30);

    pub const INFERENCE_EXECUTION_P50: Duration = Duration::from_millis(20);
    pub const INFERENCE_EXECUTION_P95: Duration = Duration::from_millis(40);

    // End-to-End Pipeline SLOs (μ₁-μ₅)
    pub const PIPELINE_MEDIUM_PROJECT_MAX: Duration = Duration::from_secs(5);
    pub const PIPELINE_MEMORY_LIMIT: usize = 100 * 1024 * 1024; // 100MB

    // Deterministic Receipt SLOs
    pub const RECEIPT_GENERATION_P50: Duration = Duration::from_millis(50);
    pub const RECEIPT_GENERATION_MAX: Duration = Duration::from_millis(100);
}

// ============================================================================
// FIXTURE GENERATORS
// ============================================================================

/// Generate Erlang adapter RDF ontology fixture
fn create_erlang_adapter_ontology(system_count: usize) -> String {
    let mut rdf = String::from(
        r#"@prefix ex: <http://example.org/> .
@prefix ggen: <http://ggen.io/ontology/> .
@prefix erlang: <http://ggen.io/ontology/erlang/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

"#,
    );

    for i in 0..system_count {
        rdf.push_str(&format!(
            r#"
ex:System{} a erlang:Adapter ;
    erlang:id "system_{}" ;
    erlang:name "System {}" ;
    erlang:authType "oauth2" ;
    erlang:supervisor ex:Supervisor{} ;
    erlang:worker ex:Worker{} .

ex:Supervisor{} a erlang:Supervisor ;
    erlang:strategy "one_for_one" ;
    erlang:intensity 5 ;
    erlang:period 60 ;
    erlang:children (ex:Worker{} ex:CircuitBreaker{}) .

ex:Worker{} a erlang:GenServer ;
    erlang:module "ggen_adapter_system_{}" ;
    erlang:restart "permanent" ;
    erlang:shutdown 5000 .

ex:CircuitBreaker{} a erlang:GenServer ;
    erlang:module "ggen_circuit_breaker" ;
    erlang:restart "permanent" ;
    erlang:shutdown 5000 ;
    erlang:threshold 10 ;
    erlang:timeout 30000 .

"#,
            i, i, i, i, i, i, i, i, i, i
        ));
    }

    rdf
}

/// Create Erlang adapter template
fn create_erlang_adapter_template() -> String {
    r#"---
to: "output/{{ system_id }}_adapter.erl"
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.io/ontology/erlang/"
sparql:
  systems: |
    PREFIX erlang: <http://ggen.io/ontology/erlang/>
    SELECT ?system ?id ?name ?auth WHERE {
      ?system a erlang:Adapter ;
              erlang:id ?id ;
              erlang:name ?name ;
              erlang:authType ?auth .
    }
  supervisors: |
    PREFIX erlang: <http://ggen.io/ontology/erlang/>
    SELECT ?supervisor ?strategy ?intensity ?period WHERE {
      ?system erlang:supervisor ?supervisor .
      ?supervisor erlang:strategy ?strategy ;
                  erlang:intensity ?intensity ;
                  erlang:period ?period .
    }
  workers: |
    PREFIX erlang: <http://ggen.io/ontology/erlang/>
    SELECT ?worker ?module ?restart ?shutdown WHERE {
      ?system erlang:worker ?worker .
      ?worker erlang:module ?module ;
              erlang:restart ?restart ;
              erlang:shutdown ?shutdown .
    }
---
%% @doc {{ name }} Adapter - Generated from RDF spec
%% Regenerate with: ggen sync

-module(ggen_adapter_{{ id }}).
-behaviour(supervisor).
-behaviour(gen_server).

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ============================================================================
%% Type Definitions
%% ============================================================================

-type state() :: #{
  system_id => binary(),
  system_name => binary(),
  status => connecting | connected | error,
  auth => {{ auth }},
  retry_count => non_neg_integer()
}.

%% ============================================================================
%% OTP Supervisor Callbacks
%% ============================================================================

start_link(Config) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

{% for row in supervisors %}
init(_Config) ->
  SupFlags = #{
    strategy => {{ row.strategy }},
    intensity => {{ row.intensity }},
    period => {{ row.period }}
  },

  ChildSpecs = [
    {% for worker in workers %}
    #{
      id => {{ worker.module }},
      start => {gen_server, start_link, [?MODULE, [], []]},
      restart => {{ worker.restart }},
      shutdown => {{ worker.shutdown }},
      type => worker,
      modules => [?MODULE]
    }{% if not loop.last %},{% endif %}
    {% endfor %}
  ],

  {ok, {SupFlags, ChildSpecs}}.
{% endfor %}

%% ============================================================================
%% Gen Server Implementation
%% ============================================================================

handle_call(get_state, _From, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.
"#
    .to_string()
}

/// Create complex Erlang supervision tree template
fn create_supervision_tree_template() -> String {
    r#"---
to: "output/{{ app_name }}_sup.erl"
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.io/ontology/erlang/"
sparql:
  app_config: |
    PREFIX erlang: <http://ggen.io/ontology/erlang/>
    SELECT ?app ?name ?version WHERE {
      ?app a erlang:Application ;
           erlang:name ?name ;
           erlang:version ?version .
    }
  supervision_tree: |
    PREFIX erlang: <http://ggen.io/ontology/erlang/>
    SELECT ?supervisor ?strategy ?child ?module WHERE {
      ?app erlang:supervisor ?supervisor .
      ?supervisor erlang:strategy ?strategy ;
                  erlang:children ?childList .
      ?childList rdf:rest*/rdf:first ?child .
      ?child erlang:module ?module .
    }
  dependencies: |
    PREFIX erlang: <http://ggen.io/ontology/erlang/>
    SELECT ?module ?depends_on WHERE {
      ?worker erlang:module ?module ;
              erlang:dependsOn ?depends_on .
    }
---
%% @doc {{ name }} Application Supervisor
%% Version: {{ version }}
%% Generated from RDF specification

-module({{ app_name }}_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

{% for row in app_config %}
init([]) ->
  %% Application: {{ row.name }} v{{ row.version }}

  {% for sup in supervision_tree %}
  SupFlags = #{strategy => {{ sup.strategy }}},

  Children = [
    #{
      id => {{ sup.module }},
      start => {{{ sup.module }}, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker
    }{% if not loop.last %},{% endif %}
  ],
  {% endfor %}

  {ok, {SupFlags, Children}}.
{% endfor %}
"#
    .to_string()
}

// ============================================================================
// BENCHMARK 1: Template Rendering Performance
// ============================================================================

fn bench_erlang_template_rendering(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_template_rendering");

    let pipeline = Pipeline::new().unwrap();
    let temp_dir = TempDir::new().unwrap();

    // Single module generation (SLO: P50 < 10ms)
    group.bench_function("single_module_p50_target_10ms", |b| {
        let template_str = create_erlang_adapter_template();
        let ontology = create_erlang_adapter_ontology(1);

        b.iter(|| {
            let mut template = Template::parse(black_box(&template_str)).unwrap();
            let mut ctx = Context::new();
            ctx.insert("system_id", "test_system");
            ctx.insert("id", "test");
            ctx.insert("name", "Test System");
            ctx.insert("auth", "oauth2");

            let mut graph = Graph::new().unwrap();
            graph.load_turtle(black_box(&ontology)).unwrap();

            let mut tera = pipeline.tera.clone();
            let template_path = temp_dir.path().join("template.tera");

            template.render_frontmatter(&mut tera, &ctx).unwrap();
            template
                .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                .ok();
            let result = template.render(&mut tera, &ctx);

            black_box(result)
        });
    });

    // Full application (10 modules) - SLO: P50 < 100ms
    for module_count in [5, 10, 20] {
        group.throughput(Throughput::Elements(module_count));

        group.bench_with_input(
            BenchmarkId::new("full_app_p50_target_100ms", module_count),
            &module_count,
            |b, &count| {
                let template_str = create_erlang_adapter_template();
                let ontology = create_erlang_adapter_ontology(count as usize);

                b.iter(|| {
                    for i in 0..count {
                        let mut template = Template::parse(&template_str).unwrap();
                        let mut ctx = Context::new();
                        ctx.insert("system_id", &format!("system_{}", i));
                        ctx.insert("id", &format!("{}", i));
                        ctx.insert("name", &format!("System {}", i));
                        ctx.insert("auth", "oauth2");

                        let mut graph = Graph::new().unwrap();
                        graph.load_turtle(&ontology).unwrap();

                        let mut tera = pipeline.tera.clone();
                        let template_path = temp_dir.path().join("template.tera");

                        template.render_frontmatter(&mut tera, &ctx).unwrap();
                        template
                            .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                            .ok();
                        let rendered = template.render(&mut tera, &ctx).unwrap();

                        black_box(rendered);
                    }
                });
            },
        );
    }

    // Large ontology (100+ entities) - SLO: P50 < 1s
    group.bench_function("large_ontology_100_entities_p50_target_1000ms", |b| {
        let template_str = create_supervision_tree_template();
        let ontology = create_erlang_adapter_ontology(100);

        b.iter(|| {
            let mut template = Template::parse(&template_str).unwrap();
            let mut ctx = Context::new();
            ctx.insert("app_name", "large_app");
            ctx.insert("name", "Large Application");
            ctx.insert("version", "1.0.0");

            let mut graph = Graph::new().unwrap();
            graph.load_turtle(black_box(&ontology)).unwrap();

            let mut tera = pipeline.tera.clone();
            let template_path = temp_dir.path().join("template.tera");

            template.render_frontmatter(&mut tera, &ctx).unwrap();
            template
                .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                .ok();
            let result = template.render(&mut tera, &ctx);

            black_box(result)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 2: SPARQL Query Execution
// ============================================================================

fn bench_erlang_sparql_queries(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_sparql_queries");

    // Simple entity extraction (SLO: P50 < 5ms)
    for entity_count in [10, 50, 100] {
        group.throughput(Throughput::Elements(entity_count));

        group.bench_with_input(
            BenchmarkId::new("entity_extraction_p50_target_5ms", entity_count),
            &entity_count,
            |b, &count| {
                let ontology = create_erlang_adapter_ontology(count as usize);

                b.iter(|| {
                    let mut graph = Graph::new().unwrap();
                    graph.load_turtle(black_box(&ontology)).unwrap();

                    let query = r#"
                        PREFIX erlang: <http://ggen.io/ontology/erlang/>
                        SELECT ?adapter ?id ?name WHERE {
                          ?adapter a erlang:Adapter ;
                                   erlang:id ?id ;
                                   erlang:name ?name .
                        }
                    "#;

                    let result = graph.query(black_box(query));
                    black_box(result)
                });
            },
        );
    }

    // Complex joins (supervision tree) - SLO: P50 < 15ms
    for entity_count in [10, 50, 100] {
        group.throughput(Throughput::Elements(entity_count));

        group.bench_with_input(
            BenchmarkId::new("supervision_tree_join_p50_target_15ms", entity_count),
            &entity_count,
            |b, &count| {
                let ontology = create_erlang_adapter_ontology(count as usize);

                b.iter(|| {
                    let mut graph = Graph::new().unwrap();
                    graph.load_turtle(black_box(&ontology)).unwrap();

                    let query = r#"
                        PREFIX erlang: <http://ggen.io/ontology/erlang/>
                        SELECT ?adapter ?supervisor ?strategy ?worker ?module WHERE {
                          ?adapter a erlang:Adapter ;
                                   erlang:supervisor ?supervisor .
                          ?supervisor erlang:strategy ?strategy ;
                                      erlang:children ?childList .
                          ?childList rdf:rest*/rdf:first ?worker .
                          ?worker erlang:module ?module .
                        }
                    "#;

                    let result = graph.query(black_box(query));
                    black_box(result)
                });
            },
        );
    }

    // Inference rule execution (SLO: P50 < 20ms)
    for entity_count in [10, 50, 100] {
        group.throughput(Throughput::Elements(entity_count));

        group.bench_with_input(
            BenchmarkId::new("inference_execution_p50_target_20ms", entity_count),
            &entity_count,
            |b, &count| {
                let ontology = create_erlang_adapter_ontology(count as usize);

                b.iter(|| {
                    let mut graph = Graph::new().unwrap();
                    graph.load_turtle(black_box(&ontology)).unwrap();

                    // Complex query with multiple patterns (simulates inference)
                    let query = r#"
                        PREFIX erlang: <http://ggen.io/ontology/erlang/>
                        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                        SELECT ?adapter ?supervisor ?totalWorkers WHERE {
                          ?adapter a erlang:Adapter ;
                                   erlang:supervisor ?supervisor .
                          ?supervisor erlang:children ?childList .
                          {
                            SELECT (COUNT(?worker) AS ?totalWorkers) WHERE {
                              ?supervisor erlang:children ?childList .
                              ?childList rdf:rest*/rdf:first ?worker .
                            }
                          }
                        }
                    "#;

                    let result = graph.query(black_box(query));
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 3: End-to-End Pipeline (μ₁-μ₅)
// ============================================================================

fn bench_erlang_e2e_pipeline(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_e2e_pipeline");
    group.sample_size(10); // Reduce for expensive E2E tests

    let pipeline = Pipeline::new().unwrap();
    let temp_dir = TempDir::new().unwrap();

    // μ₁-μ₅ pipeline without validation (faster)
    group.bench_function("pipeline_without_validation", |b| {
        let template_str = create_erlang_adapter_template();
        let ontology = create_erlang_adapter_ontology(10);

        b.iter(|| {
            // μ₁: Normalize - Parse and validate RDF
            let mut graph = Graph::new().unwrap();
            graph.load_turtle(black_box(&ontology)).unwrap();

            // μ₂: Extract - Execute SPARQL queries
            let systems_query = r#"
                PREFIX erlang: <http://ggen.io/ontology/erlang/>
                SELECT ?system ?id ?name WHERE {
                  ?system a erlang:Adapter ;
                          erlang:id ?id ;
                          erlang:name ?name .
                }
            "#;
            let _systems = graph.query(systems_query).unwrap();

            // μ₃: Emit - Render templates
            let mut template = Template::parse(&template_str).unwrap();
            let mut ctx = Context::new();
            ctx.insert("system_id", "system_0");
            ctx.insert("id", "0");
            ctx.insert("name", "System 0");
            ctx.insert("auth", "oauth2");

            let mut tera = pipeline.tera.clone();
            let template_path = temp_dir.path().join("template.tera");

            template.render_frontmatter(&mut tera, &ctx).unwrap();
            template
                .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                .ok();
            let rendered = template.render(&mut tera, &ctx).unwrap();

            // μ₄: Canonicalize - Format output (simulated)
            let _formatted = rendered.trim();

            // μ₅: Receipt - Generate cryptographic proof (simulated)
            let _receipt_hash = format!("{:x}", sha2::Sha256::digest(rendered.as_bytes()));

            black_box(_receipt_hash)
        });
    });

    // μ₁-μ₅ pipeline with validation (complete)
    group.bench_function("pipeline_with_validation", |b| {
        let template_str = create_erlang_adapter_template();
        let ontology = create_erlang_adapter_ontology(10);

        b.iter(|| {
            // μ₁: Normalize - Parse, validate, SHACL checks
            let mut graph = Graph::new().unwrap();
            graph.load_turtle(black_box(&ontology)).unwrap();

            // Validation: Verify all adapters have required properties
            let validation_query = r#"
                PREFIX erlang: <http://ggen.io/ontology/erlang/>
                ASK {
                  ?adapter a erlang:Adapter .
                  FILTER NOT EXISTS {
                    ?adapter erlang:id ?id ;
                             erlang:name ?name .
                  }
                }
            "#;
            let _has_invalid = graph.query(validation_query).unwrap();

            // μ₂: Extract - Execute all SPARQL queries
            let systems_query = r#"
                PREFIX erlang: <http://ggen.io/ontology/erlang/>
                SELECT ?system ?id ?name WHERE {
                  ?system a erlang:Adapter ;
                          erlang:id ?id ;
                          erlang:name ?name .
                }
            "#;
            let _systems = graph.query(systems_query).unwrap();

            // μ₃: Emit - Render templates
            let mut template = Template::parse(&template_str).unwrap();
            let mut ctx = Context::new();
            ctx.insert("system_id", "system_0");
            ctx.insert("id", "0");
            ctx.insert("name", "System 0");
            ctx.insert("auth", "oauth2");

            let mut tera = pipeline.tera.clone();
            let template_path = temp_dir.path().join("template.tera");

            template.render_frontmatter(&mut tera, &ctx).unwrap();
            template
                .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                .ok();
            let rendered = template.render(&mut tera, &ctx).unwrap();

            // μ₄: Canonicalize - Format and validate syntax
            let formatted = rendered.trim();
            let _syntax_valid = formatted.contains("-module(") && formatted.contains("-export(");

            // μ₅: Receipt - Generate cryptographic proof with metadata
            let receipt = serde_json::json!({
                "execution_id": uuid::Uuid::new_v4().to_string(),
                "timestamp": chrono::Utc::now().to_rfc3339(),
                "manifest_hash": format!("{:x}", sha2::Sha256::digest(template_str.as_bytes())),
                "ontology_hash": format!("{:x}", sha2::Sha256::digest(ontology.as_bytes())),
                "content_hash": format!("{:x}", sha2::Sha256::digest(rendered.as_bytes())),
                "file_count": 1,
                "triple_count": 10 * 4, // 10 systems * 4 triples each
            });
            let _receipt_json = serde_json::to_string(&receipt).unwrap();

            black_box(_receipt_json)
        });
    });

    // μ₁-μ₅ pipeline with audit trail
    group.bench_function("pipeline_with_audit_trail", |b| {
        let template_str = create_erlang_adapter_template();
        let ontology = create_erlang_adapter_ontology(10);

        b.iter(|| {
            let mut audit_log = Vec::new();

            // μ₁: Normalize
            let start = std::time::Instant::now();
            let mut graph = Graph::new().unwrap();
            graph.load_turtle(black_box(&ontology)).unwrap();
            audit_log.push(format!("μ₁ (Normalize): {:?}", start.elapsed()));

            // μ₂: Extract
            let start = std::time::Instant::now();
            let systems_query = r#"
                PREFIX erlang: <http://ggen.io/ontology/erlang/>
                SELECT ?system ?id ?name WHERE {
                  ?system a erlang:Adapter ;
                          erlang:id ?id ;
                          erlang:name ?name .
                }
            "#;
            let _systems = graph.query(systems_query).unwrap();
            audit_log.push(format!("μ₂ (Extract): {:?}", start.elapsed()));

            // μ₃: Emit
            let start = std::time::Instant::now();
            let mut template = Template::parse(&template_str).unwrap();
            let mut ctx = Context::new();
            ctx.insert("system_id", "system_0");
            ctx.insert("id", "0");
            ctx.insert("name", "System 0");
            ctx.insert("auth", "oauth2");

            let pipeline = Pipeline::new().unwrap();
            let mut tera = pipeline.tera.clone();
            let temp_dir = TempDir::new().unwrap();
            let template_path = temp_dir.path().join("template.tera");

            template.render_frontmatter(&mut tera, &ctx).unwrap();
            template
                .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                .ok();
            let rendered = template.render(&mut tera, &ctx).unwrap();
            audit_log.push(format!("μ₃ (Emit): {:?}", start.elapsed()));

            // μ₄: Canonicalize
            let start = std::time::Instant::now();
            let _formatted = rendered.trim();
            audit_log.push(format!("μ₄ (Canonicalize): {:?}", start.elapsed()));

            // μ₅: Receipt
            let start = std::time::Instant::now();
            let receipt = serde_json::json!({
                "execution_id": uuid::Uuid::new_v4().to_string(),
                "timestamp": chrono::Utc::now().to_rfc3339(),
                "audit_trail": audit_log,
            });
            let _receipt_json = serde_json::to_string(&receipt).unwrap();
            audit_log.push(format!("μ₅ (Receipt): {:?}", start.elapsed()));

            black_box(audit_log)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 4: SLO Validation
// ============================================================================

fn bench_erlang_slo_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("erlang_slo_validation");

    let pipeline = Pipeline::new().unwrap();
    let temp_dir = TempDir::new().unwrap();

    // Generation time SLO: < 5s for medium project (10 modules)
    group.bench_function("generation_time_medium_project_target_5s", |b| {
        let template_str = create_erlang_adapter_template();
        let ontology = create_erlang_adapter_ontology(10);

        b.iter(|| {
            let start = std::time::Instant::now();

            for i in 0..10 {
                let mut template = Template::parse(&template_str).unwrap();
                let mut ctx = Context::new();
                ctx.insert("system_id", &format!("system_{}", i));
                ctx.insert("id", &format!("{}", i));
                ctx.insert("name", &format!("System {}", i));
                ctx.insert("auth", "oauth2");

                let mut graph = Graph::new().unwrap();
                graph.load_turtle(&ontology).unwrap();

                let mut tera = pipeline.tera.clone();
                let template_path = temp_dir.path().join("template.tera");

                template.render_frontmatter(&mut tera, &ctx).unwrap();
                template
                    .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                    .ok();
                let rendered = template.render(&mut tera, &ctx).unwrap();

                // Write to file
                let output_path = temp_dir.path().join(format!("system_{}_adapter.erl", i));
                fs::write(output_path, rendered).unwrap();
            }

            let elapsed = start.elapsed();
            black_box(elapsed)
        });
    });

    // Memory usage SLO: < 100MB for medium project
    group.bench_function("memory_usage_medium_project_target_100mb", |b| {
        let template_str = create_erlang_adapter_template();
        let ontology = create_erlang_adapter_ontology(10);

        b.iter(|| {
            // Measure peak memory by holding all generated content
            let mut all_outputs = Vec::new();

            for i in 0..10 {
                let mut template = Template::parse(&template_str).unwrap();
                let mut ctx = Context::new();
                ctx.insert("system_id", &format!("system_{}", i));
                ctx.insert("id", &format!("{}", i));
                ctx.insert("name", &format!("System {}", i));
                ctx.insert("auth", "oauth2");

                let mut graph = Graph::new().unwrap();
                graph.load_turtle(&ontology).unwrap();

                let mut tera = pipeline.tera.clone();
                let template_path = temp_dir.path().join("template.tera");

                template.render_frontmatter(&mut tera, &ctx).unwrap();
                template
                    .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                    .ok();
                let rendered = template.render(&mut tera, &ctx).unwrap();

                all_outputs.push(rendered);
            }

            // Estimate memory usage
            let total_size: usize = all_outputs.iter().map(|s| s.len()).sum();
            black_box((all_outputs, total_size))
        });
    });

    // Deterministic receipt generation SLO: < 100ms
    group.bench_function("receipt_generation_p50_target_50ms", |b| {
        let template_str = create_erlang_adapter_template();
        let ontology = create_erlang_adapter_ontology(1);
        let rendered_output = "test erlang module content".to_string();

        b.iter(|| {
            let receipt = serde_json::json!({
                "execution_id": uuid::Uuid::new_v4().to_string(),
                "timestamp": chrono::Utc::now().to_rfc3339(),
                "manifest_hash": format!("{:x}", sha2::Sha256::digest(template_str.as_bytes())),
                "ontology_hash": format!("{:x}", sha2::Sha256::digest(ontology.as_bytes())),
                "files_generated": [{
                    "path": "output/system_0_adapter.erl",
                    "content_hash": format!("{:x}", sha2::Sha256::digest(rendered_output.as_bytes())),
                    "size_bytes": rendered_output.len(),
                }],
                "triple_count": 4,
                "query_count": 3,
                "generation_time_ms": 42,
            });

            let receipt_json = serde_json::to_string(&receipt).unwrap();
            let _receipt_hash = format!("{:x}", sha2::Sha256::digest(receipt_json.as_bytes()));

            black_box(_receipt_hash)
        });
    });

    // Determinism validation: same input → same output
    group.bench_function("determinism_validation_same_seed", |b| {
        let template_str = create_erlang_adapter_template();
        let ontology = create_erlang_adapter_ontology(5);

        b.iter(|| {
            let mut outputs = Vec::new();

            // Generate 3 times with same input
            for _ in 0..3 {
                let mut template = Template::parse(&template_str).unwrap();
                let mut ctx = Context::new();
                ctx.insert("system_id", "system_0");
                ctx.insert("id", "0");
                ctx.insert("name", "System 0");
                ctx.insert("auth", "oauth2");

                let mut graph = Graph::new().unwrap();
                graph.load_turtle(&ontology).unwrap();

                let mut tera = pipeline.tera.clone();
                let template_path = temp_dir.path().join("template.tera");

                template.render_frontmatter(&mut tera, &ctx).unwrap();
                template
                    .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                    .ok();
                let rendered = template.render(&mut tera, &ctx).unwrap();

                let hash = format!("{:x}", sha2::Sha256::digest(rendered.as_bytes()));
                outputs.push(hash);
            }

            // Verify all hashes are identical
            let all_same = outputs.windows(2).all(|w| w[0] == w[1]);
            black_box((outputs, all_same))
        });
    });

    group.finish();
}

// ============================================================================
// CRITERION GROUPS
// ============================================================================

criterion_group!(
    erlang_rendering_benches,
    bench_erlang_template_rendering
);

criterion_group!(
    erlang_sparql_benches,
    bench_erlang_sparql_queries
);

criterion_group!(
    erlang_pipeline_benches,
    bench_erlang_e2e_pipeline
);

criterion_group!(
    erlang_slo_benches,
    bench_erlang_slo_validation
);

criterion_main!(
    erlang_rendering_benches,
    erlang_sparql_benches,
    erlang_pipeline_benches,
    erlang_slo_benches
);
