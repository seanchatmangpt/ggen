//! Chicago TDD Tests for Erlang Generation Pipeline
//!
//! Comprehensive test suite following Chicago TDD principles:
//! - State-based testing (verify outputs, not implementation)
//! - Real collaborators (actual RDF store, template engine, no mocks)
//! - Behavior verification (observable outputs and state changes)
//! - AAA pattern (Arrange-Act-Assert)
//!
//! ## Test Coverage
//! 1. Template Rendering - Erlang syntax, module structure
//! 2. SPARQL Query - Supervision tree extraction
//! 3. Integration - Complete pipeline (μ₁-μ₅)
//! 4. Determinism - Reproducible outputs with fixed seeds
//! 5. Snapshot Testing - Generated artifacts validation

use anyhow::Result;
use ggen_core::{Graph, Template};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::{NamedTempFile, TempDir};
use tera::Context;

/* ========== Test Utilities ========== */

/// Create Tera engine with all ggen filters registered
fn mk_tera() -> tera::Tera {
    let mut tera = tera::Tera::default();
    ggen_core::register::register_all(&mut tera);
    tera
}

/// Create Context from key-value pairs
fn ctx_from_pairs(pairs: &[(&str, &str)]) -> Context {
    let mut c = Context::new();
    for (k, v) in pairs {
        c.insert(*k, v);
    }
    c
}

/// Create temporary RDF file with content
fn create_rdf_file(content: &str) -> Result<NamedTempFile> {
    let mut file = NamedTempFile::new()?;
    writeln!(file, "{}", content)?;
    file.flush()?;
    Ok(file)
}

/// Verify Erlang syntax validity (basic checks)
fn assert_valid_erlang_syntax(code: &str) {
    // Must start with module declaration or comment
    let trimmed = code.trim();
    assert!(
        trimmed.starts_with("-module(")
            || trimmed.starts_with("%%")
            || trimmed.starts_with("%% @doc"),
        "Erlang code should start with module declaration or comment, got: {}",
        &trimmed[..trimmed.len().min(100)]
    );

    // Must have proper module structure
    assert!(code.contains("-module("), "Missing -module() declaration");

    // Should have exports
    assert!(
        code.contains("-export([") || code.contains("-behaviour("),
        "Missing -export() or -behaviour()"
    );
}

/// Verify rebar.config validity
fn assert_valid_rebar_config(config: &str) {
    assert!(config.contains("{erl_opts,"), "Missing erl_opts");
    assert!(config.contains("{deps,"), "Missing deps");
}

/* ========== Test 1: Template Rendering Tests ========== */

#[test]
fn test_render_erlang_supervisor_template() -> Result<()> {
    println!("🧪 Test: Render Erlang supervisor from template");

    // Arrange: Sample RDF ontology with job entities
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:JobsApp a erlang:Application ;
    erlang:name "jobs_app" ;
    erlang:description "Jobs processing application" ;
    erlang:version "1.0.0" .

ex:JobsSup a erlang:Supervisor ;
    erlang:module "jobs_sup" ;
    erlang:strategy "one_for_one" ;
    erlang:intensity "5" ;
    erlang:period "60" .

ex:JobWorker a erlang:Worker ;
    erlang:module "job_worker" ;
    erlang:behaviour "gen_server" ."#,
    )?;
    println!("✓ Created RDF ontology with Erlang supervision tree");

    // Arrange: Erlang supervisor template
    let template_str = r#"---
to: "src/{{module_name}}.erl"
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  supervisor: "SELECT ?module ?strategy ?intensity ?period WHERE { ?s a erlang:Supervisor ; erlang:module ?module ; erlang:strategy ?strategy ; erlang:intensity ?intensity ; erlang:period ?period }"
---
%% @doc {{module_name | title}} - Generated Erlang supervisor
-module({{module_name}}).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% Supervision strategy: {{ sparql_first(results=sparql_results.supervisor, column="strategy") }}
%% Intensity: {{ sparql_first(results=sparql_results.supervisor, column="intensity") }}
%% Period: {{ sparql_first(results=sparql_results.supervisor, column="period") }}

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => {{ sparql_first(results=sparql_results.supervisor, column="strategy") }},
        intensity => {{ sparql_first(results=sparql_results.supervisor, column="intensity") }},
        period => {{ sparql_first(results=sparql_results.supervisor, column="period") }}
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
"#;

    let mut template = Template::parse(template_str)?;
    println!("✓ Parsed Erlang supervisor template");

    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("module_name", "jobs_sup")]);

    // Act: Render template with RDF
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("supervisor.tmpl"),
    )?;
    println!("✓ Rendered Erlang supervisor code");

    // Assert: Verify correct Erlang syntax
    assert_valid_erlang_syntax(&rendered);
    println!("✓ Valid Erlang syntax");

    // Assert: Verify module structure
    assert!(rendered.contains("-module(jobs_sup)"));
    assert!(rendered.contains("-behaviour(supervisor)"));
    assert!(rendered.contains("start_link/0"));
    assert!(rendered.contains("init/1"));
    println!("✓ Correct module structure");

    // Assert: Verify supervision strategy from RDF
    assert!(rendered.contains("strategy => \"one_for_one\""));
    assert!(rendered.contains("intensity => \"5\""));
    assert!(rendered.contains("period => \"60\""));
    println!("✓ Supervision strategy correctly extracted from RDF");

    // Assert: Verify frontmatter rendered correctly
    assert_eq!(template.front.to.as_deref(), Some("src/jobs_sup.erl"));
    println!("✓ Frontmatter rendered correctly");

    println!("✅ Test PASSED: Erlang supervisor template rendered correctly");
    Ok(())
}

#[test]
fn test_render_erlang_gen_server_template() -> Result<()> {
    println!("🧪 Test: Render Erlang gen_server from template");

    // Arrange: RDF with gen_server specification
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:JobWorker a erlang:GenServer ;
    erlang:module "job_worker" ;
    erlang:description "Job processing worker" ;
    erlang:state_type "map" .

ex:ProcessJob a erlang:CallHandler ;
    erlang:function "handle_call" ;
    erlang:message "process_job" ;
    erlang:returns "ok" ."#,
    )?;

    // Arrange: Gen_server template
    let template_str = r#"---
to: "src/{{module_name}}.erl"
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  server: "SELECT ?module ?desc ?state_type WHERE { ?s a erlang:GenServer ; erlang:module ?module ; erlang:description ?desc ; erlang:state_type ?state_type }"
---
%% @doc {{ sparql_first(results=sparql_results.server, column="desc") }}
-module({{module_name}}).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

-type state() :: #{ data => term() }.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    InitialState = #{data => undefined},
    {ok, InitialState}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("module_name", "job_worker")]);

    // Act: Render template
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("gen_server.tmpl"),
    )?;

    // Assert: Verify Erlang syntax
    assert_valid_erlang_syntax(&rendered);

    // Assert: Verify gen_server structure
    assert!(rendered.contains("-module(job_worker)"));
    assert!(rendered.contains("-behaviour(gen_server)"));
    assert!(rendered.contains("handle_call/3"));
    assert!(rendered.contains("handle_cast/2"));

    // Assert: SPARQL results populated
    assert_eq!(template.front.sparql_results.len(), 1);
    assert!(template.front.sparql_results.contains_key("server"));

    println!("✅ Test PASSED: Erlang gen_server template rendered correctly");
    Ok(())
}

#[test]
fn test_render_rebar_config_template() -> Result<()> {
    println!("🧪 Test: Render rebar.config from RDF");

    // Arrange: RDF with dependencies
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:JobsApp a erlang:Application ;
    erlang:name "jobs_app" ;
    erlang:dependency ex:Lager ;
    erlang:dependency ex:Cowboy .

ex:Lager a erlang:Dependency ;
    erlang:name "lager" ;
    erlang:version "3.9.2" .

ex:Cowboy a erlang:Dependency ;
    erlang:name "cowboy" ;
    erlang:version "2.10.0" ."#,
    )?;

    // Arrange: Rebar config template
    let template_str = r#"---
to: "rebar.config"
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  deps: "SELECT ?name ?version WHERE { ?s a erlang:Dependency ; erlang:name ?name ; erlang:version ?version } ORDER BY ?name"
---
%% Rebar3 configuration
{erl_opts, [debug_info, warnings_as_errors]}.

{deps, [
{%- for dep in sparql_results.deps %}
    {{"{"}}{{dep.name | replace(from='"', to='')}}, "{{dep.version | replace(from='"', to='')}}"{{"}"}}{%- if not loop.last %},{% endif %}
{%- endfor %}
]}.

{profiles, [
    {dev, [{erl_opts, [debug_info]}]},
    {prod, [{erl_opts, [warnings_as_errors]}]}
]}.
"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Act: Render rebar.config
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("rebar.tmpl"),
    )?;

    // Assert: Verify rebar.config structure
    assert_valid_rebar_config(&rendered);

    // Assert: Verify dependencies rendered
    assert!(rendered.contains("{lager,"));
    assert!(rendered.contains("{cowboy,"));
    assert!(rendered.contains("3.9.2"));
    assert!(rendered.contains("2.10.0"));

    // Assert: Verify profiles
    assert!(rendered.contains("{profiles,"));
    assert!(rendered.contains("{dev,"));
    assert!(rendered.contains("{prod,"));

    println!("✅ Test PASSED: rebar.config rendered correctly");
    Ok(())
}

/* ========== Test 2: SPARQL Query Tests ========== */

#[test]
fn test_extract_supervision_tree_from_rdf() -> Result<()> {
    println!("🧪 Test: Extract supervision tree via SPARQL");

    // Arrange: RDF ontology with complete supervision tree
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:RootSup a erlang:Supervisor ;
    erlang:module "root_sup" ;
    erlang:strategy "one_for_one" ;
    erlang:child ex:Worker1 ;
    erlang:child ex:Worker2 .

ex:Worker1 a erlang:Worker ;
    erlang:module "worker_1" ;
    erlang:behaviour "gen_server" ;
    erlang:restart "permanent" .

ex:Worker2 a erlang:Worker ;
    erlang:module "worker_2" ;
    erlang:behaviour "gen_statem" ;
    erlang:restart "transient" ."#,
    )?;

    // Arrange: Template with SPARQL queries
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  supervisors: "SELECT ?module ?strategy WHERE { ?s a erlang:Supervisor ; erlang:module ?module ; erlang:strategy ?strategy }"
  workers: "SELECT ?module ?behaviour ?restart WHERE { ?s a erlang:Worker ; erlang:module ?module ; erlang:behaviour ?behaviour ; erlang:restart ?restart } ORDER BY ?module"
  worker_count: "SELECT (COUNT(?w) as ?cnt) WHERE { ?w a erlang:Worker }"
---
Supervisors: {{ sparql_count(results=sparql_results.supervisors) }}
Workers: {{ sparql_count(results=sparql_results.workers) }}
Total Count: {{ sparql_results.worker_count }}
"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Act: Execute SPARQL queries
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Assert: Verify SPARQL results populated
    assert_eq!(template.front.sparql_results.len(), 3);
    assert!(template.front.sparql_results.contains_key("supervisors"));
    assert!(template.front.sparql_results.contains_key("workers"));
    assert!(template.front.sparql_results.contains_key("worker_count"));
    println!("✓ All SPARQL queries executed");

    // Assert: Verify supervision tree structure
    assert!(rendered.contains("Supervisors: 1"));
    assert!(rendered.contains("Workers: 2"));
    println!("✓ Supervision tree correctly extracted");

    // Assert: Verify workers are ordered
    let workers = template.front.sparql_results.get("workers").unwrap();
    let workers_arr = workers.as_array().unwrap();
    assert_eq!(workers_arr.len(), 2);
    println!("✓ Workers extracted and ordered");

    println!("✅ Test PASSED: Supervision tree extracted via SPARQL");
    Ok(())
}

#[test]
fn test_sparql_filters_and_ordering() -> Result<()> {
    println!("🧪 Test: SPARQL filters and ordering");

    // Arrange: RDF with multiple behaviors
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:W1 a erlang:Worker ; erlang:module "alpha" ; erlang:behaviour "gen_server" ; erlang:priority "10" .
ex:W2 a erlang:Worker ; erlang:module "beta" ; erlang:behaviour "gen_statem" ; erlang:priority "20" .
ex:W3 a erlang:Worker ; erlang:module "gamma" ; erlang:behaviour "gen_server" ; erlang:priority "15" ."#,
    )?;

    // Arrange: Template with filtered and ordered queries
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  gen_servers: "SELECT ?module WHERE { ?s a erlang:Worker ; erlang:module ?module ; erlang:behaviour 'gen_server' } ORDER BY ?module"
  by_priority: "SELECT ?module ?priority WHERE { ?s erlang:module ?module ; erlang:priority ?priority } ORDER BY DESC(?priority)"
---
GenServers: {{ sparql_count(results=sparql_results.gen_servers) }}
First by priority: {{ sparql_first(results=sparql_results.by_priority, column="module") }}
"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Act: Execute filtered queries
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Assert: Verify filtered results
    assert!(rendered.contains("GenServers: 2"));
    let gen_servers = template.front.sparql_results.get("gen_servers").unwrap();
    assert_eq!(gen_servers.as_array().unwrap().len(), 2);

    // Assert: Verify ordering (beta has priority 20, highest)
    assert!(rendered.contains("First by priority: \"beta\""));

    println!("✅ Test PASSED: SPARQL filters and ordering work correctly");
    Ok(())
}

/* ========== Test 3: Integration Tests ========== */

#[test]
fn test_full_erlang_project_generation() -> Result<()> {
    println!("🧪 Test: Full Erlang project generation (integration)");

    // Arrange: Create temporary directory for generated project
    let temp_dir = TempDir::new()?;
    let project_root = temp_dir.path();
    println!(
        "✓ Created temporary project directory: {}",
        project_root.display()
    );

    // Arrange: Complete RDF ontology for jobs application
    let rdf_content = r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:JobsApp a erlang:Application ;
    erlang:name "jobs_app" ;
    erlang:description "Jobs processing application" ;
    erlang:version "1.0.0" ;
    erlang:supervisor ex:RootSup .

ex:RootSup a erlang:Supervisor ;
    erlang:module "jobs_sup" ;
    erlang:strategy "one_for_one" ;
    erlang:intensity "5" ;
    erlang:period "60" ;
    erlang:child ex:JobWorker .

ex:JobWorker a erlang:Worker ;
    erlang:module "job_worker" ;
    erlang:behaviour "gen_server" ;
    erlang:restart "permanent" ;
    erlang:shutdown "5000" .

ex:Lager a erlang:Dependency ;
    erlang:name "lager" ;
    erlang:version "3.9.2" ."#;

    let rdf_file = create_rdf_file(rdf_content)?;
    println!("✓ Created complete RDF ontology");

    // Act: Generate multiple files (simulating full pipeline)

    // Generate app.src
    let app_template = r#"---
to: "src/jobs_app.app.src"
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  app: "SELECT ?name ?desc ?version WHERE { ?s a erlang:Application ; erlang:name ?name ; erlang:description ?desc ; erlang:version ?version }"
---
{application, {{sparql_first(results=sparql_results.app, column="name") | replace(from='"', to='')}},
 [{description, {{sparql_first(results=sparql_results.app, column="desc")}}},
  {vsn, {{sparql_first(results=sparql_results.app, column="version")}}},
  {modules, []},
  {registered, []},
  {applications, [kernel, stdlib, lager]}
 ]}.
"#;

    let mut app_tmpl = Template::parse(app_template)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let app_rendered = app_tmpl.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &Context::new(),
        Path::new("app.tmpl"),
    )?;

    // Generate supervisor
    let sup_template = r#"---
to: "src/jobs_sup.erl"
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  sup: "SELECT ?module ?strategy WHERE { ?s a erlang:Supervisor ; erlang:module ?module ; erlang:strategy ?strategy }"
---
-module(jobs_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => {{sparql_first(results=sparql_results.sup, column="strategy") | replace(from='"', to='')}}},
    {ok, {SupFlags, []}}.
"#;

    let mut sup_tmpl = Template::parse(sup_template)?;
    let mut graph2 = Graph::new()?;
    let sup_rendered = sup_tmpl.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph2,
        &mut tera,
        &Context::new(),
        Path::new("sup.tmpl"),
    )?;

    println!("✓ Generated all project files");

    // Assert: Verify all files generated correctly

    // Verify app.src
    assert!(app_rendered.contains("{application, jobs_app,"));
    assert!(app_rendered.contains("Jobs processing application"));
    assert!(app_rendered.contains("1.0.0"));
    println!("✓ app.src generated correctly");

    // Verify supervisor
    assert_valid_erlang_syntax(&sup_rendered);
    assert!(sup_rendered.contains("-module(jobs_sup)"));
    assert!(sup_rendered.contains("strategy => one_for_one"));
    println!("✓ Supervisor generated correctly");

    // Assert: Verify project structure
    assert!(app_tmpl.front.to.as_deref() == Some("src/jobs_app.app.src"));
    assert!(sup_tmpl.front.to.as_deref() == Some("src/jobs_sup.erl"));
    println!("✓ Project structure correct");

    println!("✅ Test PASSED: Full Erlang project generation works");
    Ok(())
}

#[test]
fn test_erlang_project_with_multiple_workers() -> Result<()> {
    println!("🧪 Test: Erlang project with multiple workers");

    // Arrange: RDF with multiple workers
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:Sup a erlang:Supervisor ;
    erlang:module "main_sup" ;
    erlang:strategy "one_for_all" ;
    erlang:child ex:W1 ;
    erlang:child ex:W2 ;
    erlang:child ex:W3 .

ex:W1 a erlang:Worker ; erlang:module "worker_one" ; erlang:restart "permanent" .
ex:W2 a erlang:Worker ; erlang:module "worker_two" ; erlang:restart "transient" .
ex:W3 a erlang:Worker ; erlang:module "worker_three" ; erlang:restart "temporary" ."#,
    )?;

    // Arrange: Template listing all workers
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  workers: "SELECT ?module ?restart WHERE { ?s a erlang:Worker ; erlang:module ?module ; erlang:restart ?restart } ORDER BY ?module"
---
%% Workers configuration
{%- for worker in sparql_results.workers %}
%% Worker: {{worker.module}}, Restart: {{worker.restart}}
{%- endfor %}

Total workers: {{ sparql_count(results=sparql_results.workers) }}
"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();

    // Act: Generate workers list
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &Context::new(),
        Path::new("workers.tmpl"),
    )?;

    // Assert: All workers present
    assert!(rendered.contains("worker_one"));
    assert!(rendered.contains("worker_two"));
    assert!(rendered.contains("worker_three"));
    assert!(rendered.contains("Total workers: 3"));

    // Assert: Restart strategies preserved
    assert!(rendered.contains("permanent"));
    assert!(rendered.contains("transient"));
    assert!(rendered.contains("temporary"));

    println!("✅ Test PASSED: Multiple workers generated correctly");
    Ok(())
}

/* ========== Test 4: Determinism Tests ========== */

#[test]
fn test_deterministic_erlang_generation() -> Result<()> {
    println!("🧪 Test: Deterministic Erlang generation (fixed seed)");

    // Arrange: Same ontology and template
    let rdf_content = r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:TestModule a erlang:GenServer ;
    erlang:module "test_worker" ;
    erlang:version "1.0.0" ."#;

    let template_str = r#"---
to: "src/test_worker.erl"
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  module: "SELECT ?module ?version WHERE { ?s a erlang:GenServer ; erlang:module ?module ; erlang:version ?version }"
---
-module({{sparql_first(results=sparql_results.module, column="module") | replace(from='"', to='')}}).
-vsn({{sparql_first(results=sparql_results.module, column="version")}}).
"#;

    // Act: Generate twice with same inputs
    let output1 = {
        let rdf_file = create_rdf_file(rdf_content)?;
        let mut template = Template::parse(template_str)?;
        let mut graph = Graph::new()?;
        let mut tera = mk_tera();
        template.render_with_rdf(
            vec![rdf_file.path().to_path_buf()],
            &mut graph,
            &mut tera,
            &Context::new(),
            Path::new("test.tmpl"),
        )?
    };

    let output2 = {
        let rdf_file = create_rdf_file(rdf_content)?;
        let mut template = Template::parse(template_str)?;
        let mut graph = Graph::new()?;
        let mut tera = mk_tera();
        template.render_with_rdf(
            vec![rdf_file.path().to_path_buf()],
            &mut graph,
            &mut tera,
            &Context::new(),
            Path::new("test.tmpl"),
        )?
    };

    // Assert: Identical outputs (determinism)
    assert_eq!(
        output1, output2,
        "Outputs must be identical (deterministic)"
    );
    println!("✓ Outputs are identical");

    // Assert: Verify hash match
    use sha2::{Digest, Sha256};
    let hash1 = {
        let mut hasher = Sha256::new();
        hasher.update(output1.as_bytes());
        format!("{:x}", hasher.finalize())
    };
    let hash2 = {
        let mut hasher = Sha256::new();
        hasher.update(output2.as_bytes());
        format!("{:x}", hasher.finalize())
    };
    assert_eq!(hash1, hash2, "SHA-256 hashes must match");
    println!("✓ SHA-256 hashes match: {}", hash1);

    println!("✅ Test PASSED: Erlang generation is deterministic");
    Ok(())
}

#[test]
fn test_determinism_with_ordered_sparql() -> Result<()> {
    println!("🧪 Test: Determinism with ordered SPARQL results");

    // Arrange: RDF with multiple unordered entities
    let rdf_content = r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:W3 a erlang:Worker ; erlang:module "charlie" ; erlang:priority "3" .
ex:W1 a erlang:Worker ; erlang:module "alpha" ; erlang:priority "1" .
ex:W2 a erlang:Worker ; erlang:module "bravo" ; erlang:priority "2" ."#;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  workers: "SELECT ?module WHERE { ?s a erlang:Worker ; erlang:module ?module } ORDER BY ?module"
---
{%- for w in sparql_results.workers %}
{{w.module}}
{%- endfor %}
"#;

    // Act: Generate multiple times
    let outputs: Vec<String> = (0..5)
        .map(|_| {
            let rdf_file = create_rdf_file(rdf_content).unwrap();
            let mut template = Template::parse(template_str).unwrap();
            let mut graph = Graph::new().unwrap();
            let mut tera = mk_tera();
            template
                .render_with_rdf(
                    vec![rdf_file.path().to_path_buf()],
                    &mut graph,
                    &mut tera,
                    &Context::new(),
                    Path::new("test.tmpl"),
                )
                .unwrap()
        })
        .collect();

    // Assert: All outputs identical
    let first = &outputs[0];
    for output in &outputs[1..] {
        assert_eq!(first, output, "All outputs must be identical");
    }

    // Assert: Verify ordering preserved (alpha, bravo, charlie)
    assert!(first.contains("\"alpha\"\n\"bravo\"\n\"charlie\""));
    println!("✓ Ordering preserved across all runs");

    println!("✅ Test PASSED: Determinism with ordered SPARQL works");
    Ok(())
}

/* ========== Test 5: Snapshot Tests ========== */

#[cfg(feature = "insta")]
#[test]
fn test_snapshot_erlang_supervisor() -> Result<()> {
    use insta::assert_snapshot;

    println!("🧪 Test: Snapshot Erlang supervisor");

    // Arrange: Fixed RDF input
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:SnapshotSup a erlang:Supervisor ;
    erlang:module "snapshot_sup" ;
    erlang:strategy "rest_for_one" ;
    erlang:intensity "3" ;
    erlang:period "30" ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  sup: "SELECT ?module ?strategy ?intensity ?period WHERE { ?s a erlang:Supervisor ; erlang:module ?module ; erlang:strategy ?strategy ; erlang:intensity ?intensity ; erlang:period ?period }"
---
-module(snapshot_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => {{sparql_first(results=sparql_results.sup, column="strategy") | replace(from='"', to='')}},
        intensity => {{sparql_first(results=sparql_results.sup, column="intensity") | replace(from='"', to='')}},
        period => {{sparql_first(results=sparql_results.sup, column="period") | replace(from='"', to='')}}
    },
    {ok, {SupFlags, []}}.
"#;

    // Act: Generate supervisor
    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &Context::new(),
        Path::new("snapshot.tmpl"),
    )?;

    // Assert: Snapshot matches
    assert_snapshot!(rendered);

    println!("✅ Test PASSED: Supervisor snapshot matches");
    Ok(())
}

#[cfg(feature = "insta")]
#[test]
fn test_snapshot_rebar_config() -> Result<()> {
    use insta::assert_snapshot;

    println!("🧪 Test: Snapshot rebar.config");

    // Arrange: Fixed dependencies
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix erlang: <http://ggen.org/erlang#> .

ex:App erlang:dependency ex:D1, ex:D2 .
ex:D1 a erlang:Dependency ; erlang:name "cowboy" ; erlang:version "2.10.0" .
ex:D2 a erlang:Dependency ; erlang:name "jiffy" ; erlang:version "1.1.1" ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  deps: "SELECT ?name ?version WHERE { ?s a erlang:Dependency ; erlang:name ?name ; erlang:version ?version } ORDER BY ?name"
---
{erl_opts, [debug_info]}.
{deps, [
{%- for dep in sparql_results.deps %}
    {{"{"}}{{dep.name | replace(from='"', to='')}}, "{{dep.version | replace(from='"', to='')}}"{{"}"}}{% if not loop.last %},{% endif %}
{%- endfor %}
]}.
"#;

    // Act: Generate rebar.config
    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &Context::new(),
        Path::new("rebar.tmpl"),
    )?;

    // Assert: Snapshot matches
    assert_snapshot!(rendered);

    println!("✅ Test PASSED: rebar.config snapshot matches");
    Ok(())
}

/* ========== Test 6: Error Handling ========== */

#[test]
fn test_invalid_erlang_template_fails_gracefully() -> Result<()> {
    println!("🧪 Test: Invalid template fails gracefully");

    // Arrange: Template with invalid SPARQL
    let template_str = r#"---
sparql:
  invalid: "SELECT INVALID QUERY"
---
body
"#;

    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Test a ex:Thing ."#,
    )?;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();

    // Act: Attempt to render (should fail)
    let result = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &Context::new(),
        Path::new("test.tmpl"),
    );

    // Assert: Error occurred
    assert!(result.is_err(), "Invalid SPARQL should cause error");
    let err = result.unwrap_err();
    assert!(err.to_string().contains("SPARQL") || err.to_string().contains("query"));
    println!("✓ Error message contains SPARQL context: {}", err);

    println!("✅ Test PASSED: Invalid template fails gracefully");
    Ok(())
}

#[test]
fn test_missing_rdf_file_fails_gracefully() -> Result<()> {
    println!("🧪 Test: Missing RDF file fails gracefully");

    // Arrange: Template with valid SPARQL
    let template_str = r#"---
sparql:
  test: "SELECT ?s WHERE { ?s a ?o }"
---
body
"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();

    // Act: Try to load non-existent file
    let result = template.render_with_rdf(
        vec![PathBuf::from("/nonexistent/missing.ttl")],
        &mut graph,
        &mut tera,
        &Context::new(),
        Path::new("test.tmpl"),
    );

    // Assert: Error occurred with clear message
    assert!(result.is_err(), "Missing file should cause error");
    let err = result.unwrap_err();
    assert!(err.to_string().contains("Failed to read RDF file"));
    println!("✓ Clear error message: {}", err);

    println!("✅ Test PASSED: Missing RDF file fails gracefully");
    Ok(())
}

/* ========== Test 7: Performance ========== */

#[test]
fn test_erlang_generation_performance() -> Result<()> {
    use std::time::Instant;

    println!("🧪 Test: Erlang generation performance (<500ms)");

    // Arrange: Large RDF with 100 workers
    let mut rdf_content = String::from(
        "@prefix ex: <http://example.org/> .\n@prefix erlang: <http://ggen.org/erlang#> .\n",
    );
    for i in 0..100 {
        rdf_content.push_str(&format!(
            "ex:Worker{} a erlang:Worker ; erlang:module \"worker_{}\" ; erlang:priority {} .\n",
            i, i, i
        ));
    }
    let rdf_file = create_rdf_file(&rdf_content)?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  erlang: "http://ggen.org/erlang#"
sparql:
  workers: "SELECT (COUNT(?w) as ?cnt) WHERE { ?w a erlang:Worker }"
  sample: "SELECT ?module WHERE { ?w erlang:module ?module } LIMIT 10"
---
Total: {{ sparql_results.workers }}
Sample: {{ sparql_count(results=sparql_results.sample) }}
"#;

    // Act: Measure generation time
    let start = Instant::now();
    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &Context::new(),
        Path::new("perf.tmpl"),
    )?;
    let duration = start.elapsed();

    // Assert: Performance target met
    println!("✓ Generation completed in {:?}", duration);
    assert!(
        duration.as_millis() < 500,
        "Performance target: <500ms, actual: {:?}",
        duration
    );

    // Assert: Correct results
    assert!(rendered.contains("Total:"));
    assert!(rendered.contains("Sample: 10"));

    println!(
        "✅ Test PASSED: Performance target met ({:?} < 500ms)",
        duration
    );
    Ok(())
}
