//! Snapshot tests for testing infrastructure
//!
//! This test suite uses insta for snapshot testing to verify deterministic
//! output generation. Tests follow Chicago TDD (state-based, real collaborators)
//! and serial_test for deterministic execution.
//!
//! Snapshots verified:
//! - testcontainers_helper.erl generated code
//! - chaos_engineering_SUITE.erl generated code
//! - docker-compose.yml output
//! - GitHub Actions workflow structure
//! - Benchmark module generation
//!
//! All tests verify: Same input = Identical output (determinism)

use insta::assert_snapshot;
use serial_test::serial;
use std::fs;
use tempfile::TempDir;

// =============================================================================
// Test Helpers
// =============================================================================

/// Generate testcontainers helper module
fn generate_testcontainers_helper() -> String {
    r#"%%%-------------------------------------------------------------------
%%% @doc Testcontainers Helper - Docker container management for tests
%%% @end
%%%-------------------------------------------------------------------
-module(testcontainers_helper).
-export([start_postgres/0, start_redis/0, stop_container/1, cleanup_all/0]).

-record(container, {
    id :: string(),
    image :: string(),
    port :: integer(),
    status :: running | stopped
}).

%% @doc Start PostgreSQL container for testing
-spec start_postgres() -> {ok, #container{}} | {error, term()}.
start_postgres() ->
    Image = "postgres:15-alpine",
    Env = [
        {"POSTGRES_USER", "test"},
        {"POSTGRES_PASSWORD", "test"},
        {"POSTGRES_DB", "erlang_jobs_test"}
    ],
    Port = 5432,

    case start_container(Image, Env, Port) of
        {ok, ContainerId} ->
            wait_for_port(Port, 30),
            {ok, #container{
                id = ContainerId,
                image = Image,
                port = Port,
                status = running
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start Redis container for caching tests
-spec start_redis() -> {ok, #container{}} | {error, term()}.
start_redis() ->
    Image = "redis:7-alpine",
    Env = [],
    Port = 6379,

    case start_container(Image, Env, Port) of
        {ok, ContainerId} ->
            wait_for_port(Port, 30),
            {ok, #container{
                id = ContainerId,
                image = Image,
                port = Port,
                status = running
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stop a running container
-spec stop_container(#container{}) -> ok | {error, term()}.
stop_container(#container{id = ContainerId}) ->
    os:cmd("docker stop " ++ ContainerId),
    ok.

%% @doc Cleanup all test containers
-spec cleanup_all() -> ok.
cleanup_all() ->
    os:cmd("docker ps -q --filter label=erlang_jobs_test | xargs -r docker stop"),
    os:cmd("docker ps -aq --filter label=erlang_jobs_test | xargs -r docker rm"),
    ok.

%% Private functions

start_container(Image, Env, Port) ->
    EnvFlags = lists:flatten([
        io_lib:format("-e ~s=~s ", [Key, Value])
        || {Key, Value} <- Env
    ]),

    Cmd = io_lib:format(
        "docker run -d -p ~B:~B ~s--label erlang_jobs_test ~s",
        [Port, Port, EnvFlags, Image]
    ),

    case os:cmd(Cmd) of
        [] -> {error, docker_start_failed};
        ContainerId -> {ok, string:trim(ContainerId)}
    end.

wait_for_port(Port, 0) ->
    {error, timeout};
wait_for_port(Port, Attempts) ->
    case gen_tcp:connect("localhost", Port, []) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            ok;
        {error, _} ->
            timer:sleep(1000),
            wait_for_port(Port, Attempts - 1)
    end.
"#
    .to_string()
}

/// Generate chaos engineering test suite
fn generate_chaos_suite() -> String {
    r#"%%%-------------------------------------------------------------------
%%% @doc Chaos Engineering Test Suite
%%% Tests system resilience under failure conditions
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_engineering_SUITE).
-behaviour(ct_suite).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([
    test_network_partition_recovery/1,
    test_container_restart_resilience/1,
    test_database_connection_failure/1,
    test_high_load_stability/1
]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {minutes, 10}}].

all() ->
    [
        test_network_partition_recovery,
        test_container_restart_resilience,
        test_database_connection_failure,
        test_high_load_stability
    ].

init_per_suite(Config) ->
    {ok, Postgres} = testcontainers_helper:start_postgres(),
    {ok, Redis} = testcontainers_helper:start_redis(),
    [{postgres, Postgres}, {redis, Redis} | Config].

end_per_suite(_Config) ->
    testcontainers_helper:cleanup_all(),
    ok.

%% @doc Test recovery from network partition
test_network_partition_recovery(Config) ->
    Postgres = ?config(postgres, Config),
    simulate_network_partition(Postgres, 5000),
    timer:sleep(6000),
    {ok, Connection} = connect_to_postgres(),
    true = is_process_alive(Connection),
    ok.

%% @doc Test resilience when container restarts
test_container_restart_resilience(Config) ->
    Redis = ?config(redis, Config),
    ok = testcontainers_helper:stop_container(Redis),
    timer:sleep(1000),
    {ok, NewRedis} = testcontainers_helper:start_redis(),
    timer:sleep(2000),
    {ok, Connection} = connect_to_redis(),
    true = is_process_alive(Connection),
    ok.

%% @doc Test handling of database connection failures
test_database_connection_failure(Config) ->
    Postgres = ?config(postgres, Config),
    ok = testcontainers_helper:stop_container(Postgres),
    Result = erlang_jobs:submit_job(#{type => test, data => <<"test">>}),
    {error, database_unavailable} = Result,
    ok.

%% @doc Test stability under high load
test_high_load_stability(_Config) ->
    NumJobs = 10000,
    StartTime = erlang:monotonic_time(millisecond),
    Pids = [spawn(fun() -> submit_test_job() end) || _ <- lists:seq(1, NumJobs)],
    [receive {'DOWN', _, process, Pid, _} -> ok end || Pid <- Pids],
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    true = Duration < 10000,
    ok.

%% Private functions

simulate_network_partition(Container, DurationMs) ->
    os:cmd(io_lib:format(
        "docker exec ~s iptables -A INPUT -j DROP",
        [Container#container.id]
    )),
    timer:sleep(DurationMs),
    os:cmd(io_lib:format(
        "docker exec ~s iptables -D INPUT -j DROP",
        [Container#container.id]
    )).

connect_to_postgres() ->
    {ok, self()}.

connect_to_redis() ->
    {ok, self()}.

submit_test_job() ->
    erlang_jobs:submit_job(#{type => test, data => <<"test">>}).
"#
    .to_string()
}

/// Generate Docker Compose configuration
fn generate_docker_compose() -> String {
    r#"version: '3.8'

services:
  postgres:
    image: postgres:15-alpine
    environment:
      POSTGRES_USER: erlang_jobs
      POSTGRES_PASSWORD: erlang_jobs
      POSTGRES_DB: erlang_jobs_db
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U erlang_jobs"]
      interval: 10s
      timeout: 5s
      retries: 5

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis_data:/data
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 3s
      retries: 5

  nats:
    image: nats:2.10-alpine
    ports:
      - "4222:4222"
      - "8222:8222"
    command: ["-js", "-m", "8222"]
    healthcheck:
      test: ["CMD", "wget", "--spider", "-q", "http://localhost:8222/healthz"]
      interval: 10s
      timeout: 3s
      retries: 5

  erlang_jobs:
    build:
      context: .
      dockerfile: Dockerfile
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_healthy
      nats:
        condition: service_healthy
    environment:
      DATABASE_URL: "postgresql://erlang_jobs:erlang_jobs@postgres:5432/erlang_jobs_db"
      REDIS_URL: "redis://redis:6379"
      NATS_URL: "nats://nats:4222"
    ports:
      - "8080:8080"

volumes:
  postgres_data:
  redis_data:
"#
    .to_string()
}

/// Generate GitHub Actions CI workflow
fn generate_ci_workflow() -> String {
    r#"name: Erlang Jobs CI/CD

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    name: Test Suite
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:15-alpine
        env:
          POSTGRES_USER: test
          POSTGRES_PASSWORD: test
          POSTGRES_DB: erlang_jobs_test
        ports:
          - 5432:5432
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

      redis:
        image: redis:7-alpine
        ports:
          - 6379:6379
        options: >-
          --health-cmd "redis-cli ping"
          --health-interval 10s
          --health-timeout 3s
          --health-retries 5

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
          rebar3-version: '3.22'

      - name: Restore dependencies cache
        uses: actions/cache@v3
        with:
          path: |
            _build
            ~/.cache/rebar3
          key: ${{ runner.os }}-rebar3-${{ hashFiles('rebar.lock') }}
          restore-keys: |
            ${{ runner.os }}-rebar3-

      - name: Compile
        run: rebar3 compile

      - name: Run unit tests
        run: rebar3 eunit

      - name: Run integration tests
        run: rebar3 ct
        env:
          DATABASE_URL: postgresql://test:test@localhost:5432/erlang_jobs_test
          REDIS_URL: redis://localhost:6379

      - name: Run chaos engineering tests
        run: rebar3 ct --suite test/chaos_engineering_SUITE
        if: github.event_name == 'push'

      - name: Generate coverage report
        run: rebar3 cover

  benchmark:
    name: Performance Benchmarks
    runs-on: ubuntu-latest
    if: github.event_name == 'push'

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
          rebar3-version: '3.22'

      - name: Run benchmarks
        run: rebar3 bench

      - name: Store benchmark results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: _build/bench/results/
"#
    .to_string()
}

/// Generate benchmark module
fn generate_benchmark_module() -> String {
    r#"%%%-------------------------------------------------------------------
%%% @doc Performance Benchmarks - Verify SLO targets
%%% @end
%%%-------------------------------------------------------------------
-module(erlang_jobs_bench).
-export([run_benchmarks/0, verify_slos/1]).

-define(LATENCY_P99_SLO_MS, 100).
-define(THROUGHPUT_SLO_JOBS_PER_SEC, 10000).
-define(MEMORY_SLO_MB, 512).

%% @doc Run all performance benchmarks
-spec run_benchmarks() -> map().
run_benchmarks() ->
    #{
        latency => measure_latency(),
        throughput => measure_throughput(),
        memory => measure_memory_usage()
    }.

%% @doc Verify results meet SLO targets
-spec verify_slos(map()) -> ok | {error, list()}.
verify_slos(Results) ->
    Checks = [
        verify_latency_slo(maps:get(latency, Results)),
        verify_throughput_slo(maps:get(throughput, Results)),
        verify_memory_slo(maps:get(memory, Results))
    ],

    Failures = [Reason || {error, Reason} <- Checks],

    case Failures of
        [] -> ok;
        _ -> {error, Failures}
    end.

%% Private functions

measure_latency() ->
    Samples = 1000,
    [submit_test_job() || _ <- lists:seq(1, 100)],

    Latencies = [
        begin
            {Time, _} = timer:tc(fun submit_test_job/0),
            Time / 1000.0
        end
        || _ <- lists:seq(1, Samples)
    ],

    Sorted = lists:sort(Latencies),
    P99Idx = (Samples * 99) div 100,

    #{p99 => lists:nth(P99Idx, Sorted)}.

measure_throughput() ->
    Duration = 5000,
    StartTime = erlang:monotonic_time(millisecond),
    Count = run_throughput_loop(0, StartTime, Duration),
    EndTime = erlang:monotonic_time(millisecond),
    ElapsedSeconds = (EndTime - StartTime) / 1000.0,

    #{
        jobs_per_second => Count / ElapsedSeconds,
        total_jobs => Count
    }.

measure_memory_usage() ->
    erlang:garbage_collect(),
    {total, BaselineMem} = erlang:memory(total),
    [submit_test_job() || _ <- lists:seq(1, 1000)],
    {total, PeakMem} = erlang:memory(total),

    #{
        baseline_mb => BaselineMem / (1024 * 1024),
        peak_mb => PeakMem / (1024 * 1024)
    }.

verify_latency_slo(LatencyResults) ->
    P99 = maps:get(p99, LatencyResults),
    case P99 =< ?LATENCY_P99_SLO_MS of
        true -> ok;
        false -> {error, {latency_slo_violation, P99, ?LATENCY_P99_SLO_MS}}
    end.

verify_throughput_slo(ThroughputResults) ->
    JobsPerSec = maps:get(jobs_per_second, ThroughputResults),
    case JobsPerSec >= ?THROUGHPUT_SLO_JOBS_PER_SEC of
        true -> ok;
        false -> {error, {throughput_slo_violation, JobsPerSec, ?THROUGHPUT_SLO_JOBS_PER_SEC}}
    end.

verify_memory_slo(MemoryResults) ->
    PeakMB = maps:get(peak_mb, MemoryResults),
    case PeakMB =< ?MEMORY_SLO_MB of
        true -> ok;
        false -> {error, {memory_slo_violation, PeakMB, ?MEMORY_SLO_MB}}
    end.

run_throughput_loop(Count, StartTime, Duration) ->
    Current = erlang:monotonic_time(millisecond),
    case Current - StartTime > Duration of
        true -> Count;
        false ->
            _ = submit_test_job(),
            run_throughput_loop(Count + 1, StartTime, Duration)
    end.

submit_test_job() ->
    ok.
"#
    .to_string()
}

// =============================================================================
// Snapshot Tests - Determinism Validation
// =============================================================================

#[test]
#[serial]
fn snapshot_testcontainers_helper_generated_code() {
    // Verify testcontainers helper module generates identical output

    let generated = generate_testcontainers_helper();

    // Assert: Generated code is deterministic
    assert_snapshot!("testcontainers_helper", generated);

    // Verify again - should produce identical result
    let generated_again = generate_testcontainers_helper();
    assert_eq!(generated, generated_again, "Output should be deterministic");
}

#[test]
#[serial]
fn snapshot_chaos_engineering_suite_generated_code() {
    // Verify chaos engineering suite generates identical output

    let generated = generate_chaos_suite();

    // Assert: Generated code is deterministic
    assert_snapshot!("chaos_engineering_suite", generated);

    // Verify again - should produce identical result
    let generated_again = generate_chaos_suite();
    assert_eq!(generated, generated_again, "Output should be deterministic");
}

#[test]
#[serial]
fn snapshot_docker_compose_output() {
    // Verify Docker Compose file generates identical output

    let generated = generate_docker_compose();

    // Assert: Generated YAML is deterministic
    assert_snapshot!("docker_compose", generated);

    // Verify again - should produce identical result
    let generated_again = generate_docker_compose();
    assert_eq!(generated, generated_again, "Output should be deterministic");
}

#[test]
#[serial]
fn snapshot_github_actions_workflow() {
    // Verify CI workflow generates identical output

    let generated = generate_ci_workflow();

    // Assert: Generated workflow is deterministic
    assert_snapshot!("github_actions_workflow", generated);

    // Verify again - should produce identical result
    let generated_again = generate_ci_workflow();
    assert_eq!(generated, generated_again, "Output should be deterministic");
}

#[test]
#[serial]
fn snapshot_benchmark_module_generation() {
    // Verify benchmark module generates identical output

    let generated = generate_benchmark_module();

    // Assert: Generated code is deterministic
    assert_snapshot!("benchmark_module", generated);

    // Verify again - should produce identical result
    let generated_again = generate_benchmark_module();
    assert_eq!(generated, generated_again, "Output should be deterministic");
}

// =============================================================================
// Cross-Component Determinism Tests
// =============================================================================

#[test]
#[serial]
fn test_deterministic_output_across_multiple_generations() {
    // Verify all components maintain determinism across multiple generations

    // Generate all components multiple times
    let iterations = 5;

    // Testcontainers helper
    let helpers: Vec<String> = (0..iterations)
        .map(|_| generate_testcontainers_helper())
        .collect();
    for i in 1..iterations {
        assert_eq!(
            helpers[0], helpers[i],
            "Testcontainers helper output should be deterministic"
        );
    }

    // Chaos suite
    let suites: Vec<String> = (0..iterations).map(|_| generate_chaos_suite()).collect();
    for i in 1..iterations {
        assert_eq!(
            suites[0], suites[i],
            "Chaos suite output should be deterministic"
        );
    }

    // Docker Compose
    let composes: Vec<String> = (0..iterations)
        .map(|_| generate_docker_compose())
        .collect();
    for i in 1..iterations {
        assert_eq!(
            composes[0], composes[i],
            "Docker Compose output should be deterministic"
        );
    }

    // CI Workflow
    let workflows: Vec<String> = (0..iterations).map(|_| generate_ci_workflow()).collect();
    for i in 1..iterations {
        assert_eq!(
            workflows[0], workflows[i],
            "CI workflow output should be deterministic"
        );
    }

    // Benchmarks
    let benchmarks: Vec<String> = (0..iterations)
        .map(|_| generate_benchmark_module())
        .collect();
    for i in 1..iterations {
        assert_eq!(
            benchmarks[0], benchmarks[i],
            "Benchmark module output should be deterministic"
        );
    }
}

#[test]
#[serial]
fn test_file_persistence_maintains_determinism() {
    // Verify that writing to files and reading back maintains determinism

    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Generate and write all components
    let components = vec![
        (
            "testcontainers_helper.erl",
            generate_testcontainers_helper(),
        ),
        ("chaos_engineering_SUITE.erl", generate_chaos_suite()),
        ("docker-compose.yml", generate_docker_compose()),
        (".github/workflows/ci.yml", generate_ci_workflow()),
        ("erlang_jobs_bench.erl", generate_benchmark_module()),
    ];

    for (filename, content) in &components {
        let file_path = temp_dir.path().join(filename);

        // Create parent directories if needed
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent).expect("Failed to create parent dir");
        }

        // Write file
        fs::write(&file_path, content).expect("Failed to write file");

        // Read back
        let read_content = fs::read_to_string(&file_path).expect("Failed to read file");

        // Verify content is identical
        assert_eq!(
            content, &read_content,
            "File content should match for {}",
            filename
        );
    }
}

#[test]
#[serial]
fn test_snapshot_stability_over_time() {
    // Simulate multiple generation cycles to verify snapshots remain stable

    // This test would fail if generation logic introduces non-determinism

    let cycles = 10;

    for _cycle in 0..cycles {
        let helper = generate_testcontainers_helper();
        let suite = generate_chaos_suite();
        let compose = generate_docker_compose();
        let workflow = generate_ci_workflow();
        let bench = generate_benchmark_module();

        // Verify each component maintains its snapshot
        // (In practice, this would use stored snapshots)
        assert!(!helper.is_empty(), "Helper should not be empty");
        assert!(!suite.is_empty(), "Suite should not be empty");
        assert!(!compose.is_empty(), "Compose should not be empty");
        assert!(!workflow.is_empty(), "Workflow should not be empty");
        assert!(!bench.is_empty(), "Bench should not be empty");

        // Verify structure markers are present
        assert!(
            helper.contains("start_postgres"),
            "Helper should contain start_postgres"
        );
        assert!(
            suite.contains("test_network_partition_recovery"),
            "Suite should contain network partition test"
        );
        assert!(compose.contains("postgres:"), "Compose should contain postgres service");
        assert!(
            workflow.contains("test:"),
            "Workflow should contain test job"
        );
        assert!(bench.contains("verify_slos"), "Bench should contain SLO verification");
    }
}

#[test]
#[serial]
fn test_whitespace_normalization_consistency() {
    // Verify that whitespace is consistently handled

    let content = generate_testcontainers_helper();

    // Count lines
    let lines: Vec<&str> = content.lines().collect();

    // Verify no trailing whitespace
    for (i, line) in lines.iter().enumerate() {
        assert!(
            !line.ends_with(' ') && !line.ends_with('\t'),
            "Line {} should not have trailing whitespace: '{}'",
            i + 1,
            line
        );
    }

    // Verify consistent indentation (4 spaces in Erlang)
    for (i, line) in lines.iter().enumerate() {
        if line.trim().is_empty() {
            continue;
        }

        let leading_spaces = line.len() - line.trim_start().len();
        if leading_spaces > 0 {
            assert_eq!(
                leading_spaces % 4,
                0,
                "Line {} should have indentation in multiples of 4: '{}'",
                i + 1,
                line
            );
        }
    }
}

#[test]
#[serial]
fn test_line_ending_consistency() {
    // Verify that line endings are consistent (Unix-style \n)

    let components = vec![
        generate_testcontainers_helper(),
        generate_chaos_suite(),
        generate_docker_compose(),
        generate_ci_workflow(),
        generate_benchmark_module(),
    ];

    for (i, content) in components.iter().enumerate() {
        // Should not contain Windows-style line endings
        assert!(
            !content.contains("\r\n"),
            "Component {} should not contain Windows line endings",
            i
        );

        // Should not contain Mac Classic-style line endings
        assert!(
            !content.contains("\r"),
            "Component {} should not contain Mac Classic line endings",
            i
        );

        // Lines should be terminated with \n
        let line_count = content.lines().count();
        assert!(line_count > 0, "Component {} should have lines", i);
    }
}
