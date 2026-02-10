//! Integration tests for μ₃:emission determinism checks
//!
//! Tests comprehensive nondeterminism detection including:
//! - Timestamp patterns (now(), Utc::now(), SystemTime)
//! - Random patterns (rand(), uuid(), thread_rng())
//! - Process patterns (pid(), thread_id())
//! - Network I/O patterns (reqwest, hyper, tokio::net)
//! - Filesystem metadata patterns (modified(), accessed())
//! - Ordering patterns (HashMap, HashSet)
//! - Grouping patterns (group_by, GROUP BY)
//! - Join patterns (join, JOIN, INNER JOIN)
//! - Idempotence verification (double-render check)

use ggen_core::graph::Graph;
use ggen_core::v6::pass::{Pass, PassContext};
use ggen_core::v6::passes::{EmissionPass, EmissionRule};
use std::path::PathBuf;
use tempfile::TempDir;

/// Helper to create a test emission pass with a template
fn create_test_pass(template_name: &str, template_content: &str) -> (TempDir, EmissionPass) {
    let temp_dir = TempDir::new().unwrap();
    let template_dir = temp_dir.path().join("templates");
    std::fs::create_dir_all(&template_dir).unwrap();
    std::fs::write(template_dir.join(template_name), template_content).unwrap();

    let mut pass = EmissionPass::new();
    pass = pass.with_guards(ggen_core::v6::guard::GuardSet::new()); // Disable guards for tests

    pass.add_rule(EmissionRule {
        name: format!("test-{}", template_name),
        template_path: PathBuf::from(format!("templates/{}", template_name)),
        output_pattern: "output.rs".to_string(),
        binding_key: "data".to_string(),
        iterate: false,
        skip_empty: false,
        description: None,
    });

    (temp_dir, pass)
}

// ============================================================================
// Timestamp Pattern Detection Tests
// ============================================================================

#[test]
fn test_emission_detects_utc_now() {
    let (temp_dir, pass) = create_test_pass(
        "timestamp.rs.tera",
        r#"
        pub fn get_time() -> String {
            chrono::Utc::now().to_string()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("timestamp"));
    assert!(err.contains("STOPPED THE LINE"));
}

#[test]
fn test_emission_detects_system_time_now() {
    let (temp_dir, pass) = create_test_pass(
        "systemtime.rs.tera",
        r#"
        use std::time::SystemTime;
        pub fn current() -> SystemTime {
            SystemTime::now()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("timestamp"));
}

#[test]
fn test_emission_detects_instant_now() {
    let (temp_dir, pass) = create_test_pass(
        "instant.rs.tera",
        r#"
        use std::time::Instant;
        pub fn start() -> Instant {
            Instant::now()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("timestamp"));
}

// ============================================================================
// Random Pattern Detection Tests
// ============================================================================

#[test]
fn test_emission_detects_rand_random() {
    let (temp_dir, pass) = create_test_pass(
        "random.rs.tera",
        r#"
        use rand::random;
        pub fn random_number() -> u64 {
            random()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("random"));
}

#[test]
fn test_emission_detects_uuid_new_v4() {
    let (temp_dir, pass) = create_test_pass(
        "uuid.rs.tera",
        r#"
        use uuid::Uuid;
        pub fn generate_id() -> Uuid {
            Uuid::new_v4()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("random"));
}

#[test]
fn test_emission_detects_thread_rng() {
    let (temp_dir, pass) = create_test_pass(
        "rng.rs.tera",
        r#"
        use rand::thread_rng;
        pub fn init_rng() {
            let mut rng = thread_rng();
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("random"));
}

// ============================================================================
// Process Pattern Detection Tests
// ============================================================================

#[test]
fn test_emission_detects_process_id() {
    let (temp_dir, pass) = create_test_pass(
        "pid.rs.tera",
        r#"
        use std::process;
        pub fn get_pid() -> u32 {
            process::id()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("process"));
}

#[test]
fn test_emission_detects_thread_id() {
    let (temp_dir, pass) = create_test_pass(
        "thread.rs.tera",
        r#"
        use std::thread::ThreadId;
        pub fn current_id() -> ThreadId {
            std::thread::current().id()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("process"));
}

// ============================================================================
// Network I/O Pattern Detection Tests
// ============================================================================

#[test]
fn test_emission_detects_reqwest() {
    let (temp_dir, pass) = create_test_pass(
        "http.rs.tera",
        r#"
        use reqwest::Client;
        pub async fn fetch() -> String {
            let client = Client::new();
            client.get("https://example.com").send().await.unwrap().text().await.unwrap()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("network"));
}

#[test]
fn test_emission_detects_tcp_stream() {
    let (temp_dir, pass) = create_test_pass(
        "tcp.rs.tera",
        r#"
        use std::net::TcpStream;
        pub fn connect() -> TcpStream {
            TcpStream::connect("127.0.0.1:8080").unwrap()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("network"));
}

// ============================================================================
// Filesystem Metadata Pattern Detection Tests
// ============================================================================

#[test]
fn test_emission_detects_file_metadata() {
    let (temp_dir, pass) = create_test_pass(
        "metadata.rs.tera",
        r#"
        use std::fs;
        pub fn get_modified(path: &str) -> std::time::SystemTime {
            fs::metadata(path).unwrap().modified().unwrap()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("filesystem_metadata") || err.contains("metadata"));
}

// ============================================================================
// Ordering Pattern Detection Tests (HashMap/HashSet)
// ============================================================================

#[test]
fn test_emission_detects_hashmap_usage() {
    let (temp_dir, pass) = create_test_pass(
        "hashmap.rs.tera",
        r#"
        use std::collections::HashMap;
        pub fn create_map() -> HashMap<String, String> {
            let mut map = HashMap::new();
            map.insert("key".to_string(), "value".to_string());
            map
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("ordering") || err.contains("HashMap"));
}

#[test]
fn test_emission_detects_hashset_usage() {
    let (temp_dir, pass) = create_test_pass(
        "hashset.rs.tera",
        r#"
        use std::collections::HashSet;
        pub fn create_set() -> HashSet<String> {
            let mut set = HashSet::new();
            set.insert("item".to_string());
            set
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("ordering") || err.contains("HashSet"));
}

// ============================================================================
// Grouping Pattern Detection Tests
// ============================================================================

#[test]
fn test_emission_detects_group_by_pattern() {
    let (temp_dir, pass) = create_test_pass(
        "groupby.rs.tera",
        r#"
        pub fn query() -> String {
            "SELECT * FROM users GROUP BY department".to_string()
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("grouping"));
}

#[test]
fn test_emission_detects_iterator_group_by() {
    let (temp_dir, pass) = create_test_pass(
        "iter_group.rs.tera",
        r#"
        pub fn group_items(items: Vec<i32>) {
            let grouped = items.iter().group_by(|x| x % 2);
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("grouping"));
}

// ============================================================================
// Join Pattern Detection Tests
// ============================================================================

#[test]
fn test_emission_detects_sql_join() {
    let (temp_dir, pass) = create_test_pass(
        "join.sql.tera",
        r#"
        SELECT * FROM users
        INNER JOIN orders ON users.id = orders.user_id
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("joins"));
}

#[test]
fn test_emission_detects_left_join() {
    let (temp_dir, pass) = create_test_pass(
        "leftjoin.sql.tera",
        r#"
        SELECT * FROM users
        LEFT JOIN addresses ON users.id = addresses.user_id
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Non-Deterministic"));
    assert!(err.contains("joins"));
}

// ============================================================================
// Idempotence Tests (Double-Render Check)
// ============================================================================

#[test]
fn test_emission_idempotence_passes_for_deterministic_template() {
    let (temp_dir, pass) = create_test_pass(
        "deterministic.rs.tera",
        r#"
        pub struct {{ name }} {
            pub id: u64,
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);
    ctx.bindings
        .insert("name".to_string(), serde_json::json!("User"));

    let result = pass.execute(&mut ctx);
    assert!(result.is_ok());
}

// ============================================================================
// Clean Template Tests (Should Pass)
// ============================================================================

#[test]
fn test_emission_allows_btreemap() {
    let (temp_dir, pass) = create_test_pass(
        "btreemap.rs.tera",
        r#"
        use std::collections::BTreeMap;
        pub fn create_map() -> BTreeMap<String, String> {
            let mut map = BTreeMap::new();
            map.insert("key".to_string(), "value".to_string());
            map
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_ok(), "BTreeMap should be allowed (ordered)");
}

#[test]
fn test_emission_allows_pure_functions() {
    let (temp_dir, pass) = create_test_pass(
        "pure.rs.tera",
        r#"
        pub fn add(a: i32, b: i32) -> i32 {
            a + b
        }

        pub fn multiply(x: i32, y: i32) -> i32 {
            x * y
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    let result = pass.execute(&mut ctx);
    assert!(result.is_ok(), "Pure functions should be allowed");
}

// ============================================================================
// Receipt Generation Test
// ============================================================================

#[test]
fn test_emission_generates_receipt() {
    let (temp_dir, pass) = create_test_pass(
        "model.rs.tera",
        r#"
        pub struct Model {
            pub field: String,
        }
        "#,
    );

    let graph = Graph::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());

    let result = pass.execute(&mut ctx);
    assert!(result.is_ok());

    let pass_result = result.unwrap();
    assert!(pass_result.success);
    assert_eq!(pass_result.files_generated.len(), 1);

    // Verify file was actually written
    assert!(output_dir.join("output.rs").exists());
}
