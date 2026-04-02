//! Integration tests for builder patterns.

use ggen_poka_yoke::builders::*;

#[test]
fn test_config_builder_all_required_fields() {
    let config = ConfigBuilder::new()
        .with_name("my-app")
        .with_version("1.0.0")
        .with_port(8080)
        .build();

    assert_eq!(config.name(), "my-app");
    assert_eq!(config.version(), "1.0.0");
    assert_eq!(config.port(), 8080);
    assert_eq!(config.timeout_ms(), 30000); // Default timeout
}

#[test]
fn test_config_builder_with_optional_fields() {
    let config = ConfigBuilder::new()
        .with_name("test-service")
        .with_version("2.0.0")
        .with_port(9090)
        .with_timeout(5000)
        .build();

    assert_eq!(config.name(), "test-service");
    assert_eq!(config.version(), "2.0.0");
    assert_eq!(config.port(), 9090);
    assert_eq!(config.timeout_ms(), 5000);
}

#[test]
fn test_config_builder_field_order_independence() {
    // Fields can be set in any order
    let config1 = ConfigBuilder::new()
        .with_name("app1")
        .with_version("1.0")
        .with_port(8080)
        .build();

    let config2 = ConfigBuilder::new()
        .with_port(8080)
        .with_name("app1")
        .with_version("1.0")
        .build();

    let config3 = ConfigBuilder::new()
        .with_version("1.0")
        .with_port(8080)
        .with_name("app1")
        .build();

    assert_eq!(config1.name(), config2.name());
    assert_eq!(config1.version(), config2.version());
    assert_eq!(config1.port(), config2.port());

    assert_eq!(config1.name(), config3.name());
    assert_eq!(config1.version(), config3.version());
    assert_eq!(config1.port(), config3.port());
}

#[test]
fn test_db_connection_builder_minimal() {
    let conn = DbConnectionBuilder::new()
        .with_host("localhost")
        .with_database("testdb")
        .with_user("testuser")
        .build();

    assert_eq!(conn.host(), "localhost");
    assert_eq!(conn.port(), 5432); // Default port
    assert_eq!(conn.database(), "testdb");
    assert_eq!(conn.user(), "testuser");
    assert!(!conn.ssl_enabled()); // Default SSL disabled
}

#[test]
fn test_db_connection_builder_full() {
    let conn = DbConnectionBuilder::new()
        .with_host("db.example.com")
        .with_port(5433)
        .with_database("production")
        .with_user("appuser")
        .with_password("secret123")
        .with_ssl(true)
        .build();

    assert_eq!(conn.host(), "db.example.com");
    assert_eq!(conn.port(), 5433);
    assert_eq!(conn.database(), "production");
    assert_eq!(conn.user(), "appuser");
    assert!(conn.ssl_enabled());

    let conn_str = conn.connection_string();
    assert!(conn_str.contains("postgres://"));
    assert!(conn_str.contains("appuser"));
    assert!(conn_str.contains("db.example.com"));
    assert!(conn_str.contains("5433"));
    assert!(conn_str.contains("production"));
    assert!(conn_str.contains("ssl=true"));
}

#[test]
fn test_db_connection_without_ssl() {
    let conn = DbConnectionBuilder::new()
        .with_host("localhost")
        .with_database("devdb")
        .with_user("dev")
        .build();

    let conn_str = conn.connection_string();
    assert!(!conn_str.contains("ssl=true"));
}

#[test]
fn test_db_connection_field_order_independence() {
    let conn1 = DbConnectionBuilder::new()
        .with_host("host1")
        .with_database("db1")
        .with_user("user1")
        .with_port(5432)
        .build();

    let conn2 = DbConnectionBuilder::new()
        .with_database("db1")
        .with_user("user1")
        .with_port(5432)
        .with_host("host1")
        .build();

    let conn3 = DbConnectionBuilder::new()
        .with_user("user1")
        .with_port(5432)
        .with_database("db1")
        .with_host("host1")
        .build();

    assert_eq!(conn1.host(), conn2.host());
    assert_eq!(conn1.database(), conn2.database());
    assert_eq!(conn1.user(), conn2.user());
    assert_eq!(conn1.port(), conn2.port());

    assert_eq!(conn1.host(), conn3.host());
    assert_eq!(conn1.database(), conn3.database());
    assert_eq!(conn1.user(), conn3.user());
    assert_eq!(conn1.port(), conn3.port());
}

#[test]
fn test_config_builder_clone_semantics() {
    let config = ConfigBuilder::new()
        .with_name("cloneable")
        .with_version("1.0.0")
        .with_port(3000)
        .build();

    let config_clone = config.clone();
    assert_eq!(config.name(), config_clone.name());
    assert_eq!(config.version(), config_clone.version());
    assert_eq!(config.port(), config_clone.port());
}

#[test]
fn test_db_connection_clone_semantics() {
    let conn = DbConnectionBuilder::new()
        .with_host("host")
        .with_database("db")
        .with_user("user")
        .build();

    let conn_clone = conn.clone();
    assert_eq!(conn.host(), conn_clone.host());
    assert_eq!(conn.database(), conn_clone.database());
    assert_eq!(conn.user(), conn_clone.user());
}
