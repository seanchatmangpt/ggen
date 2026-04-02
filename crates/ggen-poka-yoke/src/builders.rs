//! Builder patterns with compile-time validation.
//!
//! Prevents incomplete or invalid construction via the type system.
//!
//! # Examples
//!
//! ```
//! use ggen_poka_yoke::builders::ConfigBuilder;
//!
//! let config = ConfigBuilder::new()
//!     .with_name("my-app")
//!     .with_version("1.0.0")
//!     .with_port(8080)
//!     .build();
//! ```

use std::marker::PhantomData;

/// Marker for unset builder field.
#[derive(Debug, Clone, Copy)]
pub struct Unset;

/// Marker for set builder field.
#[derive(Debug, Clone, Copy)]
pub struct Set;

/// Configuration builder with compile-time validation.
///
/// All required fields must be set before `build()` can be called.
#[derive(Debug)]
pub struct ConfigBuilder<Name, Version, Port> {
    name: Option<String>,
    version: Option<String>,
    port: Option<u16>,
    timeout_ms: Option<u64>,
    _name: PhantomData<Name>,
    _version: PhantomData<Version>,
    _port: PhantomData<Port>,
}

impl ConfigBuilder<Unset, Unset, Unset> {
    /// Creates a new builder with all fields unset.
    pub fn new() -> Self {
        Self {
            name: None,
            version: None,
            port: None,
            timeout_ms: None,
            _name: PhantomData,
            _version: PhantomData,
            _port: PhantomData,
        }
    }
}

impl Default for ConfigBuilder<Unset, Unset, Unset> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Name, Version, Port> ConfigBuilder<Name, Version, Port> {
    /// Sets the timeout (optional field).
    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.timeout_ms = Some(timeout_ms);
        self
    }
}

impl<Version, Port> ConfigBuilder<Unset, Version, Port> {
    /// Sets the name (required field).
    pub fn with_name(self, name: impl Into<String>) -> ConfigBuilder<Set, Version, Port> {
        ConfigBuilder {
            name: Some(name.into()),
            version: self.version,
            port: self.port,
            timeout_ms: self.timeout_ms,
            _name: PhantomData,
            _version: PhantomData,
            _port: PhantomData,
        }
    }
}

impl<Name, Port> ConfigBuilder<Name, Unset, Port> {
    /// Sets the version (required field).
    pub fn with_version(self, version: impl Into<String>) -> ConfigBuilder<Name, Set, Port> {
        ConfigBuilder {
            name: self.name,
            version: Some(version.into()),
            port: self.port,
            timeout_ms: self.timeout_ms,
            _name: PhantomData,
            _version: PhantomData,
            _port: PhantomData,
        }
    }
}

impl<Name, Version> ConfigBuilder<Name, Version, Unset> {
    /// Sets the port (required field).
    pub fn with_port(self, port: u16) -> ConfigBuilder<Name, Version, Set> {
        ConfigBuilder {
            name: self.name,
            version: self.version,
            port: Some(port),
            timeout_ms: self.timeout_ms,
            _name: PhantomData,
            _version: PhantomData,
            _port: PhantomData,
        }
    }
}

impl ConfigBuilder<Set, Set, Set> {
    /// Builds the configuration (only callable when all required fields are set).
    pub fn build(self) -> Config {
        Config {
            name: self.name.expect("name must be set"),
            version: self.version.expect("version must be set"),
            port: self.port.expect("port must be set"),
            timeout_ms: self.timeout_ms.unwrap_or(30000),
        }
    }
}

/// Validated configuration object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    name: String,
    version: String,
    port: u16,
    timeout_ms: u64,
}

impl Config {
    /// Returns the name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the version.
    pub fn version(&self) -> &str {
        &self.version
    }

    /// Returns the port.
    pub fn port(&self) -> u16 {
        self.port
    }

    /// Returns the timeout in milliseconds.
    pub fn timeout_ms(&self) -> u64 {
        self.timeout_ms
    }
}

/// Database connection builder with compile-time validation.
#[derive(Debug)]
pub struct DbConnectionBuilder<Host, Database, User> {
    host: Option<String>,
    port: u16,
    database: Option<String>,
    user: Option<String>,
    password: Option<String>,
    ssl: bool,
    _host: PhantomData<Host>,
    _database: PhantomData<Database>,
    _user: PhantomData<User>,
}

impl DbConnectionBuilder<Unset, Unset, Unset> {
    /// Creates a new database connection builder.
    pub fn new() -> Self {
        Self {
            host: None,
            port: 5432,
            database: None,
            user: None,
            password: None,
            ssl: false,
            _host: PhantomData,
            _database: PhantomData,
            _user: PhantomData,
        }
    }
}

impl Default for DbConnectionBuilder<Unset, Unset, Unset> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Host, Database, User> DbConnectionBuilder<Host, Database, User> {
    /// Sets the port (optional, defaults to 5432).
    pub fn with_port(mut self, port: u16) -> Self {
        self.port = port;
        self
    }

    /// Sets the password (optional).
    pub fn with_password(mut self, password: impl Into<String>) -> Self {
        self.password = Some(password.into());
        self
    }

    /// Enables SSL (optional, defaults to false).
    pub fn with_ssl(mut self, ssl: bool) -> Self {
        self.ssl = ssl;
        self
    }
}

impl<Database, User> DbConnectionBuilder<Unset, Database, User> {
    /// Sets the host (required).
    pub fn with_host(self, host: impl Into<String>) -> DbConnectionBuilder<Set, Database, User> {
        DbConnectionBuilder {
            host: Some(host.into()),
            port: self.port,
            database: self.database,
            user: self.user,
            password: self.password,
            ssl: self.ssl,
            _host: PhantomData,
            _database: PhantomData,
            _user: PhantomData,
        }
    }
}

impl<Host, User> DbConnectionBuilder<Host, Unset, User> {
    /// Sets the database name (required).
    pub fn with_database(
        self, database: impl Into<String>,
    ) -> DbConnectionBuilder<Host, Set, User> {
        DbConnectionBuilder {
            host: self.host,
            port: self.port,
            database: Some(database.into()),
            user: self.user,
            password: self.password,
            ssl: self.ssl,
            _host: PhantomData,
            _database: PhantomData,
            _user: PhantomData,
        }
    }
}

impl<Host, Database> DbConnectionBuilder<Host, Database, Unset> {
    /// Sets the user (required).
    pub fn with_user(self, user: impl Into<String>) -> DbConnectionBuilder<Host, Database, Set> {
        DbConnectionBuilder {
            host: self.host,
            port: self.port,
            database: self.database,
            user: Some(user.into()),
            password: self.password,
            ssl: self.ssl,
            _host: PhantomData,
            _database: PhantomData,
            _user: PhantomData,
        }
    }
}

impl DbConnectionBuilder<Set, Set, Set> {
    /// Builds the database connection (only callable when all required fields are set).
    pub fn build(self) -> DbConnection {
        DbConnection {
            host: self.host.expect("host must be set"),
            port: self.port,
            database: self.database.expect("database must be set"),
            user: self.user.expect("user must be set"),
            password: self.password,
            ssl: self.ssl,
        }
    }
}

/// Database connection configuration.
#[derive(Debug, Clone)]
pub struct DbConnection {
    host: String,
    port: u16,
    database: String,
    user: String,
    password: Option<String>,
    ssl: bool,
}

impl DbConnection {
    /// Returns the connection string.
    pub fn connection_string(&self) -> String {
        let ssl = if self.ssl { "?ssl=true" } else { "" };
        format!(
            "postgres://{}@{}:{}/{}{}",
            self.user, self.host, self.port, self.database, ssl
        )
    }

    /// Returns the host.
    pub fn host(&self) -> &str {
        &self.host
    }

    /// Returns the port.
    pub fn port(&self) -> u16 {
        self.port
    }

    /// Returns the database name.
    pub fn database(&self) -> &str {
        &self.database
    }

    /// Returns the user.
    pub fn user(&self) -> &str {
        &self.user
    }

    /// Checks if SSL is enabled.
    pub fn ssl_enabled(&self) -> bool {
        self.ssl
    }

    /// Returns the password if set (use carefully).
    pub fn password(&self) -> Option<&str> {
        self.password.as_deref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_builder() {
        let config = ConfigBuilder::new()
            .with_name("test-app")
            .with_version("1.0.0")
            .with_port(8080)
            .build();

        assert_eq!(config.name(), "test-app");
        assert_eq!(config.version(), "1.0.0");
        assert_eq!(config.port(), 8080);
        assert_eq!(config.timeout_ms(), 30000);
    }

    #[test]
    fn test_config_builder_with_timeout() {
        let config = ConfigBuilder::new()
            .with_name("test-app")
            .with_version("2.0.0")
            .with_port(9090)
            .with_timeout(5000)
            .build();

        assert_eq!(config.timeout_ms(), 5000);
    }

    #[test]
    fn test_db_connection_builder() {
        let conn = DbConnectionBuilder::new()
            .with_host("localhost")
            .with_database("mydb")
            .with_user("admin")
            .build();

        assert_eq!(conn.host(), "localhost");
        assert_eq!(conn.port(), 5432);
        assert_eq!(conn.database(), "mydb");
        assert_eq!(conn.user(), "admin");
        assert!(!conn.ssl_enabled());
    }

    #[test]
    fn test_db_connection_with_options() {
        let conn = DbConnectionBuilder::new()
            .with_host("db.example.com")
            .with_port(5433)
            .with_database("proddb")
            .with_user("appuser")
            .with_password("secret")
            .with_ssl(true)
            .build();

        assert_eq!(conn.port(), 5433);
        assert!(conn.ssl_enabled());

        let connection_str = conn.connection_string();
        assert!(connection_str.contains("db.example.com"));
        assert!(connection_str.contains("5433"));
        assert!(connection_str.contains("ssl=true"));
    }
}
