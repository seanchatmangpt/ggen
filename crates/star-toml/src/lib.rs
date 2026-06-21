//! # star_toml — framework for loading, layering, and validating `*.toml` config files
//!
//! `star_toml` (pronounced "star TOML", as in `*.toml`) is a general-purpose config
//! framework for Rust. It handles any TOML configuration file — not just one specific
//! project format — by composing multiple config sources in layers.
//!
//! ## Core concepts
//!
//! | Concept | Type | Description |
//! |---------|------|-------------|
//! | **Validation engine** | [`Validate`] / [`Validator`] / [`ValidationErrors`] | Pydantic-style: collect *every* error, each with a precise path, the bad value, and a machine code |
//! | Layered loading | [`Loader`] | Builder that merges sources in priority order |
//! | Loaded config + path | [`ConfigFile<T>`] | Config value + source file path for resolving relative refs |
//! | Deep merge | [`deep_merge`] | Table-recursive TOML value merge |
//! | Env expansion | [`expand_env_vars`] | `${VAR}` / `$VAR` replacement |
//! | File discovery | [`find_config_file`] | Walk parent dirs to find `*.toml` |
//! | Write-back | [`save_file`] / [`to_string`] | Serialize a config back to TOML on disk |
//!
//! ## The headline feature: Pydantic-grade validation
//!
//! Most config libraries fail on the first bad value with a stringly-typed message.
//! `star_toml` instead collects **every** failure across the whole config tree at once —
//! each with a precise location, the offending value, and a machine-matchable code:
//!
//! ```
//! use star_toml::{Validate, Validator};
//!
//! struct Server { host: String, port: u16 }
//! struct App { name: String, workers: u32, server: Server }
//!
//! impl Validate for Server {
//!     fn validate(&self, v: &mut Validator) {
//!         v.check_non_empty("host", &self.host);
//!         v.check_range("port", self.port, 1..=65535);
//!     }
//! }
//! impl Validate for App {
//!     fn validate(&self, v: &mut Validator) {
//!         v.check_non_empty("name", &self.name);
//!         v.check_range("workers", self.workers, 1..=1024);
//!         v.field("server", |v| self.server.validate(v));  // nested → server.*
//!     }
//! }
//!
//! let app = App {
//!     name: String::new(),
//!     workers: 0,
//!     server: Server { host: String::new(), port: 0 },
//! };
//!
//! let report = app.check().unwrap_err();
//! assert_eq!(report.len(), 4);  // ALL four errors, not just the first
//!
//! // Every error is path-precise and programmatically matchable:
//! let locs: Vec<String> = report.errors().iter().map(|e| e.loc.to_string()).collect();
//! assert_eq!(locs, ["name", "workers", "server.host", "server.port"]);
//!
//! // And renders as a Pydantic-style report:
//! // 4 validation errors for App
//! // name
//! //   must not be empty (got: `""`) [empty]
//! // workers
//! //   input must be in range 1..=1024 (got: `0`) [out_of_range]
//! // ...
//! println!("{report}");
//! ```
//!
//! ## Quick start
//!
//! ### Load a single file
//!
//! ```no_run
//! use star_toml::load_file;
//!
//! #[derive(serde::Deserialize)]
//! struct AppConfig {
//!     name: String,
//!     port: u16,
//! }
//!
//! let cfg: AppConfig = load_file("app.toml")?;
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ### Compose multiple sources in layers
//!
//! ```no_run
//! use star_toml::Loader;
//!
//! #[derive(serde::Deserialize)]
//! struct AppConfig { name: String, port: u16 }
//!
//! const DEFAULTS: &str = r#"
//! name = "my-app"
//! port = 8080
//! "#;
//!
//! let cfg: AppConfig = Loader::new()
//!     .layer_str(DEFAULTS, "built-in defaults")   // lowest priority
//!     .find_file("app.toml")                       // walks up from cwd
//!     .layer_file_if_exists("~/.config/app.toml")  // user overrides
//!     .env_prefix("APP_")                          // APP_PORT=9090 → port = 9090
//!     .load()?;                                    // highest priority = env
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ### Resolve relative paths from a config file
//!
//! ```no_run
//! use star_toml::{Loader, ConfigFile};
//!
//! #[derive(serde::Deserialize)]
//! struct Build { template_dir: String }
//!
//! let cf: ConfigFile<Build> = Loader::new()
//!     .find_file("build.toml")
//!     .load_file()?;
//!
//! // Resolved relative to the directory that contains build.toml
//! let abs_dir = cf.resolve(&cf.config.template_dir);
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ### Load + validate in one call
//!
//! ```no_run
//! use star_toml::{Loader, Validate, Validator};
//!
//! #[derive(serde::Deserialize)]
//! struct ServerConfig { port: u16 }
//!
//! impl Validate for ServerConfig {
//!     fn validate(&self, v: &mut Validator) {
//!         v.check_range("port", self.port, 1..=65535);
//!     }
//! }
//!
//! // On failure this returns `Error::Invalid(ValidationErrors)` with the full report.
//! let cfg: ServerConfig = Loader::new()
//!     .find_file("server.toml")
//!     .load_validated()?;   // parse + validate in one call
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ## Layer merge semantics
//!
//! Layers are merged left-to-right (later = higher priority).
//! - **Tables**: keys are merged recursively — a later layer can override one nested
//!   key without erasing siblings.
//! - **Arrays, scalars**: later layer replaces earlier entirely.
//!
//! ```
//! use toml::Value;
//! use star_toml::deep_merge;
//!
//! let mut base: Value = toml::from_str("[db]\nhost=\"localhost\"\nport=5432\n").unwrap();
//! let overlay: Value = toml::from_str("[db]\nport=5433\n").unwrap();
//! deep_merge(&mut base, overlay);
//! // host is preserved, port is overridden
//! assert_eq!(base["db"]["host"].as_str(), Some("localhost"));
//! assert_eq!(base["db"]["port"].as_integer(), Some(5433));
//! ```
//!
//! ## Environment-variable expansion
//!
//! Every TOML source is scanned for `${VAR}` and `$VAR` patterns before parsing.
//! Unknown variables are left as-is (safe for partial expansion).
//!
//! ```
//! use star_toml::expand_env_vars;
//!
//! std::env::set_var("MY_HOST", "prod.example.com");
//! let out = expand_env_vars("host = \"${MY_HOST}\"");
//! assert_eq!(out, "host = \"prod.example.com\"");
//! std::env::remove_var("MY_HOST");
//! ```

pub mod error;
pub mod expand;
pub mod loader;
pub mod merge;
pub mod validation;

pub use error::{Error, Result};
pub use expand::expand_env_vars;
pub use loader::{
    find_and_load, find_config_file, from_str, load_file, save_file, to_string, ConfigFile, Loader,
};
pub use merge::deep_merge;
pub use validation::{
    ErrorKind, Loc, LocSegment, Validate, ValidationError, ValidationErrors, Validator,
};
