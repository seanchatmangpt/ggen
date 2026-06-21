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
//! | Layered loading | [`Loader`] | Builder that merges sources in priority order |
//! | Loaded config + path | [`ConfigFile<T>`] | Config value + source file path for resolving relative refs |
//! | Deep merge | [`deep_merge`] | Table-recursive TOML value merge |
//! | Env expansion | [`expand_env_vars`] | `${VAR}` / `$VAR` replacement |
//! | File discovery | [`find_config_file`] | Walk parent dirs to find `*.toml` |
//! | Self-validation | [`Validate`] | Trait for invariant-checking config types |
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
//! ### Self-validating configs
//!
//! ```no_run
//! use star_toml::{Loader, Validate, Error};
//!
//! #[derive(serde::Deserialize)]
//! struct ServerConfig { port: u16 }
//!
//! impl Validate for ServerConfig {
//!     fn validate(&self) -> Result<(), Error> {
//!         if self.port == 0 {
//!             return Err(Error::validation("ServerConfig", "port must be > 0"));
//!         }
//!         Ok(())
//!     }
//! }
//!
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
pub mod validate;

pub use error::{Error, Result};
pub use expand::expand_env_vars;
pub use loader::{find_and_load, find_config_file, from_str, load_file, ConfigFile, Loader};
pub use merge::deep_merge;
pub use validate::Validate;
