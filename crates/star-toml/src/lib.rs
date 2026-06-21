//! # star_toml — load, layer, and validate any `*.toml` config file
//!
//! `star_toml` (pronounced "star TOML", as in `*.toml`) is a self-contained
//! Rust framework for any TOML configuration file. Three concerns, one crate:
//!
//! 1. **Loading** — parse a single file, compose multiple sources in layers, expand
//!    `${ENV_VARS}`, walk parent dirs to discover config files.
//! 2. **Validation** — Pydantic-style engine that collects *every* error at once, each
//!    with a path-precise location, the offending value, and a machine-matchable code.
//! 3. **Analytics** — Van der Aalst-grade extensions: conformance fitness, variant
//!    fingerprinting, DECLARE cross-field constraints, severity stratification.
//!
//! Dependencies: `serde`, `toml`, `thiserror`. No async, no proc-macros, no allocator
//! tricks, no crypto.
//!
//! ---
//!
//! ## Decision guide — which API for which task?
//!
//! | Task | Use |
//! |------|-----|
//! | Parse a TOML string | [`from_str::<T>(s)`](from_str) |
//! | Load one file | [`load_file::<T>(path)`](load_file) |
//! | Walk parent dirs to find a file | [`find_config_file`] / [`find_and_load`] |
//! | Merge defaults + file + env | [`Loader`] builder |
//! | Remember where the file is (relative paths) | [`Loader::load_file`] → [`ConfigFile<T>`] |
//! | Parse + validate in one call | [`Loader::load_validated`] |
//! | Serialize back to TOML | [`to_string`] / [`save_file`] |
//! | Recursive table merge | [`deep_merge`] |
//! | `${VAR}` substitution alone | [`expand_env_vars`] |
//! | Validate an already-loaded struct | [`Validate::check`] / [`Validate::validated`] |
//! | Attach severity / cross-field rules | [`Validator::with_severity`] / [`Validator::check_consistent`] |
//! | Get conformance score | [`ValidationErrors::fitness`] |
//! | Fingerprint error pattern | [`ValidationErrors::variant_id`] |
//!
//! ---
//!
//! ## Complete public API
//!
//! ### Loading
//!
//! | Symbol | Kind | Description |
//! |--------|------|-------------|
//! | [`from_str`] | fn | Expand env vars, parse TOML, deserialize to `T` |
//! | [`load_file`] | fn | Read, expand, parse a `.toml` file |
//! | [`find_config_file`] | fn | Walk parent dirs for `name`; returns `Option<PathBuf>` |
//! | [`find_and_load`] | fn | [`find_config_file`] + [`load_file`] combined |
//! | [`Loader`] | struct | Builder: chain `.layer_str`, `.find_file`, `.env_prefix`, then `.load()` |
//! | [`ConfigFile<T>`] | struct | Loaded config + path; `.resolve(rel)` anchors relative paths |
//! | [`to_string`] | fn | Serialize `T: Serialize` to a TOML string |
//! | [`save_file`] | fn | Serialize to disk, creating parent dirs as needed |
//! | [`deep_merge`] | fn | Recursively merge two `toml::Value`s (later wins) |
//! | [`expand_env_vars`] | fn | `${VAR}` / `$VAR` substitution; unknown vars left intact |
//!
//! ### Validation (Pydantic + Van der Aalst)
//!
//! | Symbol | Kind | Description |
//! |--------|------|-------------|
//! | [`Validate`] | trait | Implement `validate(&self, v: &mut Validator)` on your config type |
//! | [`Validator`] | struct | Accumulates errors; carries the current path location |
//! | [`Validator::field`] | method | Descend into table key `name` for the duration of a closure |
//! | [`Validator::index`] | method | Descend into array index `i` for the duration of a closure |
//! | [`Validator::with_severity`] | method | Set [`Severity`] for all checks inside a closure |
//! | [`Validator::check_non_empty`] | method | Fail if `&str` is empty → code `empty` |
//! | [`Validator::check_range`] | method | Fail if value outside `lo..=hi` → code `out_of_range` |
//! | [`Validator::check_one_of`] | method | Fail if value not in allowed set → code `not_one_of` |
//! | [`Validator::check_predicate`] | method | Custom boolean check with caller-defined code |
//! | [`Validator::check_consistent`] | method | DECLARE cross-field constraint → code `Inconsistent` |
//! | [`Validator::error`] | method | Record a raw [`ErrorKind`] at the current location |
//! | [`Validator::error_with`] | method | Same, capturing the offending value as a string |
//! | [`Validator::finish`] | method | Consume validator → `Ok(())` or `Err(ValidationErrors)` |
//! | [`ValidationErrors`] | struct | Non-empty collection of failures; implements `Display` + `Error` |
//! | [`ValidationErrors::errors`] | method | Slice of individual [`ValidationError`]s |
//! | [`ValidationErrors::len`] | method | Error count |
//! | [`ValidationErrors::fitness`] | method | Conformance score: `checks_passed / checks_run` (0.0–1.0) |
//! | [`ValidationErrors::variant_id`] | method | FNV-1a fingerprint of the error pattern |
//! | [`ValidationErrors::by_section`] | method | Group errors by top-level config section |
//! | [`ValidationErrors::has_fatal`] | method | True if any error is `Severity::Fatal` |
//! | [`ValidationErrors::errors_above`] | method | Filter to errors at or above a severity level |
//! | [`ValidationError`] | struct | One failure: `loc`, `kind`, `severity`, `input`, `msg` |
//! | [`ValidationError::code`] | method | Stable string code (`"empty"`, `"out_of_range"`, …) |
//! | [`ValidationError::repair_hint`] | method | Auto-derived minimum fix for this error kind |
//! | [`ValidationError::is_fatal`] | method | True when `severity == Severity::Fatal` |
//! | [`ErrorKind`] | enum | Structured reason: `Empty`, `OutOfRange`, `NotOneOf`, `Inconsistent`, … |
//! | [`Loc`] | struct | Path into the config tree, e.g. `server.tls.port` or `[2].name` |
//! | [`LocSegment`] | enum | `Key(String)` or `Index(usize)` |
//! | [`Severity`] | enum | `Advisory` < `Warning` < `Error` < `Fatal` |
//!
//! ### Error types
//!
//! | Variant | When |
//! |---------|------|
//! | [`Error::FileNotFound`] | File does not exist |
//! | [`Error::Io`] | OS error reading / writing a file |
//! | [`Error::Parse`] | TOML syntax or serde deserialization error |
//! | [`Error::Serialize`] | `toml::to_string` failed |
//! | [`Error::Invalid`] | Wraps [`ValidationErrors`]; returned by [`Loader::load_validated`] |
//! | [`Error::Validation`] | Lower-level validation string; rarely produced by this crate |
//!
//! ---
//!
//! ## Patterns cookbook (copy-pasteable)
//!
//! ### Pattern 1 — load a single file
//!
//! ```no_run
//! use star_toml::load_file;
//!
//! #[derive(serde::Deserialize)]
//! struct AppConfig { name: String, port: u16 }
//!
//! let cfg: AppConfig = load_file("app.toml")?;
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ### Pattern 2 — layered loading (defaults + file + env)
//!
//! ```no_run
//! use star_toml::Loader;
//!
//! #[derive(serde::Deserialize)]
//! struct Cfg { name: String, port: u16 }
//!
//! const DEFAULTS: &str = "name = 'my-app'\nport = 8080\n";
//!
//! let cfg: Cfg = Loader::new()
//!     .layer_str(DEFAULTS, "built-in defaults")  // lowest priority
//!     .find_file("app.toml")                      // walk up from cwd
//!     .layer_file_if_exists("~/.config/app.toml") // optional user overrides
//!     .env_prefix("APP_")                         // APP_PORT=9090 → port = 9090
//!     .load()?;                                   // highest priority
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ### Pattern 3 — load + validate in one call
//!
//! ```no_run
//! use star_toml::{Loader, Validate, Validator};
//!
//! #[derive(serde::Deserialize)]
//! struct Cfg { port: u16 }
//!
//! impl Validate for Cfg {
//!     fn validate(&self, v: &mut Validator) {
//!         v.check_range("port", self.port, 1024..=65535);
//!     }
//! }
//!
//! // Fails with Error::Invalid(ValidationErrors) if any rule is violated.
//! let cfg: Cfg = Loader::new().find_file("server.toml").load_validated()?;
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ### Pattern 4 — collect all errors, render Pydantic-style
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
//!         v.field("server", |v| self.server.validate(v)); // nested → server.*
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
//! assert_eq!(report.len(), 4); // ALL four errors, not just the first
//!
//! let locs: Vec<String> = report.errors().iter().map(|e| e.loc.to_string()).collect();
//! assert_eq!(locs, ["name", "workers", "server.host", "server.port"]);
//!
//! // Renders as:
//! // 4 validation errors for App
//! // name
//! //   must not be empty (got: `""`) [empty]
//! // ...
//! println!("{report}");
//! ```
//!
//! ### Pattern 5 — severity levels + repair hints
//!
//! ```
//! use star_toml::{Validate, Validator, Severity};
//!
//! struct Cfg { log_dir: String, port: u16 }
//!
//! impl Validate for Cfg {
//!     fn validate(&self, v: &mut Validator) {
//!         // Hard error: port is required
//!         v.check_range("port", self.port, 1024..=65535);
//!         // Advisory: non-critical best-practice hint
//!         v.with_severity(Severity::Advisory, |v| {
//!             v.check_non_empty("log_dir", &self.log_dir);
//!         });
//!     }
//! }
//!
//! let errs = Cfg { port: 0, log_dir: String::new() }.check().unwrap_err();
//! assert!(!errs.has_fatal());
//!
//! for e in errs.errors() {
//!     println!("[{}] {} → fix: {}", e.severity, e.loc, e.repair_hint());
//! }
//! ```
//!
//! ### Pattern 6 — Van der Aalst analytics
//!
//! ```
//! use star_toml::{Validate, Validator};
//!
//! struct Pair { a: u32, b: u32 }
//! impl Validate for Pair {
//!     fn validate(&self, v: &mut Validator) {
//!         v.check_range("a", self.a, 1..=10); // passes
//!         v.check_range("b", self.b, 1..=10); // fails
//!     }
//! }
//!
//! let errs = Pair { a: 5, b: 0 }.check().unwrap_err();
//!
//! // Conformance fitness: how much of the model the config satisfies
//! assert_eq!(errs.fitness(), 0.5); // 1 of 2 checks passed
//!
//! // Variant fingerprint: same error pattern = same ID across runs
//! let id = errs.variant_id();
//! assert_eq!(Pair { a: 9, b: 0 }.check().unwrap_err().variant_id(), id);
//!
//! // Object-centric grouping: errors by top-level config section
//! for (section, errors) in errs.by_section() {
//!     println!("{section}: {} error(s)", errors.len());
//! }
//! ```
//!
//! ### Pattern 7 — DECLARE cross-field constraint
//!
//! ```
//! use star_toml::{Validate, Validator};
//!
//! struct Tls { enabled: bool, cert_path: String }
//!
//! impl Validate for Tls {
//!     fn validate(&self, v: &mut Validator) {
//!         // Co-existence: TLS enabled ⟺ cert_path non-empty
//!         v.check_consistent(
//!             "cert_path",            // primary field (where error is recorded)
//!             &["enabled"],           // related fields (tagged in ErrorKind::Inconsistent)
//!             !self.enabled || !self.cert_path.is_empty(),
//!             "tls_cert_required",    // stable machine-matchable code
//!             "cert_path must be set when TLS is enabled",
//!         );
//!     }
//! }
//!
//! let bad = Tls { enabled: true, cert_path: String::new() };
//! let errs = bad.check().unwrap_err();
//! assert_eq!(errs.errors()[0].code(), "tls_cert_required");
//! ```
//!
//! ### Pattern 8 — validate arrays element-by-element
//!
//! ```
//! use star_toml::{Validate, Validator};
//!
//! struct Pipeline(Vec<String>);
//!
//! impl Validate for Pipeline {
//!     fn validate(&self, v: &mut Validator) {
//!         for (i, stage) in self.0.iter().enumerate() {
//!             v.index(i, |v| v.check_non_empty("name", stage));
//!         }
//!     }
//! }
//!
//! let p = Pipeline(vec!["build".into(), String::new(), "test".into()]);
//! let errs = p.check().unwrap_err();
//! assert_eq!(errs.errors()[0].loc.to_string(), "[1].name");
//! ```
//!
//! ### Pattern 9 — source-relative path resolution
//!
//! ```no_run
//! use star_toml::{Loader, ConfigFile};
//!
//! #[derive(serde::Deserialize)]
//! struct Build { template_dir: String }
//!
//! let cf: ConfigFile<Build> = Loader::new().find_file("build.toml").load_file()?;
//! // Resolved relative to the directory that contains build.toml,
//! // not relative to the current working directory.
//! let abs = cf.resolve(&cf.config.template_dir);
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ### Pattern 10 — write-back
//!
//! ```no_run
//! use star_toml::save_file;
//!
//! #[derive(serde::Serialize)]
//! struct Config { name: String, port: u16 }
//!
//! save_file(&Config { name: "app".into(), port: 8080 }, "ggen.toml")?;
//! // Parent directories are created automatically.
//! # Ok::<(), star_toml::Error>(())
//! ```
//!
//! ---
//!
//! ## Layer merge semantics
//!
//! Layers merge left-to-right (later = higher priority).
//! - **Tables**: keys merged recursively — a later layer overrides one key without erasing siblings.
//! - **Arrays, scalars**: later layer replaces earlier entirely.
//!
//! ```
//! use toml::Value;
//! use star_toml::deep_merge;
//!
//! let mut base: Value = toml::from_str("[db]\nhost=\"localhost\"\nport=5432\n").unwrap();
//! let overlay: Value = toml::from_str("[db]\nport=5433\n").unwrap();
//! deep_merge(&mut base, overlay);
//! // host preserved, port overridden
//! assert_eq!(base["db"]["host"].as_str(), Some("localhost"));
//! assert_eq!(base["db"]["port"].as_integer(), Some(5433));
//! ```
//!
//! ## Environment variable mapping
//!
//! `.env_prefix("APP_")` maps env vars to TOML keys:
//! - Prefix stripped: `APP_PORT` → `port`
//! - Double underscore as dot: `APP_SERVER__PORT` → `server.port`
//! - Value coercion: `"true"` → bool, `"42"` → integer, `"3.14"` → float, otherwise string
//!
//! Inline `${VAR}` / `$VAR` patterns in TOML sources are expanded before parsing.
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
//!
//! ---
//!
//! ## Error handling
//!
//! All fallible free functions return [`Result<T>`] = `Result<T, Error>`.
//! The most common error is [`Error::Invalid`] from [`Loader::load_validated`];
//! downcast with `if let Error::Invalid(report) = err { … }`.
//!
//! ```no_run
//! use star_toml::{Loader, Validate, Validator, Error};
//!
//! #[derive(serde::Deserialize)]
//! struct Cfg { port: u16 }
//! impl Validate for Cfg {
//!     fn validate(&self, v: &mut Validator) {
//!         v.check_range("port", self.port, 1024..=65535);
//!     }
//! }
//!
//! match Loader::new().find_file("server.toml").load_validated::<Cfg>() {
//!     Ok(cfg) => println!("port = {}", cfg.port),
//!     Err(Error::FileNotFound(p)) => eprintln!("config not found: {}", p.display()),
//!     Err(Error::Parse { source, .. }) => eprintln!("TOML syntax: {source}"),
//!     Err(Error::Invalid(report)) => {
//!         eprintln!("{report}");
//!         for e in report.errors() {
//!             eprintln!("  fix {} → {}", e.loc, e.repair_hint());
//!         }
//!     }
//!     Err(e) => eprintln!("other error: {e}"),
//! }
//! # Ok::<(), star_toml::Error>(())
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
    ErrorKind, Loc, LocSegment, Severity, Validate, ValidationError, ValidationErrors, Validator,
};
