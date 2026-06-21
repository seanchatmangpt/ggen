//! Pydantic-grade validation for TOML configs.
//!
//! The defining idea, borrowed from [Pydantic](https://docs.pydantic.dev/): a config
//! type is validated by *descending through its structure*, and **every** failure is
//! collected — not just the first — each carrying:
//!
//! - a precise **location** (`server.tls.port`, `stages[2].name`),
//! - the **offending value**, and
//! - a machine-matchable **error code**.
//!
//! You implement [`Validate`] for your type, using a [`Validator`] that tracks the
//! current location as you descend. Nested types compose: a parent calls
//! [`Validator::field`] and delegates to the child's `validate`.
//!
//! ```
//! use star_toml::{Validate, Validator};
//!
//! struct Server { host: String, port: u16 }
//!
//! impl Validate for Server {
//!     fn validate(&self, v: &mut Validator) {
//!         v.check_non_empty("host", &self.host);
//!         v.check_range("port", self.port, 1..=65535);
//!     }
//! }
//!
//! let bad = Server { host: String::new(), port: 0 };
//! let errs = bad.check().unwrap_err();
//! assert_eq!(errs.len(), 2);                       // both errors collected
//! assert_eq!(errs.errors()[0].loc.to_string(), "host");
//! assert_eq!(errs.errors()[1].code(), "out_of_range");
//! ```

use std::fmt;
use std::ops::RangeInclusive;

// ---------------------------------------------------------------------------
// Location — a path into the config tree
// ---------------------------------------------------------------------------

/// One segment of a [`Loc`]: either a table key or an array index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LocSegment {
    /// A table key, e.g. `server` in `server.port`.
    Key(String),
    /// An array index, e.g. `2` in `stages[2]`.
    Index(usize),
}

/// A path to a value in the config tree, rendered like `server.tls.port` or `stages[2].name`.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Loc(Vec<LocSegment>);

impl Loc {
    /// The segments making up this location, outermost first.
    #[must_use]
    pub fn segments(&self) -> &[LocSegment] {
        &self.0
    }

    /// True for a root-level (whole-model) location with no segments.
    #[must_use]
    pub fn is_root(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "(root)");
        }
        for (i, seg) in self.0.iter().enumerate() {
            match seg {
                LocSegment::Key(k) => {
                    if i > 0 {
                        write!(f, ".")?;
                    }
                    write!(f, "{k}")?;
                }
                LocSegment::Index(n) => write!(f, "[{n}]")?,
            }
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// ErrorKind — structured, machine-matchable
// ---------------------------------------------------------------------------

/// The structured reason a value failed validation.
///
/// Each variant maps to a stable [`code`](ErrorKind::code) string (Pydantic's "type"),
/// suitable for programmatic matching, while carrying the specifics inline.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    /// A required value was absent.
    Missing,
    /// A string/collection was empty but must not be.
    Empty,
    /// A number fell outside the allowed range.
    OutOfRange {
        /// Inclusive lower bound, if any.
        lower: Option<String>,
        /// Inclusive upper bound, if any.
        upper: Option<String>,
    },
    /// A string/collection was shorter than allowed.
    TooShort {
        /// Minimum length.
        min: usize,
        /// Actual length.
        actual: usize,
    },
    /// A string/collection was longer than allowed.
    TooLong {
        /// Maximum length.
        max: usize,
        /// Actual length.
        actual: usize,
    },
    /// A value was not among the permitted choices.
    NotOneOf {
        /// The permitted values.
        allowed: Vec<String>,
    },
    /// A custom predicate failed; `code` is a caller-chosen stable identifier.
    Predicate {
        /// Stable, caller-defined error code.
        code: &'static str,
    },
}

impl ErrorKind {
    /// A stable, machine-matchable code for this error kind (Pydantic's error "type").
    #[must_use]
    pub fn code(&self) -> &str {
        match self {
            Self::Missing => "missing",
            Self::Empty => "empty",
            Self::OutOfRange { .. } => "out_of_range",
            Self::TooShort { .. } => "too_short",
            Self::TooLong { .. } => "too_long",
            Self::NotOneOf { .. } => "not_one_of",
            Self::Predicate { code } => code,
        }
    }
}

// ---------------------------------------------------------------------------
// ValidationError — one failure
// ---------------------------------------------------------------------------

/// A single validation failure at a precise [`Loc`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationError {
    /// Where in the config tree the failure occurred.
    pub loc: Loc,
    /// The structured reason.
    pub kind: ErrorKind,
    /// The offending value, stringified, if it was captured.
    pub input: Option<String>,
    /// Human-readable message.
    pub msg: String,
}

impl ValidationError {
    /// Shorthand for `self.kind.code()`.
    #[must_use]
    pub fn code(&self) -> &str {
        self.kind.code()
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n  {}", self.loc, self.msg)?;
        if let Some(input) = &self.input {
            write!(f, " (got: `{input}`)")?;
        }
        write!(f, " [{}]", self.code())
    }
}

// ---------------------------------------------------------------------------
// ValidationErrors — the collected report
// ---------------------------------------------------------------------------

/// A non-empty collection of [`ValidationError`]s, rendered as a Pydantic-style report.
///
/// ```text
/// 2 validation errors for Server
/// host
///   must not be empty [empty]
/// port
///   input must be in range 1..=65535 (got: `0`) [out_of_range]
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationErrors {
    errors: Vec<ValidationError>,
    title: Option<String>,
}

impl ValidationErrors {
    /// The individual errors, in the order they were discovered (depth-first).
    #[must_use]
    pub fn errors(&self) -> &[ValidationError] {
        &self.errors
    }

    /// Number of collected errors (always ≥ 1).
    #[must_use]
    pub fn len(&self) -> usize {
        self.errors.len()
    }

    /// Always `false` — `ValidationErrors` only exists when there is ≥ 1 error.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    /// The model/type name this report is about, if set.
    #[must_use]
    pub fn title(&self) -> Option<&str> {
        self.title.as_deref()
    }

    /// Set the report title to the short name of `T` (e.g. `ServerConfig`).
    pub fn set_title_for<T: ?Sized>(&mut self) {
        let full = std::any::type_name::<T>();
        let short = full.rsplit("::").next().unwrap_or(full);
        self.title = Some(short.to_string());
    }
}

impl fmt::Display for ValidationErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n = self.errors.len();
        let noun = if n == 1 { "error" } else { "errors" };
        match &self.title {
            Some(t) => writeln!(f, "{n} validation {noun} for {t}")?,
            None => writeln!(f, "{n} validation {noun}")?,
        }
        for (i, err) in self.errors.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{err}")?;
        }
        Ok(())
    }
}

impl std::error::Error for ValidationErrors {}

// ---------------------------------------------------------------------------
// Validator — the descent context
// ---------------------------------------------------------------------------

/// Accumulates errors while tracking the current location as you descend a config tree.
///
/// Obtain one via [`Validate::check`] / [`Validate::validated`], or construct directly
/// with [`Validator::new`] for ad-hoc validation. Use [`field`](Validator::field) and
/// [`index`](Validator::index) to descend; the `check_*` helpers record errors at the
/// named sub-location with the offending value attached.
#[derive(Debug, Default)]
pub struct Validator {
    loc: Vec<LocSegment>,
    errors: Vec<ValidationError>,
}

impl Validator {
    /// A fresh validator positioned at the root.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Descend into table key `name`, run `f`, then pop back out.
    ///
    /// Any errors recorded inside `f` are prefixed with `name`.
    pub fn field(&mut self, name: &str, f: impl FnOnce(&mut Validator)) {
        self.loc.push(LocSegment::Key(name.to_string()));
        f(self);
        self.loc.pop();
    }

    /// Descend into array index `i`, run `f`, then pop back out.
    pub fn index(&mut self, i: usize, f: impl FnOnce(&mut Validator)) {
        self.loc.push(LocSegment::Index(i));
        f(self);
        self.loc.pop();
    }

    /// Record an error at the current location.
    pub fn error(&mut self, kind: ErrorKind, msg: impl Into<String>) {
        self.record(kind, None, msg.into());
    }

    /// Record an error at the current location, capturing an offending value.
    pub fn error_with(&mut self, kind: ErrorKind, input: impl fmt::Display, msg: impl Into<String>) {
        self.record(kind, Some(input.to_string()), msg.into());
    }

    /// Fail subfield `field` with [`ErrorKind::Empty`] if `value` is empty.
    pub fn check_non_empty(&mut self, field: &str, value: &str) {
        if value.is_empty() {
            self.at(field, |v| {
                v.error_with(ErrorKind::Empty, "\"\"", "must not be empty");
            });
        }
    }

    /// Fail subfield `field` with [`ErrorKind::OutOfRange`] if `value ∉ range`.
    pub fn check_range<T>(&mut self, field: &str, value: T, range: RangeInclusive<T>)
    where
        T: PartialOrd + fmt::Display + Copy,
    {
        if !range.contains(&value) {
            let (lo, hi) = (range.start().to_string(), range.end().to_string());
            let msg = format!("input must be in range {lo}..={hi}");
            self.at(field, |v| {
                v.error_with(
                    ErrorKind::OutOfRange {
                        lower: Some(lo),
                        upper: Some(hi),
                    },
                    value,
                    msg,
                );
            });
        }
    }

    /// Fail subfield `field` with [`ErrorKind::NotOneOf`] if `value` is not in `allowed`.
    pub fn check_one_of(&mut self, field: &str, value: &str, allowed: &[&str]) {
        if !allowed.contains(&value) {
            let allowed_owned: Vec<String> = allowed.iter().map(|s| (*s).to_string()).collect();
            let msg = format!("must be one of: {}", allowed.join(", "));
            self.at(field, |v| {
                v.error_with(
                    ErrorKind::NotOneOf {
                        allowed: allowed_owned,
                    },
                    value,
                    msg,
                );
            });
        }
    }

    /// Fail subfield `field` with a caller-defined `code` when `passed` is false.
    ///
    /// The escape hatch for arbitrary cross-field or domain rules.
    pub fn check_predicate(
        &mut self,
        field: &str,
        passed: bool,
        code: &'static str,
        msg: impl Into<String>,
    ) {
        if !passed {
            let msg = msg.into();
            self.at(field, |v| v.error(ErrorKind::Predicate { code }, msg));
        }
    }

    /// Consume the validator, yielding `Ok(())` if no errors were recorded.
    ///
    /// # Errors
    ///
    /// Returns [`ValidationErrors`] containing every recorded failure.
    pub fn finish(self) -> Result<(), ValidationErrors> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(ValidationErrors {
                errors: self.errors,
                title: None,
            })
        }
    }

    // -- internal ----------------------------------------------------------

    fn at(&mut self, field: &str, f: impl FnOnce(&mut Validator)) {
        self.field(field, f);
    }

    fn record(&mut self, kind: ErrorKind, input: Option<String>, msg: String) {
        self.errors.push(ValidationError {
            loc: Loc(self.loc.clone()),
            kind,
            input,
            msg,
        });
    }
}

// ---------------------------------------------------------------------------
// Validate trait
// ---------------------------------------------------------------------------

/// Implemented by config types that can check their own invariants.
///
/// Implement [`validate`](Validate::validate) — record failures into the [`Validator`].
/// The provided [`check`](Validate::check) and [`validated`](Validate::validated) methods
/// run it and produce a titled [`ValidationErrors`] report.
///
/// Compose nested types with [`Validator::field`]:
///
/// ```
/// use star_toml::{Validate, Validator};
///
/// struct Tls { cert_path: String }
/// struct Server { port: u16, tls: Option<Tls> }
///
/// impl Validate for Tls {
///     fn validate(&self, v: &mut Validator) {
///         v.check_non_empty("cert_path", &self.cert_path);
///     }
/// }
/// impl Validate for Server {
///     fn validate(&self, v: &mut Validator) {
///         v.check_range("port", self.port, 1..=65535);
///         if let Some(tls) = &self.tls {
///             v.field("tls", |v| tls.validate(v));   // nested errors → tls.cert_path
///         }
///     }
/// }
///
/// let s = Server { port: 0, tls: Some(Tls { cert_path: String::new() }) };
/// let errs = s.check().unwrap_err();
/// let locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
/// assert_eq!(locs, ["port", "tls.cert_path"]);
/// ```
pub trait Validate {
    /// Record any invariant violations into `v`.
    fn validate(&self, v: &mut Validator);

    /// Run validation, returning a titled error report on failure.
    ///
    /// # Errors
    ///
    /// Returns [`ValidationErrors`] if any invariant is violated.
    fn check(&self) -> Result<(), ValidationErrors> {
        let mut v = Validator::new();
        self.validate(&mut v);
        v.finish().map_err(|mut errs| {
            errs.set_title_for::<Self>();
            errs
        })
    }

    /// Like [`check`](Validate::check) but consumes `self` and returns it on success —
    /// handy for `let cfg = raw.validated()?;` pipelines.
    ///
    /// # Errors
    ///
    /// Returns [`ValidationErrors`] if any invariant is violated.
    fn validated(self) -> Result<Self, ValidationErrors>
    where
        Self: Sized,
    {
        match self.check() {
            Ok(()) => Ok(self),
            Err(errs) => Err(errs),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    struct Tls {
        cert_path: String,
        key_path: String,
    }
    struct Server {
        host: String,
        port: u16,
        tls: Option<Tls>,
    }
    struct App {
        name: String,
        workers: u32,
        log_level: String,
        server: Server,
    }

    impl Validate for Tls {
        fn validate(&self, v: &mut Validator) {
            v.check_non_empty("cert_path", &self.cert_path);
            v.check_non_empty("key_path", &self.key_path);
        }
    }
    impl Validate for Server {
        fn validate(&self, v: &mut Validator) {
            v.check_non_empty("host", &self.host);
            v.check_range("port", self.port, 1..=65535);
            if let Some(tls) = &self.tls {
                v.field("tls", |v| tls.validate(v));
            }
        }
    }
    impl Validate for App {
        fn validate(&self, v: &mut Validator) {
            v.check_non_empty("name", &self.name);
            v.check_range("workers", self.workers, 1..=1024);
            v.check_one_of("log_level", &self.log_level, &["trace", "debug", "info", "warn", "error"]);
            v.field("server", |v| self.server.validate(v));
        }
    }

    fn valid_app() -> App {
        App {
            name: "demo".into(),
            workers: 8,
            log_level: "info".into(),
            server: Server {
                host: "localhost".into(),
                port: 8080,
                tls: None,
            },
        }
    }

    #[test]
    fn valid_config_passes() {
        assert!(valid_app().check().is_ok());
    }

    #[test]
    fn collects_all_errors_not_just_first() {
        let app = App {
            name: String::new(),         // error 1: name empty
            workers: 0,                  // error 2: workers out of range
            log_level: "verbose".into(), // error 3: log_level not one of
            server: Server {
                host: String::new(), // error 4: host empty
                port: 0,             // error 5: port out of range
                tls: None,
            },
        };
        let errs = app.check().unwrap_err();
        assert_eq!(errs.len(), 5);
    }

    #[test]
    fn locations_are_path_precise() {
        let app = App {
            server: Server {
                host: "ok".into(),
                port: 0,
                tls: Some(Tls {
                    cert_path: String::new(),
                    key_path: "key.pem".into(),
                }),
            },
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        let locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(locs.contains(&"server.port".to_string()));
        assert!(locs.contains(&"server.tls.cert_path".to_string()));
    }

    #[test]
    fn error_codes_are_machine_matchable() {
        let app = App {
            log_level: "nope".into(),
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        assert_eq!(errs.errors()[0].code(), "not_one_of");
        match &errs.errors()[0].kind {
            ErrorKind::NotOneOf { allowed } => assert!(allowed.contains(&"info".to_string())),
            other => panic!("expected NotOneOf, got {other:?}"),
        }
    }

    #[test]
    fn captured_input_value_present() {
        let app = App {
            workers: 9999,
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        assert_eq!(errs.errors()[0].input.as_deref(), Some("9999"));
    }

    #[test]
    fn report_has_title_and_is_pretty() {
        let app = App {
            name: String::new(),
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        let report = errs.to_string();
        assert!(report.starts_with("1 validation error for App"));
        assert!(report.contains("name"));
        assert!(report.contains("[empty]"));
    }

    #[test]
    fn index_segments_render_with_brackets() {
        struct Stages(Vec<String>);
        impl Validate for Stages {
            fn validate(&self, v: &mut Validator) {
                for (i, name) in self.0.iter().enumerate() {
                    v.index(i, |v| v.check_non_empty("name", name));
                }
            }
        }
        let stages = Stages(vec!["ok".into(), String::new()]);
        let errs = stages.check().unwrap_err();
        assert_eq!(errs.errors()[0].loc.to_string(), "[1].name");
    }

    #[test]
    fn root_level_error_renders_as_root() {
        struct Thing;
        impl Validate for Thing {
            fn validate(&self, v: &mut Validator) {
                v.error(ErrorKind::Predicate { code: "always" }, "always fails");
            }
        }
        let errs = Thing.check().unwrap_err();
        assert_eq!(errs.errors()[0].loc.to_string(), "(root)");
        assert!(errs.errors()[0].loc.is_root());
    }
}
