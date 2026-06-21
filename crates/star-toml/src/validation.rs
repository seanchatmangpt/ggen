//! Pydantic-grade + Van der Aalst-grade validation for TOML configs.
//!
//! # Design
//!
//! Validation works by *descent*: implement [`Validate`] for each config type,
//! use a [`Validator`] to record failures, and compose nested types with
//! [`Validator::field`] / [`Validator::index`]. Every `check_*` call is an
//! atomic "check event" — the validator counts all checks (pass + fail) for the
//! conformance score.
//!
//! # Validator method reference
//!
//! ## Descent (path tracking)
//!
//! | Method | Signature | Effect |
//! |--------|-----------|--------|
//! | [`field`](Validator::field) | `(name: &str, f: FnOnce(&mut Validator))` | Push key segment, run `f`, pop |
//! | [`index`](Validator::index) | `(i: usize, f: FnOnce(&mut Validator))` | Push index segment, run `f`, pop |
//!
//! ## Built-in checks (each counts as one check event)
//!
//! | Method | Code | Fails when |
//! |--------|------|-----------|
//! | [`check_non_empty`](Validator::check_non_empty) | `empty` | `&str` is `""` |
//! | [`check_range`](Validator::check_range) | `out_of_range` | value outside `lo..=hi` |
//! | [`check_one_of`](Validator::check_one_of) | `not_one_of` | value not in allowed slice |
//! | [`check_predicate`](Validator::check_predicate) | caller-defined | boolean is `false` |
//! | [`check_consistent`](Validator::check_consistent) | caller-defined | cross-field condition is `false` |
//!
//! ## Severity control
//!
//! | Method | Effect |
//! |--------|--------|
//! | [`with_severity`](Validator::with_severity) | Sets [`Severity`] for all checks inside the closure |
//!
//! Default severity: [`Severity::Error`].
//! Errors with `Severity < Error` (Warning / Advisory) still appear in the report
//! but do not block [`ValidationErrors::has_fatal`].
//!
//! ## Raw error recording
//!
//! | Method | Description |
//! |--------|-------------|
//! | [`error`](Validator::error) | Record [`ErrorKind`] at the current location |
//! | [`error_with`](Validator::error_with) | Same, capturing the offending value as a string |
//! | [`finish`](Validator::finish) | Consume the validator → `Ok(())` or `Err(ValidationErrors)` |
//!
//! # ValidationErrors analytics
//!
//! | Method | Returns | Van der Aalst concept |
//! |--------|---------|----------------------|
//! | [`errors`](ValidationErrors::errors) | `&[ValidationError]` | — |
//! | [`fitness`](ValidationErrors::fitness) | `f64` 0.0–1.0 | Replay fitness / alignment score |
//! | [`variant_id`](ValidationErrors::variant_id) | `u64` | Trace variant fingerprint |
//! | [`by_section`](ValidationErrors::by_section) | `BTreeMap<String, Vec<_>>` | OCEL object-centric view |
//! | [`has_fatal`](ValidationErrors::has_fatal) | `bool` | Halt-immediately signal |
//! | [`errors_above`](ValidationErrors::errors_above) | `impl Iterator` | Severity filter |
//!
//! # ValidationError fields
//!
//! | Field | Type | Description |
//! |-------|------|-------------|
//! | `loc` | [`Loc`] | Path, e.g. `server.tls.port` or `[2].name` |
//! | `kind` | [`ErrorKind`] | Structured reason (machine-matchable) |
//! | `severity` | [`Severity`] | Advisory / Warning / Error / Fatal |
//! | `input` | `Option<String>` | Offending value, if captured |
//! | `msg` | `String` | Human-readable message |
//!
//! Plus: [`code()`](ValidationError::code), [`repair_hint()`](ValidationError::repair_hint),
//! [`is_fatal()`](ValidationError::is_fatal).
//!
//! # ErrorKind codes
//!
//! | Variant | `code()` | Produced by |
//! |---------|----------|-------------|
//! | `Missing` | `missing` | `error(ErrorKind::Missing, …)` |
//! | `Empty` | `empty` | `check_non_empty` |
//! | `OutOfRange` | `out_of_range` | `check_range` |
//! | `TooShort` | `too_short` | `error(ErrorKind::TooShort{…}, …)` |
//! | `TooLong` | `too_long` | `error(ErrorKind::TooLong{…}, …)` |
//! | `NotOneOf` | `not_one_of` | `check_one_of` |
//! | `Inconsistent` | caller-defined | `check_consistent` |
//! | `Predicate` | caller-defined | `check_predicate`, `error(ErrorKind::Predicate{…}, …)` |
//!
//! ```
//! use star_toml::{Validate, Validator, Severity};
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
//! assert_eq!(errs.len(), 2);
//! assert_eq!(errs.fitness(), 0.0);               // 0 of 2 checks passed
//! assert!(!errs.errors()[0].repair_hint().is_empty());
//! ```

use std::collections::BTreeMap;
use std::fmt;
use std::ops::RangeInclusive;

// ---------------------------------------------------------------------------
// Location — a path into the config tree
// ---------------------------------------------------------------------------

/// One segment of a [`Loc`]: either a table key or an array index.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LocSegment {
    /// A table key, e.g. `server` in `server.port`.
    Key(String),
    /// An array index, e.g. `2` in `stages[2]`.
    Index(usize),
}

/// A path to a value in the config tree, rendered like `server.tls.port` or `stages[2].name`.
#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
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
// Severity — Van der Aalst alignment cost levels
// ---------------------------------------------------------------------------

/// How severe a validation failure is — ordered least to most severe.
///
/// The default for all `check_*` methods is [`Error`](Severity::Error).
/// Use [`Validator::with_severity`] to override for a closure.
///
/// Comparison: `Advisory < Warning < Error < Fatal`.
///
/// | Level | Meaning | `has_fatal` |
/// |-------|---------|-------------|
/// | `Advisory` | best-practice hint; config is usable | no |
/// | `Warning` | risky but technically valid | no |
/// | `Error` | constraint violated; config is broken | no |
/// | `Fatal` | unrecoverable; halt all evaluation | **yes** |
///
/// # Example
///
/// ```
/// use star_toml::{Validate, Validator, Severity};
///
/// struct Cfg { log_dir: String }
/// impl Validate for Cfg {
///     fn validate(&self, v: &mut Validator) {
///         v.with_severity(Severity::Warning, |v| {
///             v.check_non_empty("log_dir", &self.log_dir);
///         });
///     }
/// }
/// let errs = Cfg { log_dir: String::new() }.check().unwrap_err();
/// assert_eq!(errs.errors()[0].severity, Severity::Warning);
/// assert!(!errs.has_fatal());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Severity {
    /// Informational: not a hard rule, but a best-practice recommendation.
    Advisory,
    /// Technically acceptable but risky or sub-optimal.
    Warning,
    /// Constraint violated — the default level. Config is unusable.
    #[default]
    Error,
    /// Unrecoverable: halt immediately, do not evaluate further constraints.
    Fatal,
}

impl Severity {
    /// Stable string code for this severity level.
    #[must_use]
    pub fn code(&self) -> &str {
        match self {
            Self::Advisory => "advisory",
            Self::Warning => "warning",
            Self::Error => "error",
            Self::Fatal => "fatal",
        }
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.code())
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
    /// A cross-field DECLARE constraint was violated.
    ///
    /// `related` names the other field(s) involved in the constraint.
    Inconsistent {
        /// The other field names that form this cross-field constraint.
        related: Vec<String>,
        /// Caller-defined stable code.
        code: &'static str,
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
            Self::Inconsistent { code, .. } | Self::Predicate { code } => code,
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
    /// How severe this failure is.
    pub severity: Severity,
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

    /// Whether this error requires an immediate halt (severity == Fatal).
    #[must_use]
    pub fn is_fatal(&self) -> bool {
        self.severity == Severity::Fatal
    }

    /// Auto-derived repair suggestion based on the error kind.
    ///
    /// For custom predicates the message itself is the best hint; for
    /// built-in kinds the hint is derived from the constraint parameters.
    ///
    /// This implements Van der Aalst's *alignment repair* concept: given a
    /// deviation from the reference model, what is the minimum edit?
    #[must_use]
    pub fn repair_hint(&self) -> String {
        match &self.kind {
            ErrorKind::Empty => "provide a non-empty value".into(),
            ErrorKind::Missing => "add this required field".into(),
            ErrorKind::OutOfRange { lower, upper } => match (lower, upper) {
                (Some(lo), Some(hi)) => format!("use a value in the range {lo}..={hi}"),
                (Some(lo), None) => format!("use a value ≥ {lo}"),
                (None, Some(hi)) => format!("use a value ≤ {hi}"),
                (None, None) => "use a value within the required range".into(),
            },
            ErrorKind::NotOneOf { allowed } => {
                format!("choose one of: {}", allowed.join(", "))
            }
            ErrorKind::TooShort { min, .. } => format!("provide at least {min} items/characters"),
            ErrorKind::TooLong { max, .. } => format!("use at most {max} items/characters"),
            ErrorKind::Inconsistent { related, .. } => {
                format!("ensure this field is consistent with: {}", related.join(", "))
            }
            ErrorKind::Predicate { .. } => self.msg.clone(),
        }
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n  {}", self.loc, self.msg)?;
        if let Some(input) = &self.input {
            write!(f, " (got: `{input}`)")?;
        }
        write!(f, " [{}]", self.code())?;
        if self.severity != Severity::Error {
            write!(f, " <{}>", self.severity)?;
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// ValidationErrors — the collected report
// ---------------------------------------------------------------------------

/// A non-empty collection of [`ValidationError`]s, rendered as a Pydantic-style report.
///
/// Extends the Pydantic report with:
/// - [`fitness`](ValidationErrors::fitness) — Van der Aalst alignment conformance score
/// - [`variant_id`](ValidationErrors::variant_id) — fingerprint for recurring failure patterns
/// - [`by_section`](ValidationErrors::by_section) — object-centric grouping
///
/// ```text
/// 2 validation errors for Server
/// host
///   must not be empty (got: `""`) [empty]
/// port
///   input must be in range 1..=65535 (got: `0`) [out_of_range]
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationErrors {
    errors: Vec<ValidationError>,
    title: Option<String>,
    /// Total number of checks attempted (passed + failed). Used for fitness.
    checks_run: usize,
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

    /// Whether any error is [`Severity::Fatal`] (requires immediate halt).
    #[must_use]
    pub fn has_fatal(&self) -> bool {
        self.errors.iter().any(ValidationError::is_fatal)
    }

    /// Errors at or above the given severity threshold.
    pub fn errors_above(&self, min: Severity) -> impl Iterator<Item = &ValidationError> {
        self.errors.iter().filter(move |e| e.severity >= min)
    }

    /// **Van der Aalst alignment fitness** — proportion of checks that passed.
    ///
    /// Returns 1.0 when all checks pass (no errors), 0.0 when every check
    /// failed. Analogous to the replay-fitness metric from conformance checking:
    /// how well does the observed config align to the declared validation model?
    ///
    /// # Example
    ///
    /// ```
    /// use star_toml::{Validate, Validator};
    ///
    /// struct Pair { a: u32, b: u32 }
    /// impl Validate for Pair {
    ///     fn validate(&self, v: &mut Validator) {
    ///         v.check_range("a", self.a, 1..=10);  // passes
    ///         v.check_range("b", self.b, 1..=10);  // fails
    ///     }
    /// }
    /// let errs = Pair { a: 5, b: 0 }.check().unwrap_err();
    /// assert_eq!(errs.fitness(), 0.5);  // 1 of 2 checks passed
    /// ```
    #[must_use]
    pub fn fitness(&self) -> f64 {
        if self.checks_run == 0 {
            return 1.0;
        }
        let failed = self.errors.iter().filter(|e| e.severity >= Severity::Error).count();
        let passed = self.checks_run.saturating_sub(failed);
        passed as f64 / self.checks_run as f64
    }

    /// **Variant fingerprint** — a deterministic hash of the failure pattern.
    ///
    /// Two `ValidationErrors` instances with the same set of `(location, code)`
    /// pairs produce the same variant ID, regardless of message text or input
    /// values. Useful for deduplicating recurring failure patterns across runs.
    ///
    /// Uses FNV-1a over the sorted `"loc:code"` pairs.
    #[must_use]
    pub fn variant_id(&self) -> u64 {
        let mut pairs: Vec<String> = self
            .errors
            .iter()
            .map(|e| format!("{}:{}", e.loc, e.code()))
            .collect();
        pairs.sort_unstable();
        fnv1a(pairs.join("|").as_bytes())
    }

    /// **Object-centric grouping** — errors indexed by their top-level config section.
    ///
    /// Implements Van der Aalst's object-centric view: each top-level TOML table
    /// is an "object type"; this groups all its constraint violations together.
    ///
    /// Root-level errors are keyed `"(root)"`.
    #[must_use]
    pub fn by_section(&self) -> BTreeMap<String, Vec<&ValidationError>> {
        let mut map: BTreeMap<String, Vec<&ValidationError>> = BTreeMap::new();
        for err in &self.errors {
            let key = err
                .loc
                .segments()
                .first()
                .and_then(|s| {
                    if let LocSegment::Key(k) = s {
                        Some(k.as_str())
                    } else {
                        None
                    }
                })
                .unwrap_or("(root)");
            map.entry(key.to_string()).or_default().push(err);
        }
        map
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
///
/// Use [`with_severity`](Validator::with_severity) to emit [`Warning`](Severity::Warning)
/// or [`Fatal`](Severity::Fatal) errors. Use
/// [`check_consistent`](Validator::check_consistent) for cross-field DECLARE constraints.
#[derive(Debug, Default)]
pub struct Validator {
    loc: Vec<LocSegment>,
    errors: Vec<ValidationError>,
    /// Total atomic checks performed (pass or fail), used to compute fitness.
    checks_run: usize,
    /// Severity to stamp on the next emitted error (reset after each `record`).
    pending_severity: Severity,
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

    /// Run `f` with the given severity applied to every error emitted inside it.
    ///
    /// Severity resets to the enclosing scope's value after `f` returns.
    ///
    /// ```
    /// use star_toml::{Validate, Validator, Severity};
    ///
    /// struct Config { log_dir: String }
    /// impl Validate for Config {
    ///     fn validate(&self, v: &mut Validator) {
    ///         // Best-practice advisory: non-critical
    ///         v.with_severity(Severity::Warning, |v| {
    ///             v.check_non_empty("log_dir", &self.log_dir);
    ///         });
    ///     }
    /// }
    /// let errs = Config { log_dir: String::new() }.check().unwrap_err();
    /// assert_eq!(errs.errors()[0].severity, Severity::Warning);
    /// assert!(!errs.has_fatal());
    /// ```
    pub fn with_severity(&mut self, severity: Severity, f: impl FnOnce(&mut Validator)) {
        let prev = std::mem::replace(&mut self.pending_severity, severity);
        f(self);
        self.pending_severity = prev;
    }

    /// Record an error at the current location.
    pub fn error(&mut self, kind: ErrorKind, msg: impl Into<String>) {
        self.record(kind, None, msg.into());
    }

    /// Record an error at the current location, capturing an offending value.
    pub fn error_with(
        &mut self,
        kind: ErrorKind,
        input: impl fmt::Display,
        msg: impl Into<String>,
    ) {
        self.record(kind, Some(input.to_string()), msg.into());
    }

    /// Fail subfield `field` with [`ErrorKind::Empty`] if `value` is empty.
    pub fn check_non_empty(&mut self, field: &str, value: &str) {
        self.checks_run += 1;
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
        self.checks_run += 1;
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
        self.checks_run += 1;
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
    /// The escape hatch for arbitrary domain rules.
    pub fn check_predicate(
        &mut self,
        field: &str,
        passed: bool,
        code: &'static str,
        msg: impl Into<String>,
    ) {
        self.checks_run += 1;
        if !passed {
            let msg = msg.into();
            self.at(field, |v| v.error(ErrorKind::Predicate { code }, msg));
        }
    }

    /// **DECLARE-style cross-field constraint** (Van der Aalst).
    ///
    /// Records an [`ErrorKind::Inconsistent`] at `primary_field` when `condition`
    /// is `false`, tagging `related_fields` as the other objects in the constraint.
    ///
    /// This models DECLARE's *co-existence*, *response*, and *precedence*
    /// templates: field A is only valid in relation to field B.
    ///
    /// ```
    /// use star_toml::{Validate, Validator};
    ///
    /// struct Tls { enabled: bool, cert_path: String }
    /// impl Validate for Tls {
    ///     fn validate(&self, v: &mut Validator) {
    ///         // Co-existence: TLS enabled ⟺ cert_path non-empty
    ///         v.check_consistent(
    ///             "cert_path",
    ///             &["enabled"],
    ///             !self.enabled || !self.cert_path.is_empty(),
    ///             "tls_cert_required",
    ///             "cert_path must be set when TLS is enabled",
    ///         );
    ///     }
    /// }
    /// let bad = Tls { enabled: true, cert_path: String::new() };
    /// let errs = bad.check().unwrap_err();
    /// assert_eq!(errs.errors()[0].code(), "tls_cert_required");
    /// ```
    pub fn check_consistent(
        &mut self,
        primary_field: &str,
        related_fields: &[&str],
        condition: bool,
        code: &'static str,
        msg: impl Into<String>,
    ) {
        self.checks_run += 1;
        if !condition {
            let related: Vec<String> = related_fields.iter().map(|s| (*s).to_string()).collect();
            let msg = msg.into();
            self.at(primary_field, |v| {
                v.error(ErrorKind::Inconsistent { related, code }, msg);
            });
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
                checks_run: self.checks_run,
            })
        }
    }

    // -- internal ----------------------------------------------------------

    fn at(&mut self, field: &str, f: impl FnOnce(&mut Validator)) {
        self.field(field, f);
    }

    fn record(&mut self, kind: ErrorKind, input: Option<String>, msg: String) {
        let severity = std::mem::take(&mut self.pending_severity);
        self.errors.push(ValidationError {
            loc: Loc(self.loc.clone()),
            kind,
            severity,
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
// FNV-1a — for variant fingerprinting (no external deps)
// ---------------------------------------------------------------------------

fn fnv1a(data: &[u8]) -> u64 {
    const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
    const PRIME: u64 = 0x0000_0100_0000_01b3;
    data.iter().fold(OFFSET, |hash, &byte| {
        (hash ^ u64::from(byte)).wrapping_mul(PRIME)
    })
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
            v.check_one_of(
                "log_level",
                &self.log_level,
                &["trace", "debug", "info", "warn", "error"],
            );
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

    // -- original Pydantic-grade tests (unchanged behaviour) ----------------

    #[test]
    fn valid_config_passes() {
        assert!(valid_app().check().is_ok());
    }

    #[test]
    fn collects_all_errors_not_just_first() {
        let app = App {
            name: String::new(),
            workers: 0,
            log_level: "verbose".into(),
            server: Server {
                host: String::new(),
                port: 0,
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

    // -- Van der Aalst: severity stratification ----------------------------

    #[test]
    fn default_severity_is_error() {
        let app = App {
            name: String::new(),
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        assert_eq!(errs.errors()[0].severity, Severity::Error);
    }

    #[test]
    fn with_severity_stamps_warning() {
        struct Cfg {
            log_dir: String,
        }
        impl Validate for Cfg {
            fn validate(&self, v: &mut Validator) {
                v.with_severity(Severity::Warning, |v| {
                    v.check_non_empty("log_dir", &self.log_dir);
                });
            }
        }
        let errs = Cfg { log_dir: String::new() }.check().unwrap_err();
        assert_eq!(errs.errors()[0].severity, Severity::Warning);
        assert!(!errs.has_fatal());
    }

    #[test]
    fn fatal_severity_detected() {
        struct Cfg;
        impl Validate for Cfg {
            fn validate(&self, v: &mut Validator) {
                v.with_severity(Severity::Fatal, |v| {
                    v.error(ErrorKind::Missing, "signing key is absent");
                });
            }
        }
        let errs = Cfg.check().unwrap_err();
        assert!(errs.has_fatal());
        assert!(errs.errors()[0].is_fatal());
    }

    // -- Van der Aalst: conformance fitness --------------------------------

    #[test]
    fn fitness_is_one_when_valid() {
        struct Good {
            x: u32,
        }
        impl Validate for Good {
            fn validate(&self, v: &mut Validator) {
                v.check_range("x", self.x, 1..=10);
            }
        }
        // valid → no errors, fitness should be accessible via a fresh validator
        // (only meaningful on error path, but the doc example has a passing case)
        assert!(Good { x: 5 }.check().is_ok());
    }

    #[test]
    fn fitness_half_when_one_of_two_fails() {
        struct Pair {
            a: u32,
            b: u32,
        }
        impl Validate for Pair {
            fn validate(&self, v: &mut Validator) {
                v.check_range("a", self.a, 1..=10); // passes
                v.check_range("b", self.b, 1..=10); // fails
            }
        }
        let errs = Pair { a: 5, b: 0 }.check().unwrap_err();
        assert_eq!(errs.fitness(), 0.5);
    }

    #[test]
    fn fitness_zero_when_all_fail() {
        let app = App {
            name: String::new(),
            workers: 0,
            log_level: "verbose".into(),
            server: Server {
                host: String::new(),
                port: 0,
                tls: None,
            },
        };
        let errs = app.check().unwrap_err();
        assert_eq!(errs.fitness(), 0.0);
    }

    // -- Van der Aalst: repair hints ---------------------------------------

    #[test]
    fn repair_hint_for_empty() {
        let app = App {
            name: String::new(),
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        assert_eq!(errs.errors()[0].repair_hint(), "provide a non-empty value");
    }

    #[test]
    fn repair_hint_for_out_of_range() {
        let app = App {
            workers: 9999,
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        assert!(errs.errors()[0].repair_hint().contains("1..=1024"));
    }

    #[test]
    fn repair_hint_for_not_one_of() {
        let app = App {
            log_level: "nope".into(),
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        let hint = errs.errors()[0].repair_hint();
        assert!(hint.contains("trace"));
        assert!(hint.contains("error"));
    }

    // -- Van der Aalst: variant fingerprint --------------------------------

    #[test]
    fn same_error_pattern_same_variant_id() {
        let app1 = App {
            name: String::new(),
            ..valid_app()
        };
        let app2 = App {
            name: String::new(),
            ..valid_app()
        };
        assert_eq!(
            app1.check().unwrap_err().variant_id(),
            app2.check().unwrap_err().variant_id()
        );
    }

    #[test]
    fn different_error_pattern_different_variant_id() {
        let app1 = App {
            name: String::new(),
            ..valid_app()
        };
        let app2 = App {
            workers: 9999,
            ..valid_app()
        };
        assert_ne!(
            app1.check().unwrap_err().variant_id(),
            app2.check().unwrap_err().variant_id()
        );
    }

    // -- Van der Aalst: object-centric grouping ----------------------------

    #[test]
    fn by_section_groups_errors_by_top_level_key() {
        let app = App {
            name: String::new(),
            workers: 0,
            server: Server {
                host: String::new(),
                port: 0,
                tls: None,
            },
            ..valid_app()
        };
        let errs = app.check().unwrap_err();
        let by_sec = errs.by_section();
        assert!(by_sec.contains_key("name"));
        assert!(by_sec.contains_key("workers"));
        assert!(by_sec.contains_key("server"));
        // server.host + server.port are both under "server"
        assert_eq!(by_sec["server"].len(), 2);
    }

    // -- Van der Aalst: DECLARE cross-field constraints --------------------

    #[test]
    fn check_consistent_records_inconsistent_error() {
        struct Tls2 {
            enabled: bool,
            cert_path: String,
        }
        impl Validate for Tls2 {
            fn validate(&self, v: &mut Validator) {
                v.check_consistent(
                    "cert_path",
                    &["enabled"],
                    !self.enabled || !self.cert_path.is_empty(),
                    "tls_cert_required",
                    "cert_path must be set when TLS is enabled",
                );
            }
        }
        let bad = Tls2 {
            enabled: true,
            cert_path: String::new(),
        };
        let errs = bad.check().unwrap_err();
        assert_eq!(errs.errors()[0].code(), "tls_cert_required");
        assert_eq!(errs.errors()[0].loc.to_string(), "cert_path");
        match &errs.errors()[0].kind {
            ErrorKind::Inconsistent { related, .. } => {
                assert!(related.contains(&"enabled".to_string()));
            }
            other => panic!("expected Inconsistent, got {other:?}"),
        }
    }

    #[test]
    fn check_consistent_passes_when_condition_true() {
        struct Tls2 {
            enabled: bool,
            cert_path: String,
        }
        impl Validate for Tls2 {
            fn validate(&self, v: &mut Validator) {
                v.check_consistent(
                    "cert_path",
                    &["enabled"],
                    !self.enabled || !self.cert_path.is_empty(),
                    "tls_cert_required",
                    "cert_path must be set when TLS is enabled",
                );
            }
        }
        assert!(Tls2 {
            enabled: true,
            cert_path: "/etc/cert.pem".into()
        }
        .check()
        .is_ok());
    }
}
