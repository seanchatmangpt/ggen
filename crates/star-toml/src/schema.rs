//! Declarative `Schema` builder — validate any `toml::Value` without Rust structs.
//!
//! The Pydantic + Van der Aalst engines in [`validation`](crate::validation) require
//! implementing [`Validate`](crate::Validate) for each config type. That's powerful for
//! typed configs — but sometimes you want to validate a raw TOML file without writing a
//! struct at all.
//!
//! `Schema` is the 1,700× version: declare constraints once, validate any
//! `toml::Value` or TOML string directly.
//!
//! # Phase-shift comparison
//!
//! **Before (imperative `Validate`):**
//! ```
//! use star_toml::{Validate, Validator};
//!
//! struct ServerConfig { host: String, port: u16 }
//!
//! impl Validate for ServerConfig {
//!     fn validate(&self, v: &mut Validator) {
//!         v.check_non_empty("host", &self.host);
//!         v.check_range("port", self.port, 1..=65535);
//!     }
//! }
//!
//! // Must deserialize to the struct first
//! let s = ServerConfig { host: String::new(), port: 0 };
//! let errs = s.check().unwrap_err();
//! assert_eq!(errs.len(), 2);
//! ```
//!
//! **After (declarative `Schema`):**
//! ```
//! use star_toml::Schema;
//!
//! let schema = Schema::new()
//!     .field("host").non_empty().done()
//!     .field("port").range_i64(1, 65535).done();
//!
//! let errs = schema.validate_str("[server]\nhost=''\nport=0").unwrap_err();
//! assert_eq!(errs.len(), 2);
//! ```
//!
//! Each `.field()` call opens a [`FieldBuilder`]; call `.done()` to return to the
//! parent `Schema` before opening the next field.
//!
//! # Nesting
//!
//! ```
//! use star_toml::Schema;
//!
//! let schema = Schema::new()
//!     .field("name").non_empty().done()
//!     .field("workers").range_i64(1, 1024).done()
//!     .section("server", Schema::new()
//!         .field("host").non_empty().done()
//!         .field("port").range_i64(1, 65535).done());
//!
//! let toml = r#"
//! name = "demo"
//! workers = 8
//! [server]
//! host = "localhost"
//! port = 8080
//! "#;
//! assert!(schema.validate_str(toml).is_ok());
//! ```

use crate::validation::{ErrorKind, Loc, LocSegment, Severity, ValidationError, ValidationErrors};
use toml::Value;

// ---------------------------------------------------------------------------
// Constraint — one atomic rule on a TOML value
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Constraint {
    NonEmpty,
    RangeI64 { lo: i64, hi: i64 },
    RangeF64 { lo: f64, hi: f64 },
    OneOf { allowed: Vec<String> },
    Required,
    Predicate { code: &'static str, msg: String, test: fn(&Value) -> bool },
}

impl Constraint {
    fn check(&self, field: &str, value: Option<&Value>, checks_run: &mut usize) -> Option<ValidationError> {
        *checks_run += 1;
        match self {
            Self::Required => {
                if value.is_none() {
                    return Some(make_err(field, ErrorKind::Missing, None, "field is required"));
                }
                None
            }
            Self::NonEmpty => {
                let s = value.and_then(Value::as_str).unwrap_or("");
                if s.is_empty() {
                    return Some(make_err(
                        field,
                        ErrorKind::Empty,
                        Some("\"\"".into()),
                        "must not be empty",
                    ));
                }
                None
            }
            Self::RangeI64 { lo, hi } => {
                let n = value.and_then(Value::as_integer).unwrap_or(0);
                if n < *lo || n > *hi {
                    let msg = format!("input must be in range {lo}..={hi}");
                    return Some(make_err(
                        field,
                        ErrorKind::OutOfRange {
                            lower: Some(lo.to_string()),
                            upper: Some(hi.to_string()),
                        },
                        Some(n.to_string()),
                        msg,
                    ));
                }
                None
            }
            Self::RangeF64 { lo, hi } => {
                let n = value.and_then(Value::as_float).unwrap_or(0.0);
                if n < *lo || n > *hi {
                    let msg = format!("input must be in range {lo}..={hi}");
                    return Some(make_err(
                        field,
                        ErrorKind::OutOfRange {
                            lower: Some(lo.to_string()),
                            upper: Some(hi.to_string()),
                        },
                        Some(n.to_string()),
                        msg,
                    ));
                }
                None
            }
            Self::OneOf { allowed } => {
                let s = value.and_then(Value::as_str).unwrap_or("");
                if !allowed.iter().any(|a| a == s) {
                    let msg = format!("must be one of: {}", allowed.join(", "));
                    return Some(make_err(
                        field,
                        ErrorKind::NotOneOf { allowed: allowed.clone() },
                        Some(s.to_string()),
                        msg,
                    ));
                }
                None
            }
            Self::Predicate { code, msg, test } => {
                if !test(value.unwrap_or(&Value::Boolean(false))) {
                    return Some(make_err(
                        field,
                        ErrorKind::Predicate { code },
                        None,
                        msg.clone(),
                    ));
                }
                None
            }
        }
    }
}

fn make_err(field: &str, kind: ErrorKind, input: Option<String>, msg: impl Into<String>) -> ValidationError {
    ValidationError {
        loc: Loc(vec![LocSegment::Key(field.to_string())]),
        kind,
        severity: Severity::Error,
        input,
        msg: msg.into(),
    }
}

// ---------------------------------------------------------------------------
// FieldBuilder — fluent constraint attachment
// ---------------------------------------------------------------------------

/// Fluent builder returned by [`Schema::field`].
///
/// Attach constraints with method chaining; call [`done`](FieldBuilder::done)
/// to return to the parent schema.
pub struct FieldBuilder<'a> {
    schema: &'a mut Schema,
    name: String,
}

impl<'a> FieldBuilder<'a> {
    /// Fail with `empty` if the value is an empty string.
    pub fn non_empty(self) -> Self {
        self.add(Constraint::NonEmpty)
    }

    /// Fail with `out_of_range` if the integer value is outside `[lo, hi]`.
    pub fn range_i64(self, lo: i64, hi: i64) -> Self {
        self.add(Constraint::RangeI64 { lo, hi })
    }

    /// Fail with `out_of_range` if the float value is outside `[lo, hi]`.
    pub fn range_f64(self, lo: f64, hi: f64) -> Self {
        self.add(Constraint::RangeF64 { lo, hi })
    }

    /// Fail with `not_one_of` if the string value is not in `allowed`.
    pub fn one_of(self, allowed: &[&str]) -> Self {
        let allowed = allowed.iter().map(|s| (*s).to_string()).collect();
        self.add(Constraint::OneOf { allowed })
    }

    /// Fail with `missing` if the field is absent from the config.
    pub fn required(self) -> Self {
        self.add(Constraint::Required)
    }

    /// Fail with `code` when `test(value)` returns false.
    pub fn predicate(self, code: &'static str, msg: impl Into<String>, test: fn(&Value) -> bool) -> Self {
        self.add(Constraint::Predicate { code, msg: msg.into(), test })
    }

    fn add(self, c: Constraint) -> Self {
        let idx = self.schema.fields.iter().position(|(n, _)| n == &self.name);
        match idx {
            Some(i) => self.schema.fields[i].1.push(c),
            None => self.schema.fields.push((self.name.clone(), vec![c])),
        }
        self
    }

    /// Finish field configuration and return to the parent [`Schema`].
    pub fn done(self) -> Schema {
        Schema {
            fields: self.schema.fields.clone(),
            sections: self.schema.sections.clone(),
        }
    }
}

// ---------------------------------------------------------------------------
// Schema — the declarative model
// ---------------------------------------------------------------------------

/// Declarative validation schema for raw TOML values.
///
/// Build with [`Schema::new`] and the fluent [`field`](Schema::field) /
/// [`section`](Schema::section) methods, then call [`validate_value`](Schema::validate_value),
/// [`validate_str`](Schema::validate_str), or use as a [`Validate`](crate::Validate)
/// implementor via [`into_validator`](Schema::into_validator).
///
/// ```
/// use star_toml::Schema;
///
/// let schema = Schema::new()
///     .field("name").non_empty().done()
///     .field("port").range_i64(1, 65535).done();
///
/// let ok = schema.validate_str("name = \"app\"\nport = 8080");
/// assert!(ok.is_ok());
///
/// let errs = schema.validate_str("name = \"\"\nport = 0").unwrap_err();
/// assert_eq!(errs.len(), 2);
/// assert_eq!(errs.fitness(), 0.0);
/// ```
#[derive(Debug, Clone, Default)]
pub struct Schema {
    fields: Vec<(String, Vec<Constraint>)>,
    sections: Vec<(String, Schema)>,
}

impl Schema {
    /// Create an empty schema.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Start adding constraints for `name`.
    ///
    /// Returns a [`FieldBuilder`]; call `.done()` to return to the schema.
    pub fn field(&mut self, name: &str) -> FieldBuilder<'_> {
        FieldBuilder { schema: self, name: name.to_string() }
    }

    /// Add a nested sub-schema for table key `name`.
    ///
    /// Errors inside the sub-schema are prefixed with `name.`.
    ///
    /// ```
    /// use star_toml::Schema;
    ///
    /// let schema = Schema::new()
    ///     .section("db", Schema::new()
    ///         .field("host").non_empty().done()
    ///         .field("port").range_i64(1, 65535).done());
    ///
    /// let errs = schema.validate_str("[db]\nhost=''\nport=0").unwrap_err();
    /// assert_eq!(errs.errors()[0].loc.to_string(), "db.host");
    /// ```
    pub fn section(mut self, name: &str, sub: Schema) -> Self {
        self.sections.push((name.to_string(), sub));
        self
    }

    /// Validate a `toml::Value` (typically the root table after parsing).
    ///
    /// # Errors
    ///
    /// Returns [`ValidationErrors`] if any constraint is violated.
    pub fn validate_value(&self, value: &Value) -> Result<(), ValidationErrors> {
        let mut errors = Vec::new();
        let mut checks_run = 0usize;
        self.check_value(value, &[], &mut errors, &mut checks_run);
        if errors.is_empty() {
            Ok(())
        } else {
            Err(ValidationErrors { errors, title: None, checks_run })
        }
    }

    /// Parse `toml_str`, then validate the resulting value.
    ///
    /// # Errors
    ///
    /// Returns `Err(ValidationErrors)` when parsing fails (single synthetic error)
    /// or when any constraint is violated.
    pub fn validate_str(&self, toml_str: &str) -> Result<(), ValidationErrors> {
        let value: Value = match toml::from_str(toml_str) {
            Ok(v) => v,
            Err(e) => {
                let err = ValidationError {
                    loc: Loc::default(),
                    kind: ErrorKind::Predicate { code: "parse_error" },
                    severity: Severity::Fatal,
                    input: None,
                    msg: e.to_string(),
                };
                return Err(ValidationErrors {
                    errors: vec![err],
                    title: Some("TOML".into()),
                    checks_run: 0,
                });
            }
        };
        self.validate_value(&value)
    }

    /// Total number of fields (direct + nested) that have constraints.
    ///
    /// Useful for computing expected check counts in tests.
    #[must_use]
    pub fn constraint_count(&self) -> usize {
        let direct: usize = self.fields.iter().map(|(_, cs)| cs.len()).sum();
        let nested: usize = self.sections.iter().map(|(_, s)| s.constraint_count()).sum();
        direct + nested
    }

    // -- internal ----------------------------------------------------------

    fn check_value(
        &self,
        value: &Value,
        prefix: &[LocSegment],
        errors: &mut Vec<ValidationError>,
        checks_run: &mut usize,
    ) {
        for (name, constraints) in &self.fields {
            let child = value.get(name.as_str());
            for c in constraints {
                if let Some(mut e) = c.check(name, child, checks_run) {
                    let mut loc_segs = prefix.to_vec();
                    loc_segs.extend(e.loc.0.drain(..));
                    e.loc = Loc(loc_segs);
                    errors.push(e);
                }
            }
        }
        for (section_name, sub_schema) in &self.sections {
            let sub_value = value.get(section_name.as_str());
            let mut sub_prefix = prefix.to_vec();
            sub_prefix.push(LocSegment::Key(section_name.clone()));
            match sub_value {
                Some(v) => sub_schema.check_value(v, &sub_prefix, errors, checks_run),
                None => {
                    // If the section is absent, each required field in it is missing
                    sub_schema.report_section_missing(section_name, &sub_prefix, errors, checks_run);
                }
            }
        }
    }

    fn report_section_missing(
        &self,
        _section: &str,
        prefix: &[LocSegment],
        errors: &mut Vec<ValidationError>,
        checks_run: &mut usize,
    ) {
        for (name, constraints) in &self.fields {
            for c in constraints {
                if let Some(mut e) = c.check(name, None, checks_run) {
                    let mut loc_segs = prefix.to_vec();
                    loc_segs.extend(e.loc.0.drain(..));
                    e.loc = Loc(loc_segs);
                    errors.push(e);
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn server_schema() -> Schema {
        Schema::new()
            .field("host").non_empty().done()
            .field("port").range_i64(1, 65535).done()
    }

    fn app_schema() -> Schema {
        Schema::new()
            .field("name").non_empty().done()
            .field("workers").range_i64(1, 1024).done()
            .field("log_level").one_of(&["trace", "debug", "info", "warn", "error"]).done()
            .section("server", server_schema())
    }

    #[test]
    fn valid_config_passes() {
        let toml = r#"
name = "demo"
workers = 8
log_level = "info"
[server]
host = "localhost"
port = 8080
"#;
        assert!(app_schema().validate_str(toml).is_ok());
    }

    #[test]
    fn collects_all_errors() {
        let toml = r#"
name = ""
workers = 0
log_level = "verbose"
[server]
host = ""
port = 0
"#;
        let errs = app_schema().validate_str(toml).unwrap_err();
        assert_eq!(errs.len(), 5);
    }

    #[test]
    fn nested_section_paths_are_prefixed() {
        let toml = "name = \"ok\"\nworkers = 4\nlog_level = \"info\"\n[server]\nhost = \"\"\nport = 8080\n";
        let errs = app_schema().validate_str(toml).unwrap_err();
        let locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(locs.contains(&"server.host".to_string()));
    }

    #[test]
    fn fitness_reflects_partial_pass() {
        let toml = "name = \"ok\"\nworkers = 0\nlog_level = \"info\"\n[server]\nhost = \"h\"\nport = 8080\n";
        let errs = app_schema().validate_str(toml).unwrap_err();
        assert!(errs.fitness() > 0.0 && errs.fitness() < 1.0);
    }

    #[test]
    fn variant_id_stable_across_equal_error_patterns() {
        let toml = "name = \"\"\nworkers = 1\nlog_level = \"info\"\n[server]\nhost = \"h\"\nport = 80\n";
        let id1 = app_schema().validate_str(toml).unwrap_err().variant_id();
        let id2 = app_schema().validate_str(toml).unwrap_err().variant_id();
        assert_eq!(id1, id2);
    }

    #[test]
    fn parse_error_produces_fatal_error() {
        let errs = Schema::new().validate_str("not valid toml :::").unwrap_err();
        assert!(errs.errors()[0].is_fatal());
        assert_eq!(errs.errors()[0].code(), "parse_error");
    }

    #[test]
    fn one_of_constraint() {
        let schema = Schema::new()
            .field("level").one_of(&["info", "warn", "error"]).done();
        assert!(schema.validate_str("level = \"info\"").is_ok());
        let errs = schema.validate_str("level = \"verbose\"").unwrap_err();
        assert_eq!(errs.errors()[0].code(), "not_one_of");
    }

    #[test]
    fn range_f64_constraint() {
        let schema = Schema::new()
            .field("ratio").range_f64(0.0, 1.0).done();
        assert!(schema.validate_str("ratio = 0.5").is_ok());
        let errs = schema.validate_str("ratio = 2.0").unwrap_err();
        assert_eq!(errs.errors()[0].code(), "out_of_range");
    }

    #[test]
    fn predicate_constraint() {
        let schema = Schema::new()
            .field("port").predicate(
                "no_well_known",
                "prefer ports above 1024",
                |v| v.as_integer().map_or(true, |n| n > 1024),
            ).done();
        assert!(schema.validate_str("port = 8080").is_ok());
        let errs = schema.validate_str("port = 80").unwrap_err();
        assert_eq!(errs.errors()[0].code(), "no_well_known");
    }

    #[test]
    fn by_section_grouping_works_on_schema_errors() {
        let toml = "name = \"\"\nworkers = 0\nlog_level = \"info\"\n[server]\nhost = \"\"\nport = 8080\n";
        let errs = app_schema().validate_str(toml).unwrap_err();
        let by_sec = errs.by_section();
        assert!(by_sec.contains_key("name"));
        assert!(by_sec.contains_key("workers"));
        assert!(by_sec.contains_key("server"));
    }
}
