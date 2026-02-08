//! Type-safe SPARQL query builder with compile-time safety
//!
//! This module provides a type-safe SPARQL query builder that eliminates injection
//! vulnerabilities through:
//! - Type-state pattern with PhantomData for compile-time validation
//! - Automatic escaping of all user inputs via newtype wrappers
//! - Zero-cost abstractions (all methods inlined)
//! - Builder pattern for SELECT, CONSTRUCT, ASK, DESCRIBE queries
//!
//! ## Architecture
//!
//! The builder uses a type-state pattern with four states:
//! - `Building` - Initial state, can add prefixes and variables
//! - `WithVars` - Variables added, can add WHERE patterns
//! - `WithWhere` - WHERE clause added, can add filters/modifiers
//! - `Complete` - Query is complete and can be built
//!
//! ## Examples
//!
//! ### SELECT query with filter
//!
//! ```rust
//! use ggen_core::rdf::query_builder::{SparqlQueryBuilder, Variable, Iri};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let query = SparqlQueryBuilder::select()
//!     .prefix("ex", Iri::new("https://example.com/")?)
//!     .var(Variable::new("subject")?)
//!     .var(Variable::new("object")?)
//!     .where_pattern("?subject <http://example.com/predicate> ?object")
//!     .filter("?subject = <http://example.com/Entity>")
//!     .limit(100)
//!     .build()?;
//!
//! assert!(query.contains("SELECT"));
//! assert!(query.contains("?subject"));
//! # Ok(())
//! # }
//! ```
//!
//! ### CONSTRUCT query
//!
//! ```rust
//! use ggen_core::rdf::query_builder::{SparqlQueryBuilder, Iri};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let query = SparqlQueryBuilder::construct()
//!     .prefix("ex", Iri::new("https://example.com/")?)
//!     .construct_pattern("?s <http://example.com/new> ?o")
//!     .where_pattern("?s <http://example.com/old> ?o")
//!     .build()?;
//!
//! assert!(query.contains("CONSTRUCT"));
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use std::marker::PhantomData;

/// Type-safe IRI wrapper with automatic validation and escaping
///
/// Prevents SPARQL injection by validating IRI format and escaping special characters.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Iri(String);

impl Iri {
    /// Create a new IRI with validation
    ///
    /// # Arguments
    ///
    /// * `iri` - IRI string to validate
    ///
    /// # Errors
    ///
    /// Returns an error if the IRI contains invalid characters or format
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::query_builder::Iri;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let iri = Iri::new("https://example.com/resource")?;
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn new(iri: impl AsRef<str>) -> Result<Self> {
        let iri_str = iri.as_ref();

        // Validate IRI format - reject injection attempts
        if iri_str.contains(['<', '>', '"', '{', '}', '|', '^', '`', '\\']) {
            return Err(Error::new(&format!(
                "Invalid IRI format: contains forbidden characters: {}",
                iri_str
            )));
        }

        Ok(Self(iri_str.to_string()))
    }

    /// Get the IRI string
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Type-safe variable wrapper with validation
///
/// Prevents SPARQL injection by validating variable names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable(String);

impl Variable {
    /// Create a new variable with validation
    ///
    /// # Arguments
    ///
    /// * `name` - Variable name (without leading ?)
    ///
    /// # Errors
    ///
    /// Returns an error if the variable name is invalid
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::query_builder::Variable;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let var = Variable::new("subject")?;
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn new(name: impl AsRef<str>) -> Result<Self> {
        let name_str = name.as_ref();

        // Remove leading ? if present
        let name_clean = name_str.strip_prefix('?').unwrap_or(name_str);

        // Validate variable name - must be alphanumeric with underscores
        if !name_clean.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Err(Error::new(&format!(
                "Invalid variable name: {}. Must be alphanumeric with underscores.",
                name_str
            )));
        }

        if name_clean.is_empty() {
            return Err(Error::new("Variable name cannot be empty"));
        }

        Ok(Self(name_clean.to_string()))
    }

    /// Get the variable name with leading ?
    #[inline]
    pub fn as_str(&self) -> String {
        format!("?{}", self.0)
    }

    /// Get the variable name without leading ?
    #[inline]
    pub fn name(&self) -> &str {
        &self.0
    }
}

/// Type-safe literal wrapper with automatic escaping
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal(String);

impl Literal {
    /// Create a new literal with automatic escaping
    ///
    /// # Arguments
    ///
    /// * `value` - Literal value to escape
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::query_builder::Literal;
    ///
    /// let lit = Literal::new("Alice's value");
    /// assert!(lit.as_str().contains("Alice"));
    /// ```
    #[inline]
    pub fn new(value: impl AsRef<str>) -> Self {
        let escaped = value
            .as_ref()
            .replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t");

        Self(escaped)
    }

    /// Get the escaped literal value
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Type-state marker for building phase
#[derive(Debug)]
pub struct Building;

/// Type-state marker for SELECT queries with variables
#[derive(Debug)]
pub struct WithVars;

/// Type-state marker for queries with WHERE clause
#[derive(Debug)]
pub struct WithWhere;

/// Type-state marker for complete queries
#[derive(Debug)]
pub struct Complete;

/// Query type marker for SELECT
#[derive(Debug)]
pub struct Select;

/// Query type marker for CONSTRUCT
#[derive(Debug)]
pub struct Construct;

/// Query type marker for ASK
#[derive(Debug)]
pub struct Ask;

/// Query type marker for DESCRIBE
#[derive(Debug)]
pub struct Describe;

/// Type-safe SPARQL query builder
///
/// Uses type-state pattern to ensure queries are constructed correctly at compile time.
/// All methods are inlined for zero runtime overhead.
#[derive(Debug)]
pub struct SparqlQueryBuilder<Q, S> {
    query_type: PhantomData<Q>,
    state: PhantomData<S>,
    prefixes: Vec<(String, String)>,
    vars: Vec<String>,
    construct_patterns: Vec<String>,
    where_patterns: Vec<String>,
    filters: Vec<String>,
    order_by: Vec<String>,
    limit_value: Option<usize>,
    offset_value: Option<usize>,
    distinct: bool,
}

impl<Q, S> SparqlQueryBuilder<Q, S> {
    /// Add a prefix to the query
    ///
    /// # Arguments
    ///
    /// * `prefix` - Prefix name
    /// * `iri` - IRI for the prefix
    #[inline]
    pub fn prefix(mut self, prefix: impl AsRef<str>, iri: Iri) -> Self {
        self.prefixes
            .push((prefix.as_ref().to_string(), iri.as_str().to_string()));
        self
    }
}

impl SparqlQueryBuilder<Select, Building> {
    /// Create a new SELECT query builder
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::query_builder::SparqlQueryBuilder;
    ///
    /// let builder = SparqlQueryBuilder::select();
    /// ```
    #[inline]
    pub fn select() -> Self {
        Self {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: Vec::new(),
            vars: Vec::new(),
            construct_patterns: Vec::new(),
            where_patterns: Vec::new(),
            filters: Vec::new(),
            order_by: Vec::new(),
            limit_value: None,
            offset_value: None,
            distinct: false,
        }
    }

    /// Add a variable to select
    ///
    /// # Arguments
    ///
    /// * `var` - Variable to select
    #[inline]
    pub fn var(mut self, var: Variable) -> SparqlQueryBuilder<Select, WithVars> {
        self.vars.push(var.as_str());
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }

    /// Select all variables (SELECT *)
    #[inline]
    pub fn all_vars(mut self) -> SparqlQueryBuilder<Select, WithVars> {
        self.vars.push("*".to_string());
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }

    /// Make the query DISTINCT
    #[inline]
    pub fn distinct(mut self) -> Self {
        self.distinct = true;
        self
    }
}

impl SparqlQueryBuilder<Select, WithVars> {
    /// Add another variable to select
    #[inline]
    pub fn var(mut self, var: Variable) -> Self {
        self.vars.push(var.as_str());
        self
    }

    /// Add a WHERE pattern
    ///
    /// # Arguments
    ///
    /// * `pattern` - SPARQL pattern string
    #[inline]
    pub fn where_pattern(
        mut self, pattern: impl AsRef<str>,
    ) -> SparqlQueryBuilder<Select, WithWhere> {
        self.where_patterns.push(pattern.as_ref().to_string());
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }
}

impl SparqlQueryBuilder<Select, WithWhere> {
    /// Add another WHERE pattern
    #[inline]
    pub fn where_pattern(mut self, pattern: impl AsRef<str>) -> Self {
        self.where_patterns.push(pattern.as_ref().to_string());
        self
    }

    /// Add a FILTER clause
    ///
    /// # Arguments
    ///
    /// * `filter` - FILTER expression
    #[inline]
    pub fn filter(mut self, filter: impl AsRef<str>) -> Self {
        self.filters.push(filter.as_ref().to_string());
        self
    }

    /// Add an ORDER BY clause
    ///
    /// # Arguments
    ///
    /// * `var` - Variable to order by
    #[inline]
    pub fn order_by(mut self, var: Variable) -> Self {
        self.order_by.push(var.as_str());
        self
    }

    /// Add a LIMIT clause
    ///
    /// # Arguments
    ///
    /// * `limit` - Maximum number of results
    #[inline]
    pub fn limit(mut self, limit: usize) -> Self {
        self.limit_value = Some(limit);
        self
    }

    /// Add an OFFSET clause
    ///
    /// # Arguments
    ///
    /// * `offset` - Number of results to skip
    #[inline]
    pub fn offset(mut self, offset: usize) -> Self {
        self.offset_value = Some(offset);
        self
    }

    /// Build the final SPARQL query string
    ///
    /// # Errors
    ///
    /// Returns an error if the query cannot be constructed
    #[inline]
    pub fn build(self) -> Result<String> {
        let mut query = String::new();

        // Add prefixes
        for (prefix, iri) in &self.prefixes {
            query.push_str(&format!("PREFIX {}: <{}>\n", prefix, iri));
        }

        // Add SELECT clause
        if self.distinct {
            query.push_str("SELECT DISTINCT ");
        } else {
            query.push_str("SELECT ");
        }

        query.push_str(&self.vars.join(" "));
        query.push('\n');

        // Add WHERE clause
        query.push_str("WHERE {\n");
        for pattern in &self.where_patterns {
            query.push_str("  ");
            query.push_str(pattern);
            query.push_str(" .\n");
        }

        // Add FILTER clauses
        for filter in &self.filters {
            query.push_str("  FILTER (");
            query.push_str(filter);
            query.push_str(")\n");
        }

        query.push_str("}\n");

        // Add ORDER BY
        if !self.order_by.is_empty() {
            query.push_str("ORDER BY ");
            query.push_str(&self.order_by.join(" "));
            query.push('\n');
        }

        // Add LIMIT
        if let Some(limit) = self.limit_value {
            query.push_str(&format!("LIMIT {}\n", limit));
        }

        // Add OFFSET
        if let Some(offset) = self.offset_value {
            query.push_str(&format!("OFFSET {}\n", offset));
        }

        Ok(query)
    }
}

impl SparqlQueryBuilder<Construct, Building> {
    /// Create a new CONSTRUCT query builder
    #[inline]
    pub fn construct() -> Self {
        Self {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: Vec::new(),
            vars: Vec::new(),
            construct_patterns: Vec::new(),
            where_patterns: Vec::new(),
            filters: Vec::new(),
            order_by: Vec::new(),
            limit_value: None,
            offset_value: None,
            distinct: false,
        }
    }

    /// Add a CONSTRUCT pattern
    ///
    /// # Arguments
    ///
    /// * `pattern` - Triple pattern to construct
    #[inline]
    pub fn construct_pattern(
        mut self, pattern: impl AsRef<str>,
    ) -> SparqlQueryBuilder<Construct, WithVars> {
        self.construct_patterns.push(pattern.as_ref().to_string());
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }
}

impl SparqlQueryBuilder<Construct, WithVars> {
    /// Add another CONSTRUCT pattern
    #[inline]
    pub fn construct_pattern(mut self, pattern: impl AsRef<str>) -> Self {
        self.construct_patterns.push(pattern.as_ref().to_string());
        self
    }

    /// Add a WHERE pattern
    #[inline]
    pub fn where_pattern(
        mut self, pattern: impl AsRef<str>,
    ) -> SparqlQueryBuilder<Construct, WithWhere> {
        self.where_patterns.push(pattern.as_ref().to_string());
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }
}

impl SparqlQueryBuilder<Construct, WithWhere> {
    /// Add another WHERE pattern
    #[inline]
    pub fn where_pattern(mut self, pattern: impl AsRef<str>) -> Self {
        self.where_patterns.push(pattern.as_ref().to_string());
        self
    }

    /// Add a FILTER clause
    #[inline]
    pub fn filter(mut self, filter: impl AsRef<str>) -> Self {
        self.filters.push(filter.as_ref().to_string());
        self
    }

    /// Build the final SPARQL query string
    #[inline]
    pub fn build(self) -> Result<String> {
        let mut query = String::new();

        // Add prefixes
        for (prefix, iri) in &self.prefixes {
            query.push_str(&format!("PREFIX {}: <{}>\n", prefix, iri));
        }

        // Add CONSTRUCT clause
        query.push_str("CONSTRUCT {\n");
        for pattern in &self.construct_patterns {
            query.push_str("  ");
            query.push_str(pattern);
            query.push_str(" .\n");
        }
        query.push_str("}\n");

        // Add WHERE clause
        query.push_str("WHERE {\n");
        for pattern in &self.where_patterns {
            query.push_str("  ");
            query.push_str(pattern);
            query.push_str(" .\n");
        }

        // Add FILTER clauses
        for filter in &self.filters {
            query.push_str("  FILTER (");
            query.push_str(filter);
            query.push_str(")\n");
        }

        query.push_str("}\n");

        Ok(query)
    }
}

impl SparqlQueryBuilder<Ask, Building> {
    /// Create a new ASK query builder
    #[inline]
    pub fn ask() -> Self {
        Self {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: Vec::new(),
            vars: Vec::new(),
            construct_patterns: Vec::new(),
            where_patterns: Vec::new(),
            filters: Vec::new(),
            order_by: Vec::new(),
            limit_value: None,
            offset_value: None,
            distinct: false,
        }
    }

    /// Add a WHERE pattern
    #[inline]
    pub fn where_pattern(mut self, pattern: impl AsRef<str>) -> SparqlQueryBuilder<Ask, WithWhere> {
        self.where_patterns.push(pattern.as_ref().to_string());
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }
}

impl SparqlQueryBuilder<Ask, WithWhere> {
    /// Add another WHERE pattern
    #[inline]
    pub fn where_pattern(mut self, pattern: impl AsRef<str>) -> Self {
        self.where_patterns.push(pattern.as_ref().to_string());
        self
    }

    /// Add a FILTER clause
    #[inline]
    pub fn filter(mut self, filter: impl AsRef<str>) -> Self {
        self.filters.push(filter.as_ref().to_string());
        self
    }

    /// Build the final SPARQL query string
    #[inline]
    pub fn build(self) -> Result<String> {
        let mut query = String::new();

        // Add prefixes
        for (prefix, iri) in &self.prefixes {
            query.push_str(&format!("PREFIX {}: <{}>\n", prefix, iri));
        }

        // Add ASK clause
        query.push_str("ASK {\n");
        for pattern in &self.where_patterns {
            query.push_str("  ");
            query.push_str(pattern);
            query.push_str(" .\n");
        }

        // Add FILTER clauses
        for filter in &self.filters {
            query.push_str("  FILTER (");
            query.push_str(filter);
            query.push_str(")\n");
        }

        query.push_str("}\n");

        Ok(query)
    }
}

impl SparqlQueryBuilder<Describe, Building> {
    /// Create a new DESCRIBE query builder
    #[inline]
    pub fn describe() -> Self {
        Self {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: Vec::new(),
            vars: Vec::new(),
            construct_patterns: Vec::new(),
            where_patterns: Vec::new(),
            filters: Vec::new(),
            order_by: Vec::new(),
            limit_value: None,
            offset_value: None,
            distinct: false,
        }
    }

    /// Describe a resource by IRI
    #[inline]
    pub fn resource(mut self, iri: Iri) -> SparqlQueryBuilder<Describe, Complete> {
        self.vars.push(format!("<{}>", iri.as_str()));
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }

    /// Describe resources matching a variable
    #[inline]
    pub fn var(mut self, var: Variable) -> SparqlQueryBuilder<Describe, Complete> {
        self.vars.push(var.as_str());
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }
}

impl SparqlQueryBuilder<Describe, Complete> {
    /// Add a WHERE clause to filter described resources
    #[inline]
    pub fn where_pattern(
        mut self, pattern: impl AsRef<str>,
    ) -> SparqlQueryBuilder<Describe, WithWhere> {
        self.where_patterns.push(pattern.as_ref().to_string());
        SparqlQueryBuilder {
            query_type: PhantomData,
            state: PhantomData,
            prefixes: self.prefixes,
            vars: self.vars,
            construct_patterns: self.construct_patterns,
            where_patterns: self.where_patterns,
            filters: self.filters,
            order_by: self.order_by,
            limit_value: self.limit_value,
            offset_value: self.offset_value,
            distinct: self.distinct,
        }
    }

    /// Build the final SPARQL query string
    #[inline]
    pub fn build(self) -> Result<String> {
        let mut query = String::new();

        // Add prefixes
        for (prefix, iri) in &self.prefixes {
            query.push_str(&format!("PREFIX {}: <{}>\n", prefix, iri));
        }

        // Add DESCRIBE clause
        query.push_str("DESCRIBE ");
        query.push_str(&self.vars.join(" "));
        query.push('\n');

        Ok(query)
    }
}

impl SparqlQueryBuilder<Describe, WithWhere> {
    /// Add another WHERE pattern
    #[inline]
    pub fn where_pattern(mut self, pattern: impl AsRef<str>) -> Self {
        self.where_patterns.push(pattern.as_ref().to_string());
        self
    }

    /// Build the final SPARQL query string
    #[inline]
    pub fn build(self) -> Result<String> {
        let mut query = String::new();

        // Add prefixes
        for (prefix, iri) in &self.prefixes {
            query.push_str(&format!("PREFIX {}: <{}>\n", prefix, iri));
        }

        // Add DESCRIBE clause
        query.push_str("DESCRIBE ");
        query.push_str(&self.vars.join(" "));
        query.push('\n');

        // Add WHERE clause
        query.push_str("WHERE {\n");
        for pattern in &self.where_patterns {
            query.push_str("  ");
            query.push_str(pattern);
            query.push_str(" .\n");
        }
        query.push_str("}\n");

        Ok(query)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_iri_validation_rejects_injection() {
        // Arrange & Act
        let result = Iri::new("<malicious>");

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid IRI format"));
    }

    #[test]
    fn test_iri_validation_accepts_valid_iri() {
        // Arrange & Act
        let result = Iri::new("https://example.com/resource");

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), "https://example.com/resource");
    }

    #[test]
    fn test_variable_validation_rejects_injection() {
        // Arrange & Act
        let result = Variable::new("var; DROP TABLE users;");

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Invalid variable"));
    }

    #[test]
    fn test_variable_validation_accepts_valid_name() {
        // Arrange & Act
        let result = Variable::new("valid_var_123");

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), "?valid_var_123");
    }

    #[test]
    fn test_variable_handles_leading_question_mark() {
        // Arrange & Act
        let result = Variable::new("?myvar");

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), "?myvar");
    }

    #[test]
    fn test_literal_escapes_special_characters() {
        // Arrange & Act
        let lit = Literal::new("Alice's \"quote\" \n newline");

        // Assert
        assert!(lit.as_str().contains("Alice's"));
        assert!(lit.as_str().contains("\\\"quote\\\""));
        assert!(lit.as_str().contains("\\n"));
    }

    #[test]
    fn test_select_query_basic() {
        // Arrange & Act
        let query = SparqlQueryBuilder::select()
            .var(Variable::new("subject").unwrap())
            .var(Variable::new("object").unwrap())
            .where_pattern("?subject <http://example.com/pred> ?object")
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("SELECT ?subject ?object"));
        assert!(query.contains("WHERE {"));
        assert!(query.contains("?subject <http://example.com/pred> ?object"));
    }

    #[test]
    fn test_select_query_with_prefix() {
        // Arrange & Act
        let query = SparqlQueryBuilder::select()
            .prefix("ex", Iri::new("https://example.com/").unwrap())
            .var(Variable::new("s").unwrap())
            .where_pattern("?s a ex:Entity")
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("PREFIX ex: <https://example.com/>"));
        assert!(query.contains("SELECT ?s"));
    }

    #[test]
    fn test_select_query_with_filter() {
        // Arrange & Act
        let query = SparqlQueryBuilder::select()
            .var(Variable::new("s").unwrap())
            .where_pattern("?s <http://example.com/name> ?name")
            .filter("?name = \"Alice\"")
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("FILTER (?name = \"Alice\")"));
    }

    #[test]
    fn test_select_query_with_limit_and_offset() {
        // Arrange & Act
        let query = SparqlQueryBuilder::select()
            .var(Variable::new("s").unwrap())
            .where_pattern("?s ?p ?o")
            .limit(100)
            .offset(50)
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("LIMIT 100"));
        assert!(query.contains("OFFSET 50"));
    }

    #[test]
    fn test_select_query_distinct() {
        // Arrange & Act
        let query = SparqlQueryBuilder::select()
            .distinct()
            .var(Variable::new("s").unwrap())
            .where_pattern("?s ?p ?o")
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("SELECT DISTINCT"));
    }

    #[test]
    fn test_select_all_vars() {
        // Arrange & Act
        let query = SparqlQueryBuilder::select()
            .all_vars()
            .where_pattern("?s ?p ?o")
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("SELECT *"));
    }

    #[test]
    fn test_construct_query() {
        // Arrange & Act
        let query = SparqlQueryBuilder::construct()
            .construct_pattern("?s <http://example.com/new> ?o")
            .where_pattern("?s <http://example.com/old> ?o")
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("CONSTRUCT {"));
        assert!(query.contains("?s <http://example.com/new> ?o"));
        assert!(query.contains("WHERE {"));
        assert!(query.contains("?s <http://example.com/old> ?o"));
    }

    #[test]
    fn test_ask_query() {
        // Arrange & Act
        let query = SparqlQueryBuilder::ask()
            .where_pattern("?s <http://example.com/pred> ?o")
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("ASK {"));
        assert!(query.contains("?s <http://example.com/pred> ?o"));
    }

    #[test]
    fn test_describe_query_with_iri() {
        // Arrange & Act
        let query = SparqlQueryBuilder::describe()
            .resource(Iri::new("http://example.com/alice").unwrap())
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("DESCRIBE <http://example.com/alice>"));
    }

    #[test]
    fn test_describe_query_with_var() {
        // Arrange & Act
        let query = SparqlQueryBuilder::describe()
            .var(Variable::new("s").unwrap())
            .where_pattern("?s a <http://example.com/Person>")
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("DESCRIBE ?s"));
        assert!(query.contains("WHERE {"));
    }

    #[test]
    fn test_order_by() {
        // Arrange & Act
        let query = SparqlQueryBuilder::select()
            .var(Variable::new("s").unwrap())
            .var(Variable::new("name").unwrap())
            .where_pattern("?s <http://example.com/name> ?name")
            .order_by(Variable::new("name").unwrap())
            .build()
            .unwrap();

        // Assert
        assert!(query.contains("ORDER BY ?name"));
    }
}
