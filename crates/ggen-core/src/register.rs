//! Tera template environment registration and text transformation helpers
//!
//! This module provides comprehensive text transformation filters and functions
//! for the Tera templating engine. It includes case conversion, string manipulation,
//! SPARQL query helpers, and context blessing for Hygen compatibility.
//!
//! ## Features
//!
//! - **Case Conversion**: camelCase, PascalCase, snake_case, kebab-case, and more
//! - **String Manipulation**: Pluralization, singularization, ordinalization
//! - **SPARQL Helpers**: Functions for working with SPARQL query results in templates
//! - **Context Blessing**: Auto-generate derived variables (Name, locals) for Hygen compatibility
//! - **Inflector Integration**: Full integration with the Inflector crate
//! - **Heck Integration**: Additional case conversion utilities from Heck
//!
//! ## Available Filters
//!
//! ### Case Conversion
//! - `camel` - camelCase
//! - `pascal` - PascalCase
//! - `snake` - snake_case
//! - `kebab` - kebab-case
//! - `class` - ClassCase
//! - `title` - Title Case
//! - `sentence` - Sentence case
//! - `train` - Train-Case
//! - `shouty_snake` - SCREAMING_SNAKE_CASE
//! - `shouty_kebab` - SCREAMING-KEBAB-CASE
//!
//! ### String Manipulation
//! - `pluralize` - Convert to plural form
//! - `singularize` - Convert to singular form
//! - `ordinalize` - Convert number to ordinal (1st, 2nd, 3rd)
//! - `deordinalize` - Remove ordinal suffix
//! - `foreign_key` - Generate foreign key name
//!
//! ### SPARQL Functions
//! - `sparql_column` - Extract a column from SPARQL results
//! - `sparql_row` - Get a specific row from SPARQL results
//! - `sparql_first` - Get first value from a column
//! - `sparql_values` - Get all values from a column
//! - `sparql_empty` - Check if results are empty
//! - `sparql_count` - Count results
//!
//! ## Examples
//!
//! ### Registering Filters
//!
//! ```rust
//! use ggen_core::register::register_all;
//! use tera::Tera;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut tera = Tera::default();
//! register_all(&mut tera);
//!
//! // Now you can use filters in templates
//! let mut ctx = tera::Context::new();
//! ctx.insert("name", "hello_world");
//! let result = tera.render_str("{{ name | pascal }}", &ctx)?;
//! assert_eq!(result, "HelloWorld");
//! # Ok(())
//! # }
//! ```
//!
//! ### Context Blessing
//!
//! ```rust
//! use ggen_core::register::bless_context;
//! use tera::Context;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut ctx = Context::new();
//! ctx.insert("name", "user_profile");
//!
//! bless_context(&mut ctx);
//!
//! // Name is auto-generated from name
//! assert_eq!(ctx.get("Name").unwrap().as_str().unwrap(), "UserProfile");
//! # Ok(())
//! # }
//! ```

use heck::{
    ToShoutyKebabCase,
    ToShoutySnakeCase,
    ToTitleCase, // supplemental only
};
use inflector::{
    cases::{
        camelcase, classcase, kebabcase, pascalcase, sentencecase, snakecase, titlecase, traincase,
    },
    numbers::{deordinalize, ordinalize},
    string::{deconstantize, demodulize, pluralize, singularize},
    suffix::foreignkey,
};
use std::collections::HashMap;
use tera::{Context, Result as TeraResult, Tera, Value};

/// Register all text transformation helpers into Tera.
pub fn register_all(tera: &mut Tera) {
    // ---------- Inflector canonical ----------
    reg_str(tera, "camel", camelcase::to_camel_case);
    reg_str(tera, "pascal", pascalcase::to_pascal_case);
    reg_str(tera, "snake", snakecase::to_snake_case);
    reg_str(tera, "kebab", kebabcase::to_kebab_case);
    reg_str(tera, "class", classcase::to_class_case);
    reg_str(tera, "title", titlecase::to_title_case);
    reg_str(tera, "sentence", sentencecase::to_sentence_case);
    reg_str(tera, "train", traincase::to_train_case);

    reg_str(tera, "pluralize", pluralize::to_plural);
    reg_str(tera, "singularize", singularize::to_singular);
    reg_str(tera, "deconstantize", deconstantize::deconstantize);
    reg_str(tera, "demodulize", demodulize::demodulize);

    reg_str(tera, "ordinalize", ordinalize::ordinalize);
    reg_str(tera, "deordinalize", deordinalize::deordinalize);

    reg_str(tera, "foreign_key", foreignkey::to_foreign_key);

    // ---------- Heck fill-ins (not in Inflector) ----------
    reg_str(tera, "shouty_snake", |s| s.to_shouty_snake_case());
    reg_str(tera, "shouty_kebab", |s| s.to_shouty_kebab_case());
    reg_str(tera, "titlecase", |s| s.to_title_case()); // better consistency

    // ---------- Common change-case style aliases ----------
    reg_str(tera, "param", kebabcase::to_kebab_case);
    reg_str(tera, "constant", |s| s.to_shouty_snake_case());
    reg_str(tera, "upper", |s| s.to_uppercase());
    reg_str(tera, "lower", |s| s.to_lowercase());
    reg_str(tera, "lcfirst", |s| {
        let mut c = s.chars();
        match c.next() {
            Some(f) => f.to_lowercase().collect::<String>() + c.as_str(),
            None => String::new(),
        }
    });
    reg_str(tera, "ucfirst", |s| {
        let mut c = s.chars();
        match c.next() {
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
            None => String::new(),
        }
    });

    // ---------- SPARQL projection helpers ----------
    register_sparql_helpers(tera);
}

/// Auto-bless context variables for Hygen compatibility.
///
/// This function adds common derived variables to the context:
/// - `Name` = `name | pascal` (when `name` exists)
/// - `locals` = alias to the same context (for Hygen compatibility)
pub fn bless_context(context: &mut Context) {
    // Auto-bless Name when name exists
    if let Some(name_value) = context.get("name") {
        if let Some(name_str) = name_value.as_str() {
            let pascal_name = pascalcase::to_pascal_case(name_str);
            context.insert("Name", &pascal_name);
        }
    }

    // Add locals alias (Hygen compatibility)
    // In Hygen, locals is an alias to the context itself
    // We'll add it as a function that returns the context
    context.insert("locals", &HashMap::<String, Value>::new());
}

// ---------- SPARQL projection helpers ----------
fn register_sparql_helpers(tera: &mut Tera) {
    // Helper to get a specific column from SPARQL results
    tera.register_function("sparql_column", SparqlColumnFn);

    // Helper to get a specific row from SPARQL results
    tera.register_function("sparql_row", SparqlRowFn);

    // Helper to get the first value from a specific column
    tera.register_function("sparql_first", SparqlFirstFn);

    // Helper to get all values from a specific column
    tera.register_function("sparql_values", SparqlValuesFn);

    // Helper to check if SPARQL results are empty
    tera.register_function("sparql_empty", SparqlEmptyFn);

    // Helper to get the count of SPARQL results
    tera.register_function("sparql_count", SparqlCountFn);
}

#[derive(Clone)]
struct SparqlColumnFn;

impl tera::Function for SparqlColumnFn {
    fn call(&self, args: &HashMap<String, Value>) -> TeraResult<Value> {
        let results = args
            .get("results")
            .ok_or_else(|| tera::Error::msg("sparql_column: results parameter required"))?;
        let column = args
            .get("column")
            .and_then(|v| v.as_str())
            .ok_or_else(|| tera::Error::msg("sparql_column: column parameter required"))?;

        if let Some(array) = results.as_array() {
            let mut column_values = Vec::new();
            for row in array {
                if let Some(obj) = row.as_object() {
                    // Try exact column name first
                    if let Some(value) = obj.get(column) {
                        column_values.push(value.clone());
                    } else {
                        // Try with ? prefix (SPARQL variable names)
                        let sparql_column = format!("?{}", column);
                        if let Some(value) = obj.get(&sparql_column) {
                            column_values.push(value.clone());
                        }
                    }
                }
            }
            Ok(Value::Array(column_values))
        } else {
            Ok(Value::Array(Vec::new()))
        }
    }
}

#[derive(Clone)]
struct SparqlRowFn;

impl tera::Function for SparqlRowFn {
    fn call(&self, args: &HashMap<String, Value>) -> TeraResult<Value> {
        let results = args
            .get("results")
            .ok_or_else(|| tera::Error::msg("sparql_row: results parameter required"))?;
        let index = args
            .get("index")
            .and_then(|v| v.as_number())
            .and_then(|n| n.as_u64())
            .ok_or_else(|| tera::Error::msg("sparql_row: index parameter required"))?;

        if let Some(array) = results.as_array() {
            if let Some(row) = array.get(index as usize) {
                Ok(row.clone())
            } else {
                Ok(Value::Null)
            }
        } else {
            Ok(Value::Null)
        }
    }
}

#[derive(Clone)]
struct SparqlFirstFn;

impl tera::Function for SparqlFirstFn {
    fn call(&self, args: &HashMap<String, Value>) -> TeraResult<Value> {
        let results = args
            .get("results")
            .ok_or_else(|| tera::Error::msg("sparql_first: results parameter required"))?;
        let column = args
            .get("column")
            .and_then(|v| v.as_str())
            .ok_or_else(|| tera::Error::msg("sparql_first: column parameter required"))?;

        if let Some(array) = results.as_array() {
            if let Some(first_row) = array.first() {
                if let Some(obj) = first_row.as_object() {
                    // Try exact column name first
                    if let Some(value) = obj.get(column) {
                        return Ok(value.clone());
                    }
                    // Try with ? prefix (SPARQL variable names)
                    let sparql_column = format!("?{}", column);
                    if let Some(value) = obj.get(&sparql_column) {
                        return Ok(value.clone());
                    }
                }
            }
        }
        Ok(Value::Null)
    }
}

#[derive(Clone)]
struct SparqlValuesFn;

impl tera::Function for SparqlValuesFn {
    fn call(&self, args: &HashMap<String, Value>) -> TeraResult<Value> {
        let results = args
            .get("results")
            .ok_or_else(|| tera::Error::msg("sparql_values: results parameter required"))?;
        let column = args
            .get("column")
            .and_then(|v| v.as_str())
            .ok_or_else(|| tera::Error::msg("sparql_values: column parameter required"))?;

        if let Some(array) = results.as_array() {
            let mut values = Vec::new();
            for row in array {
                if let Some(obj) = row.as_object() {
                    // Try exact column name first
                    if let Some(value) = obj.get(column) {
                        if let Some(str_val) = value.as_str() {
                            values.push(Value::String(str_val.to_string()));
                        } else {
                            values.push(value.clone());
                        }
                    } else {
                        // Try with ? prefix (SPARQL variable names)
                        let sparql_column = format!("?{}", column);
                        if let Some(value) = obj.get(&sparql_column) {
                            if let Some(str_val) = value.as_str() {
                                values.push(Value::String(str_val.to_string()));
                            } else {
                                values.push(value.clone());
                            }
                        }
                    }
                }
            }
            Ok(Value::Array(values))
        } else {
            Ok(Value::Array(Vec::new()))
        }
    }
}

#[derive(Clone)]
struct SparqlEmptyFn;

impl tera::Function for SparqlEmptyFn {
    fn call(&self, args: &HashMap<String, Value>) -> TeraResult<Value> {
        let results = args
            .get("results")
            .ok_or_else(|| tera::Error::msg("sparql_empty: results parameter required"))?;

        let is_empty = match results {
            Value::Array(arr) => arr.is_empty(),
            Value::Bool(_) => false, // Boolean results are never "empty"
            _ => true,
        };

        Ok(Value::Bool(is_empty))
    }
}

#[derive(Clone)]
struct SparqlCountFn;

impl tera::Function for SparqlCountFn {
    fn call(&self, args: &HashMap<String, Value>) -> TeraResult<Value> {
        let results = args
            .get("results")
            .ok_or_else(|| tera::Error::msg("sparql_count: results parameter required"))?;

        let count = match results {
            Value::Array(arr) => arr.len(),
            Value::Bool(_) => 1, // Boolean results count as 1
            _ => 0,
        };

        Ok(Value::Number(tera::Number::from(count)))
    }
}

// ---------- internals ----------
fn reg_str<F>(tera: &mut Tera, name: &str, f: F)
where
    F: Fn(&str) -> String + Send + Sync + 'static,
{
    tera.register_filter(
        name,
        move |v: &Value, _a: &HashMap<String, Value>| -> TeraResult<Value> {
            let input_str = match v.as_str() {
                Some(s) => s.to_string(),
                None => v.to_string(),
            };
            Ok(Value::String(f(&input_str)))
        },
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use tera::Context;

    fn create_test_tera() -> Tera {
        let mut tera = Tera::default();
        register_all(&mut tera);
        tera
    }

    test!(test_case_conversions, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();
        ctx.insert("name", "hello_world_example");

        // Test camel case
        let result = tera.render_str("{{ name | camel }}", &ctx).unwrap();
        assert_eq!(result, "helloWorldExample");

        // Test pascal case
        let result = tera.render_str("{{ name | pascal }}", &ctx).unwrap();
        assert_eq!(result, "HelloWorldExample");

        // Test snake case
        let result = tera.render_str("{{ name | snake }}", &ctx).unwrap();
        assert_eq!(result, "hello_world_example");

        // Test kebab case
        let result = tera.render_str("{{ name | kebab }}", &ctx).unwrap();
        assert_eq!(result, "hello-world-example");

        // Test class case
        let result = tera.render_str("{{ name | class }}", &ctx).unwrap();
        assert_eq!(result, "HelloWorldExample");

        // Test title case
        let result = tera.render_str("{{ name | title }}", &ctx).unwrap();
        assert_eq!(result, "Hello World Example");

        // Test sentence case
        let result = tera.render_str("{{ name | sentence }}", &ctx).unwrap();
        assert_eq!(result, "Hello world example");

        // Test train case
        let result = tera.render_str("{{ name | train }}", &ctx).unwrap();
        assert_eq!(result, "Hello-World-Example");
    });

    test!(test_heck_fill_ins, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();
        ctx.insert("name", "hello_world_example");

        // Test shouty snake case
        let result = tera.render_str("{{ name | shouty_snake }}", &ctx).unwrap();
        assert_eq!(result, "HELLO_WORLD_EXAMPLE");

        // Test shouty kebab case
        let result = tera.render_str("{{ name | shouty_kebab }}", &ctx).unwrap();
        assert_eq!(result, "HELLO-WORLD-EXAMPLE");

        // Test titlecase (heck version)
        let result = tera.render_str("{{ name | titlecase }}", &ctx).unwrap();
        assert_eq!(result, "Hello World Example");
    });

    test!(test_ordinalization, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test ordinalize
        ctx.insert("num", "1");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "1st");

        ctx.insert("num", "2");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "2nd");

        ctx.insert("num", "3");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "3rd");

        ctx.insert("num", "4");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "4th");

        ctx.insert("num", "11");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "11th");

        // Test deordinalize
        ctx.insert("num", "1st");
        let result = tera.render_str("{{ num | deordinalize }}", &ctx).unwrap();
        assert_eq!(result, "1");

        ctx.insert("num", "2nd");
        let result = tera.render_str("{{ num | deordinalize }}", &ctx).unwrap();
        assert_eq!(result, "2");
    });

    test!(test_string_manipulation, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test deconstantize
        ctx.insert("path", "ActiveRecord::Base");
        let result = tera.render_str("{{ path | deconstantize }}", &ctx).unwrap();
        assert_eq!(result, "ActiveRecord");

        // Test demodulize
        ctx.insert("path", "ActiveRecord::Base");
        let result = tera.render_str("{{ path | demodulize }}", &ctx).unwrap();
        assert_eq!(result, "Base");

        // Test foreign_key
        ctx.insert("name", "Message");
        let result = tera.render_str("{{ name | foreign_key }}", &ctx).unwrap();
        assert_eq!(result, "message_id");
    });

    test!(test_complex_combinations, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();
        ctx.insert("class_name", "UserProfile");

        // Test chaining transformations
        let result = tera
            .render_str("{{ class_name | snake | pluralize }}", &ctx)
            .unwrap();
        assert_eq!(result, "user_profiles");

        let result = tera
            .render_str("{{ class_name | kebab | shouty_kebab }}", &ctx)
            .unwrap();
        assert_eq!(result, "USER-PROFILE");

        let result = tera
            .render_str("{{ class_name | demodulize | snake }}", &ctx)
            .unwrap();
        assert_eq!(result, "user_profile");
    });

    test!(test_edge_cases, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test empty string
        ctx.insert("empty", "");
        let result = tera.render_str("{{ empty | camel }}", &ctx).unwrap();
        assert_eq!(result, "");

        // Test single character
        ctx.insert("single", "A");
        let result = tera.render_str("{{ single | snake }}", &ctx).unwrap();
        assert_eq!(result, "a");

        // Test numbers
        ctx.insert("number", "123");
        let result = tera.render_str("{{ number | camel }}", &ctx).unwrap();
        assert_eq!(result, "123");

        // Test special characters
        ctx.insert("special", "hello@world#test");
        let result = tera.render_str("{{ special | kebab }}", &ctx).unwrap();
        assert_eq!(result, "hello-world-test");
    });

    test!(test_non_string_input, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test with number
        ctx.insert("num", &42);
        let result = tera.render_str("{{ num | camel }}", &ctx).unwrap();
        assert_eq!(result, "42");

        // Test with boolean
        ctx.insert("flag", &true);
        let result = tera.render_str("{{ flag | snake }}", &ctx).unwrap();
        assert_eq!(result, "true");
    });

    test!(test_real_world_scenarios, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test database table naming
        ctx.insert("model", "UserAccount");
        let table_name = tera
            .render_str("{{ model | snake | pluralize }}", &ctx)
            .unwrap();
        assert_eq!(table_name, "user_accounts");

        // Test API endpoint naming
        ctx.insert("resource", "UserProfile");
        let endpoint = tera
            .render_str("{{ resource | kebab | pluralize }}", &ctx)
            .unwrap();
        assert_eq!(endpoint, "user-profiles");

        // Test constant naming
        ctx.insert("name", "max_retry_count");
        let constant = tera.render_str("{{ name | shouty_snake }}", &ctx).unwrap();
        assert_eq!(constant, "MAX_RETRY_COUNT");

        // Test class method naming
        ctx.insert("action", "get_user_profile");
        let method = tera.render_str("{{ action | camel }}", &ctx).unwrap();
        assert_eq!(method, "getUserProfile");
    });

    test!(test_change_case_aliases, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();
        ctx.insert("name", "hello_world_example");

        // Test param (kebab-case)
        let result = tera.render_str("{{ name | param }}", &ctx).unwrap();
        assert_eq!(result, "hello-world-example");

        // Test constant (SCREAMING_SNAKE_CASE)
        let result = tera.render_str("{{ name | constant }}", &ctx).unwrap();
        assert_eq!(result, "HELLO_WORLD_EXAMPLE");

        // Test upper
        let result = tera.render_str("{{ name | upper }}", &ctx).unwrap();
        assert_eq!(result, "HELLO_WORLD_EXAMPLE");

        // Test lower
        ctx.insert("name", "HELLO_WORLD_EXAMPLE");
        let result = tera.render_str("{{ name | lower }}", &ctx).unwrap();
        assert_eq!(result, "hello_world_example");

        // Test lcfirst
        ctx.insert("name", "HelloWorld");
        let result = tera.render_str("{{ name | lcfirst }}", &ctx).unwrap();
        assert_eq!(result, "helloWorld");

        // Test ucfirst
        ctx.insert("name", "helloWorld");
        let result = tera.render_str("{{ name | ucfirst }}", &ctx).unwrap();
        assert_eq!(result, "HelloWorld");

        // Test edge cases for lcfirst/ucfirst
        ctx.insert("name", "");
        let result = tera.render_str("{{ name | lcfirst }}", &ctx).unwrap();
        assert_eq!(result, "");

        let result = tera.render_str("{{ name | ucfirst }}", &ctx).unwrap();
        assert_eq!(result, "");

        // Test Unicode handling
        ctx.insert("name", "ñáéíóú");
        let result = tera.render_str("{{ name | ucfirst }}", &ctx).unwrap();
        assert_eq!(result, "Ñáéíóú");
    });

    test!(test_all_filters_registered, {
        let mut tera = create_test_tera();

        // Test that all expected filters are registered
        let expected_filters = vec![
            "camel",
            "pascal",
            "snake",
            "kebab",
            "class",
            "title",
            "sentence",
            "train",
            "pluralize",
            "singularize",
            "deconstantize",
            "demodulize",
            "ordinalize",
            "deordinalize",
            "foreign_key",
            "shouty_snake",
            "shouty_kebab",
            "titlecase",
            "param",
            "constant",
            "upper",
            "lower",
            "lcfirst",
            "ucfirst",
        ];

        for filter in expected_filters {
            let mut ctx = Context::new();
            ctx.insert("test", "hello_world");

            // This should not panic if the filter is registered
            let result = tera.render_str(&format!("{{{{ test | {} }}}}", filter), &ctx);
            assert!(result.is_ok(), "Filter '{}' should be registered", filter);
        }
    });

    test!(test_bless_context_name, {
        let mut ctx = Context::new();
        ctx.insert("name", "hello_world");

        bless_context(&mut ctx);

        // Should have Name auto-blessed
        assert_eq!(ctx.get("Name").unwrap().as_str().unwrap(), "HelloWorld");
        // Original name should still be there
        assert_eq!(ctx.get("name").unwrap().as_str().unwrap(), "hello_world");
    });

    test!(test_bless_context_no_name, {
        let mut ctx = Context::new();
        ctx.insert("other", "value");

        bless_context(&mut ctx);

        // Should not have Name when name doesn't exist
        assert!(ctx.get("Name").is_none());
        // Should have locals placeholder
        assert!(ctx.get("locals").is_some());
    });

    test!(test_bless_context_non_string_name, {
        let mut ctx = Context::new();
        ctx.insert("name", &42); // Non-string value

        bless_context(&mut ctx);

        // Should not bless Name for non-string values
        assert!(ctx.get("Name").is_none());
    });

    test!(test_sparql_column_function, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Create mock SPARQL results
        let results = serde_json::json!([
            {"name": "Alice", "age": "30"},
            {"name": "Bob", "age": "25"},
            {"name": "Charlie", "age": "35"}
        ]);
        ctx.insert("results", &results);

        // Test extracting name column
        let result = tera
            .render_str(
                "{{ sparql_column(results=results, column=\"name\") }}",
                &ctx,
            )
            .unwrap();
        // Tera returns a string representation of the array
        assert_eq!(result, "[Alice, Bob, Charlie]");
    });

    test!(test_sparql_row_function, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        let results = serde_json::json!([
            {"name": "Alice", "age": "30"},
            {"name": "Bob", "age": "25"}
        ]);
        ctx.insert("results", &results);

        // Test getting first row
        let result = tera
            .render_str("{{ sparql_row(results=results, index=0) }}", &ctx)
            .unwrap();
        assert_eq!(result, "[object]");

        // Test getting second row
        let result = tera
            .render_str("{{ sparql_row(results=results, index=1) }}", &ctx)
            .unwrap();
        assert_eq!(result, "[object]");

        // Test out of bounds
        let result = tera
            .render_str("{{ sparql_row(results=results, index=5) }}", &ctx)
            .unwrap();
        assert_eq!(result, "");
    });

    test!(test_sparql_first_function, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        let results = serde_json::json!([
            {"name": "Alice", "age": "30"},
            {"name": "Bob", "age": "25"}
        ]);
        ctx.insert("results", &results);

        // Test getting first name
        let result = tera
            .render_str("{{ sparql_first(results=results, column=\"name\") }}", &ctx)
            .unwrap();
        assert_eq!(result, "Alice");

        // Test getting first age
        let result = tera
            .render_str("{{ sparql_first(results=results, column=\"age\") }}", &ctx)
            .unwrap();
        assert_eq!(result, "30");

        // Test non-existent column
        let result = tera
            .render_str(
                "{{ sparql_first(results=results, column=\"nonexistent\") }}",
                &ctx,
            )
            .unwrap();
        assert_eq!(result, "");
    });

    test!(test_sparql_values_function, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        let results = serde_json::json!([
            {"name": "Alice", "age": "30"},
            {"name": "Bob", "age": "25"},
            {"name": "Charlie", "age": "35"}
        ]);
        ctx.insert("results", &results);

        // Test extracting all names
        let result = tera
            .render_str(
                "{{ sparql_values(results=results, column=\"name\") }}",
                &ctx,
            )
            .unwrap();
        assert_eq!(result, "[Alice, Bob, Charlie]");
    });

    test!(test_sparql_empty_function, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test empty results
        let empty_results = serde_json::json!([]);
        ctx.insert("empty_results", &empty_results);
        let result = tera
            .render_str("{{ sparql_empty(results=empty_results) }}", &ctx)
            .unwrap();
        assert_eq!(result, "true");

        // Test non-empty results
        let non_empty_results = serde_json::json!([
            {"name": "Alice", "age": "30"}
        ]);
        ctx.insert("non_empty_results", &non_empty_results);
        let result = tera
            .render_str("{{ sparql_empty(results=non_empty_results) }}", &ctx)
            .unwrap();
        assert_eq!(result, "false");

        // Test boolean results
        let bool_results = serde_json::json!(true);
        ctx.insert("bool_results", &bool_results);
        let result = tera
            .render_str("{{ sparql_empty(results=bool_results) }}", &ctx)
            .unwrap();
        assert_eq!(result, "false");
    });

    test!(test_sparql_count_function, {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test counting results
        let results = serde_json::json!([
            {"name": "Alice", "age": "30"},
            {"name": "Bob", "age": "25"},
            {"name": "Charlie", "age": "35"}
        ]);
        ctx.insert("results", &results);
        let result = tera
            .render_str("{{ sparql_count(results=results) }}", &ctx)
            .unwrap();
        assert_eq!(result, "3");

        // Test empty results
        let empty_results = serde_json::json!([]);
        ctx.insert("empty_results", &empty_results);
        let result = tera
            .render_str("{{ sparql_count(results=empty_results) }}", &ctx)
            .unwrap();
        assert_eq!(result, "0");

        // Test boolean results
        let bool_results = serde_json::json!(true);
        ctx.insert("bool_results", &bool_results);
        let result = tera
            .render_str("{{ sparql_count(results=bool_results) }}", &ctx)
            .unwrap();
        assert_eq!(result, "1");
    });
}
