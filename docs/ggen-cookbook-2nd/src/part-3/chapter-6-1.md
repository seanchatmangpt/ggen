<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 1: Single File Generator](#pattern-1-single-file-generator)
  - [Intent](#intent)
  - [Motivation](#motivation)
  - [Applicability](#applicability)
  - [Structure](#structure)
  - [Implementation](#implementation)
    - [1. Define the Output Location](#1-define-the-output-location)
    - [2. Structure the Data](#2-structure-the-data)
    - [3. Generate the Implementation](#3-generate-the-implementation)
  - [Example: User Struct](#example-user-struct)
    - [Input Data](#input-data)
    - [Generated Output](#generated-output)
  - [Example: Configuration File](#example-configuration-file)
    - [Input Data](#input-data-1)
    - [Generated Output](#generated-output-1)
  - [Example: Utility Function](#example-utility-function)
    - [Input Data](#input-data-2)
    - [Generated Output](#generated-output-2)
  - [Consequences](#consequences)
    - [Benefits](#benefits)
    - [Drawbacks](#drawbacks)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 1: Single File Generator](#pattern-1-single-file-generator)
  - [Intent](#intent)
  - [Motivation](#motivation)
  - [Applicability](#applicability)
  - [Structure](#structure)
  - [Implementation](#implementation)
  - [Example: User Struct](#example-user-struct)
  - [Example: Configuration File](#example-configuration-file)
  - [Example: Utility Function](#example-utility-function)
  - [Consequences](#consequences)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 1: Single File Generator

**Generate a single source file from structured data**

## Intent

Transform structured data into a single, coherent source file while maintaining type safety and following language conventions.

## Motivation

Many code generation tasks involve creating individual files that encapsulate specific functionality. These files often follow predictable patterns but need customization based on domain-specific data.

## Applicability

Use this pattern when:
- Generating individual structs, enums, or classes
- Creating configuration files or resource definitions
- Producing utility functions or helper modules
- Working with data that naturally maps to a single file

## Structure

```yaml
---
to: "output/{{ entity_name | lower }}.rs"
vars:
  entity_name: "User"
  fields:
    - name: "id"
      type: "u64"
      required: true
    - name: "username"
      type: "String"
      required: true
    - name: "email"
      type: "String"
      required: false
---

{{#if derive_traits}}
#[derive({{#each derive_traits}}{{ this }}{{^last}}, {{/last}}{{/each}})]
{{/if}}
pub struct {{ entity_name }} {
    {{#each fields}}
    {{#if required}}
    pub {{ name }}: {{ type }},
    {{else}}
    pub {{ name }}: Option<{{ type }}>,
    {{/if}}
    {{/each}}
}

impl {{ entity_name }} {
    pub fn new(
        {{#each fields}}
        {{#if required}}
        {{ name }}: {{ type }},
        {{else}}
        {{ name }}: Option<{{ type }}>,
        {{/if}}
        {{/each}}
    ) -> Self {
        Self {
            {{#each fields}}
            {{ name }},
            {{/each}}
        }
    }
}
```

## Implementation

### 1. Define the Output Location
```yaml
to: "src/{{ module }}/{{ name | lower }}.rs"
```

### 2. Structure the Data
```yaml
vars:
  module: "models"
  name: "Product"
  fields:
    - name: "id"
      type: "u64"
    - name: "name"
      type: "String"
    - name: "price"
      type: "f64"
```

### 3. Generate the Implementation
```rust
impl {{ name }} {
    pub fn calculate_tax(&self, rate: f64) -> f64 {
        self.price * rate
    }

    pub fn is_expensive(&self) -> bool {
        self.price > 100.0
    }
}
```

## Example: User Struct

### Input Data
```yaml
vars:
  struct_name: "User"
  fields:
    - name: "id"
      type: "u64"
      description: "Primary key"
    - name: "username"
      type: "String"
      description: "Unique username"
    - name: "email"
      type: "String"
      description: "Email address"
    - name: "created_at"
      type: "chrono::DateTime<chrono::Utc>"
      description: "Account creation time"
```

### Generated Output
```rust
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct User {
    /// Primary key
    pub id: u64,
    /// Unique username
    pub username: String,
    /// Email address
    pub email: String,
    /// Account creation time
    pub created_at: DateTime<Utc>,
}

impl User {
    pub fn new(
        id: u64,
        username: String,
        email: String,
        created_at: DateTime<Utc>,
    ) -> Self {
        Self {
            id,
            username,
            email,
            created_at,
        }
    }

    pub fn is_active(&self) -> bool {
        // Check if user was created recently
        let week_ago = Utc::now() - chrono::Duration::weeks(1);
        self.created_at > week_ago
    }
}
```

## Example: Configuration File

### Input Data
```yaml
vars:
  config_name: "DatabaseConfig"
  settings:
    - name: "host"
      type: "String"
      default: "\"localhost\""
    - name: "port"
      type: "u16"
      default: "5432"
    - name: "database"
      type: "String"
      default: "\"myapp\""
    - name: "pool_size"
      type: "usize"
      default: "10"
```

### Generated Output
```rust
#[derive(Debug, Clone, serde::Deserialize)]
pub struct DatabaseConfig {
    pub host: String,
    pub port: u16,
    pub database: String,
    pub pool_size: usize,
}

impl Default for DatabaseConfig {
    fn default() -> Self {
        Self {
            host: "localhost".to_string(),
            port: 5432,
            database: "myapp".to_string(),
            pool_size: 10,
        }
    }
}

impl DatabaseConfig {
    pub fn connection_string(&self) -> String {
        format!(
            "postgresql://{}:{}/{}",
            self.host, self.port, self.database
        )
    }

    pub fn from_env() -> Self {
        Self {
            host: std::env::var("DB_HOST").unwrap_or_else(|_| "localhost".to_string()),
            port: std::env::var("DB_PORT")
                .unwrap_or_else(|_| "5432".to_string())
                .parse()
                .unwrap_or(5432),
            database: std::env::var("DB_NAME").unwrap_or_else(|_| "myapp".to_string()),
            pool_size: std::env::var("DB_POOL_SIZE")
                .unwrap_or_else(|_| "10".to_string())
                .parse()
                .unwrap_or(10),
        }
    }
}
```

## Example: Utility Function

### Input Data
```yaml
vars:
  function_name: "parse_duration"
  input_type: "&str"
  output_type: "Result<std::time::Duration, String>"
  examples:
    - input: "\"1h\""
      output: "Ok(Duration::from_secs(3600))"
    - input: "\"30m\""
      output: "Ok(Duration::from_secs(1800))"
    - input: "\"invalid\""
      output: "Err(\"Invalid duration format\".to_string())"
```

### Generated Output
```rust
use std::time::Duration;

/// Parse a duration string into a Duration object
///
/// # Examples
///
/// ```
/// use myapp::parse_duration;
///
/// assert_eq!(parse_duration("1h").unwrap(), Duration::from_secs(3600));
/// assert!(parse_duration("invalid").is_err());
/// ```
pub fn parse_duration(input: &str) -> Result<Duration, String> {
    let input = input.trim();

    if let Some(hours) = input.strip_suffix('h') {
        hours.parse::<u64>()
            .map(|h| Duration::from_secs(h * 3600))
            .map_err(|_| "Invalid hour format".to_string())
    } else if let Some(minutes) = input.strip_suffix('m') {
        minutes.parse::<u64>()
            .map(|m| Duration::from_secs(m * 60))
            .map_err(|_| "Invalid minute format".to_string())
    } else if let Some(seconds) = input.strip_suffix('s') {
        seconds.parse::<u64>()
            .map(Duration::from_secs)
            .map_err(|_| "Invalid second format".to_string())
    } else {
        Err("Invalid duration format. Use formats like '1h', '30m', '45s'".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_hours() {
        assert_eq!(parse_duration("1h").unwrap(), Duration::from_secs(3600));
        assert_eq!(parse_duration("2h").unwrap(), Duration::from_secs(7200));
    }

    #[test]
    fn test_parse_minutes() {
        assert_eq!(parse_duration("30m").unwrap(), Duration::from_secs(1800));
        assert_eq!(parse_duration("45m").unwrap(), Duration::from_secs(2700));
    }

    #[test]
    fn test_parse_seconds() {
        assert_eq!(parse_duration("30s").unwrap(), Duration::from_secs(30));
        assert_eq!(parse_duration("90s").unwrap(), Duration::from_secs(90));
    }

    #[test]
    fn test_invalid_format() {
        assert!(parse_duration("invalid").is_err());
        assert!(parse_duration("1x").is_err());
    }
}
```

## Consequences

### Benefits
- **Simplicity**: Single responsibility per template
- **Testability**: Easy to test individual file generation
- **Maintainability**: Clear input/output relationship
- **Reusability**: Can be used as building blocks for larger patterns

### Drawbacks
- **Limited scope**: Not suitable for multi-file generation
- **Context isolation**: No awareness of other generated files
- **Potential duplication**: Similar logic may be repeated across templates

## Related Patterns

- **Pattern 2: Multi-File Project** - For generating related files together
- **Pattern 3: Conditional Generation** - For including/excluding parts based on conditions
- **Pattern 4: Template Inheritance** - For extending existing single-file templates
- **Pattern 5: Dynamic Variables** - For extracting data from external sources

This pattern is the foundation of all GGen template development. Start with single-file generators to understand the basic mechanics, then combine them using more advanced patterns.
