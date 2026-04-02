//! Hello World Utilities
//!
//! Simple utility package demonstrating ggen marketplace functionality.
//! This package provides basic utilities that can be installed and used
//! to demonstrate the marketplace system.

use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Configuration for hello world utilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HelloConfig {
    pub greeting: String,
    pub name: String,
    pub repeat_count: usize,
}

impl Default for HelloConfig {
    fn default() -> Self {
        Self {
            greeting: "Hello".to_string(),
            name: "World".to_string(),
            repeat_count: 1,
        }
    }
}

/// Main hello world utility
pub struct HelloWorld {
    config: HelloConfig,
}

impl HelloWorld {
    /// Create a new hello world instance
    pub fn new(config: HelloConfig) -> Self {
        Self { config }
    }

    /// Generate a hello world message
    pub fn greet(&self) -> String {
        format!("{} {}!", self.config.greeting, self.config.name)
    }

    /// Generate multiple greetings
    pub fn greet_many(&self) -> Vec<String> {
        (0..self.config.repeat_count)
            .map(|i| format!("{} {}! (#{})", self.config.greeting, self.config.name, i + 1))
            .collect()
    }

    /// Get configuration as JSON
    pub fn config_json(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(&self.config)?)
    }

    /// Validate the configuration
    pub fn validate_config(&self) -> Result<()> {
        if self.config.greeting.trim().is_empty() {
            return Err(anyhow::anyhow!("Greeting cannot be empty"));
        }
        if self.config.name.trim().is_empty() {
            return Err(anyhow::anyhow!("Name cannot be empty"));
        }
        if self.config.repeat_count == 0 {
            return Err(anyhow::anyhow!("Repeat count must be greater than 0"));
        }
        Ok(())
    }
}

impl Default for HelloWorld {
    fn default() -> Self {
        Self::new(HelloConfig::default())
    }
}

/// Utility functions
pub mod utils {
    use super::*;

    /// Create a greeting with custom configuration
    pub fn create_greeting(greeting: &str, name: &str) -> String {
        format!("{} {}!", greeting, name)
    }

    /// Create multiple greetings
    pub fn create_multiple_greetings(greeting: &str, name: &str, count: usize) -> Vec<String> {
        (0..count)
            .map(|i| format!("{} {}! (#{})", greeting, name, i + 1))
            .collect()
    }

    /// Validate greeting configuration
    pub fn validate_greeting_config(greeting: &str, name: &str, count: usize) -> Result<()> {
        if greeting.trim().is_empty() {
            return Err(anyhow::anyhow!("Greeting cannot be empty"));
        }
        if name.trim().is_empty() {
            return Err(anyhow::anyhow!("Name cannot be empty"));
        }
        if count == 0 {
            return Err(anyhow::anyhow!("Count must be greater than 0"));
        }
        Ok(())
    }
}

/// Example usage and tests
pub mod examples {
    use super::*;

    /// Run a basic example
    pub fn run_basic_example() -> Result<()> {
        let hello = HelloWorld::default();
        println!("{}", hello.greet());

        let greetings = hello.greet_many();
        for greeting in greetings {
            println!("{}", greeting);
        }

        Ok(())
    }

    /// Run a custom example
    pub fn run_custom_example() -> Result<()> {
        let config = HelloConfig {
            greeting: "Greetings".to_string(),
            name: "Universe".to_string(),
            repeat_count: 3,
        };

        let hello = HelloWorld::new(config);
        hello.validate_config()?;

        println!("Custom greeting:");
        println!("{}", hello.greet());

        println!("\nMultiple greetings:");
        for greeting in hello.greet_many() {
            println!("{}", greeting);
        }

        println!("\nConfiguration JSON:");
        println!("{}", hello.config_json()?);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_greeting() {
        let hello = HelloWorld::default();
        assert_eq!(hello.greet(), "Hello World!");
    }

    #[test]
    fn test_custom_greeting() {
        let config = HelloConfig {
            greeting: "Hi".to_string(),
            name: "Rust".to_string(),
            repeat_count: 1,
        };
        let hello = HelloWorld::new(config);
        assert_eq!(hello.greet(), "Hi Rust!");
    }

    #[test]
    fn test_multiple_greetings() {
        let config = HelloConfig {
            greeting: "Hello".to_string(),
            name: "Test".to_string(),
            repeat_count: 3,
        };
        let hello = HelloWorld::new(config);
        let greetings = hello.greet_many();
        assert_eq!(greetings.len(), 3);
        assert_eq!(greetings[0], "Hello Test! (#1)");
        assert_eq!(greetings[1], "Hello Test! (#2)");
        assert_eq!(greetings[2], "Hello Test! (#3)");
    }

    #[test]
    fn test_config_validation() {
        let config = HelloConfig {
            greeting: "Hello".to_string(),
            name: "World".to_string(),
            repeat_count: 1,
        };
        let hello = HelloWorld::new(config);
        assert!(hello.validate_config().is_ok());
    }

    #[test]
    fn test_config_validation_empty_greeting() {
        let config = HelloConfig {
            greeting: "".to_string(),
            name: "World".to_string(),
            repeat_count: 1,
        };
        let hello = HelloWorld::new(config);
        assert!(hello.validate_config().is_err());
    }

    #[test]
    fn test_config_validation_empty_name() {
        let config = HelloConfig {
            greeting: "Hello".to_string(),
            name: "".to_string(),
            repeat_count: 1,
        };
        let hello = HelloWorld::new(config);
        assert!(hello.validate_config().is_err());
    }

    #[test]
    fn test_config_validation_zero_count() {
        let config = HelloConfig {
            greeting: "Hello".to_string(),
            name: "World".to_string(),
            repeat_count: 0,
        };
        let hello = HelloWorld::new(config);
        assert!(hello.validate_config().is_err());
    }

    #[test]
    fn test_utils_create_greeting() {
        let greeting = utils::create_greeting("Hi", "Rust");
        assert_eq!(greeting, "Hi Rust!");
    }

    #[test]
    fn test_utils_create_multiple_greetings() {
        let greetings = utils::create_multiple_greetings("Hello", "Test", 2);
        assert_eq!(greetings.len(), 2);
        assert_eq!(greetings[0], "Hello Test! (#1)");
        assert_eq!(greetings[1], "Hello Test! (#2)");
    }

    #[test]
    fn test_utils_validate_greeting_config() {
        assert!(utils::validate_greeting_config("Hello", "World", 1).is_ok());
        assert!(utils::validate_greeting_config("", "World", 1).is_err());
        assert!(utils::validate_greeting_config("Hello", "", 1).is_err());
        assert!(utils::validate_greeting_config("Hello", "World", 0).is_err());
    }

    #[test]
    fn test_config_json() {
        let config = HelloConfig {
            greeting: "Hello".to_string(),
            name: "World".to_string(),
            repeat_count: 1,
        };
        let hello = HelloWorld::new(config);
        let json = hello.config_json().unwrap();
        assert!(json.contains("Hello"));
        assert!(json.contains("World"));
        assert!(json.contains("1"));
    }
}
