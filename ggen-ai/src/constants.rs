//! Constants for ggen-ai
//!
//! Centralized configuration values to avoid magic numbers across the codebase.
//! This module provides default values for LLM configurations, autonomous systems,
//! governance policies, timeouts, and other system-wide settings.

/// LLM Configuration Defaults
///
/// Standard configuration values for language model interactions
pub mod llm {
    /// Default maximum tokens to generate (4096 tokens)
    pub const DEFAULT_MAX_TOKENS: u32 = 4096;

    /// Default temperature for sampling (0.7 = balanced creativity/determinism)
    pub const DEFAULT_TEMPERATURE: f32 = 0.7;

    /// Default top-p for nucleus sampling (0.9 = diverse but focused)
    pub const DEFAULT_TOP_P: f32 = 0.9;

    /// Default timeout for LLM requests in seconds
    pub const DEFAULT_TIMEOUT_SECS: u32 = 30;

    /// Default model for general use
    pub const DEFAULT_MODEL: &str = "gpt-3.5-turbo";

    /// Maximum token limit boundary (upper validation limit)
    pub const MAX_TOKEN_LIMIT: u32 = 199_999;

    /// Minimum token limit boundary (lower validation limit)
    pub const MIN_TOKEN_LIMIT: u32 = 1;

    /// Maximum temperature value (2.0 = very creative/random)
    pub const MAX_TEMPERATURE: f32 = 2.0;

    /// Minimum temperature value (0.0 = deterministic)
    pub const MIN_TEMPERATURE: f32 = 0.0;

    /// Maximum top-p value (1.0 = consider all tokens)
    pub const MAX_TOP_P: f32 = 1.0;

    /// Minimum top-p value (0.0 = most restrictive)
    pub const MIN_TOP_P: f32 = 0.0;
}

/// Autonomous System Defaults
///
/// Configuration for autonomous graph evolution and regeneration
pub mod autonomous {
    /// Default confidence threshold for accepting inferred triples (0.7 = 70% confidence)
    pub const DEFAULT_CONFIDENCE_THRESHOLD: f32 = 0.7;

    /// Default minimum changes to trigger regeneration
    pub const DEFAULT_REGENERATION_THRESHOLD: usize = 5;

    /// Target cycle time for orchestration in milliseconds (30 seconds)
    pub const TARGET_CYCLE_TIME_MS: u64 = 30_000;

    /// Health check interval in seconds
    pub const HEALTH_CHECK_INTERVAL_SECS: u64 = 60;

    /// Default estimated time per template regeneration (milliseconds)
    pub const ESTIMATED_REGEN_TIME_PER_TEMPLATE_MS: u64 = 1000;

    /// Minimum success rate threshold (0.9 = 90%)
    pub const MIN_SUCCESS_RATE: f64 = 0.9;
}

/// Governance System Limits
///
/// Policy enforcement and safety constraints
pub mod governance {
    /// Maximum policy time window in seconds (1 hour)
    pub const MAX_POLICY_WINDOW_SECS: u64 = 3600;

    /// Maximum output characters for safety
    pub const MAX_OUTPUT_CHARS: usize = 30_000;

    /// Default rate limit window in seconds
    pub const DEFAULT_RATE_LIMIT_WINDOW_SECS: u64 = 3600;

    /// Default maximum operations in rate limit window
    pub const DEFAULT_MAX_OPERATIONS: u64 = 100;
}

/// HTTP and Network Configuration
///
/// Timeouts and connection settings
pub mod network {
    /// Default HTTP request timeout in seconds
    pub const DEFAULT_REQUEST_TIMEOUT_SECS: u64 = 30;

    /// Ollama health check endpoint
    pub const OLLAMA_HEALTH_ENDPOINT: &str = "http://localhost:11434/api/tags";

    /// Expected HTTP success status code
    pub const HTTP_STATUS_OK: &str = "200";
}

/// Model-Specific Defaults
///
/// Default model names for each provider
pub mod models {
    /// Default OpenAI model
    pub const OPENAI_DEFAULT: &str = "gpt-3.5-turbo";

    /// Default Anthropic model
    pub const ANTHROPIC_DEFAULT: &str = "claude-3-sonnet-20240229";

    /// Default Ollama model
    pub const OLLAMA_DEFAULT: &str = "qwen3-coder:30b";

    /// Mock model for testing
    pub const MOCK_MODEL: &str = "mock-model";
}

/// Concurrency and Parallelism
///
/// Settings for parallel execution
pub mod concurrency {
    /// Default parallel workers (will be overridden by num_cpus::get() at runtime)
    pub const DEFAULT_PARALLEL_WORKERS: usize = 4;

    /// Maximum concurrent operations limit
    pub const MAX_CONCURRENT_OPERATIONS: usize = 16;

    /// Minimum concurrent operations
    pub const MIN_CONCURRENT_OPERATIONS: usize = 1;
}

/// Version and Versioning
///
/// Semantic versioning constants
pub mod versioning {
    /// Default initial version for artifacts
    pub const INITIAL_VERSION: &str = "1.0.0";

    /// Version component count (major.minor.patch)
    pub const VERSION_COMPONENTS: usize = 3;
}

/// Testing and Mock Defaults
///
/// Constants used in test environments
pub mod testing {
    /// Mock client default max tokens
    pub const MOCK_MAX_TOKENS: u32 = 1000;

    /// Mock client temperature (deterministic)
    pub const MOCK_TEMPERATURE: f32 = 0.0;

    /// Mock client top-p (no filtering)
    pub const MOCK_TOP_P: f32 = 1.0;

    /// Default mock response content
    pub const MOCK_RESPONSE: &str = "Mock response";
}

/// Cache and Storage
///
/// Settings for caching and data persistence
pub mod cache {
    /// Default cache size limit
    pub const DEFAULT_CACHE_SIZE: usize = 1000;

    /// Default cache TTL in seconds
    pub const DEFAULT_CACHE_TTL_SECS: u64 = 3600;
}

/// Environment Variables
///
/// Standard environment variable names
pub mod env_vars {
    /// Provider selection
    pub const LLM_PROVIDER: &str = "GGEN_LLM_PROVIDER";

    /// Model selection
    pub const LLM_MODEL: &str = "GGEN_LLM_MODEL";

    /// Default model fallback
    pub const DEFAULT_MODEL: &str = "GGEN_DEFAULT_MODEL";

    /// Temperature setting
    pub const LLM_TEMPERATURE: &str = "GGEN_LLM_TEMPERATURE";

    /// Max tokens setting
    pub const LLM_MAX_TOKENS: &str = "GGEN_LLM_MAX_TOKENS";

    /// Top-p setting
    pub const LLM_TOP_P: &str = "GGEN_LLM_TOP_P";

    /// Streaming mode
    pub const LLM_STREAMING: &str = "GGEN_LLM_STREAMING";

    /// Test mode flag
    pub const TEST_MODE: &str = "GGEN_TEST_MODE";

    /// OpenAI API key
    pub const OPENAI_API_KEY: &str = "OPENAI_API_KEY";

    /// Anthropic API key
    pub const ANTHROPIC_API_KEY: &str = "ANTHROPIC_API_KEY";

    /// OpenAI model
    pub const OPENAI_MODEL: &str = "OPENAI_MODEL";

    /// Anthropic model
    pub const ANTHROPIC_MODEL: &str = "ANTHROPIC_MODEL";

    /// Ollama model
    pub const OLLAMA_MODEL: &str = "OLLAMA_MODEL";
}

/// MCP Server Configuration
///
/// Settings for Model Context Protocol server
pub mod mcp {
    /// Server name
    pub const SERVER_NAME: &str = "ggen-ai-mcp";

    /// Server website
    pub const SERVER_WEBSITE: &str = "https://github.com/seanchatmangpt/ggen";

    /// Server description
    pub const SERVER_DESCRIPTION: &str = "AI-powered code generation and refactoring tools for ggen";

    /// Default number of tools
    pub const EXPECTED_TOOL_COUNT: usize = 9;
}

/// Target Languages
///
/// Supported programming languages for code generation
pub mod languages {
    /// Rust language identifier
    pub const RUST: &str = "rust";

    /// TypeScript language identifier
    pub const TYPESCRIPT: &str = "typescript";

    /// Python language identifier
    pub const PYTHON: &str = "python";
}

/// File System Paths
///
/// Default directory paths
pub mod paths {
    /// Default template directory
    pub const TEMPLATES_DIR: &str = "templates";

    /// Default generated output directory
    pub const GENERATED_DIR: &str = "generated";
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_llm_constants() {
        assert_eq!(llm::DEFAULT_MAX_TOKENS, 4096);
        assert_eq!(llm::DEFAULT_TEMPERATURE, 0.7);
        assert_eq!(llm::DEFAULT_TOP_P, 0.9);
        assert_eq!(llm::DEFAULT_TIMEOUT_SECS, 30);
    }

    #[test]
    fn test_autonomous_constants() {
        assert_eq!(autonomous::DEFAULT_CONFIDENCE_THRESHOLD, 0.7);
        assert_eq!(autonomous::DEFAULT_REGENERATION_THRESHOLD, 5);
        assert_eq!(autonomous::TARGET_CYCLE_TIME_MS, 30_000);
    }

    #[test]
    fn test_governance_constants() {
        assert_eq!(governance::MAX_POLICY_WINDOW_SECS, 3600);
        assert_eq!(governance::MAX_OUTPUT_CHARS, 30_000);
    }

    #[test]
    fn test_model_defaults() {
        assert_eq!(models::OPENAI_DEFAULT, "gpt-3.5-turbo");
        assert_eq!(models::ANTHROPIC_DEFAULT, "claude-3-sonnet-20240229");
        assert_eq!(models::OLLAMA_DEFAULT, "qwen3-coder:30b");
    }

    #[test]
    fn test_validation_limits() {
        assert!(llm::MIN_TEMPERATURE <= llm::DEFAULT_TEMPERATURE);
        assert!(llm::DEFAULT_TEMPERATURE <= llm::MAX_TEMPERATURE);
        assert!(llm::MIN_TOP_P <= llm::DEFAULT_TOP_P);
        assert!(llm::DEFAULT_TOP_P <= llm::MAX_TOP_P);
    }
}
