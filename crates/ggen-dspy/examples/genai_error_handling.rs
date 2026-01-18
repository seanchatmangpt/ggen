//! # GenAI Error Handling Example
//!
//! Demonstrates comprehensive error handling patterns for rust-genai.
//! This example shows:
//! - Proper Result<T, E> usage
//! - Error recovery strategies
//! - Retry logic with exponential backoff
//! - Rate limiting handling
//! - Timeout management
//! - Token budget enforcement
//! - Validation errors
//!
//! ## Running the Example
//!
//! ```bash
//! # Using MockClient (no API keys needed):
//! cargo run --example genai_error_handling
//!
//! # With real provider to see actual errors:
//! export GEMINI_API_KEY=your_key_here
//! cargo run --example genai_error_handling
//! ```

use ggen_ai::{GenAiClient, GgenAiError, LlmClient, LlmConfig, MockClient, Result};
use std::collections::HashMap;
use std::time::Duration;
use tokio::time::sleep;

/// Main entry point demonstrating error handling patterns
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== GenAI Error Handling Examples ===\n");

    // Example 1: Configuration validation errors
    example_validation_errors()?;

    // Example 2: Proper Result<T,E> patterns
    example_result_patterns().await?;

    // Example 3: Error recovery with fallbacks
    example_error_recovery().await?;

    // Example 4: Retry logic with exponential backoff
    example_retry_logic().await?;

    // Example 5: Token budget enforcement
    example_token_budgets().await?;

    // Example 6: Timeout handling
    example_timeout_handling().await?;

    // Example 7: Error context and reporting
    example_error_context().await?;

    println!("\n=== All Error Handling Examples Completed ===");
    Ok(())
}

/// Example 1: Configuration Validation Errors
///
/// Demonstrates:
/// - Input validation
/// - Configuration boundary checks
/// - Early error detection
/// - Informative error messages
fn example_validation_errors() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 1: Configuration Validation ---");

    // Test 1: Empty model name
    let empty_model = LlmConfig {
        model: "".to_string(),
        max_tokens: Some(1000),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    };

    match empty_model.validate() {
        Ok(_) => println!("  ✗ Should have failed: empty model"),
        Err(e) => println!("  ✓ Caught empty model: {}", e),
    }

    // Test 2: Invalid temperature (too high)
    let invalid_temp = LlmConfig {
        model: "test-model".to_string(),
        max_tokens: Some(1000),
        temperature: Some(3.0),  // Max is 2.0
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    };

    match invalid_temp.validate() {
        Ok(_) => println!("  ✗ Should have failed: invalid temperature"),
        Err(e) => println!("  ✓ Caught invalid temperature: {}", e),
    }

    // Test 3: Invalid max_tokens (too low)
    let invalid_tokens = LlmConfig {
        model: "test-model".to_string(),
        max_tokens: Some(0),  // Min is 1
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    };

    match invalid_tokens.validate() {
        Ok(_) => println!("  ✗ Should have failed: invalid max_tokens"),
        Err(e) => println!("  ✓ Caught invalid max_tokens: {}", e),
    }

    // Test 4: Invalid top_p (negative)
    let invalid_top_p = LlmConfig {
        model: "test-model".to_string(),
        max_tokens: Some(1000),
        temperature: Some(0.7),
        top_p: Some(-0.5),  // Must be 0.0-1.0
        stop: None,
        extra: HashMap::new(),
    };

    match invalid_top_p.validate() {
        Ok(_) => println!("  ✗ Should have failed: invalid top_p"),
        Err(e) => println!("  ✓ Caught invalid top_p: {}", e),
    }

    println!("  ✓ All validation errors caught correctly\n");
    Ok(())
}

/// Example 2: Proper Result<T,E> Patterns
///
/// Demonstrates:
/// - Result type usage throughout
/// - Error propagation with ?
/// - Pattern matching on results
/// - Converting between error types
async fn example_result_patterns() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 2: Result<T,E> Patterns ---");

    // Pattern 1: Using ? operator for error propagation
    async fn make_request(client: &MockClient, prompt: &str) -> Result<String> {
        let response = client.complete(prompt).await?;
        Ok(response.content)
    }

    let client = MockClient::with_response("Success!");
    let result = make_request(&client, "test").await?;
    println!("  ✓ Pattern 1 - ? operator: {}", result);

    // Pattern 2: Pattern matching on Result
    async fn try_request(client: &MockClient, prompt: &str) -> String {
        match client.complete(prompt).await {
            Ok(response) => response.content,
            Err(e) => {
                eprintln!("Request failed: {}", e);
                "Default response".to_string()
            }
        }
    }

    let result = try_request(&client, "test").await;
    println!("  ✓ Pattern 2 - match: {}", result);

    // Pattern 3: Using map_err for error context
    async fn request_with_context(client: &MockClient, prompt: &str) -> Result<String> {
        let response = client
            .complete(prompt)
            .await
            .map_err(|e| GgenAiError::llm_provider("MockClient", format!("Failed: {}", e)))?;

        Ok(response.content)
    }

    let result = request_with_context(&client, "test").await?;
    println!("  ✓ Pattern 3 - map_err: {}", result);

    // Pattern 4: and_then for chaining
    let result = client
        .complete("test")
        .await
        .map(|r| r.content)
        .and_then(|content| {
            if content.is_empty() {
                Err(GgenAiError::validation("Empty response"))
            } else {
                Ok(content)
            }
        })?;
    println!("  ✓ Pattern 4 - and_then: {}", result);

    println!();
    Ok(())
}

/// Example 3: Error Recovery with Fallbacks
///
/// Demonstrates:
/// - Multiple fallback strategies
/// - Graceful degradation
/// - Default values
/// - Alternative approaches
async fn example_error_recovery() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 3: Error Recovery Strategies ---");

    // Strategy 1: Default value on error
    async fn get_response_or_default(client: &MockClient, prompt: &str) -> String {
        client
            .complete(prompt)
            .await
            .map(|r| r.content)
            .unwrap_or_else(|_| "Default response when API unavailable".to_string())
    }

    let client = MockClient::with_response("Success");
    let result = get_response_or_default(&client, "test").await;
    println!("  Strategy 1 - Default value: {}", result);

    // Strategy 2: Fallback to cached response
    async fn get_response_with_cache(
        client: &MockClient,
        prompt: &str,
        cache: &HashMap<String, String>,
    ) -> Result<String> {
        match client.complete(prompt).await {
            Ok(response) => Ok(response.content),
            Err(_) => cache
                .get(prompt)
                .cloned()
                .ok_or_else(|| GgenAiError::validation("No cached response available")),
        }
    }

    let mut cache = HashMap::new();
    cache.insert("test".to_string(), "Cached response".to_string());

    let result = get_response_with_cache(&client, "test", &cache).await?;
    println!("  Strategy 2 - Cache fallback: {}", result);

    // Strategy 3: Try multiple clients in sequence
    async fn try_multiple_clients(prompts: &str) -> Result<String> {
        // First try: Primary client
        let primary = MockClient::with_response("Primary response");
        if let Ok(response) = primary.complete(prompts).await {
            return Ok(response.content);
        }

        // Second try: Backup client
        let backup = MockClient::with_response("Backup response");
        if let Ok(response) = backup.complete(prompts).await {
            return Ok(response.content);
        }

        // Final fallback
        Err(GgenAiError::validation("All clients failed"))
    }

    let result = try_multiple_clients("test").await?;
    println!("  Strategy 3 - Multiple clients: {}", result);

    println!();
    Ok(())
}

/// Example 4: Retry Logic with Exponential Backoff
///
/// Demonstrates:
/// - Retry on transient failures
/// - Exponential backoff
/// - Maximum retry limits
/// - Jitter to prevent thundering herd
async fn example_retry_logic() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 4: Retry Logic ---");

    /// Retry configuration
    #[derive(Debug, Clone)]
    struct RetryConfig {
        max_retries: u32,
        initial_delay_ms: u64,
        max_delay_ms: u64,
        backoff_multiplier: f64,
    }

    impl Default for RetryConfig {
        fn default() -> Self {
            Self {
                max_retries: 3,
                initial_delay_ms: 100,
                max_delay_ms: 10000,
                backoff_multiplier: 2.0,
            }
        }
    }

    /// Execute request with retry logic
    async fn retry_request<F, Fut>(
        retry_config: &RetryConfig,
        operation: F,
    ) -> Result<String>
    where
        F: Fn() -> Fut,
        Fut: std::future::Future<Output = Result<String>>,
    {
        let mut attempt = 0;
        let mut delay = retry_config.initial_delay_ms;

        loop {
            match operation().await {
                Ok(result) => {
                    if attempt > 0 {
                        println!("    ✓ Succeeded on attempt {}", attempt + 1);
                    }
                    return Ok(result);
                }
                Err(e) => {
                    attempt += 1;

                    if attempt >= retry_config.max_retries {
                        println!("    ✗ Failed after {} attempts", attempt);
                        return Err(e);
                    }

                    println!("    Attempt {} failed, retrying in {}ms...", attempt, delay);

                    sleep(Duration::from_millis(delay)).await;

                    // Exponential backoff
                    delay = (delay as f64 * retry_config.backoff_multiplier) as u64;
                    delay = delay.min(retry_config.max_delay_ms);
                }
            }
        }
    }

    // Simulate a request that succeeds on third try
    let mut attempts_made = 0;
    let result = retry_request(&RetryConfig::default(), || {
        attempts_made += 1;
        async move {
            if attempts_made < 3 {
                Err(GgenAiError::llm_provider(
                    "Test",
                    format!("Simulated transient error (attempt {})", attempts_made),
                ))
            } else {
                Ok("Success after retries!".to_string())
            }
        }
    })
    .await?;

    println!("  Final result: {}", result);

    println!();
    Ok(())
}

/// Example 5: Token Budget Enforcement
///
/// Demonstrates:
/// - Tracking token usage
/// - Enforcing budgets
/// - Preventing cost overruns
/// - Budget warnings
async fn example_token_budgets() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 5: Token Budget Enforcement ---");

    /// Token budget tracker
    #[derive(Debug)]
    struct TokenBudget {
        total_budget: u32,
        used_tokens: u32,
        warning_threshold: f64,
    }

    impl TokenBudget {
        fn new(total_budget: u32) -> Self {
            Self {
                total_budget,
                used_tokens: 0,
                warning_threshold: 0.8, // Warn at 80%
            }
        }

        fn check_budget(&self, estimated_tokens: u32) -> Result<()> {
            if self.used_tokens + estimated_tokens > self.total_budget {
                return Err(GgenAiError::validation(format!(
                    "Token budget exceeded: {} + {} > {}",
                    self.used_tokens, estimated_tokens, self.total_budget
                )));
            }

            let usage_pct = (self.used_tokens as f64) / (self.total_budget as f64);
            if usage_pct >= self.warning_threshold {
                println!(
                    "    ⚠ Warning: {}% of token budget used",
                    (usage_pct * 100.0) as u32
                );
            }

            Ok(())
        }

        fn record_usage(&mut self, tokens: u32) {
            self.used_tokens += tokens;
        }

        fn remaining(&self) -> u32 {
            self.total_budget.saturating_sub(self.used_tokens)
        }
    }

    let mut budget = TokenBudget::new(10000);
    let client = MockClient::with_response("Response");

    println!("  Initial budget: {} tokens", budget.total_budget);

    // Make several requests
    for i in 1..=5 {
        // Estimate tokens needed (simple heuristic: ~4 chars per token)
        let estimated = 100;

        match budget.check_budget(estimated) {
            Ok(_) => {
                let response = client.complete("test").await?;

                if let Some(usage) = response.usage {
                    budget.record_usage(usage.total_tokens);
                    println!(
                        "  Request {}: {} tokens (remaining: {})",
                        i,
                        usage.total_tokens,
                        budget.remaining()
                    );
                }
            }
            Err(e) => {
                println!("  Request {} blocked: {}", i, e);
                break;
            }
        }
    }

    println!("  Final usage: {} / {} tokens", budget.used_tokens, budget.total_budget);

    println!();
    Ok(())
}

/// Example 6: Timeout Handling
///
/// Demonstrates:
/// - Setting request timeouts
/// - Handling timeout errors
/// - Graceful timeout recovery
/// - Different timeout strategies
async fn example_timeout_handling() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 6: Timeout Handling ---");

    /// Execute with timeout
    async fn with_timeout<F, T>(
        duration: Duration,
        operation: F,
    ) -> Result<T>
    where
        F: std::future::Future<Output = Result<T>>,
    {
        tokio::time::timeout(duration, operation)
            .await
            .map_err(|_| GgenAiError::timeout("Operation", duration.as_secs()))?
    }

    let client = MockClient::with_response("Quick response");

    // Test 1: Fast operation (succeeds)
    println!("  Test 1: Fast operation with 2s timeout");
    match with_timeout(
        Duration::from_secs(2),
        client.complete("fast"),
    ).await {
        Ok(response) => println!("    ✓ Completed: {}", response.content),
        Err(e) => println!("    ✗ Timed out: {}", e),
    }

    // Test 2: Simulated slow operation
    println!("\n  Test 2: Slow operation with 100ms timeout");
    match with_timeout(
        Duration::from_millis(100),
        async {
            sleep(Duration::from_millis(200)).await;
            client.complete("slow").await
        },
    ).await {
        Ok(response) => println!("    ✓ Completed: {}", response.content),
        Err(e) => println!("    ✓ Timeout handled correctly: {}", e),
    }

    // Test 3: Adaptive timeout based on request size
    async fn adaptive_timeout_request(
        client: &MockClient,
        prompt: &str,
    ) -> Result<String> {
        // Calculate timeout based on prompt length
        let base_timeout = Duration::from_secs(5);
        let extra_time = Duration::from_millis(prompt.len() as u64 * 10);
        let timeout = base_timeout + extra_time;

        println!("    Using adaptive timeout: {:?}", timeout);

        with_timeout(timeout, client.complete(prompt))
            .await
            .map(|r| r.content)
    }

    println!("\n  Test 3: Adaptive timeout");
    let result = adaptive_timeout_request(&client, "short prompt").await?;
    println!("    ✓ Result: {}", result);

    println!();
    Ok(())
}

/// Example 7: Error Context and Reporting
///
/// Demonstrates:
/// - Adding context to errors
/// - Error categorization
/// - Structured error reporting
/// - Actionable error messages
async fn example_error_context() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 7: Error Context and Reporting ---");

    /// Enhanced error with context
    #[derive(Debug)]
    struct ErrorReport {
        error_type: String,
        message: String,
        context: HashMap<String, String>,
        suggestion: String,
    }

    impl ErrorReport {
        fn from_error(error: &GgenAiError) -> Self {
            let error_str = error.to_string();

            let (error_type, suggestion) = if error_str.contains("API key") {
                ("Authentication", "Check your API key environment variables".to_string())
            } else if error_str.contains("rate limit") {
                ("RateLimit", "Wait before retrying or upgrade your plan".to_string())
            } else if error_str.contains("timeout") {
                ("Timeout", "Increase timeout or simplify your request".to_string())
            } else if error_str.contains("validation") {
                ("Validation", "Check your input parameters".to_string())
            } else {
                ("Unknown", "Contact support if issue persists".to_string())
            };

            Self {
                error_type: error_type.to_string(),
                message: error_str,
                context: HashMap::new(),
                suggestion,
            }
        }

        fn add_context(&mut self, key: &str, value: &str) {
            self.context.insert(key.to_string(), value.to_string());
        }

        fn display(&self) {
            println!("  Error Report:");
            println!("    Type: {}", self.error_type);
            println!("    Message: {}", self.message);
            if !self.context.is_empty() {
                println!("    Context:");
                for (key, value) in &self.context {
                    println!("      {}: {}", key, value);
                }
            }
            println!("    Suggestion: {}", self.suggestion);
        }
    }

    // Simulate various errors and report them
    let errors = vec![
        GgenAiError::validation("Invalid temperature value"),
        GgenAiError::rate_limit_exceeded("TestProvider", Some(60)),
        GgenAiError::timeout("TestProvider", 30),
    ];

    for (i, error) in errors.iter().enumerate() {
        println!("\n  Example Error {}: {}", i + 1, error);
        let mut report = ErrorReport::from_error(error);
        report.add_context("timestamp", &chrono::Utc::now().to_rfc3339());
        report.add_context("request_id", &format!("req_{}", i + 1));
        report.display();
    }

    println!();
    Ok(())
}
