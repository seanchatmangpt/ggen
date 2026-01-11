//! Thread-local context management for temporary configuration overrides
//!
//! Provides Python DSPy's `with dspy.context()` functionality using tokio task-local storage.

use std::cell::RefCell;
use std::future::Future;
use tokio::task_local;

use super::{get_dspy_config, DspySettings};

// Task-local storage for configuration overrides
task_local! {
    static TEMPERATURE: RefCell<Option<f64>>;
    static MAX_TOKENS: RefCell<Option<usize>>;
    static TOP_P: RefCell<Option<f64>>;
    static CACHE_ENABLED: RefCell<Option<bool>>;
    static TRACK_USAGE: RefCell<Option<bool>>;
    static STREAMING: RefCell<Option<bool>>;
}

/// Get current temperature (context override or global default)
pub fn get_current_temperature() -> f64 {
    TEMPERATURE
        .try_with(|cell| *cell.borrow())
        .ok()
        .flatten()
        .unwrap_or_else(|| get_dspy_config().default_temperature)
}

/// Get current max tokens (context override or global default)
pub fn get_current_max_tokens() -> usize {
    MAX_TOKENS
        .try_with(|cell| *cell.borrow())
        .ok()
        .flatten()
        .unwrap_or_else(|| get_dspy_config().default_max_tokens)
}

/// Get current top_p (context override or global default)
pub fn get_current_top_p() -> f64 {
    TOP_P
        .try_with(|cell| *cell.borrow())
        .ok()
        .flatten()
        .unwrap_or_else(|| get_dspy_config().default_top_p)
}

/// Check if caching is enabled (context override or global default)
pub fn is_cache_enabled() -> bool {
    CACHE_ENABLED
        .try_with(|cell| *cell.borrow())
        .ok()
        .flatten()
        .unwrap_or_else(|| get_dspy_config().enable_cache)
}

/// Check if usage tracking is enabled (context override or global default)
pub fn is_tracking_enabled() -> bool {
    TRACK_USAGE
        .try_with(|cell| *cell.borrow())
        .ok()
        .flatten()
        .unwrap_or_else(|| get_dspy_config().track_usage)
}

/// Check if streaming is enabled (context override or global default)
pub fn is_streaming_enabled() -> bool {
    STREAMING
        .try_with(|cell| *cell.borrow())
        .ok()
        .flatten()
        .unwrap_or_else(|| get_dspy_config().use_streaming)
}

/// Builder for creating context with overrides
pub struct ContextBuilder {
    temperature: Option<f64>,
    max_tokens: Option<usize>,
    top_p: Option<f64>,
    cache_enabled: Option<bool>,
    track_usage: Option<bool>,
    streaming: Option<bool>,
}

impl Default for ContextBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ContextBuilder {
    /// Create new context builder with no overrides
    pub fn new() -> Self {
        Self {
            temperature: None,
            max_tokens: None,
            top_p: None,
            cache_enabled: None,
            track_usage: None,
            streaming: None,
        }
    }

    /// Override temperature
    pub fn temperature(mut self, temp: f64) -> Self {
        self.temperature = Some(temp);
        self
    }

    /// Override max tokens
    pub fn max_tokens(mut self, tokens: usize) -> Self {
        self.max_tokens = Some(tokens);
        self
    }

    /// Override top_p
    pub fn top_p(mut self, top_p: f64) -> Self {
        self.top_p = Some(top_p);
        self
    }

    /// Override cache enabled
    pub fn cache_enabled(mut self, enabled: bool) -> Self {
        self.cache_enabled = Some(enabled);
        self
    }

    /// Override usage tracking
    pub fn track_usage(mut self, enabled: bool) -> Self {
        self.track_usage = Some(enabled);
        self
    }

    /// Override streaming
    pub fn streaming(mut self, enabled: bool) -> Self {
        self.streaming = Some(enabled);
        self
    }

    /// Run async block with context overrides
    pub async fn run<F, T, E>(self, future: F) -> Result<T, E>
    where
        F: Future<Output = Result<T, E>>,
    {
        // Save previous values
        let prev_temp = TEMPERATURE.try_with(|cell| *cell.borrow()).ok().flatten();
        let prev_tokens = MAX_TOKENS.try_with(|cell| *cell.borrow()).ok().flatten();
        let prev_top_p = TOP_P.try_with(|cell| *cell.borrow()).ok().flatten();
        let prev_cache = CACHE_ENABLED.try_with(|cell| *cell.borrow()).ok().flatten();
        let prev_tracking = TRACK_USAGE.try_with(|cell| *cell.borrow()).ok().flatten();
        let prev_streaming = STREAMING.try_with(|cell| *cell.borrow()).ok().flatten();

        // Run with overrides
        let result = TEMPERATURE
            .scope(
                RefCell::new(self.temperature.or(prev_temp)),
                MAX_TOKENS.scope(
                    RefCell::new(self.max_tokens.or(prev_tokens)),
                    TOP_P.scope(
                        RefCell::new(self.top_p.or(prev_top_p)),
                        CACHE_ENABLED.scope(
                            RefCell::new(self.cache_enabled.or(prev_cache)),
                            TRACK_USAGE.scope(
                                RefCell::new(self.track_usage.or(prev_tracking)),
                                STREAMING.scope(RefCell::new(self.streaming.or(prev_streaming)), future),
                            ),
                        ),
                    ),
                ),
            )
            .await;

        result
    }
}

/// Create a context builder for temporary configuration overrides
///
/// # Example
///
/// ```rust,no_run
/// use ggen_dspy::config::with_context;
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// with_context()
///     .temperature(0.0)
///     .cache_enabled(false)
///     .run(async {
///         // Code here uses overridden settings
///         Ok::<(), ()>(())
///     })
///     .await?;
/// # Ok(())
/// # }
/// ```
pub fn with_context() -> ContextBuilder {
    ContextBuilder::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_context_override() {
        crate::config::init_dspy_config();

        let global_temp = get_dspy_config().default_temperature;

        with_context()
            .temperature(0.5)
            .run(async {
                assert_eq!(get_current_temperature(), 0.5);
                Ok::<(), ()>(())
            })
            .await
            .unwrap();

        // Should revert to global
        assert_eq!(get_current_temperature(), global_temp);
    }

    #[tokio::test]
    async fn test_nested_context() {
        crate::config::init_dspy_config();

        with_context()
            .temperature(0.8)
            .run(async {
                assert_eq!(get_current_temperature(), 0.8);

                with_context()
                    .temperature(0.2)
                    .run(async {
                        assert_eq!(get_current_temperature(), 0.2);
                        Ok::<(), ()>(())
                    })
                    .await
                    .unwrap();

                assert_eq!(get_current_temperature(), 0.8);
                Ok::<(), ()>(())
            })
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn test_multiple_overrides() {
        crate::config::init_dspy_config();

        with_context()
            .temperature(0.5)
            .max_tokens(4096)
            .top_p(0.95)
            .cache_enabled(false)
            .run(async {
                assert_eq!(get_current_temperature(), 0.5);
                assert_eq!(get_current_max_tokens(), 4096);
                assert_eq!(get_current_top_p(), 0.95);
                assert!(!is_cache_enabled());
                Ok::<(), ()>(())
            })
            .await
            .unwrap();
    }
}
