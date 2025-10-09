use anyhow::Result;
use std::collections::BTreeMap;
use std::path::Path;
use std::time::Instant;

/// Simple tracing system for pipeline debugging
pub struct SimpleTracer;

impl SimpleTracer {
    /// Check if tracing is enabled
    pub fn is_enabled() -> bool {
        std::env::var("RGEN_TRACE").is_ok()
    }
    
    /// Get trace level
    pub fn trace_level() -> TraceLevel {
        match std::env::var("RGEN_TRACE").unwrap_or_default().to_lowercase().as_str() {
            "error" => TraceLevel::Error,
            "warn" => TraceLevel::Warn,
            "info" => TraceLevel::Info,
            "debug" => TraceLevel::Debug,
            "trace" => TraceLevel::Trace,
            "1" | "true" | "yes" => TraceLevel::Debug,
            "0" | "false" | "no" => TraceLevel::Error,
            _ => TraceLevel::Info,
        }
    }
    
    /// Log a trace message
    pub fn trace(level: TraceLevel, message: &str, context: Option<&str>) {
        if !Self::is_enabled() {
            return;
        }
        
        let current_level = Self::trace_level();
        if level as u8 > current_level as u8 {
            return;
        }
        
        let prefix = match level {
            TraceLevel::Error => "ERROR",
            TraceLevel::Warn => "WARN ",
            TraceLevel::Info => "INFO ",
            TraceLevel::Debug => "DEBUG",
            TraceLevel::Trace => "TRACE",
        };
        
        if let Some(ctx) = context {
            eprintln!("[RGEN {}] {}: {}", prefix, ctx, message);
        } else {
            eprintln!("[RGEN {}] {}", prefix, message);
        }
    }
    
    /// Log template processing start
    pub fn template_start(template_path: &Path) {
        Self::trace(TraceLevel::Info, &format!("Starting template processing: {}", template_path.display()), None);
    }
    
    /// Log template processing completion
    pub fn template_complete(template_path: &Path, output_path: &Path, content_size: usize) {
        Self::trace(TraceLevel::Info, &format!("Template processing complete: {} -> {} ({} bytes)", 
            template_path.display(), output_path.display(), content_size), None);
    }
    
    /// Log frontmatter processing
    pub fn frontmatter_processed(frontmatter: &crate::template::Frontmatter) {
        Self::trace(TraceLevel::Debug, &format!("Frontmatter processed: to={:?}, inject={}, vars={}", 
            frontmatter.to, frontmatter.inject, frontmatter.vars.len()), Some("frontmatter"));
    }
    
    /// Log context blessing
    pub fn context_blessed(vars_count: usize) {
        Self::trace(TraceLevel::Debug, &format!("Context blessed: {} variables (Name, locals added)", vars_count), Some("context"));
    }
    
    /// Log RDF loading
    pub fn rdf_loading(files: &[String], inline_blocks: usize, triples: usize) {
        Self::trace(TraceLevel::Info, &format!("RDF loaded: {} files, {} inline blocks, {} triples", 
            files.len(), inline_blocks, triples), Some("rdf"));
    }
    
    /// Log SPARQL query
    pub fn sparql_query(query: &str, result_count: Option<usize>) {
        let count_str = result_count.map(|c| c.to_string()).unwrap_or_else(|| "N/A".to_string());
        Self::trace(TraceLevel::Debug, &format!("SPARQL query: {} results", count_str), Some("sparql"));
        Self::trace(TraceLevel::Trace, query, Some("sparql"));
    }
    
    /// Log file injection
    pub fn file_injection(target_path: &Path, mode: &str, success: bool) {
        let status = if success { "completed" } else { "failed" };
        Self::trace(TraceLevel::Info, &format!("File injection {}: {} mode", status, mode), 
            Some(&format!("injection:{}", target_path.display())));
    }
    
    /// Log shell hook
    pub fn shell_hook(command: &str, timing: &str, exit_code: i32) {
        let status = if exit_code == 0 { "completed" } else { "failed" };
        Self::trace(TraceLevel::Info, &format!("Shell hook {}: {} (exit code: {})", status, timing, exit_code), 
            Some(&format!("hook:{}", command)));
    }
    
    /// Log validation
    pub fn validation(validation_type: &str, passed: bool) {
        let status = if passed { "passed" } else { "failed" };
        Self::trace(TraceLevel::Debug, &format!("Validation {}: {}", status, validation_type), Some("validation"));
    }
    
    /// Log performance metric
    pub fn performance(operation: &str, duration_ms: u64) {
        Self::trace(TraceLevel::Debug, &format!("Performance: {} took {}ms", operation, duration_ms), Some("performance"));
    }
    
    /// Log dry run
    pub fn dry_run(output_path: &Path, content_size: usize) {
        Self::trace(TraceLevel::Info, &format!("DRY RUN: Would generate {} ({} bytes)", 
            output_path.display(), content_size), Some("dry_run"));
    }
    
    /// Log backup creation
    pub fn backup_created(original_path: &Path, backup_path: &Path) {
        Self::trace(TraceLevel::Info, &format!("Backup created: {} -> {}", 
            original_path.display(), backup_path.display()), Some("backup"));
    }
    
    /// Log skip condition
    pub fn skip_condition(condition: &str, reason: &str) {
        Self::trace(TraceLevel::Info, &format!("Skipped: {} ({})", condition, reason), Some("skip"));
    }
    
    /// Log error
    pub fn error(error: &anyhow::Error, context: &str) {
        Self::trace(TraceLevel::Error, &format!("Error in {}: {}", context, error), Some("error"));
    }
    
    /// Log warning
    pub fn warning(message: &str, context: Option<&str>) {
        Self::trace(TraceLevel::Warn, message, context);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TraceLevel {
    Error = 0,
    Warn = 1,
    Info = 2,
    Debug = 3,
    Trace = 4,
}

/// Performance timer for measuring operation duration
pub struct SimpleTimer {
    start: Instant,
    operation: String,
}

impl SimpleTimer {
    /// Start timing an operation
    pub fn start(operation: &str) -> Self {
        Self {
            start: Instant::now(),
            operation: operation.to_string(),
        }
    }
    
    /// Finish timing and log the result
    pub fn finish(self) {
        let duration = self.start.elapsed();
        SimpleTracer::performance(&self.operation, duration.as_millis() as u64);
    }
}

/// Macro for easy performance timing
#[macro_export]
macro_rules! time_operation {
    ($name:expr, $block:block) => {{
        let _timer = $crate::simple_tracing::SimpleTimer::start($name);
        let result = $block;
        _timer.finish();
        result
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;
    
    #[test]
    fn test_trace_level_ordering() {
        assert!(TraceLevel::Error < TraceLevel::Warn);
        assert!(TraceLevel::Warn < TraceLevel::Info);
        assert!(TraceLevel::Info < TraceLevel::Debug);
        assert!(TraceLevel::Debug < TraceLevel::Trace);
    }
    
    #[test]
    fn test_simple_timer() {
        let timer = SimpleTimer::start("test_operation");
        std::thread::sleep(std::time::Duration::from_millis(10));
        timer.finish(); // Should not panic
    }
    
    #[test]
    fn test_tracing_methods() {
        let temp_dir = TempDir::new().unwrap();
        let test_path = temp_dir.path().join("test.tmpl");
        fs::write(&test_path, "test content").unwrap();
        
        // Test all tracing methods compile and work
        SimpleTracer::template_start(&test_path);
        SimpleTracer::template_complete(&test_path, &test_path, 100);
        
        let frontmatter = crate::template::Frontmatter::default();
        SimpleTracer::frontmatter_processed(&frontmatter);
        
        SimpleTracer::context_blessed(5);
        SimpleTracer::rdf_loading(&["file1.ttl".to_string()], 2, 100);
        SimpleTracer::sparql_query("SELECT * WHERE { ?s ?p ?o }", Some(10));
        
        SimpleTracer::file_injection(&test_path, "append", true);
        SimpleTracer::shell_hook("echo 'test'", "before", 0);
        SimpleTracer::validation("frontmatter", true);
        SimpleTracer::performance("test_operation", 50);
        SimpleTracer::dry_run(&test_path, 500);
        SimpleTracer::backup_created(&test_path, &temp_dir.path().join("backup.tmpl"));
        SimpleTracer::skip_condition("skip_if", "pattern found");
        
        let error = anyhow::anyhow!("Test error");
        SimpleTracer::error(&error, "test context");
        SimpleTracer::warning("Test warning", Some("test context"));
        SimpleTracer::warning("Test warning", None);
    }
    
    #[test]
    fn test_tracing_environment_variables() {
        // Test different RGEN_TRACE values
        let test_values = ["error", "warn", "info", "debug", "trace", "1", "0", "true", "false"];
        
        for value in &test_values {
            std::env::set_var("RGEN_TRACE", value);
            let level = SimpleTracer::trace_level();
            assert!(matches!(level, TraceLevel::Error | TraceLevel::Warn | TraceLevel::Info | TraceLevel::Debug | TraceLevel::Trace));
        }
    }
    
    #[test]
    fn test_time_operation_macro() {
        let result = crate::time_operation!("test_op", {
            std::thread::sleep(std::time::Duration::from_millis(2));
            42
        });
        assert_eq!(result, 42);
    }
}
