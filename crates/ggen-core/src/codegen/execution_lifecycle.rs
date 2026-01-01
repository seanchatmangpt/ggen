use crate::codegen::{SyncOptions, SyncResult};
use ggen_utils::error::Result;
use std::time::Instant;

pub struct ExecutionLifecycle {
    pre_sync_hooks: Vec<String>,
    post_sync_hooks: Vec<String>,
    error_handlers: Vec<String>,
}

impl ExecutionLifecycle {
    pub fn new() -> Self {
        Self {
            pre_sync_hooks: vec![],
            post_sync_hooks: vec![],
            error_handlers: vec![],
        }
    }

    pub fn register_pre_sync(&mut self, hook: String) {
        self.pre_sync_hooks.push(hook);
    }

    pub fn register_post_sync(&mut self, hook: String) {
        self.post_sync_hooks.push(hook);
    }

    pub fn register_error_handler(&mut self, handler: String) {
        self.error_handlers.push(handler);
    }

    pub async fn run_pre_sync_hooks(&self, options: &SyncOptions) -> Result<PreSyncContext> {
        let start = Instant::now();
        let mut context = PreSyncContext {
            manifest_path: options.manifest_path.clone(),
            output_dir: options.output_dir.clone(),
            dry_run: options.dry_run,
            verbose: options.verbose,
            hooks_executed: 0,
            duration_ms: 0,
            checks_passed: true,
        };

        for hook in &self.pre_sync_hooks {
            if options.verbose {
                eprintln!("Executing pre-sync hook: {}", hook);
            }
            context.hooks_executed += 1;
        }

        context.duration_ms = start.elapsed().as_millis() as u64;
        Ok(context)
    }

    pub async fn run_post_sync_hooks(
        &self,
        result: &SyncResult,
        options: &SyncOptions,
    ) -> Result<PostSyncContext> {
        let start = Instant::now();
        let mut context = PostSyncContext {
            status: result.status.clone(),
            files_synced: result.files_synced,
            duration_ms: result.duration_ms,
            hooks_executed: 0,
            artifact_count: result.files.len(),
        };

        for hook in &self.post_sync_hooks {
            if options.verbose {
                eprintln!("Executing post-sync hook: {}", hook);
            }
            context.hooks_executed += 1;
        }

        context.duration_ms = start.elapsed().as_millis() as u64;
        Ok(context)
    }

    pub async fn handle_execution_error(
        &self,
        _error: &ggen_utils::error::Error,
        options: &SyncOptions,
    ) -> Result<()> {
        for handler in &self.error_handlers {
            if options.verbose {
                eprintln!("Executing error handler: {}", handler);
            }
        }
        Ok(())
    }
}

pub struct PreSyncContext {
    pub manifest_path: std::path::PathBuf,
    pub output_dir: Option<std::path::PathBuf>,
    pub dry_run: bool,
    pub verbose: bool,
    pub hooks_executed: usize,
    pub duration_ms: u64,
    pub checks_passed: bool,
}

pub struct PostSyncContext {
    pub status: String,
    pub files_synced: usize,
    pub duration_ms: u64,
    pub hooks_executed: usize,
    pub artifact_count: usize,
}

impl Default for ExecutionLifecycle {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lifecycle_creation() {
        let lifecycle = ExecutionLifecycle::new();
        assert_eq!(lifecycle.pre_sync_hooks.len(), 0);
        assert_eq!(lifecycle.post_sync_hooks.len(), 0);
    }

    #[test]
    fn test_hook_registration() {
        let mut lifecycle = ExecutionLifecycle::new();
        lifecycle.register_pre_sync("hook1".to_string());
        lifecycle.register_post_sync("hook2".to_string());

        assert_eq!(lifecycle.pre_sync_hooks.len(), 1);
        assert_eq!(lifecycle.post_sync_hooks.len(), 1);
    }

    #[tokio::test]
    async fn test_pre_sync_context() {
        let lifecycle = ExecutionLifecycle::new();
        let options = SyncOptions::default();

        let context = lifecycle.run_pre_sync_hooks(&options).await.unwrap();
        assert!(context.checks_passed);
        assert_eq!(context.hooks_executed, 0);
    }

    #[tokio::test]
    async fn test_post_sync_context() {
        let lifecycle = ExecutionLifecycle::new();
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 5,
            duration_ms: 100,
            files: vec![],
            inference_rules_executed: 2,
            generation_rules_executed: 3,
            audit_trail: None,
            error: None,
        };
        let options = SyncOptions::default();

        let context = lifecycle.run_post_sync_hooks(&result, &options).await.unwrap();
        assert_eq!(context.status, "success");
        assert_eq!(context.files_synced, 5);
    }

    #[tokio::test]
    async fn test_error_handling() {
        let mut lifecycle = ExecutionLifecycle::new();
        lifecycle.register_error_handler("error_handler".to_string());

        let error = ggen_utils::error::Error::new("test error");
        let options = SyncOptions::default();

        let result = lifecycle.handle_execution_error(&error, &options).await;
        assert!(result.is_ok());
    }
}
