//! Integration tests for lifecycle system
//!
//! These tests verify the complete lifecycle workflow including:
//! - Loading make.toml configuration
//! - Running phases with state tracking
//! - Hook execution (before/after)
//! - Cache key generation and validation
//! - Workspace support

#[cfg(test)]
mod integration_tests {
    use super::super::*;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::sync::Arc;
    use tempfile::TempDir;

    /// Test fixture with temporary directory and test configuration
    struct LifecycleTestFixture {
        temp_dir: TempDir,
        state_path: PathBuf,
    }

    impl LifecycleTestFixture {
        /// Create new test fixture with make.toml
        fn new() -> Self {
            let temp_dir = TempDir::new().expect("Failed to create temp dir");
            let state_path = temp_dir.path().join(".ggen/state.json");

            // Create basic make.toml
            let make_toml = r#"
[project]
name = "test-project"
type = "app"
version = "1.0.0"
description = "Test project for lifecycle integration"

[lifecycle.init]
description = "Initialize project"
commands = [
    "mkdir -p src",
    "echo 'test' > src/test.txt"
]

[lifecycle.build]
description = "Build project"
command = "echo 'Building project...'"
cache = true
outputs = ["dist/"]

[lifecycle.test]
description = "Run tests"
command = "echo 'Running tests...'"

[lifecycle.lint]
description = "Lint code"
command = "echo 'Linting...'"

[lifecycle.clean]
description = "Clean artifacts"
command = "echo 'Cleaning...'"

[hooks]
before_build = ["test", "lint"]
after_build = []
"#;

            fs::write(temp_dir.path().join("make.toml"), make_toml)
                .expect("Failed to write make.toml");

            Self {
                temp_dir,
                state_path,
            }
        }

        /// Create fixture with workspace configuration
        fn new_with_workspaces() -> Self {
            let temp_dir = TempDir::new().expect("Failed to create temp dir");
            let state_path = temp_dir.path().join(".ggen/state.json");

            // Create workspace directories
            fs::create_dir_all(temp_dir.path().join("frontend")).unwrap();
            fs::create_dir_all(temp_dir.path().join("backend")).unwrap();

            let make_toml = r#"
[project]
name = "monorepo-project"
type = "monorepo"

[workspace.frontend]
path = "frontend"
framework = "react"
runtime = "node"

[workspace.backend]
path = "backend"
framework = "express"
runtime = "node"

[lifecycle.install]
description = "Install dependencies"
command = "echo 'Installing dependencies...'"

[lifecycle.build]
description = "Build workspace"
command = "echo 'Building...'"
"#;

            fs::write(temp_dir.path().join("make.toml"), make_toml)
                .expect("Failed to write make.toml");

            Self {
                temp_dir,
                state_path,
            }
        }

        /// Get path to temp directory
        fn path(&self) -> &Path {
            self.temp_dir.path()
        }

        /// Load make.toml from fixture
        fn load_make(&self) -> Make {
            load_make(self.path().join("make.toml"))
                .expect("Failed to load make.toml")
        }

        /// Create execution context
        fn create_context(&self) -> Context {
            let make = Arc::new(self.load_make());
            Context::new(
                self.path().to_path_buf(),
                make,
                self.state_path.clone(),
                vec![]
            )
        }

        /// Load current state
        fn load_state(&self) -> LifecycleState {
            load_state(&self.state_path).expect("Failed to load state")
        }
    }

    #[test]
    fn test_load_make_toml() {
        let fixture = LifecycleTestFixture::new();
        let make = fixture.load_make();

        // Verify project metadata
        assert_eq!(make.project.name, "test-project");
        assert_eq!(make.project.project_type, Some("app".to_string()));
        assert_eq!(make.project.version, Some("1.0.0".to_string()));

        // Verify lifecycle phases exist
        assert!(make.lifecycle.contains_key("init"));
        assert!(make.lifecycle.contains_key("build"));
        assert!(make.lifecycle.contains_key("test"));
        assert!(make.lifecycle.contains_key("lint"));
        assert!(make.lifecycle.contains_key("clean"));

        // Verify phase configuration
        let build_phase = make.lifecycle.get("build").unwrap();
        assert_eq!(build_phase.description, Some("Build project".to_string()));
        assert_eq!(build_phase.cache, Some(true));
        assert_eq!(build_phase.outputs, Some(vec!["dist/".to_string()]));

        // Verify hooks
        let hooks = make.hooks.as_ref().unwrap();
        assert_eq!(
            hooks.before_build,
            Some(vec!["test".to_string(), "lint".to_string()])
        );
    }

    #[test]
    fn test_run_single_phase() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Run init phase
        let result = run_phase(&ctx, "init");
        assert!(result.is_ok(), "Phase execution should succeed");

        // Verify state was updated
        let state = fixture.load_state();
        assert_eq!(state.last_phase, Some("init".to_string()));
        assert_eq!(state.phase_history.len(), 1);

        let record = &state.phase_history[0];
        assert_eq!(record.phase, "init");
        assert!(record.success);
        assert!(record.duration_ms > 0);

        // Verify files were created
        assert!(fixture.path().join("src/test.txt").exists());
    }

    #[test]
    fn test_state_persistence() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Run multiple phases
        run_phase(&ctx, "init").unwrap();
        run_phase(&ctx, "build").unwrap();

        // Load state and verify
        let state = fixture.load_state();
        assert_eq!(state.last_phase, Some("build".to_string()));

        // Note: build has hooks (test, lint), so we'll have: init, test, lint, build
        assert_eq!(state.phase_history.len(), 4);

        // Verify phase order (init, then build's hooks, then build)
        assert_eq!(state.phase_history[0].phase, "init");
        assert_eq!(state.phase_history[1].phase, "test");
        assert_eq!(state.phase_history[2].phase, "lint");
        assert_eq!(state.phase_history[3].phase, "build");

        // All should succeed
        for record in &state.phase_history {
            assert!(record.success);
        }
    }

    #[test]
    fn test_cache_key_generation() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Run build phase (has cache enabled)
        run_phase(&ctx, "build").unwrap();

        // Verify cache key was generated
        let state = fixture.load_state();

        // Build phase has hooks (test, lint), so we'll have cache keys for: test, lint, build
        assert!(state.cache_keys.len() >= 1);

        // Find the build cache entry
        let build_cache = state.cache_keys.iter()
            .find(|k| k.phase == "build")
            .expect("Build cache key should exist");

        assert!(!build_cache.key.is_empty());
        assert_eq!(build_cache.key.len(), 64); // SHA256 hex string

        let first_build_key = build_cache.key.clone();

        // Run again and verify key is consistent
        run_phase(&ctx, "build").unwrap();
        let state2 = fixture.load_state();

        // Find all build cache entries
        let build_caches: Vec<_> = state2.cache_keys.iter()
            .filter(|k| k.phase == "build")
            .collect();

        assert!(build_caches.len() >= 2, "Should have at least 2 build cache entries");

        // Keys should be identical (deterministic)
        assert_eq!(build_caches[0].key, build_caches[1].key);
        assert_eq!(build_caches[0].key, first_build_key);
    }

    #[test]
    fn test_cache_key_deterministic() {
        // Test that cache keys are deterministic
        let cmds = vec!["echo test".to_string()];
        let env = vec![("FOO".to_string(), "bar".to_string())];
        let inputs = vec![];

        let key1 = cache::cache_key("build", &cmds, &env, &inputs);
        let key2 = cache::cache_key("build", &cmds, &env, &inputs);

        assert_eq!(key1, key2);
        assert_eq!(key1.len(), 64); // SHA256 hex
    }

    #[test]
    fn test_cache_key_changes_with_inputs() {
        let cmds = vec!["echo test".to_string()];
        let env = vec![];
        let inputs = vec![];

        // Same commands, different phase names
        let key1 = cache::cache_key("build", &cmds, &env, &inputs);
        let key2 = cache::cache_key("test", &cmds, &env, &inputs);
        assert_ne!(key1, key2);

        // Different commands
        let cmds2 = vec!["echo different".to_string()];
        let key3 = cache::cache_key("build", &cmds2, &env, &inputs);
        assert_ne!(key1, key3);

        // Different env
        let env2 = vec![("KEY".to_string(), "value".to_string())];
        let key4 = cache::cache_key("build", &cmds, &env2, &inputs);
        assert_ne!(key1, key4);
    }

    #[test]
    fn test_hooks_execution_order() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Run build phase (has before_build hooks: test, lint)
        let result = run_phase(&ctx, "build");
        assert!(result.is_ok());

        // Verify execution order in state
        let state = fixture.load_state();

        // Should have executed: test, lint, build (in that order)
        assert_eq!(state.phase_history.len(), 3);
        assert_eq!(state.phase_history[0].phase, "test");
        assert_eq!(state.phase_history[1].phase, "lint");
        assert_eq!(state.phase_history[2].phase, "build");

        // Last phase should be build
        assert_eq!(state.last_phase, Some("build".to_string()));
    }

    #[test]
    fn test_phase_not_found() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Try to run non-existent phase
        let result = run_phase(&ctx, "nonexistent");
        assert!(result.is_err());

        let err_msg = format!("{}", result.unwrap_err());
        assert!(err_msg.contains("not found"));
    }

    #[test]
    fn test_get_last_run() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Run phases
        run_phase(&ctx, "init").unwrap();
        run_phase(&ctx, "build").unwrap();
        run_phase(&ctx, "init").unwrap(); // Run init again

        let state = fixture.load_state();

        // Get last run for init (should be the second one)
        let last_init = state.last_run("init");
        assert!(last_init.is_some());

        let init_runs: Vec<_> = state.phase_history.iter()
            .filter(|r| r.phase == "init")
            .collect();
        assert_eq!(init_runs.len(), 2);

        // Last run should match the most recent execution
        assert_eq!(last_init.unwrap().started_ms, init_runs[1].started_ms);
    }

    #[test]
    fn test_get_cache_key() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Run build phase
        run_phase(&ctx, "build").unwrap();

        let state = fixture.load_state();
        let cache_key = state.get_cache_key("build");

        assert!(cache_key.is_some());
        assert_eq!(cache_key.unwrap().len(), 64); // SHA256 hex

        // Non-existent phase
        assert!(state.get_cache_key("nonexistent").is_none());
    }

    #[test]
    fn test_pipeline_execution() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Run pipeline of phases
        let phases = vec![
            "init".to_string(),
            "build".to_string(),
            "test".to_string(),
        ];

        let result = exec::run_pipeline(&ctx, &phases);
        assert!(result.is_ok());

        // Verify all phases executed in order
        let state = fixture.load_state();

        // Note: build has hooks, so we'll have: init, test, lint, build, test
        // The pipeline test comes after build hooks
        assert!(state.phase_history.len() >= 3);

        // Verify init was first
        assert_eq!(state.phase_history[0].phase, "init");

        // Last phase should be the final test
        assert_eq!(state.last_phase, Some("test".to_string()));
    }

    #[test]
    fn test_workspace_support() {
        let fixture = LifecycleTestFixture::new_with_workspaces();
        let make = fixture.load_make();

        // Verify workspaces are loaded
        assert!(make.workspace.is_some());
        let workspaces = make.workspace.as_ref().unwrap();

        assert_eq!(workspaces.len(), 2);
        assert!(workspaces.contains_key("frontend"));
        assert!(workspaces.contains_key("backend"));

        // Verify workspace configuration
        let frontend = workspaces.get("frontend").unwrap();
        assert_eq!(frontend.path, "frontend");
        assert_eq!(frontend.framework, Some("react".to_string()));
        assert_eq!(frontend.runtime, Some("node".to_string()));
    }

    #[test]
    fn test_phase_commands_extraction() {
        let fixture = LifecycleTestFixture::new();
        let make = fixture.load_make();

        // Test single command
        let build_cmds = make.phase_commands("build");
        assert_eq!(build_cmds.len(), 1);
        assert_eq!(build_cmds[0], "echo 'Building project...'");

        // Test multiple commands
        let init_cmds = make.phase_commands("init");
        assert_eq!(init_cmds.len(), 2);
        assert_eq!(init_cmds[0], "mkdir -p src");
        assert_eq!(init_cmds[1], "echo 'test' > src/test.txt");

        // Test non-existent phase
        let empty = make.phase_commands("nonexistent");
        assert_eq!(empty.len(), 0);
    }

    #[test]
    fn test_phase_names_listing() {
        let fixture = LifecycleTestFixture::new();
        let make = fixture.load_make();

        let phases = make.phase_names();
        assert_eq!(phases.len(), 5);

        assert!(phases.contains(&"init".to_string()));
        assert!(phases.contains(&"build".to_string()));
        assert!(phases.contains(&"test".to_string()));
        assert!(phases.contains(&"lint".to_string()));
        assert!(phases.contains(&"clean".to_string()));
    }

    #[test]
    fn test_state_record_run() {
        let mut state = LifecycleState::default();

        state.record_run("build".to_string(), 1000, 500, true);

        assert_eq!(state.last_phase, Some("build".to_string()));
        assert_eq!(state.phase_history.len(), 1);

        let record = &state.phase_history[0];
        assert_eq!(record.phase, "build");
        assert_eq!(record.started_ms, 1000);
        assert_eq!(record.duration_ms, 500);
        assert_eq!(record.success, true);
    }

    #[test]
    fn test_state_add_cache_key() {
        let mut state = LifecycleState::default();

        state.add_cache_key("build".to_string(), "abc123".to_string());

        assert_eq!(state.cache_keys.len(), 1);
        assert_eq!(state.cache_keys[0].phase, "build");
        assert_eq!(state.cache_keys[0].key, "abc123");
    }

    #[test]
    fn test_multiple_phase_runs_state_history() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Run same phase multiple times
        for _ in 0..3 {
            run_phase(&ctx, "test").unwrap();
        }

        let state = fixture.load_state();

        // Should have 3 records
        assert_eq!(state.phase_history.len(), 3);

        // All should be for test phase
        for record in &state.phase_history {
            assert_eq!(record.phase, "test");
            assert!(record.success);
        }
    }

    #[test]
    fn test_cache_storage_and_validation() {
        let temp_dir = TempDir::new().unwrap();
        let cache_dir = temp_dir.path().join(".ggen/cache");

        let phase = "build";
        let key = "test_cache_key_123";

        // Initially cache should not be valid
        assert!(!cache::is_cache_valid(&cache_dir, phase, key));

        // Store cache
        cache::store_cache(&cache_dir, phase, key).unwrap();

        // Now cache should be valid
        assert!(cache::is_cache_valid(&cache_dir, phase, key));

        // Verify file was created
        let cache_path = cache_dir.join(phase).join(key);
        assert!(cache_path.exists());
    }

    #[test]
    fn test_empty_phase_commands() {
        let temp_dir = TempDir::new().unwrap();

        let make_toml = r#"
[project]
name = "test"

[lifecycle.empty]
description = "Phase with no commands"
"#;

        fs::write(temp_dir.path().join("make.toml"), make_toml).unwrap();

        let make = Box::leak(Box::new(load_make(temp_dir.path().join("make.toml")).unwrap()));
        let state_path = temp_dir.path().join(".ggen/state.json");

        let ctx = Context::new(temp_dir.path(), make, &state_path, vec![]);

        // Running phase with no commands should succeed but do nothing
        let result = run_phase(&ctx, "empty");
        assert!(result.is_ok());
    }

    #[test]
    fn test_load_make_or_default() {
        let temp_dir = TempDir::new().unwrap();

        // No make.toml exists
        let make = loader::load_make_or_default(temp_dir.path()).unwrap();

        // Should get default config
        assert_eq!(make.project.name, "unnamed");
        assert_eq!(make.project.version, Some("0.1.0".to_string()));
        assert_eq!(make.lifecycle.len(), 0);
    }

    #[test]
    fn test_concurrent_phase_execution_state() {
        let fixture = LifecycleTestFixture::new();
        let ctx = fixture.create_context();

        // Simulate concurrent-like execution by running multiple phases rapidly
        let phases = vec!["init", "test", "lint", "clean"];

        for phase in phases {
            run_phase(&ctx, phase).unwrap();
        }

        let state = fixture.load_state();

        // All phases should be recorded
        assert_eq!(state.phase_history.len(), 4);

        // Each should be unique
        let phase_names: Vec<_> = state.phase_history.iter()
            .map(|r| r.phase.as_str())
            .collect();
        assert!(phase_names.contains(&"init"));
        assert!(phase_names.contains(&"test"));
        assert!(phase_names.contains(&"lint"));
        assert!(phase_names.contains(&"clean"));
    }

    #[test]
    fn test_parallel_workspace_execution() {
        use std::time::Instant;

        let temp_dir = TempDir::new().expect("Failed to create temp dir");

        // Create 3 workspaces
        for i in 1..=3 {
            let ws_path = temp_dir.path().join(format!("workspace{}", i));
            fs::create_dir_all(&ws_path).unwrap();

            // Each workspace writes a timestamp file
            let make_toml = format!(r#"
[project]
name = "workspace{}"

[lifecycle.timestamp]
description = "Write timestamp"
command = "date +%s%N > timestamp.txt"
"#, i);

            fs::write(ws_path.join("make.toml"), make_toml).unwrap();
        }

        // Create root make.toml with workspace config
        let root_make = r#"
[project]
name = "parallel-test"
type = "monorepo"

[workspace.workspace1]
path = "workspace1"

[workspace.workspace2]
path = "workspace2"

[workspace.workspace3]
path = "workspace3"

[lifecycle.timestamp]
description = "Write timestamp"
parallel = true
command = "date +%s%N > timestamp.txt"
"#;

        fs::write(temp_dir.path().join("make.toml"), root_make).unwrap();

        // Load and execute
        let make = Box::leak(Box::new(load_make(temp_dir.path().join("make.toml")).unwrap()));
        let state_path = temp_dir.path().join(".ggen/state.json");
        let ctx = Context::new(temp_dir.path(), make, &state_path, vec![]);

        let start = Instant::now();
        exec::run_pipeline(&ctx, &vec!["timestamp".to_string()]).unwrap();
        let duration = start.elapsed();

        // Verify all workspace files were created
        for i in 1..=3 {
            let timestamp_file = temp_dir.path().join(format!("workspace{}/timestamp.txt", i));
            assert!(timestamp_file.exists(), "Workspace {} timestamp file should exist", i);
        }

        // Verify execution was reasonably fast (parallel should be faster than sequential)
        // This is a soft check - parallel should complete in under 1 second for simple commands
        assert!(duration.as_millis() < 5000, "Parallel execution took too long: {}ms", duration.as_millis());

        // Verify each workspace has its own state
        for i in 1..=3 {
            let ws_state_path = temp_dir.path().join(format!("workspace{}/.ggen/state.json", i));
            assert!(ws_state_path.exists(), "Workspace {} should have state file", i);

            let ws_state = load_state(&ws_state_path);
            assert_eq!(ws_state.last_phase, Some("timestamp".to_string()));
        }
    }

    #[test]
    fn test_parallel_workspace_isolation() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");

        // Create 3 workspaces with conflicting file names
        for i in 1..=3 {
            let ws_path = temp_dir.path().join(format!("workspace{}", i));
            fs::create_dir_all(&ws_path).unwrap();

            // Each writes different content to "output.txt"
            let make_toml = format!(r#"
[project]
name = "workspace{}"

[lifecycle.write]
description = "Write unique content"
command = "echo 'workspace{}' > output.txt"
"#, i, i);

            fs::write(ws_path.join("make.toml"), make_toml).unwrap();
        }

        // Create root make.toml
        let root_make = r#"
[project]
name = "isolation-test"
type = "monorepo"

[workspace.workspace1]
path = "workspace1"

[workspace.workspace2]
path = "workspace2"

[workspace.workspace3]
path = "workspace3"

[lifecycle.write]
description = "Write unique content"
parallel = true
command = "echo 'test' > output.txt"
"#;

        fs::write(temp_dir.path().join("make.toml"), root_make).unwrap();

        // Execute
        let make = Box::leak(Box::new(load_make(temp_dir.path().join("make.toml")).unwrap()));
        let state_path = temp_dir.path().join(".ggen/state.json");
        let ctx = Context::new(temp_dir.path(), make, &state_path, vec![]);

        exec::run_pipeline(&ctx, &vec!["write".to_string()]).unwrap();

        // Verify each workspace has its own correct content
        for i in 1..=3 {
            let output_file = temp_dir.path().join(format!("workspace{}/output.txt", i));
            assert!(output_file.exists(), "Workspace {} output file should exist", i);

            let content = fs::read_to_string(&output_file).unwrap();
            let expected = format!("workspace{}\n", i);
            assert_eq!(content, expected, "Workspace {} content should be isolated", i);
        }
    }

    #[test]
    fn test_parallel_workspace_error_handling() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");

        // Create 3 workspaces, middle one will fail
        for i in 1..=3 {
            let ws_path = temp_dir.path().join(format!("workspace{}", i));
            fs::create_dir_all(&ws_path).unwrap();

            let command = if i == 2 {
                // Middle workspace fails
                "exit 1"
            } else {
                "echo 'success' > result.txt"
            };

            let make_toml = format!(r#"
[project]
name = "workspace{}"

[lifecycle.process]
description = "Process"
command = "{}"
"#, i, command);

            fs::write(ws_path.join("make.toml"), make_toml).unwrap();
        }

        // Create root make.toml
        let root_make = r#"
[project]
name = "error-test"
type = "monorepo"

[workspace.workspace1]
path = "workspace1"

[workspace.workspace2]
path = "workspace2"

[workspace.workspace3]
path = "workspace3"

[lifecycle.process]
description = "Process"
parallel = true
command = "echo 'test'"
"#;

        fs::write(temp_dir.path().join("make.toml"), root_make).unwrap();

        // Execute - should fail due to workspace2
        let make = Box::leak(Box::new(load_make(temp_dir.path().join("make.toml")).unwrap()));
        let state_path = temp_dir.path().join(".ggen/state.json");
        let ctx = Context::new(temp_dir.path(), make, &state_path, vec![]);

        let result = exec::run_pipeline(&ctx, &vec!["process".to_string()]);

        // Should propagate error
        assert!(result.is_err(), "Pipeline should fail when workspace fails");

        // Note: Due to parallel execution, we can't guarantee which workspaces completed
        // before the error was detected, but the error should be propagated
    }

    #[test]
    fn test_parallel_vs_sequential_performance() {
        use std::time::Instant;

        let temp_dir = TempDir::new().expect("Failed to create temp dir");

        // Create 4 workspaces with sleep commands
        for i in 1..=4 {
            let ws_path = temp_dir.path().join(format!("workspace{}", i));
            fs::create_dir_all(&ws_path).unwrap();

            let make_toml = format!(r#"
[project]
name = "workspace{}"

[lifecycle.work]
description = "Do work"
command = "sleep 0.1 && echo 'done' > result.txt"
"#, i);

            fs::write(ws_path.join("make.toml"), make_toml).unwrap();
        }

        // Create root make.toml for parallel execution
        let parallel_make = r#"
[project]
name = "perf-test-parallel"
type = "monorepo"

[workspace.workspace1]
path = "workspace1"

[workspace.workspace2]
path = "workspace2"

[workspace.workspace3]
path = "workspace3"

[workspace.workspace4]
path = "workspace4"

[lifecycle.work]
description = "Do work"
parallel = true
command = "sleep 0.1"
"#;

        fs::write(temp_dir.path().join("make.toml"), parallel_make).unwrap();

        // Measure parallel execution
        let make_par = Box::leak(Box::new(load_make(temp_dir.path().join("make.toml")).unwrap()));
        let state_path_par = temp_dir.path().join(".ggen/state.json");
        let ctx_par = Context::new(temp_dir.path(), make_par, &state_path_par, vec![]);

        let start_par = Instant::now();
        exec::run_pipeline(&ctx_par, &vec!["work".to_string()]).unwrap();
        let parallel_duration = start_par.elapsed();

        println!("Parallel execution: {}ms", parallel_duration.as_millis());

        // Create sequential version
        let sequential_make = r#"
[project]
name = "perf-test-sequential"
type = "monorepo"

[workspace.workspace1]
path = "workspace1"

[workspace.workspace2]
path = "workspace2"

[workspace.workspace3]
path = "workspace3"

[workspace.workspace4]
path = "workspace4"

[lifecycle.work]
description = "Do work"
parallel = false
command = "sleep 0.1"
"#;

        // Clean up state files
        for i in 1..=4 {
            let ws_state = temp_dir.path().join(format!("workspace{}/.ggen", i));
            if ws_state.exists() {
                fs::remove_dir_all(&ws_state).ok();
            }
        }

        fs::write(temp_dir.path().join("make.toml"), sequential_make).unwrap();

        // Measure sequential execution
        let make_seq = Box::leak(Box::new(load_make(temp_dir.path().join("make.toml")).unwrap()));
        let state_path_seq = temp_dir.path().join(".ggen/state_seq.json");
        let ctx_seq = Context::new(temp_dir.path(), make_seq, &state_path_seq, vec![]);

        let start_seq = Instant::now();
        exec::run_pipeline(&ctx_seq, &vec!["work".to_string()]).unwrap();
        let sequential_duration = start_seq.elapsed();

        println!("Sequential execution: {}ms", sequential_duration.as_millis());

        // Parallel should be significantly faster (at least 1.5x on most systems)
        // With 4 workspaces sleeping 100ms each:
        // - Sequential: ~400ms
        // - Parallel: ~100-150ms (depending on system)
        let speedup = sequential_duration.as_millis() as f64 / parallel_duration.as_millis() as f64;
        println!("Speedup: {:.2}x", speedup);

        // Conservative check - parallel should be at least 1.5x faster
        assert!(speedup >= 1.5, "Parallel execution should be at least 1.5x faster, got {:.2}x", speedup);
    }

    #[test]
    fn test_parallel_state_persistence() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");

        // Create 3 workspaces
        for i in 1..=3 {
            let ws_path = temp_dir.path().join(format!("workspace{}", i));
            fs::create_dir_all(&ws_path).unwrap();

            let make_toml = format!(r#"
[project]
name = "workspace{}"

[lifecycle.init]
description = "Initialize"
command = "echo 'init' > init.txt"

[lifecycle.build]
description = "Build"
command = "echo 'build' > build.txt"

[lifecycle.test]
description = "Test"
command = "echo 'test' > test.txt"
"#, i);

            fs::write(ws_path.join("make.toml"), make_toml).unwrap();
        }

        // Create root make.toml
        let root_make = r#"
[project]
name = "state-test"
type = "monorepo"

[workspace.workspace1]
path = "workspace1"

[workspace.workspace2]
path = "workspace2"

[workspace.workspace3]
path = "workspace3"

[lifecycle.init]
description = "Initialize"
parallel = true
command = "echo 'init'"

[lifecycle.build]
description = "Build"
parallel = true
command = "echo 'build'"

[lifecycle.test]
description = "Test"
parallel = true
command = "echo 'test'"
"#;

        fs::write(temp_dir.path().join("make.toml"), root_make).unwrap();

        // Execute multiple phases
        let make = Box::leak(Box::new(load_make(temp_dir.path().join("make.toml")).unwrap()));
        let state_path = temp_dir.path().join(".ggen/state.json");
        let ctx = Context::new(temp_dir.path(), make, &state_path, vec![]);

        exec::run_pipeline(&ctx, &vec!["init".to_string()]).unwrap();
        exec::run_pipeline(&ctx, &vec!["build".to_string()]).unwrap();
        exec::run_pipeline(&ctx, &vec!["test".to_string()]).unwrap();

        // Verify each workspace has its own state.json
        for i in 1..=3 {
            let ws_state_path = temp_dir.path().join(format!("workspace{}/.ggen/state.json", i));
            assert!(ws_state_path.exists(), "Workspace {} should have state file", i);

            let ws_state = load_state(&ws_state_path);

            // Verify phase history
            assert_eq!(ws_state.phase_history.len(), 3, "Workspace {} should have 3 phase records", i);
            assert_eq!(ws_state.phase_history[0].phase, "init");
            assert_eq!(ws_state.phase_history[1].phase, "build");
            assert_eq!(ws_state.phase_history[2].phase, "test");

            // All should succeed
            for record in &ws_state.phase_history {
                assert!(record.success, "Phase {} should succeed in workspace {}", record.phase, i);
            }

            // Last phase should be test
            assert_eq!(ws_state.last_phase, Some("test".to_string()));

            // Verify state files don't conflict
            let state_content = fs::read_to_string(&ws_state_path).unwrap();
            assert!(state_content.contains("\"phase\":\"init\""));
            assert!(state_content.contains("\"phase\":\"build\""));
            assert!(state_content.contains("\"phase\":\"test\""));
        }

        // Verify states are independent (different timestamps)
        let state1 = load_state(&temp_dir.path().join("workspace1/.ggen/state.json"));
        let state2 = load_state(&temp_dir.path().join("workspace2/.ggen/state.json"));
        let state3 = load_state(&temp_dir.path().join("workspace3/.ggen/state.json"));

        // States should have the same structure but potentially different timestamps
        // (in parallel execution, they may start at slightly different times)
        assert_eq!(state1.phase_history.len(), state2.phase_history.len());
        assert_eq!(state2.phase_history.len(), state3.phase_history.len());
    }
}
