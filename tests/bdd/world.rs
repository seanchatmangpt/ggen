use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Output;
use tempfile::TempDir;

/// World state for BDD tests
/// 
/// Maintains state across scenario steps including:
/// - Temporary directories for isolated test runs
/// - Command outputs and exit codes
/// - Captured file contents and hashes
/// - Registry mocking state
#[derive(Default)]
pub struct RgenWorld {
    /// Temporary directory for test isolation
    pub temp_dir: Option<TempDir>,
    
    /// Project directory path (usually temp_dir.path())
    pub project_dir: PathBuf,
    
    /// Last command output
    pub last_output: Option<Output>,
    
    /// Last command exit code
    pub last_exit_code: Option<i32>,
    
    /// Captured file contents by path
    pub captured_files: HashMap<String, String>,
    
    /// Captured output hashes for determinism testing
    pub captured_hashes: Vec<String>,
    
    /// Registry URL override for testing
    pub registry_url: Option<String>,
    
    /// Mock server for registry testing
    pub mock_server: Option<mockito::Server>,
}

impl RgenWorld {
    /// Create a new world with a temporary directory
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let project_dir = temp_dir.path().to_path_buf();
        
        Self {
            temp_dir: Some(temp_dir),
            project_dir,
            ..Default::default()
        }
    }
    
    /// Get the project directory path
    pub fn project_dir(&self) -> &PathBuf {
        &self.project_dir
    }
    
    /// Set registry URL for testing
    pub fn set_registry_url(&mut self, url: String) {
        self.registry_url = Some(url);
    }
    
    /// Capture file content for later comparison
    pub fn capture_file(&mut self, path: &str, content: String) {
        self.captured_files.insert(path.to_string(), content);
    }
    
    /// Capture output hash for determinism testing
    pub fn capture_hash(&mut self, hash: String) {
        self.captured_hashes.push(hash);
    }
    
    /// Get last captured hash
    pub fn last_hash(&self) -> Option<&String> {
        self.captured_hashes.last()
    }
    
    /// Check if last command succeeded
    pub fn last_command_succeeded(&self) -> bool {
        self.last_exit_code.map_or(false, |code| code == 0)
    }
    
    /// Get last command stdout as string
    pub fn last_stdout(&self) -> String {
        self.last_output
            .as_ref()
            .map(|output| String::from_utf8_lossy(&output.stdout).to_string())
            .unwrap_or_default()
    }
    
    /// Get last command stderr as string
    pub fn last_stderr(&self) -> String {
        self.last_output
            .as_ref()
            .map(|output| String::from_utf8_lossy(&output.stderr).to_string())
            .unwrap_or_default()
    }
}

impl cucumber::World for RgenWorld {
    type Error = Box<dyn std::error::Error + Send + Sync>;

    fn new() -> Result<Self, Self::Error> {
        Ok(Self::new())
    }
}
