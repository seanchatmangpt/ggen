//! [`CliHarness`] and [`CliOutput`] — real subprocess boundary crossing.
//!
//! No mocks. No stubs. The binary must exist on disk.

use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

use crate::cli_proof::TempWorkspace;

/// Errors that can occur when setting up or running a [`CliHarness`].
#[derive(Debug)]
#[non_exhaustive]
pub enum CliHarnessError {
    /// An I/O error occurred while spawning or reading the child process.
    Io(std::io::Error),
    /// The requested binary could not be found on disk or in `PATH`.
    BinaryNotFound(String),
}

impl std::fmt::Display for CliHarnessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => write!(f, "I/O error running CLI harness: {e}"),
            Self::BinaryNotFound(name) => {
                write!(f, "binary not found: '{name}' (checked CARGO_BIN_EXE_* and PATH)")
            }
        }
    }
}

impl std::error::Error for CliHarnessError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::BinaryNotFound(_) => None,
        }
    }
}

impl From<std::io::Error> for CliHarnessError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

/// How to locate the binary to execute.
enum BinarySpec {
    /// Binary built by the current Cargo workspace.
    /// Resolved via `CARGO_BIN_EXE_<name>` env var during `cargo test`,
    /// with a PATH fallback for other contexts.
    CargoName(String),
    /// Absolute path to the binary — used as-is.
    AbsolutePath(PathBuf),
}

/// Runs a real CLI binary as a subprocess and returns typed output.
///
/// No mocks. No stubs. The binary must exist on disk.
///
/// # Example
///
/// ```rust,no_run
/// # #[cfg(feature = "cli-proof")]
/// # {
/// use chicago_tdd_tools::cli_proof::CliHarness;
///
/// let output = CliHarness::cargo_bin("my-tool")
///     .args(["--version"])
///     .run()
///     .expect("harness run");
///
/// output.assert_success().assert_stdout_contains("my-tool");
/// # }
/// ```
pub struct CliHarness {
    binary: BinarySpec,
    args: Vec<String>,
    envs: HashMap<String, String>,
    cwd: Option<PathBuf>,
}

impl CliHarness {
    /// Look up a binary built by the current Cargo workspace.
    ///
    /// During `cargo test`, Cargo sets `CARGO_BIN_EXE_<name>` pointing to the
    /// compiled binary. This method checks that env var first, then falls back to
    /// a PATH search so it also works outside of `cargo test`.
    #[must_use]
    pub fn cargo_bin(name: &str) -> Self {
        Self {
            binary: BinarySpec::CargoName(name.to_owned()),
            args: Vec::new(),
            envs: HashMap::new(),
            cwd: None,
        }
    }

    /// Use an explicit binary path.
    #[must_use]
    pub fn from_path(path: impl AsRef<Path>) -> Self {
        Self {
            binary: BinarySpec::AbsolutePath(path.as_ref().to_path_buf()),
            args: Vec::new(),
            envs: HashMap::new(),
            cwd: None,
        }
    }

    /// Append arguments to the command line.
    #[must_use]
    pub fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        self.args.extend(args.into_iter().map(|s| s.as_ref().to_owned()));
        self
    }

    /// Set a single environment variable for the subprocess.
    #[must_use]
    pub fn env(mut self, key: &str, value: &str) -> Self {
        self.envs.insert(key.to_owned(), value.to_owned());
        self
    }

    /// Set the working directory to the root of a [`TempWorkspace`].
    #[must_use]
    pub fn workspace(mut self, ws: &TempWorkspace) -> Self {
        self.cwd = Some(ws.path().to_path_buf());
        self
    }

    /// Set an explicit working directory for the subprocess.
    #[must_use]
    pub fn current_dir(mut self, path: impl AsRef<Path>) -> Self {
        self.cwd = Some(path.as_ref().to_path_buf());
        self
    }

    /// Execute the command. This is the real boundary crossing.
    ///
    /// # Errors
    ///
    /// Returns [`CliHarnessError::BinaryNotFound`] when the binary cannot be located,
    /// or [`CliHarnessError::Io`] when the subprocess cannot be spawned.
    pub fn run(self) -> Result<CliOutput, CliHarnessError> {
        let bin_path = self.resolve_binary()?;

        let mut cmd = Command::new(&bin_path);
        cmd.args(self.args.iter().map(OsStr::new));

        for (k, v) in &self.envs {
            cmd.env(k, v);
        }

        if let Some(ref dir) = self.cwd {
            cmd.current_dir(dir);
        }

        let start = Instant::now();
        let output = cmd.output()?;
        let duration = start.elapsed();

        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let exit_code = output.status.code().unwrap_or(-1);

        Ok(CliOutput { stdout, stderr, exit_code, duration })
    }

    /// Resolve the binary path from the spec.
    fn resolve_binary(&self) -> Result<PathBuf, CliHarnessError> {
        match &self.binary {
            BinarySpec::AbsolutePath(p) => {
                if p.exists() {
                    Ok(p.clone())
                } else {
                    Err(CliHarnessError::BinaryNotFound(p.display().to_string()))
                }
            }
            BinarySpec::CargoName(name) => {
                // During `cargo test`, CARGO_BIN_EXE_<name> is set (with `-` → `_` normalisation).
                let env_key = format!("CARGO_BIN_EXE_{}", name.replace('-', "_"));
                if let Ok(path_str) = std::env::var(&env_key) {
                    let p = PathBuf::from(path_str);
                    if p.exists() {
                        return Ok(p);
                    }
                }
                // 2. Look in the Cargo workspace target directory.
                //    Walk from CARGO_MANIFEST_DIR up to find Cargo.lock (workspace root),
                //    then check target/debug/<name> and target/release/<name>.
                let target_root =
                    std::env::var_os("CARGO_TARGET_DIR").map(PathBuf::from).or_else(|| {
                        let manifest_dir =
                            std::env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
                        let mut dir: &std::path::Path = manifest_dir.as_path();
                        loop {
                            if dir.join("Cargo.lock").exists() {
                                return Some(dir.join("target"));
                            }
                            match dir.parent() {
                                Some(p) => dir = p,
                                None => return None,
                            }
                        }
                    });

                if let Some(target) = target_root {
                    for profile in &["debug", "release"] {
                        let candidate = target.join(profile).join(name);
                        if candidate.is_file() {
                            return Ok(candidate);
                        }
                        let candidate_exe = target.join(profile).join(format!("{name}.exe"));
                        if candidate_exe.is_file() {
                            return Ok(candidate_exe);
                        }
                    }
                }

                // 3. Last resort: PATH search (may find a system-installed version).
                which_in_path(name).ok_or_else(|| CliHarnessError::BinaryNotFound(name.clone()))
            }
        }
    }
}

/// Search for `name` in the directories listed in the `PATH` environment variable.
fn which_in_path(name: &str) -> Option<PathBuf> {
    let path_var = std::env::var_os("PATH")?;
    std::env::split_paths(&path_var).find_map(|dir| {
        let candidate = dir.join(name);
        if candidate.is_file() {
            Some(candidate)
        } else {
            // On Windows binaries have .exe extension; try that too.
            let with_exe = dir.join(format!("{name}.exe"));
            if with_exe.is_file() {
                Some(with_exe)
            } else {
                None
            }
        }
    })
}

/// Output captured from a CLI subprocess execution.
#[derive(Debug, Clone)]
pub struct CliOutput {
    /// Everything written to stdout, decoded as UTF-8 (lossy).
    pub stdout: String,
    /// Everything written to stderr, decoded as UTF-8 (lossy).
    pub stderr: String,
    /// Process exit code (`-1` when the OS did not provide one).
    pub exit_code: i32,
    /// Wall-clock duration of the subprocess execution.
    pub duration: Duration,
}

impl CliOutput {
    /// Assert the process exited with code `0`. Panics with a detailed message otherwise.
    pub fn assert_success(&self) -> &Self {
        if self.exit_code != 0 {
            panic!(
                "expected success (exit 0) but got exit code {}\n\
                 stdout:\n{}\nstderr:\n{}",
                self.exit_code, self.stdout, self.stderr
            );
        }
        self
    }

    /// Assert the process exited with a non-zero code. Panics if it succeeded.
    pub fn assert_failure(&self) -> &Self {
        if self.exit_code == 0 {
            panic!(
                "expected non-zero exit but process succeeded (exit 0)\n\
                 stdout:\n{}\nstderr:\n{}",
                self.stdout, self.stderr
            );
        }
        self
    }

    /// Assert the process exited with exactly `expected`. Panics otherwise.
    pub fn assert_exit_code(&self, expected: i32) -> &Self {
        if self.exit_code != expected {
            panic!(
                "expected exit code {expected} but got {}\n\
                 stdout:\n{}\nstderr:\n{}",
                self.exit_code, self.stdout, self.stderr
            );
        }
        self
    }

    /// Assert that stdout contains `needle`. Panics with a diff-friendly message otherwise.
    pub fn assert_stdout_contains(&self, needle: &str) -> &Self {
        if !self.stdout.contains(needle) {
            panic!(
                "expected stdout to contain {:?} but it did not\n\
                 stdout:\n{}",
                needle, self.stdout
            );
        }
        self
    }

    /// Assert that stdout does NOT contain `needle`. Panics if found.
    pub fn assert_stdout_not_contains(&self, needle: &str) -> &Self {
        if self.stdout.contains(needle) {
            panic!(
                "expected stdout NOT to contain {:?} but it did\n\
                 stdout:\n{}",
                needle, self.stdout
            );
        }
        self
    }

    /// Assert that stderr is empty. Panics if stderr contains any bytes.
    pub fn assert_stderr_empty(&self) -> &Self {
        if !self.stderr.is_empty() {
            panic!("expected empty stderr but got:\n{}", self.stderr);
        }
        self
    }

    /// Assert that stderr contains `needle`. Panics otherwise.
    pub fn assert_stderr_contains(&self, needle: &str) -> &Self {
        if !self.stderr.contains(needle) {
            panic!(
                "expected stderr to contain {:?} but it did not\n\
                 stderr:\n{}",
                needle, self.stderr
            );
        }
        self
    }

    /// Parse stdout as JSON and assert that the top-level field `key` equals
    /// `expected_value` (string representation).
    pub fn assert_stdout_json_field(&self, key: &str, expected_value: &str) -> &Self {
        let parsed: serde_json::Value = serde_json::from_str(&self.stdout).unwrap_or_else(|e| {
            panic!("stdout is not valid JSON ({})\nstdout:\n{}", e, self.stdout)
        });
        let actual = parsed.get(key).unwrap_or_else(|| {
            panic!("JSON field {:?} not found in stdout\nstdout:\n{}", key, self.stdout)
        });
        let actual_str = match actual {
            serde_json::Value::String(s) => s.clone(),
            other => other.to_string(),
        };
        if actual_str != expected_value {
            panic!(
                "JSON field {:?}: expected {:?} but got {:?}\nstdout:\n{}",
                key, expected_value, actual_str, self.stdout
            );
        }
        self
    }
}
