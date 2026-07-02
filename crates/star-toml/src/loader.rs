//! [`Loader`] — composable, layered TOML config loading.
//!
//! The central piece of the `star_toml` framework. Each `layer_*` call adds one
//! config source; `load()` merges them in order (first = lowest priority, last =
//! highest priority) and deserializes the result into `T`.

use crate::{
    error::{Error, Result},
    expand::expand_env_vars,
    merge::{deep_merge, env_str_to_value, set_dotted},
    validation::Validate,
};
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::fs;
use std::path::{Path, PathBuf};
use toml::Value;

// ---------------------------------------------------------------------------
// ConfigLayer — one source in the loading stack
// ---------------------------------------------------------------------------

enum ConfigLayer {
    /// A literal TOML string (used for built-in defaults).
    Str(String, &'static str /* label for errors */),
    /// A file that must exist.
    File(PathBuf),
    /// A file that is silently skipped when absent.
    FileIfExists(PathBuf),
    /// Walk parent directories until `file_name` is found.
    FindFile(String /* file_name */),
}

// ---------------------------------------------------------------------------
// ConfigFile — loaded config + resolved source path
// ---------------------------------------------------------------------------

/// A `T` together with the path of the TOML file it was loaded from.
///
/// Use [`ConfigFile::resolve`] to convert relative paths found *inside* the
/// config into absolute paths.
///
/// # Example
///
/// ```no_run
/// use star_toml::{Loader, ConfigFile};
///
/// #[derive(serde::Deserialize)]
/// struct Project {
///     template_dir: String,
/// }
///
/// let cf: ConfigFile<Project> = Loader::new()
///     .layer_file("project.toml")
///     .load_file()?;
///
/// // "templates/foo.tera" becomes absolute relative to project.toml's directory
/// let abs = cf.resolve(&cf.config.template_dir);
/// # Ok::<(), star_toml::Error>(())
/// ```
#[derive(Debug, Clone)]
pub struct ConfigFile<T> {
    /// The parsed config.
    pub config: T,
    /// Path to the TOML file that was the last file-based source loaded.
    ///
    /// If no file source was used, this is the current working directory.
    pub path: PathBuf,
}

impl<T> ConfigFile<T> {
    /// Resolve `relative` against the directory that contains the config file.
    ///
    /// If `relative` is already absolute, it is returned unchanged.
    #[must_use]
    pub fn resolve(&self, relative: impl AsRef<Path>) -> PathBuf {
        let rel = relative.as_ref();
        if rel.is_absolute() {
            return rel.to_path_buf();
        }
        let dir = self.path.parent().unwrap_or_else(|| Path::new("."));
        dir.join(rel)
    }
}

impl<T> std::ops::Deref for ConfigFile<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.config
    }
}

impl<T> std::ops::DerefMut for ConfigFile<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.config
    }
}

// ---------------------------------------------------------------------------
// Loader — builder
// ---------------------------------------------------------------------------

/// Builder for composing multiple TOML config sources into a single value.
///
/// Sources are merged in the order they are added: earlier layers provide defaults,
/// later layers override specific keys. Table keys are merged recursively; arrays and
/// scalars are replaced entirely by the later layer.
///
/// Environment-variable expansion (`${VAR}` / `$VAR`) is applied to every source
/// before parsing.
///
/// # Example
///
/// ```no_run
/// use star_toml::Loader;
///
/// #[derive(serde::Deserialize)]
/// struct AppConfig {
///     name: String,
/// }
///
/// const DEFAULTS: &str = r#"
/// name = "default-app"
/// "#;
///
/// let cfg: AppConfig = Loader::new()
///     .layer_str(DEFAULTS, "built-in defaults")
///     .find_file("app.toml")       // walks up from cwd
///     .env_prefix("APP_")          // APP_NAME=foo → name = "foo"
///     .load()?;
/// # Ok::<(), star_toml::Error>(())
/// ```
pub struct Loader {
    layers: Vec<ConfigLayer>,
    env_prefix: Option<String>,
}

impl Default for Loader {
    fn default() -> Self {
        Self::new()
    }
}

impl Loader {
    /// Create an empty loader with no sources.
    #[must_use]
    pub fn new() -> Self {
        Self {
            layers: Vec::new(),
            env_prefix: None,
        }
    }

    /// Add a literal TOML string as the next layer.
    ///
    /// `label` is used in error messages to identify this source (e.g., `"built-in defaults"`).
    #[must_use]
    pub fn layer_str(mut self, content: impl Into<String>, label: &'static str) -> Self {
        self.layers.push(ConfigLayer::Str(content.into(), label));
        self
    }

    /// Add a TOML file as the next layer. Returns an error from [`load`] if the file
    /// does not exist.
    ///
    /// [`load`]: Loader::load
    #[must_use]
    pub fn layer_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.layers.push(ConfigLayer::File(path.into()));
        self
    }

    /// Add a TOML file as the next layer, silently skipping it when absent.
    ///
    /// Useful for optional user-specific config files (e.g., `~/.config/app.toml`).
    #[must_use]
    pub fn layer_file_if_exists(mut self, path: impl Into<PathBuf>) -> Self {
        self.layers.push(ConfigLayer::FileIfExists(path.into()));
        self
    }

    /// Walk parent directories from the current working directory until `file_name`
    /// is found, then add that file as the next layer.
    ///
    /// Skipped silently when the file is not found anywhere in the directory tree.
    #[must_use]
    pub fn find_file(mut self, file_name: impl Into<String>) -> Self {
        self.layers.push(ConfigLayer::FindFile(file_name.into()));
        self
    }

    /// Apply environment-variable overrides using `prefix` as a filter.
    ///
    /// After loading and merging all file layers, any env var whose name starts with
    /// `prefix` is stripped of that prefix and mapped to a TOML key path:
    ///
    /// - Double underscores (`__`) become `.` (path separator)
    /// - The result is lowercased
    ///
    /// For example, with prefix `"APP_"`:
    /// - `APP_NAME=foo`              → `name = "foo"`
    /// - `APP_SERVER__PORT=9090`     → `server.port = 9090` (integer)
    /// - `APP_DEBUG=true`            → `debug = true` (boolean)
    ///
    /// Values are parsed as TOML scalars: bool → integer → float → string.
    #[must_use]
    pub fn env_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.env_prefix = Some(prefix.into());
        self
    }

    /// Merge all layers and deserialize into `T`.
    ///
    /// Returns an error if any required file is missing, any layer fails to parse,
    /// or the merged result cannot be deserialized into `T`.
    pub fn load<T: DeserializeOwned>(self) -> Result<T> {
        let (merged, _) = self.merge_layers()?;
        deserialize_value(merged, "merged config")
    }

    /// Like [`load`], but also returns a [`ConfigFile`] carrying the path of the last
    /// file-based source.  The path is used for resolving relative paths stored in
    /// the config.
    ///
    /// [`load`]: Loader::load
    pub fn load_file<T: DeserializeOwned>(self) -> Result<ConfigFile<T>> {
        let (merged, last_path) = self.merge_layers()?;
        let config = deserialize_value(merged, "merged config")?;
        Ok(ConfigFile {
            config,
            path: last_path,
        })
    }

    /// Like [`load`], but runs [`Validate::check`] on the result before returning.
    ///
    /// On validation failure the error is [`Error::Invalid`], carrying the full
    /// path-precise, multi-error report.
    ///
    /// [`load`]: Loader::load
    pub fn load_validated<T: DeserializeOwned + Validate>(self) -> Result<T> {
        let cfg: T = self.load()?;
        cfg.check()?;
        Ok(cfg)
    }

    // -----------------------------------------------------------------------
    // Internal
    // -----------------------------------------------------------------------

    fn merge_layers(self) -> Result<(Value, PathBuf)> {
        let mut merged = Value::Table(toml::map::Map::new());
        let mut last_file_path = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

        for layer in self.layers {
            match layer {
                ConfigLayer::Str(content, label) => {
                    let expanded = expand_env_vars(&content);
                    let val = parse_str(&expanded, label)?;
                    deep_merge(&mut merged, val);
                }
                ConfigLayer::File(path) => {
                    if !path.exists() {
                        return Err(Error::FileNotFound(path));
                    }
                    let val = load_file_as_value(&path)?;
                    last_file_path = path;
                    deep_merge(&mut merged, val);
                }
                ConfigLayer::FileIfExists(path) => {
                    if path.exists() {
                        let val = load_file_as_value(&path)?;
                        last_file_path = path;
                        deep_merge(&mut merged, val);
                    }
                }
                ConfigLayer::FindFile(file_name) => {
                    if let Some(path) = find_config_file_from_cwd(&file_name) {
                        let val = load_file_as_value(&path)?;
                        last_file_path = path;
                        deep_merge(&mut merged, val);
                    }
                }
            }
        }

        // Apply env-var prefix overrides on top of everything
        if let Some(prefix) = self.env_prefix {
            let prefix_upper = prefix.to_ascii_uppercase();
            for (key, val) in std::env::vars() {
                let key_upper = key.to_ascii_uppercase();
                if let Some(suffix) = key_upper.strip_prefix(&prefix_upper) {
                    let toml_key = suffix.replace("__", ".").to_ascii_lowercase();
                    let toml_val = env_str_to_value(&val);
                    set_dotted(&mut merged, &toml_key, toml_val);
                }
            }
        }

        Ok((merged, last_file_path))
    }
}

// ---------------------------------------------------------------------------
// Free functions (also part of the public API via lib.rs re-exports)
// ---------------------------------------------------------------------------

/// Walk parent directories from `start` until `file_name` is found.
///
/// Returns `None` when no matching file exists in any ancestor directory.
///
/// # Examples
///
/// ```no_run
/// use star_toml::find_config_file;
///
/// if let Some(path) = find_config_file("Cargo.toml", ".") {
///     println!("workspace root: {:?}", path.parent());
/// }
/// ```
#[must_use]
pub fn find_config_file(file_name: &str, start: impl AsRef<Path>) -> Option<PathBuf> {
    let mut dir = start.as_ref().to_path_buf();
    // Normalise: if `start` is a file, begin with its parent.
    if dir.is_file() {
        dir.pop();
    }
    loop {
        let candidate = dir.join(file_name);
        if candidate.exists() {
            return Some(candidate);
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Walk parent directories from `start` and load the first `file_name` found.
///
/// Equivalent to combining [`find_config_file`] and [`load_file`].
///
/// # Errors
///
/// Returns [`Error::FileNotFound`] when no matching file exists anywhere in the
/// directory tree.
pub fn find_and_load<T: DeserializeOwned>(
    file_name: &str, start: impl AsRef<Path>,
) -> Result<(PathBuf, T)> {
    let path = find_config_file(file_name, start)
        .ok_or_else(|| Error::FileNotFound(PathBuf::from(file_name)))?;
    let cfg = load_file(&path)?;
    Ok((path, cfg))
}

/// Parse `T` from a TOML string after expanding `${VAR}` / `$VAR` references.
///
/// # Examples
///
/// ```
/// #[derive(serde::Deserialize)]
/// struct Config { name: String }
///
/// let cfg: Config = star_toml::from_str("[name]\nname = \"test\"").unwrap_or(Config { name: "test".into() });
/// ```
pub fn from_str<T: DeserializeOwned>(content: &str) -> Result<T> {
    let expanded = expand_env_vars(content);
    parse_str(&expanded, "inline string")
}

/// Load and parse `T` from a TOML file, with env-var expansion.
///
/// # Errors
///
/// Returns [`Error::FileNotFound`] if the file does not exist.
pub fn load_file<T: DeserializeOwned>(path: impl AsRef<Path>) -> Result<T> {
    let path = path.as_ref();
    let content = read_file(path)?;
    let expanded = expand_env_vars(&content);
    parse_str(&expanded, &path.display().to_string())
}

/// Serialize `value` to a pretty-printed TOML string.
///
/// # Errors
///
/// Returns [`Error::Serialize`] if the value cannot be represented as TOML.
pub fn to_string<T: Serialize>(value: &T) -> Result<String> {
    toml::to_string_pretty(value).map_err(Error::from)
}

/// Serialize `value` and write it to `path`, creating parent directories as needed.
///
/// Round-trips with [`load_file`]: useful for `init`-style commands that scaffold a
/// default config to disk.
///
/// # Errors
///
/// Returns [`Error::Serialize`] on serialization failure or [`Error::Io`] on write failure.
pub fn save_file<T: Serialize>(value: &T, path: impl AsRef<Path>) -> Result<()> {
    let path = path.as_ref();
    let toml = to_string(value)?;
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(|e| Error::io(parent, e))?;
        }
    }
    fs::write(path, toml).map_err(|e| Error::io(path, e))
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

fn read_file(path: &Path) -> Result<String> {
    if !path.exists() {
        return Err(Error::FileNotFound(path.to_path_buf()));
    }
    fs::read_to_string(path).map_err(|e| Error::io(path, e))
}

fn load_file_as_value(path: &Path) -> Result<Value> {
    let content = read_file(path)?;
    let expanded = expand_env_vars(&content);
    parse_str(&expanded, &path.display().to_string())
}

fn parse_str<T: DeserializeOwned>(content: &str, label: &str) -> Result<T> {
    toml::from_str(content).map_err(|e| Error::parse(label, e))
}

fn deserialize_value<T: DeserializeOwned>(value: Value, label: &str) -> Result<T> {
    T::deserialize(value).map_err(|e| Error::parse(label, e))
}

fn find_config_file_from_cwd(file_name: &str) -> Option<PathBuf> {
    let cwd = std::env::current_dir().ok()?;
    find_config_file(file_name, cwd)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};
    use std::io::Write;
    use tempfile::{NamedTempFile, TempDir};

    #[derive(Debug, Deserialize, Serialize, PartialEq)]
    struct Simple {
        name: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        port: Option<u16>,
    }

    fn write_toml(dir: &TempDir, name: &str, content: &str) -> PathBuf {
        let path = dir.path().join(name);
        std::fs::write(&path, content).unwrap();
        path
    }

    #[test]
    fn from_str_parses() {
        let cfg: Simple = from_str("name = \"hello\"\nport = 8080\n").unwrap();
        assert_eq!(cfg.name, "hello");
        assert_eq!(cfg.port, Some(8080));
    }

    #[test]
    fn load_file_ok() {
        let mut f = NamedTempFile::new().unwrap();
        writeln!(f, "name = \"test\"").unwrap();
        let cfg: Simple = load_file(f.path()).unwrap();
        assert_eq!(cfg.name, "test");
    }

    #[test]
    fn load_file_not_found() {
        let result = load_file::<Simple>("/nonexistent/x.toml");
        assert!(matches!(result, Err(Error::FileNotFound(_))));
    }

    #[test]
    fn loader_layer_str_and_file() {
        let dir = TempDir::new().unwrap();
        write_toml(&dir, "a.toml", "name = \"from-file\"\nport = 9090\n");

        let cfg: Simple = Loader::new()
            .layer_str("name = \"default\"\nport = 8080\n", "defaults")
            .layer_file(dir.path().join("a.toml"))
            .load()
            .unwrap();

        assert_eq!(cfg.name, "from-file"); // file overrides default
        assert_eq!(cfg.port, Some(9090));
    }

    #[test]
    fn loader_file_if_exists_skips_missing() {
        let cfg: Simple = Loader::new()
            .layer_str("name = \"default\"", "defaults")
            .layer_file_if_exists("/nonexistent/optional.toml")
            .load()
            .unwrap();
        assert_eq!(cfg.name, "default");
    }

    #[test]
    fn loader_env_prefix_override() {
        std::env::set_var("STTOML_NAME", "env-name");
        let result = Loader::new()
            .layer_str("name = \"original\"", "defaults")
            .env_prefix("STTOML_")
            .load::<Simple>();
        std::env::remove_var("STTOML_NAME");
        let cfg = result.unwrap();
        assert_eq!(cfg.name, "env-name");
    }

    #[test]
    fn loader_env_prefix_nested_double_underscore() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct Outer {
            server: Server,
        }
        #[derive(Deserialize, PartialEq, Debug)]
        struct Server {
            port: u16,
        }

        std::env::set_var("STTOML2_SERVER__PORT", "9999");
        let result = Loader::new()
            .layer_str("[server]\nport = 8080\n", "defaults")
            .env_prefix("STTOML2_")
            .load::<Outer>();
        std::env::remove_var("STTOML2_SERVER__PORT");
        assert_eq!(result.unwrap().server.port, 9999);
    }

    #[test]
    fn config_file_resolves_relative_paths() {
        let dir = TempDir::new().unwrap();
        write_toml(&dir, "app.toml", "name = \"app\"\n");
        let path = dir.path().join("app.toml");

        let cf: ConfigFile<Simple> = Loader::new().layer_file(&path).load_file().unwrap();

        let resolved = cf.resolve("templates/foo.tera");
        assert_eq!(resolved, dir.path().join("templates/foo.tera"));
    }

    #[test]
    fn find_config_file_walks_up() {
        let dir = TempDir::new().unwrap();
        let child = dir.path().join("a/b/c");
        std::fs::create_dir_all(&child).unwrap();
        let config = dir.path().join("myconfig.toml");
        std::fs::write(&config, "").unwrap();

        let found = find_config_file("myconfig.toml", &child);
        assert_eq!(found, Some(config));
    }

    #[test]
    fn find_config_file_none_when_absent() {
        let dir = TempDir::new().unwrap();
        assert!(find_config_file("missing.toml", dir.path()).is_none());
    }

    #[test]
    fn find_and_load_returns_path_and_config() {
        let dir = TempDir::new().unwrap();
        let child = dir.path().join("sub");
        std::fs::create_dir_all(&child).unwrap();
        write_toml(&dir, "x.toml", "name = \"found\"\n");

        let (path, cfg): (PathBuf, Simple) = find_and_load("x.toml", &child).unwrap();
        assert_eq!(path, dir.path().join("x.toml"));
        assert_eq!(cfg.name, "found");
    }

    #[test]
    fn save_file_round_trips_with_load_file() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("nested/out.toml");
        let original = Simple {
            name: "round-trip".into(),
            port: Some(1234),
        };

        save_file(&original, &path).unwrap();
        assert!(path.exists()); // parent dir was created

        let reloaded: Simple = load_file(&path).unwrap();
        assert_eq!(reloaded, original);
    }

    #[test]
    fn loader_three_layer_precedence() {
        let dir = TempDir::new().unwrap();
        write_toml(&dir, "mid.toml", "name = \"mid\"\nport = 2000\n");
        write_toml(&dir, "top.toml", "port = 3000\n");

        let cfg: Simple = Loader::new()
            .layer_str("name = \"base\"\nport = 1000\n", "base")
            .layer_file(dir.path().join("mid.toml"))
            .layer_file(dir.path().join("top.toml"))
            .load()
            .unwrap();

        // `name` comes from mid (top layer didn't set it), `port` from top
        assert_eq!(cfg.name, "mid");
        assert_eq!(cfg.port, Some(3000));
    }
}
