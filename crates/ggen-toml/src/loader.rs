use crate::error::{ConfigError, Result};
use serde::de::DeserializeOwned;
use std::fs;
use std::path::{Path, PathBuf};

/// Generic TOML config loader.
///
/// Works with any `T: DeserializeOwned` — use it for [`GgenToml`] or your own struct.
///
/// [`GgenToml`]: crate::schema::GgenToml
pub struct TomlLoader;

impl TomlLoader {
    /// Parse `T` from a TOML string after expanding `${VAR}` / `$VAR` env references.
    pub fn from_str<T: DeserializeOwned>(content: &str) -> Result<T> {
        let expanded = expand_env_vars(content);
        toml::from_str(&expanded).map_err(ConfigError::Parse)
    }

    /// Load and parse `T` from a TOML file.
    pub fn from_file<T: DeserializeOwned>(path: impl AsRef<Path>) -> Result<T> {
        let path = path.as_ref();
        if !path.exists() {
            return Err(ConfigError::FileNotFound(path.to_path_buf()));
        }
        let content = fs::read_to_string(path)?;
        Self::from_str(&content)
    }

    /// Walk up from `start` looking for `file_name`, then load it.
    ///
    /// Returns `(resolved_path, parsed_config)`.
    pub fn find_and_load<T: DeserializeOwned>(
        file_name: &str,
        start: impl AsRef<Path>,
    ) -> Result<(PathBuf, T)> {
        let path = find_config_file(file_name, start)
            .ok_or_else(|| ConfigError::FileNotFound(PathBuf::from(file_name)))?;
        let config = Self::from_file(&path)?;
        Ok((path, config))
    }
}

/// Walk parent directories from `start` until `file_name` is found or the filesystem root.
pub fn find_config_file(file_name: &str, start: impl AsRef<Path>) -> Option<PathBuf> {
    let mut dir = start.as_ref().to_path_buf();
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

/// Expand `${VAR}` and `$VAR` references from the current process environment.
///
/// Unknown variables are left as-is so partial expansion is safe.
pub fn expand_env_vars(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut result = String::with_capacity(input.len());
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] != b'$' {
            result.push(bytes[i] as char);
            i += 1;
            continue;
        }

        i += 1; // skip '$'

        if i >= bytes.len() {
            result.push('$');
            break;
        }

        if bytes[i] == b'{' {
            // ${VAR} form
            i += 1; // skip '{'
            let start = i;
            while i < bytes.len() && bytes[i] != b'}' {
                i += 1;
            }
            let name = &input[start..i];
            if i < bytes.len() {
                i += 1; // skip '}'
            }
            match std::env::var(name) {
                Ok(val) => result.push_str(&val),
                Err(_) => {
                    result.push_str("${");
                    result.push_str(name);
                    result.push('}');
                }
            }
        } else if bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' {
            // $VAR form — collect alphanumeric + underscore chars
            let start = i;
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                i += 1;
            }
            let name = &input[start..i];
            match std::env::var(name) {
                Ok(val) => result.push_str(&val),
                Err(_) => {
                    result.push('$');
                    result.push_str(name);
                }
            }
        } else {
            result.push('$');
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    // NOTE: env-var tests are intentionally sequential to avoid test interference.

    #[test]
    fn from_str_parses_toml() {
        #[derive(serde::Deserialize)]
        struct Simple {
            value: u32,
        }
        let s: Simple = TomlLoader::from_str("value = 42").unwrap();
        assert_eq!(s.value, 42);
    }

    #[test]
    fn from_file_loads_toml() {
        use std::io::Write;
        let mut f = NamedTempFile::new().unwrap();
        writeln!(f, "value = 7").unwrap();

        #[derive(serde::Deserialize)]
        struct Simple {
            value: u32,
        }
        let s: Simple = TomlLoader::from_file(f.path()).unwrap();
        assert_eq!(s.value, 7);
    }

    #[test]
    fn from_file_missing_returns_file_not_found() {
        let result = TomlLoader::from_file::<toml::Value>("/nonexistent/path.toml");
        assert!(matches!(result, Err(ConfigError::FileNotFound(_))));
    }

    #[test]
    fn expand_braced_var_known() {
        std::env::set_var("GGEN_TOML_TEST_BRACED", "hello");
        let out = expand_env_vars("prefix_${GGEN_TOML_TEST_BRACED}_suffix");
        std::env::remove_var("GGEN_TOML_TEST_BRACED");
        assert_eq!(out, "prefix_hello_suffix");
    }

    #[test]
    fn expand_bare_var_known() {
        std::env::set_var("GGEN_TOML_TEST_BARE", "world");
        let out = expand_env_vars("val=$GGEN_TOML_TEST_BARE");
        std::env::remove_var("GGEN_TOML_TEST_BARE");
        assert_eq!(out, "val=world");
    }

    #[test]
    fn unknown_braced_var_left_intact() {
        let out = expand_env_vars("${GGEN_TOML_DEFINITELY_NOT_SET_XYZ123}");
        assert_eq!(out, "${GGEN_TOML_DEFINITELY_NOT_SET_XYZ123}");
    }

    #[test]
    fn unknown_bare_var_left_intact() {
        let out = expand_env_vars("$GGEN_TOML_DEFINITELY_NOT_SET_ABC");
        assert_eq!(out, "$GGEN_TOML_DEFINITELY_NOT_SET_ABC");
    }

    #[test]
    fn lone_dollar_left_intact() {
        let out = expand_env_vars("price: $5.00");
        assert_eq!(out, "price: $5.00");
    }

    #[test]
    fn find_config_file_walks_up() {
        use std::io::Write;
        let dir = tempfile::tempdir().unwrap();
        let child = dir.path().join("sub/deep");
        std::fs::create_dir_all(&child).unwrap();

        let config_path = dir.path().join("ggen.toml");
        std::fs::File::create(&config_path)
            .unwrap()
            .write_all(b"")
            .unwrap();

        let found = find_config_file("ggen.toml", &child);
        assert_eq!(found, Some(config_path));
    }

    #[test]
    fn find_config_file_returns_none_when_absent() {
        let dir = tempfile::tempdir().unwrap();
        let found = find_config_file("ggen.toml", dir.path());
        assert!(found.is_none());
    }
}
