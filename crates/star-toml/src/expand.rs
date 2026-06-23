//! Environment-variable expansion for TOML text.

/// Expand `${VAR}` and `$VAR` references using the current process environment.
///
/// - Known variables are replaced with their value.
/// - Unknown variables are left as-is (safe for partial expansion).
/// - The surrounding TOML text is copied verbatim via string slices, so multi-byte
///   UTF-8 characters outside of `$…` expressions are never corrupted.
///
/// # Examples
///
/// ```
/// std::env::set_var("STAR_HOME", "/opt/star");
/// let out = star_toml::expand_env_vars("path = \"${STAR_HOME}/config\"");
/// assert_eq!(out, "path = \"/opt/star/config\"");
/// std::env::remove_var("STAR_HOME");
/// ```
#[must_use]
pub fn expand_env_vars(input: &str) -> String {
    let mut result = String::with_capacity(input.len());
    let bytes = input.as_bytes();
    let mut i = 0;
    let mut seg = 0; // byte offset of the start of the current passthrough segment

    while i < bytes.len() {
        if bytes[i] != b'$' {
            i += 1;
            continue;
        }

        // Flush the passthrough segment up to (not including) this '$'
        result.push_str(&input[seg..i]);
        i += 1; // skip '$'

        if i >= bytes.len() {
            result.push('$');
            seg = i;
            break;
        }

        if bytes[i] == b'{' {
            // ${VAR} form
            i += 1; // skip '{'
            let name_start = i;
            while i < bytes.len() && bytes[i] != b'}' {
                i += 1;
            }
            let name = &input[name_start..i];
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
            seg = i;
        } else if bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' {
            // $VAR form — collect alphanumeric + underscore chars
            let name_start = i;
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                i += 1;
            }
            let name = &input[name_start..i];
            match std::env::var(name) {
                Ok(val) => result.push_str(&val),
                Err(_) => {
                    result.push('$');
                    result.push_str(name);
                }
            }
            seg = i;
        } else {
            // Bare '$' not followed by a name character — preserve it
            result.push('$');
            seg = i;
        }
    }

    // Flush any remaining passthrough segment (handles inputs with no '$' cheaply)
    result.push_str(&input[seg..]);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn braced_var_known() {
        std::env::set_var("STAR_TOML_TEST_BRACED", "hello");
        let out = expand_env_vars("prefix_${STAR_TOML_TEST_BRACED}_suffix");
        std::env::remove_var("STAR_TOML_TEST_BRACED");
        assert_eq!(out, "prefix_hello_suffix");
    }

    #[test]
    fn bare_var_known() {
        std::env::set_var("STAR_TOML_TEST_BARE", "world");
        let out = expand_env_vars("val=$STAR_TOML_TEST_BARE");
        std::env::remove_var("STAR_TOML_TEST_BARE");
        assert_eq!(out, "val=world");
    }

    #[test]
    fn braced_var_unknown_left_intact() {
        let out = expand_env_vars("${STAR_TOML_DEFINITELY_NOT_SET_XYZ}");
        assert_eq!(out, "${STAR_TOML_DEFINITELY_NOT_SET_XYZ}");
    }

    #[test]
    fn bare_var_unknown_left_intact() {
        let out = expand_env_vars("$STAR_TOML_DEFINITELY_NOT_SET_ABC");
        assert_eq!(out, "$STAR_TOML_DEFINITELY_NOT_SET_ABC");
    }

    #[test]
    fn lone_dollar_preserved() {
        assert_eq!(expand_env_vars("price: $5.00"), "price: $5.00");
    }

    #[test]
    fn dollar_at_end_preserved() {
        assert_eq!(expand_env_vars("foo$"), "foo$");
    }

    #[test]
    fn utf8_passthrough_unmodified() {
        // Non-ASCII text must pass through byte-perfect (the bug we guard against).
        let input = "# 日本語コメント\nname = \"München\"";
        let out = expand_env_vars(input);
        assert_eq!(out, input);
    }

    #[test]
    fn multiple_expansions() {
        std::env::set_var("STAR_A", "foo");
        std::env::set_var("STAR_B", "bar");
        let out = expand_env_vars("${STAR_A}-${STAR_B}");
        std::env::remove_var("STAR_A");
        std::env::remove_var("STAR_B");
        assert_eq!(out, "foo-bar");
    }

    #[test]
    fn no_dollar_returns_input_unchanged() {
        let input = "name = \"ggen\"\nversion = \"1.0.0\"\n";
        assert_eq!(expand_env_vars(input), input);
    }
}
