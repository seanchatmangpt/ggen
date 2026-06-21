//! Deep-merge for `toml::Value` trees.
//!
//! Used by [`crate::Loader`] to compose layered configs: earlier layers supply defaults,
//! later layers override specific keys.

use toml::Value;

/// Recursively merge `overlay` into `base`.
///
/// - **Tables**: keys present in `overlay` are inserted or recursively merged into `base`.
///   Keys absent from `overlay` are left unchanged in `base`.
/// - **Everything else** (arrays, strings, numbers, booleans, datetimes): `overlay`
///   replaces `base` entirely.
///
/// # Examples
///
/// ```
/// use toml::Value;
/// use star_toml::deep_merge;
///
/// let mut base: Value = toml::from_str(r#"
/// [server]
/// host = "localhost"
/// port = 8080
/// "#).unwrap();
///
/// let overlay: Value = toml::from_str(r#"
/// [server]
/// port = 9090
/// "#).unwrap();
///
/// deep_merge(&mut base, overlay);
///
/// let tbl = base.as_table().unwrap();
/// let server = tbl["server"].as_table().unwrap();
/// assert_eq!(server["host"].as_str(), Some("localhost")); // preserved
/// assert_eq!(server["port"].as_integer(), Some(9090));    // overridden
/// ```
pub fn deep_merge(base: &mut Value, overlay: Value) {
    match (base, overlay) {
        (Value::Table(base_tbl), Value::Table(overlay_tbl)) => {
            for (key, val) in overlay_tbl {
                match base_tbl.get_mut(&key) {
                    Some(existing) => deep_merge(existing, val),
                    None => {
                        base_tbl.insert(key, val);
                    }
                }
            }
        }
        (base, overlay) => *base = overlay,
    }
}

/// Inject a dotted key path into a `toml::Value::Table`, creating intermediate tables
/// as needed.
///
/// `path` is a dot-separated string like `"server.port"`. `value` is inserted at the
/// leaf; any intermediate table that does not exist is created automatically.
///
/// This is used internally by [`crate::Loader::env_prefix`] to convert env-var overrides
/// into TOML tree nodes.
pub(crate) fn set_dotted(root: &mut Value, path: &str, value: Value) {
    let mut parts = path.splitn(2, '.');
    let head = match parts.next() {
        Some(h) if !h.is_empty() => h,
        _ => return,
    };
    let tail = parts.next();

    let tbl = match root {
        Value::Table(t) => t,
        other => {
            *other = Value::Table(toml::map::Map::new());
            other.as_table_mut().unwrap()
        }
    };

    match tail {
        None => {
            tbl.insert(head.to_owned(), value);
        }
        Some(rest) => {
            let entry = tbl
                .entry(head.to_owned())
                .or_insert_with(|| Value::Table(toml::map::Map::new()));
            set_dotted(entry, rest, value);
        }
    }
}

/// Best-effort conversion of an env-var string to a typed `toml::Value`.
///
/// Tries bool → integer → float → string in that order, so `"true"` becomes
/// `Value::Boolean(true)`, `"42"` becomes `Value::Integer(42)`, etc.
pub(crate) fn env_str_to_value(s: &str) -> Value {
    if s.eq_ignore_ascii_case("true") {
        return Value::Boolean(true);
    }
    if s.eq_ignore_ascii_case("false") {
        return Value::Boolean(false);
    }
    if let Ok(n) = s.parse::<i64>() {
        return Value::Integer(n);
    }
    if let Ok(f) = s.parse::<f64>() {
        return Value::Float(f);
    }
    Value::String(s.to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;
    use toml::Value;

    fn parse(s: &str) -> Value {
        toml::from_str(s).unwrap()
    }

    #[test]
    fn merges_tables_recursively() {
        let mut base = parse("[a]\nx = 1\ny = 2\n");
        let overlay = parse("[a]\ny = 99\nz = 3\n");
        deep_merge(&mut base, overlay);
        let a = base["a"].as_table().unwrap();
        assert_eq!(a["x"].as_integer(), Some(1)); // preserved
        assert_eq!(a["y"].as_integer(), Some(99)); // overridden
        assert_eq!(a["z"].as_integer(), Some(3)); // added
    }

    #[test]
    fn overlay_replaces_scalar() {
        let mut base = parse("x = 1\n");
        let overlay = parse("x = 42\n");
        deep_merge(&mut base, overlay);
        assert_eq!(base["x"].as_integer(), Some(42));
    }

    #[test]
    fn overlay_replaces_array_entirely() {
        let mut base = parse("arr = [1, 2, 3]\n");
        let overlay = parse("arr = [4, 5]\n");
        deep_merge(&mut base, overlay);
        let arr = base["arr"].as_array().unwrap();
        assert_eq!(arr.len(), 2);
        assert_eq!(arr[0].as_integer(), Some(4));
    }

    #[test]
    fn base_key_absent_in_overlay_preserved() {
        let mut base = parse("keep = \"me\"\nchange = 1\n");
        let overlay = parse("change = 2\n");
        deep_merge(&mut base, overlay);
        assert_eq!(base["keep"].as_str(), Some("me"));
        assert_eq!(base["change"].as_integer(), Some(2));
    }

    #[test]
    fn set_dotted_creates_nested() {
        let mut root = Value::Table(toml::map::Map::new());
        set_dotted(&mut root, "a.b.c", Value::Integer(7));
        assert_eq!(root["a"]["b"]["c"].as_integer(), Some(7));
    }

    #[test]
    fn env_str_parses_bool() {
        assert_eq!(env_str_to_value("true"), Value::Boolean(true));
        assert_eq!(env_str_to_value("false"), Value::Boolean(false));
        assert_eq!(env_str_to_value("True"), Value::Boolean(true));
    }

    #[test]
    fn env_str_parses_integer() {
        assert_eq!(env_str_to_value("42"), Value::Integer(42));
        assert_eq!(env_str_to_value("-1"), Value::Integer(-1));
    }

    #[test]
    fn env_str_parses_float() {
        assert!(matches!(env_str_to_value("3.14"), Value::Float(_)));
    }

    #[test]
    fn env_str_falls_back_to_string() {
        assert_eq!(env_str_to_value("hello"), Value::String("hello".to_owned()));
    }
}
