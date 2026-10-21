//! Canonical identity for identity-bearing subjects (v26.10.20 lane L05/A05).
//!
//! Vocabulary: `canon:` per `.specify/canonical-identity-lattice.ttl` (L04) —
//! forms are exactly [`CanonicalForm`]'s three variants; digests are BLAKE3,
//! keyed via `blake3::derive_key(domain, ..)` (domain-separated; unkeyed only
//! where explicitly requested with `keyed = false`, which is NOT admissible
//! for lattice types `canon:GraphReceipt`/`canon:Envelope`).
//!
//! Reuse, not divergence: sorted-key compact JSON delegates to
//! `ggen_config::canonical::json::canonicalize_json` (the workspace's one
//! canonical JSON implementation, ported from the retired ggen-core). This
//! module adds dispatch + domain-separated digesting on top; it does not
//! hand-grow a second canonical JSON.

/// The three canonical serialization forms pinned by the `canon:` lattice
/// (`canon:Rdfc10NQuads`, `canon:SortedKeyCompactJson`,
/// `canon:BTreeOrderedCollection`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CanonicalForm {
    /// RDFC-10 canonicalization: N-Quads lines, lexicographically sorted.
    Rdfc10NQuads,
    /// JSON with recursively sorted object keys, compact separators.
    SortedKeyCompactJson,
    /// Collection members serialized in sorted (`BTree`) order.
    BTreeOrderedCollection,
}

/// Errors from canonical serialization.
#[derive(Debug, thiserror::Error)]
pub enum CanonicalError {
    /// Payload shape does not match the requested canonical form.
    #[error("payload not valid for canonical form {form:?}: {reason}")]
    Shape {
        /// The requested form.
        form: CanonicalForm,
        /// Why the payload does not fit it.
        reason: String,
    },
    /// Underlying JSON canonicalization failure (from ggen-config).
    #[error("json canonicalization failed: {0}")]
    Json(String),
}

impl From<ggen_config::canonical::CanonicalError> for CanonicalError {
    fn from(e: ggen_config::canonical::CanonicalError) -> Self {
        Self::Json(e.to_string())
    }
}

/// Canonical serialization, dispatching totally on `form`.
///
/// - [`CanonicalForm::Rdfc10NQuads`]: payload must be a JSON array of N-Quads
///   line strings; output is the lines sorted lexicographically, `\n`-joined
///   with a trailing newline (the sorted-quad step of RDFC-10 output; the
///   blank-node canonicalization itself lives in `ggen-graph`).
/// - [`CanonicalForm::SortedKeyCompactJson`]: delegates to ggen-config's
///   canonicalizer (recursively sorted keys, compact).
/// - [`CanonicalForm::BTreeOrderedCollection`]: payload must be a JSON array;
///   members are serialized compactly and emitted in sorted order, `\n`-joined.
///
/// # Errors
/// [`CanonicalError::Shape`] when the payload does not fit the form.
pub fn canonical_bytes(
    form: CanonicalForm, payload: &serde_json::Value,
) -> Result<Vec<u8>, CanonicalError> {
    match form {
        CanonicalForm::Rdfc10NQuads => {
            let lines = payload_as_lines(form, payload)?;
            let mut sorted = lines;
            sorted.sort_unstable();
            let mut out = sorted.join("\n");
            if !out.is_empty() {
                out.push('\n');
            }
            Ok(out.into_bytes())
        }
        CanonicalForm::SortedKeyCompactJson => {
            let s = ggen_config::canonical::json::canonicalize_json(payload)?;
            Ok(s.into_bytes())
        }
        CanonicalForm::BTreeOrderedCollection => {
            let members = payload_as_lines(form, payload)?;
            let mut rendered: Vec<String> = members
                .iter()
                .map(ggen_config::canonical::json::canonicalize_json)
                .collect::<Result<Vec<_>, _>>()?;
            rendered.sort();
            Ok(rendered.join("\n").into_bytes())
        }
    }
}

/// Domain-separated digest over the canonical bytes.
///
/// `keyed = true` (the lattice-admissible default for identity) computes
/// `blake3::derive_key(domain, canonical)` — distinct domains yield
/// independent digests over identical payloads. `keyed = false` computes an
/// unkeyed BLAKE3 hash; only for non-adversarial checksums.
///
/// # Errors
/// Propagates [`canonical_bytes`] errors.
pub fn canonical_digest(
    form: CanonicalForm, domain: &str, keyed: bool, payload: &serde_json::Value,
) -> Result<[u8; 32], CanonicalError> {
    let canonical = canonical_bytes(form, payload)?;
    Ok(if keyed {
        blake3::derive_key(domain, &canonical)
    } else {
        *blake3::hash(&canonical).as_bytes()
    })
}

fn payload_as_lines(
    form: CanonicalForm, payload: &serde_json::Value,
) -> Result<Vec<String>, CanonicalError> {
    match payload {
        serde_json::Value::Array(items) => items
            .iter()
            .map(|item| match item {
                serde_json::Value::String(s) => Ok(s.clone()),
                other => Err(CanonicalError::Shape {
                    form,
                    reason: format!("expected string element, got {other:?}"),
                }),
            })
            .collect(),
        other => Err(CanonicalError::Shape {
            form,
            reason: format!("expected JSON array payload, got {other:?}"),
        }),
    }
}

// ---------------------------------------------------------------------------
// Codegen: const-size assertions and generated canonical modules
// ---------------------------------------------------------------------------

/// Whether a field kind has a compile-time-fixed width (no heap indirection).
#[must_use]
pub fn is_fixed_width(kind: &str) -> bool {
    matches!(
        kind,
        "u8" | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "bool"
            | "[u8; 32]"
    )
}

/// The exact const-size assertion line emitted for a fixed-width type.
/// Only ever emitted when every field of the subject is fixed-width —
/// variable-width fields (`String`, `Vec<_>`) make the size a runtime fact,
/// and pinning it would assert nothing.
#[must_use]
pub fn const_size_assertion(type_name: &str, expected: usize) -> String {
    format!("const _: () = assert!(::core::mem::size_of::<{type_name}>() == {expected});")
}

/// A field of a subject being code-generated.
#[derive(Debug, Clone)]
pub struct FieldSpec {
    /// Field name.
    pub name: &'static str,
    /// Rust type string.
    pub kind: &'static str,
}

/// Generate a canonical-identity module source for a subject with the given
/// fields (the same shape `.specify/templates/canonical/serializer.rs.tera`
/// renders). Emits a const-size assertion ONLY when every field is
/// fixed-width.
#[must_use]
pub fn generate_canonical_module(type_name: &str, fields: &[FieldSpec]) -> String {
    let all_fixed = fields.iter().all(|f| is_fixed_width(f.kind));
    let mut out = String::from("// GENERATED canonical module — do not hand-edit.\n");
    out.push_str("#[derive(Debug, Clone, PartialEq, Eq)]\npub struct ");
    out.push_str(type_name);
    out.push_str(" {\n");
    for f in fields {
        out.push_str("    pub ");
        out.push_str(f.name);
        out.push_str(": ");
        out.push_str(f.kind);
        out.push_str(",\n");
    }
    out.push_str("}\n");
    if all_fixed {
        // Fixed-width layout size, computed from the declared field kinds.
        let sizes: Vec<&str> = fields.iter().map(|f| f.kind).collect();
        let total = fixed_width_total(&sizes);
        out.push_str(&const_size_assertion(type_name, total));
        out.push('\n');
    }
    out
}

fn fixed_width_total(kinds: &[&str]) -> usize {
    kinds
        .iter()
        .map(|k| match *k {
            "u8" | "i8" | "bool" => 1,
            "u16" | "i16" => 2,
            "u32" | "i32" => 4,
            "u64" | "i64" | "usize" | "isize" | "[u8; 32]" => 8,
            "u128" | "i128" => 16,
            _ => 0,
        })
        .sum()
}

/// Compile-time const-size assertion for fixed-width subjects, usable at any
/// call site: `const_size_assert!(MyFixed, 16);` compiles to
/// `const _: () = assert!(size_of::<MyFixed>() == 16);` and fails the build
/// (not the test run) when the layout drifts.
#[macro_export]
macro_rules! const_size_assert {
    ($t:ty, $n:expr) => {
        const _: () = assert!(::core::mem::size_of::<$t>() == $n);
    };
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn sorted_key_compact_json_sorts_recursively() {
        let payload = json!({"b": 2, "a": {"y": 1, "x": 2}});
        let bytes = canonical_bytes(CanonicalForm::SortedKeyCompactJson, &payload).unwrap();
        let s = String::from_utf8(bytes).unwrap();
        assert_eq!(s, r#"{"a":{"x":2,"y":1},"b":2}"#);
    }

    #[test]
    fn rdfc10_form_sorts_quad_lines() {
        let payload = json!(["<b> <p> 1 .", "<a> <p> 0 ."]);
        let bytes = canonical_bytes(CanonicalForm::Rdfc10NQuads, &payload).unwrap();
        assert_eq!(
            String::from_utf8(bytes).unwrap(),
            "<a> <p> 0 .\n<b> <p> 1 .\n"
        );
    }

    #[test]
    fn btree_form_sorts_members() {
        let payload = json!(["zebra", "apple"]);
        let bytes = canonical_bytes(CanonicalForm::BTreeOrderedCollection, &payload).unwrap();
        assert_eq!(String::from_utf8(bytes).unwrap(), "\"apple\"\n\"zebra\"");
    }

    #[test]
    fn wrong_shape_is_typed_error_not_panic() {
        let err =
            canonical_bytes(CanonicalForm::Rdfc10NQuads, &json!({"not": "an array"})).unwrap_err();
        assert!(matches!(err, CanonicalError::Shape { .. }));
    }
}
