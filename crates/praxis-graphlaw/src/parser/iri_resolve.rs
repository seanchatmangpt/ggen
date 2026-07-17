use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Prefix mapper
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct PrefixMapper {
    prefixes: HashMap<String, String>,
    /// The current `@base`/BASE IRI, if any has been declared so far in the
    /// document. `None` means no base is in effect, in which case `resolve`
    /// is a no-op -- matching this engine's pre-existing behaviour of taking
    /// bracketed IRIs verbatim when no base has been declared.
    base: Option<String>,
}

impl PrefixMapper {
    pub fn new() -> PrefixMapper {
        PrefixMapper {
            prefixes: HashMap::new(),
            base: None,
        }
    }

    pub fn add(&mut self, prefix: String, full_iri: String) {
        // A `@prefix`/PREFIX IRI can itself be a relative reference, resolved
        // against the current base at the point of declaration (RFC 3986 +
        // N3/SPARQL semantics).
        let resolved = self.resolve(&full_iri);
        self.prefixes.insert(prefix, resolved);
    }

    /// Set (or update) the current base IRI, resolving it against whatever
    /// base was already in effect if it is itself a relative reference.
    pub fn set_base(&mut self, iri: &str) {
        let resolved = self.resolve(iri);
        self.base = Some(resolved);
    }

    /// Resolve a raw IRI reference (as it appeared inside `<...>`, unbracketed)
    /// against the current base, per RFC 3986 sec. 5. If no base is in effect,
    /// the reference is returned unchanged (an absolute reference is also
    /// returned unchanged, since `resolve_reference` recognizes its scheme).
    pub fn resolve(&self, iri: &str) -> String {
        match &self.base {
            Some(base) => resolve_reference(base, iri),
            None => iri.to_string(),
        }
    }

    /// Expand a prefixed name, a bare `a`, or a `<IRI>` reference.
    /// Returns the canonical `<IRI>` form.
    pub fn expand(&self, raw: &str) -> String {
        // Trim whitespace and trailing dots (N3 TP terminator may have been consumed)
        let trimmed = raw.trim().trim_end_matches('.');

        // rdf:type shorthand
        if trimmed == "a" {
            return "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".to_string();
        }

        // Already a full <IRI>
        if trimmed.starts_with('<') && trimmed.ends_with('>') {
            return trimmed.to_string();
        }

        // Prefixed name (prefix:local) — local may also have trailing dots that were consumed
        if let Some(colon) = trimmed.find(':') {
            let prefix = &trimmed[..colon];
            // Strip any residual trailing dot from the local name
            let local = trimmed[colon + 1..].trim_end_matches('.');
            if let Some(expanded) = self.prefixes.get(prefix) {
                return format!("<{}{}>", expanded, local);
            }
        }

        // Return as-is (e.g., variable or unknown term)
        trimmed.to_string()
    }
}

impl Default for PrefixMapper {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// RFC 3986 sec. 5 IRI-reference resolution
// ---------------------------------------------------------------------------

/// Split `s` into (scheme, rest-after-colon) if `s` starts with a valid
/// `scheme:` prefix (RFC 3986 sec. 3.1), else `None`.
pub fn split_scheme(s: &str) -> Option<(&str, &str)> {
    let bytes = s.as_bytes();
    if bytes.is_empty() || !bytes[0].is_ascii_alphabetic() {
        return None;
    }
    for (i, b) in bytes.iter().enumerate() {
        match b {
            b':' => {
                if i == 0 {
                    return None;
                }
                return Some((&s[..i], &s[i + 1..]));
            }
            b if b.is_ascii_alphanumeric() || *b == b'+' || *b == b'-' || *b == b'.' => continue,
            _ => return None,
        }
    }
    None
}

/// Split off a `#fragment` suffix, if present.
pub fn split_fragment(s: &str) -> (&str, Option<&str>) {
    match s.find('#') {
        Some(i) => (&s[..i], Some(&s[i + 1..])),
        None => (s, None),
    }
}

/// Split off a `?query` suffix (input must already have any fragment removed).
pub fn split_query(s: &str) -> (&str, Option<&str>) {
    match s.find('?') {
        Some(i) => (&s[..i], Some(&s[i + 1..])),
        None => (s, None),
    }
}

/// Split `//authority/path...` into (Some(authority), "/path...") -- or, if
/// `s` doesn't start with `//`, (None, s) unchanged.
pub fn split_authority(s: &str) -> (Option<&str>, &str) {
    if let Some(rest) = s.strip_prefix("//") {
        let end = rest.find('/').unwrap_or(rest.len());
        (Some(&rest[..end]), &rest[end..])
    } else {
        (None, s)
    }
}

/// RFC 3986 sec. 5.2.4 `remove_dot_segments`.
pub fn remove_dot_segments(path: &str) -> String {
    let mut input = path;
    let mut output = String::new();
    while !input.is_empty() {
        if let Some(rest) = input.strip_prefix("../") {
            input = rest;
        } else if let Some(rest) = input.strip_prefix("./") {
            input = rest;
        } else if let Some(rest) = input.strip_prefix("/./") {
            input = rest;
            output.push('/');
        } else if input == "/." {
            input = "/";
        } else if let Some(rest) = input.strip_prefix("/../") {
            input = rest;
            if let Some(pos) = output.rfind('/') {
                output.truncate(pos);
            } else {
                output.clear();
            }
            output.push('/');
        } else if input == "/.." {
            input = "/";
            if let Some(pos) = output.rfind('/') {
                output.truncate(pos);
            } else {
                output.clear();
            }
        } else if input == "." || input == ".." {
            input = "";
        } else {
            // Move the first path segment to the output, including its
            // leading "/" (if any) but stopping *before* (not consuming) the
            // next "/" -- that terminating slash must stay at the front of
            // `input` so the next iteration can recognize a following
            // "/../" or "/./" prefix (consuming it here would misparse e.g.
            // "/a/b/c/../g": the correctly-RFC-specified rewrite to "/a/b/g"
            // depends on each "/../" being seen with its leading "/" intact).
            let start = input.strip_prefix('/').unwrap_or(input);
            let seg_end = start.find('/').unwrap_or(start.len());
            let prefix_len = input.len() - start.len();
            let take = prefix_len + seg_end;
            output.push_str(&input[..take]);
            input = &input[take..];
        }
    }
    output
}

/// Merge a relative-reference path with the base path per RFC 3986 sec. 5.3.
pub fn merge_path(base_has_authority: bool, base_path: &str, ref_path: &str) -> String {
    if base_has_authority && base_path.is_empty() {
        format!("/{}", ref_path)
    } else if let Some(pos) = base_path.rfind('/') {
        format!("{}{}", &base_path[..=pos], ref_path)
    } else {
        ref_path.to_string()
    }
}

pub fn build_iri(
    scheme: &str,
    authority: Option<&str>,
    path: &str,
    query: Option<&str>,
    fragment: Option<&str>,
) -> String {
    let mut out = String::new();
    out.push_str(scheme);
    out.push(':');
    if let Some(auth) = authority {
        out.push_str("//");
        out.push_str(auth);
    }
    out.push_str(path);
    if let Some(q) = query {
        out.push('?');
        out.push_str(q);
    }
    if let Some(f) = fragment {
        out.push('#');
        out.push_str(f);
    }
    out
}

/// Resolve `reference` (a possibly-relative IRI reference) against `base` (an
/// absolute IRI), implementing the RFC 3986 sec. 5.3 "Transform References"
/// algorithm (component-wise; the "strict" variant -- this engine has no need
/// for the backward-compatible non-strict `.` scheme quirk).
pub fn resolve_reference(base: &str, reference: &str) -> String {
    if let Some((r_scheme, r_after_scheme)) = split_scheme(reference) {
        let (r_no_frag, r_frag) = split_fragment(r_after_scheme);
        let (r_no_query, r_query) = split_query(r_no_frag);
        let (r_auth, r_path) = split_authority(r_no_query);
        let path = remove_dot_segments(r_path);
        return build_iri(r_scheme, r_auth, &path, r_query, r_frag);
    }

    let Some((b_scheme, b_after_scheme)) = split_scheme(base) else {
        // Base itself has no scheme (shouldn't normally happen) -- fall back
        // to returning the reference unresolved rather than panicking.
        return reference.to_string();
    };
    let (b_no_frag, _b_frag) = split_fragment(b_after_scheme);
    let (b_no_query, _b_query) = split_query(b_no_frag);
    let (b_auth, b_path) = split_authority(b_no_query);

    let (ref_no_frag, r_frag) = split_fragment(reference);

    if ref_no_frag.is_empty() {
        // Same-document reference: just the fragment changes.
        return build_iri(b_scheme, b_auth, b_path, None, r_frag);
    }

    if let Some(rest) = ref_no_frag.strip_prefix("//") {
        // Network-path reference: keep scheme, take reference's authority/path/query.
        let end = rest.find('/').unwrap_or(rest.len());
        let r_auth = &rest[..end];
        let (r_path_query, r_query) = split_query(&rest[end..]);
        let path = remove_dot_segments(r_path_query);
        return build_iri(b_scheme, Some(r_auth), &path, r_query, r_frag);
    }

    let (r_path_query, r_query) = split_query(ref_no_frag);

    if let Some(stripped) = r_path_query.strip_prefix('/') {
        let path = remove_dot_segments(&format!("/{}", stripped));
        return build_iri(b_scheme, b_auth, &path, r_query, r_frag);
    }

    if r_path_query.is_empty() {
        return build_iri(b_scheme, b_auth, b_path, r_query, r_frag);
    }

    let merged = merge_path(b_auth.is_some(), b_path, r_path_query);
    let path = remove_dot_segments(&merged);
    build_iri(b_scheme, b_auth, &path, r_query, r_frag)
}
