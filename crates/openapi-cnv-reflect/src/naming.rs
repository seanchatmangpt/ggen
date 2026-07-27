//! Dependency-free identifier sanitizers.
//!
//! Kept hand-rolled rather than pulling in a slug/case-conversion crate: the
//! 80/20 slice only needs "make this a safe Rust-identifier-ish snake_case
//! token" and "make this a safe kebab-case binary/crate name," both of which
//! are a handful of lines.

/// Lowercase, non-alphanumeric runs collapsed to a single separator,
/// trimmed of leading/trailing separators. `sep` is `'_'` for identifiers,
/// `'-'` for crate/binary names.
fn slugify(input: &str, sep: char) -> String {
    let mut out = String::with_capacity(input.len());
    let mut last_was_sep = true; // suppress a leading separator
    let mut prev_was_lower = false;
    for ch in input.chars() {
        if ch.is_ascii_alphanumeric() {
            if ch.is_ascii_uppercase() && prev_was_lower && !last_was_sep {
                // lower->UPPER boundary (camelCase) -> insert a separator.
                // Consecutive uppercase runs ("API") stay joined.
                out.push(sep);
            }
            out.push(ch.to_ascii_lowercase());
            last_was_sep = false;
            prev_was_lower = ch.is_ascii_lowercase();
        } else if !last_was_sep {
            out.push(sep);
            last_was_sep = true;
            prev_was_lower = false;
        }
    }
    while out.ends_with(sep) {
        out.pop();
    }
    out
}

/// A safe `snake_case` token for a Rust field/verb/noun name.
#[must_use]
pub fn snake_case(input: &str) -> String {
    let slug = slugify(input, '_');
    if slug.is_empty() {
        "field".to_owned()
    } else {
        slug
    }
}

/// A safe kebab-case token for a crate/binary name.
#[must_use]
pub fn kebab_case(input: &str) -> String {
    let slug = slugify(input, '-');
    if slug.is_empty() {
        "reflected-cli".to_owned()
    } else {
        slug
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn camel_case_becomes_snake_case() {
        assert_eq!(snake_case("petId"), "pet_id");
        assert_eq!(snake_case("listPets"), "list_pets");
    }

    #[test]
    fn punctuation_collapses_to_one_separator() {
        assert_eq!(snake_case("x--y__z"), "x_y_z");
        assert_eq!(kebab_case("Pet Store API!!"), "pet-store-api");
    }

    #[test]
    fn empty_input_falls_back_to_a_safe_default() {
        assert_eq!(snake_case(""), "field");
        assert_eq!(kebab_case(""), "reflected-cli");
    }
}
