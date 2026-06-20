use bp_core::{error::CoreError, Result};

/// A non-empty, trimmed name string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name(String);

impl Name {
    pub fn new(raw: impl Into<String>) -> Result<Self> {
        let s = raw.into().trim().to_owned();
        if s.is_empty() {
            return Err(CoreError::validation("name must not be empty"));
        }
        if s.len() > 255 {
            return Err(CoreError::validation("name must be ≤ 255 characters"));
        }
        Ok(Self(s))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// A validated, lowercased, trimmed email address (RFC 5321, max 254 chars).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Email(String);

impl Email {
    pub fn new(raw: impl Into<String>) -> Result<Self> {
        let s = raw.into().trim().to_lowercase();
        if s.is_empty() {
            return Err(CoreError::validation("email must not be empty"));
        }
        if s.len() > 254 {
            return Err(CoreError::validation("email must be ≤ 254 characters (RFC 5321)"));
        }
        // Count '@' occurrences — must be exactly one.
        let at_count = s.chars().filter(|&c| c == '@').count();
        if at_count != 1 {
            return Err(CoreError::validation(
                "email must contain exactly one '@' character",
            ));
        }
        // Split into local and domain parts.
        let at_pos = s.find('@').unwrap(); // safe: exactly one '@'
        let local = &s[..at_pos];
        let domain = &s[at_pos + 1..];
        if local.is_empty() {
            return Err(CoreError::validation(
                "email local part (before '@') must not be empty",
            ));
        }
        if !domain.contains('.') {
            return Err(CoreError::validation(
                "email domain (after '@') must contain at least one '.'",
            ));
        }
        Ok(Self(s))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for Email {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// A URL-safe slug: lowercase letters, digits, and hyphens only.
/// No leading or trailing hyphens. Max 100 chars.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Slug(String);

impl Slug {
    pub fn new(raw: impl Into<String>) -> Result<Self> {
        let s = raw.into().trim().to_owned();
        if s.is_empty() {
            return Err(CoreError::validation("slug must not be empty"));
        }
        if s.len() > 100 {
            return Err(CoreError::validation("slug must be ≤ 100 characters"));
        }
        if s.starts_with('-') {
            return Err(CoreError::validation("slug must not start with a hyphen"));
        }
        if s.ends_with('-') {
            return Err(CoreError::validation("slug must not end with a hyphen"));
        }
        for ch in s.chars() {
            if !matches!(ch, 'a'..='z' | '0'..='9' | '-') {
                return Err(CoreError::validation(
                    "slug may only contain lowercase letters, digits, and hyphens",
                ));
            }
        }
        Ok(Self(s))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for Slug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── Name ────────────────────────────────────────────────────────────────

    #[test]
    fn name_new_empty_string_returns_err() {
        let result = Name::new("");
        assert!(result.is_err(), "Name::new(\"\") must return Err");
    }

    #[test]
    fn name_new_whitespace_only_returns_err() {
        let result = Name::new("   ");
        assert!(
            result.is_err(),
            "Name::new with only whitespace must return Err"
        );
    }

    #[test]
    fn name_new_valid_string_returns_ok() {
        let result = Name::new("Alice");
        assert!(result.is_ok(), "Name::new(\"Alice\") must return Ok");
        assert_eq!(result.unwrap().as_str(), "Alice");
    }

    #[test]
    fn name_new_trims_surrounding_whitespace() {
        let name = Name::new("  Bob  ").unwrap();
        assert_eq!(name.as_str(), "Bob");
    }

    #[test]
    fn name_new_too_long_returns_err() {
        let long = "x".repeat(256);
        assert!(
            Name::new(long).is_err(),
            "Name exceeding 255 chars must return Err"
        );
    }

    // ── Email ────────────────────────────────────────────────────────────────

    #[test]
    fn email_new_empty_returns_err() {
        assert!(Email::new("").is_err(), "empty email must return Err");
    }

    #[test]
    fn email_new_whitespace_only_returns_err() {
        assert!(Email::new("   ").is_err(), "whitespace-only email must return Err");
    }

    #[test]
    fn email_new_no_at_returns_err() {
        assert!(
            Email::new("nodomain.example").is_err(),
            "email without '@' must return Err"
        );
    }

    #[test]
    fn email_new_double_at_returns_err() {
        assert!(
            Email::new("a@@b.com").is_err(),
            "email with two '@' must return Err"
        );
    }

    #[test]
    fn email_new_no_dot_in_domain_returns_err() {
        assert!(
            Email::new("user@nodot").is_err(),
            "email domain without '.' must return Err"
        );
    }

    #[test]
    fn email_new_empty_local_returns_err() {
        assert!(
            Email::new("@example.com").is_err(),
            "email with empty local part must return Err"
        );
    }

    #[test]
    fn email_new_valid_returns_ok() {
        let e = Email::new("User@Example.COM").unwrap();
        assert_eq!(e.as_str(), "user@example.com");
    }

    #[test]
    fn email_new_trims_and_lowercases() {
        let e = Email::new("  Alice@EXAMPLE.ORG  ").unwrap();
        assert_eq!(e.as_str(), "alice@example.org");
    }

    #[test]
    fn email_new_too_long_returns_err() {
        // local part 243 chars + '@' + 'x.com' (5) = 249 chars, then pad to 255
        let local = "a".repeat(248);
        let addr = format!("{}@x.com", local); // 248 + 1 + 5 = 254 chars → valid
        assert!(Email::new(&addr).is_ok(), "254-char email must be Ok");

        let local2 = "a".repeat(249);
        let addr2 = format!("{}@x.com", local2); // 255 chars → too long
        assert!(Email::new(&addr2).is_err(), "255-char email must return Err");
    }

    // ── Slug ─────────────────────────────────────────────────────────────────

    #[test]
    fn slug_new_empty_returns_err() {
        assert!(Slug::new("").is_err(), "empty slug must return Err");
    }

    #[test]
    fn slug_new_uppercase_returns_err() {
        assert!(
            Slug::new("Hello-World").is_err(),
            "slug with uppercase must return Err"
        );
    }

    #[test]
    fn slug_new_spaces_returns_err() {
        assert!(
            Slug::new("hello world").is_err(),
            "slug with spaces must return Err"
        );
    }

    #[test]
    fn slug_new_valid_returns_ok() {
        let s = Slug::new("hello-world").unwrap();
        assert_eq!(s.as_str(), "hello-world");
    }

    #[test]
    fn slug_new_digits_valid() {
        let s = Slug::new("item-42").unwrap();
        assert_eq!(s.as_str(), "item-42");
    }

    #[test]
    fn slug_new_leading_hyphen_returns_err() {
        assert!(
            Slug::new("-hello").is_err(),
            "slug with leading hyphen must return Err"
        );
    }

    #[test]
    fn slug_new_trailing_hyphen_returns_err() {
        assert!(
            Slug::new("hello-").is_err(),
            "slug with trailing hyphen must return Err"
        );
    }

    #[test]
    fn slug_new_too_long_returns_err() {
        let long = "a".repeat(101);
        assert!(
            Slug::new(long).is_err(),
            "slug exceeding 100 chars must return Err"
        );
    }

    #[test]
    fn slug_display() {
        let s = Slug::new("my-slug").unwrap();
        assert_eq!(s.to_string(), "my-slug");
    }

    #[test]
    fn email_display() {
        let e = Email::new("user@example.com").unwrap();
        assert_eq!(e.to_string(), "user@example.com");
    }

    use proptest::prelude::*;

    proptest! {
        #[test]
        fn name_roundtrips_valid_strings(s in "[a-zA-Z][a-zA-Z0-9 ]{0,50}") {
            let name = Name::new(s.clone()).unwrap();
            prop_assert_eq!(name.as_str(), s.trim());
        }

        #[test]
        fn name_always_rejects_empty_after_trim(s in "[ \t\n\r]*") {
            prop_assert!(Name::new(s).is_err());
        }
    }
}
