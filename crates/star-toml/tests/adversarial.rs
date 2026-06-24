use star_toml::validation::{ErrorKind, Severity, Validate, Validator};

#[test]
fn test_semver_adversarial() {
    struct TestSemver {
        version: String,
    }
    impl Validate for TestSemver {
        fn validate(&self, v: &mut Validator) {
            v.check_semver("version", &self.version);
        }
    }

    // 1. Extreme numeric inputs / Overflow testing
    let cases_overflow = vec![
        "9999999999999999999999999999999999999999.0.0",
        "0.9999999999999999999999999999999999999999.0",
        "0.0.9999999999999999999999999999999999999999",
        "4294967296.0.0", // u32::MAX + 1
        "0.4294967296.0",
        "0.0.4294967296",
    ];
    for val in cases_overflow {
        let errs = TestSemver {
            version: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(errs.len(), 1, "Failed for value: {}", val);
        assert_eq!(errs.errors()[0].code(), "invalid_semver");
    }

    // 2. Extreme bounds (u32::MAX should pass)
    let u32_max = u32::MAX.to_string();
    let valid_max = format!("{}.{}.{}", u32_max, u32_max, u32_max);
    assert!(TestSemver { version: valid_max }.check().is_ok());

    // 3. Leading zeros check
    let cases_leading_zeros = vec!["01.0.0", "0.02.0", "0.0.03", "00.0.0", "1.0.00"];
    for val in cases_leading_zeros {
        let errs = TestSemver {
            version: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(errs.errors()[0].code(), "invalid_semver");
    }

    // 4. Invalid formatting / Characters
    let cases_invalid_formats = vec![
        "1",
        "1.0",
        "1.0.0.0",
        "v1.0.0",
        "1.0.0-alpha",
        "1.0.0+build",
        "1.0.a",
        "1.a.0",
        "a.0.0",
        "1.0. ",
        " 1.0.0",
        "1.0.0 ",
        "1..0",
        ".0.0",
        "0.0.",
        "..",
        "1.0.0\0",
        "1\0.0.0",
        "-1.0.0",
        "1.-2.0",
        "1.0.-3",
        "１.０.０",
    ];
    for val in cases_invalid_formats {
        let errs = TestSemver {
            version: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(
            errs.errors()[0].code(),
            "invalid_semver",
            "Passed unexpectedly: {}",
            val
        );
    }
}

#[test]
fn test_ip_or_domain_adversarial() {
    struct TestHost {
        host: String,
    }
    impl Validate for TestHost {
        fn validate(&self, v: &mut Validator) {
            v.check_ip_or_domain("host", &self.host);
        }
    }

    // 1. Hostname length limits (> 253 chars)
    let long_label = "a".repeat(63);
    let valid_long_host = format!("{}.{}.{}.{}", long_label, long_label, long_label, "com"); // ~195 chars
    assert!(TestHost {
        host: valid_long_host
    }
    .check()
    .is_ok());

    let too_long_host = "a".repeat(254);
    let errs = TestHost {
        host: too_long_host,
    }
    .check()
    .unwrap_err();
    assert_eq!(errs.errors()[0].code(), "invalid_ip_or_domain");

    // 2. Label length limits (> 63 chars)
    let label_63 = "a".repeat(63);
    assert!(TestHost {
        host: format!("{}.com", label_63)
    }
    .check()
    .is_ok());

    let label_64 = "a".repeat(64);
    let errs = TestHost {
        host: format!("{}.com", label_64),
    }
    .check()
    .unwrap_err();
    assert_eq!(errs.errors()[0].code(), "invalid_ip_or_domain");

    // 3. Hyphen edge cases
    let cases_invalid_hyphens = vec![
        "-example.com",
        "example-.com",
        "example.com-",
        "sub.-example.com",
        "sub.example-.com",
        "-",
        "---",
    ];
    for val in cases_invalid_hyphens {
        let errs = TestHost {
            host: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(
            errs.errors()[0].code(),
            "invalid_ip_or_domain",
            "Passed unexpectedly: {}",
            val
        );
    }

    // 4. Dot edge cases
    let cases_invalid_dots = vec![
        ".",
        "..",
        "a..b",
        "domain..com",
        ".domain.com",
        "domain.com..",
    ];
    for val in cases_invalid_dots {
        let errs = TestHost {
            host: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(
            errs.errors()[0].code(),
            "invalid_ip_or_domain",
            "Passed unexpectedly: {}",
            val
        );
    }

    // 5. Invalid characters
    let cases_invalid_chars = vec![
        "domain_name.com",
        "doma$in.com",
        "domain name.com",
        "domain/name.com",
        "domain.com\0",
        "domain.com\n",
    ];
    for val in cases_invalid_chars {
        let errs = TestHost {
            host: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(
            errs.errors()[0].code(),
            "invalid_ip_or_domain",
            "Passed unexpectedly: {}",
            val
        );
    }

    // 6. Unicode equivalents (international domain names or casing anomalies)
    // Note: Kelvin sign (U+212A) gets converted to 'K' by to_uppercase(), but since is_hostname does not uppercase
    // except strip_suffix, let's see. Wait, check_ip_or_domain does NOT call to_uppercase()!
    // So "exämple.com" has non-ascii 'ä', which fails is_ascii_alphanumeric().
    let errs = TestHost {
        host: "exämple.com".to_string(),
    }
    .check()
    .unwrap_err();
    assert_eq!(errs.errors()[0].code(), "invalid_ip_or_domain");
}

#[test]
fn test_path_adversarial() {
    struct TestPath {
        path: String,
        must_be_absolute: Option<bool>,
    }
    impl Validate for TestPath {
        fn validate(&self, v: &mut Validator) {
            v.check_path("path", &self.path, self.must_be_absolute);
        }
    }

    // 1. Path traversal variations
    let cases_traversal = vec!["..", "../", "foo/../bar", "foo/..", "../foo", "a/b/../../c"];
    for val in cases_traversal {
        let errs = TestPath {
            path: val.to_string(),
            must_be_absolute: None,
        }
        .check()
        .unwrap_err();
        assert_eq!(errs.errors()[0].code(), "invalid_path");
        assert!(
            errs.errors()[0]
                .msg
                .contains("path traversal ('..') is not allowed"),
            "Failed for: {}",
            val
        );
    }

    // 2. Multi-dot variations (should pass as normal names if not ParentDir)
    assert!(TestPath {
        path: "...".to_string(),
        must_be_absolute: None
    }
    .check()
    .is_ok());
    assert!(TestPath {
        path: "foo/.../bar".to_string(),
        must_be_absolute: None
    }
    .check()
    .is_ok());

    // 3. Null bytes check
    let cases_null = vec!["foo\0bar", "\0", "foo/bar\0"];
    for val in cases_null {
        let errs = TestPath {
            path: val.to_string(),
            must_be_absolute: None,
        }
        .check()
        .unwrap_err();
        assert_eq!(errs.errors()[0].code(), "invalid_path");
        assert!(
            errs.errors()[0]
                .msg
                .contains("path must not contain null bytes"),
            "Failed for: {}",
            val
        );
    }

    // 4. Platform-specific path separator traversal (Windows backslash on Unix)
    // On Unix, 'foo\..\bar' is not parsed as containing 'Component::ParentDir' by std::path::Path
    // since backslash is a regular character on Unix. Our fix ensures it is treated as a separator.
    let res = TestPath {
        path: "foo\\..\\bar".to_string(),
        must_be_absolute: None,
    }
    .check();
    let errs = res.unwrap_err();
    assert_eq!(errs.errors()[0].code(), "invalid_path");
    assert!(errs.errors()[0]
        .msg
        .contains("path traversal ('..') is not allowed"));
}

#[test]
fn test_size_format_adversarial() {
    struct TestSize {
        size: String,
    }
    impl Validate for TestSize {
        fn validate(&self, v: &mut Validator) {
            v.check_size_format("size", &self.size);
        }
    }

    // 1. Extreme inputs / Overflow
    let cases_overflow = vec![
        "18446744073709551616B", // u64::MAX + 1
        "9999999999999999999999999999999999999999GB",
    ];
    for val in cases_overflow {
        let errs = TestSize {
            size: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(errs.errors()[0].code(), "invalid_size_format");
    }

    // 2. Extreme bounds (u64::MAX should pass)
    let u64_max = u64::MAX.to_string();
    assert!(TestSize {
        size: format!("{}B", u64_max)
    }
    .check()
    .is_ok());

    // 3. Floating points / negatives
    let cases_invalid_nums = vec!["1.5GB", "-10MB", "+100KB", " 512MB", "512MB ", "512 MB"];
    for val in cases_invalid_nums {
        let errs = TestSize {
            size: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(errs.errors()[0].code(), "invalid_size_format");
    }

    // 4. Invalid suffixes
    let cases_invalid_suffixes = vec!["10PB", "10XB", "10M", "10G", "10", "MB", ""];
    for val in cases_invalid_suffixes {
        let errs = TestSize {
            size: val.to_string(),
        }
        .check()
        .unwrap_err();
        assert_eq!(errs.errors()[0].code(), "invalid_size_format");
    }

    // 5. Case insensitivity and Unicode kelvin sign check
    // Rust's `to_uppercase()` preserves the Kelvin sign (U+212A) intact,
    // so it does not match the ASCII 'K' in 'KB'.
    // Therefore, the Kelvin sign input is correctly rejected (no false pass).
    let kelvin_input = "10\u{212A}B"; // 10KB
    let res = TestSize {
        size: kelvin_input.to_string(),
    }
    .check();
    println!("Kelvin sign size test result: {:?}", res);
    let err = res.expect_err("Expected Kelvin sign input to be rejected");
    assert_eq!(err.errors()[0].code(), "invalid_size_format");
}

#[test]
fn test_stress_validation() {
    struct StressModel {
        semver: String,
        host: String,
        path: String,
        size: String,
    }
    impl Validate for StressModel {
        fn validate(&self, v: &mut Validator) {
            v.check_semver("semver", &self.semver);
            v.check_ip_or_domain("host", &self.host);
            v.check_path("path", &self.path, None);
            v.check_size_format("size", &self.size);
        }
    }

    // A helper to run checks and ignore the result, only asserting that it does not panic.
    let check_no_panic = |semver: &str, host: &str, path: &str, size: &str| {
        let model = StressModel {
            semver: semver.to_string(),
            host: host.to_string(),
            path: path.to_string(),
            size: size.to_string(),
        };
        let _ = model.check();
    };

    // 1. Extreme inputs (very long strings)
    let huge_str = "a".repeat(10_000);
    let huge_digits = "9".repeat(10_000);
    let lots_of_dots = ".".repeat(5000);

    check_no_panic(&huge_str, &huge_str, &huge_str, &huge_str);
    check_no_panic(&huge_digits, &huge_digits, &huge_digits, &huge_digits);
    check_no_panic(&lots_of_dots, &lots_of_dots, &lots_of_dots, &lots_of_dots);

    // 2. Deep path structures
    let deep_path = "a/".repeat(2000);
    let deep_traversal = "a/../".repeat(2000);
    check_no_panic("1.0.0", "localhost", &deep_path, "1GB");
    check_no_panic("1.0.0", "localhost", &deep_traversal, "1GB");

    // 3. Fuzzing with diverse Unicode and special character sequences
    let test_chars = [
        '\0', '\n', '\r', '\t', ' ', '.', '-', '/', '\\', '_', ':', 'K', 'ß', 'ä', '１', '０',
    ];
    for len in [1, 5, 10, 50, 100] {
        for char1 in &test_chars {
            for char2 in &test_chars {
                let s = format!(
                    "{}{}",
                    char1.to_string().repeat(len),
                    char2.to_string().repeat(len)
                );
                check_no_panic(&s, &s, &s, &s);
            }
        }
    }
}

#[test]
fn test_additional_path_and_host_adversarial() {
    struct TestModel {
        path: String,
        host: String,
        size: String,
        must_be_absolute: Option<bool>,
    }
    impl Validate for TestModel {
        fn validate(&self, v: &mut Validator) {
            v.check_path("path", &self.path, self.must_be_absolute);
            v.check_ip_or_domain("host", &self.host);
            v.check_size_format("size", &self.size);
        }
    }

    // 1. Path Traversal Edge Cases
    let traversals = vec![
        "foo//..//bar",
        "foo///..///bar",
        "foo/./../bar",
        "foo/../..",
        "../../foo",
        "\\..\\foo",
        "/../foo",
        "a/b/c/../../../../d",
    ];
    for t in traversals {
        let errs = TestModel {
            path: t.to_string(),
            host: "localhost".to_string(),
            size: "1GB".to_string(),
            must_be_absolute: None,
        }
        .check()
        .unwrap_err();
        assert!(
            errs.errors()
                .iter()
                .any(|e| e.loc.to_string() == "path" && e.code() == "invalid_path"),
            "Expected failure for path: {}",
            t
        );
    }

    // Safe path edge cases that should pass
    let safe_paths = vec![
        ".",
        "...",
        "....",
        "foo/.../bar",
        "foo/bar/.",
        "foo/bar/./baz",
    ];
    for p in safe_paths {
        let res = TestModel {
            path: p.to_string(),
            host: "localhost".to_string(),
            size: "1GB".to_string(),
            must_be_absolute: None,
        }
        .check();
        assert!(
            res.is_ok(),
            "Expected safe path to pass: {}, got error: {:?}",
            p,
            res
        );
    }

    // 2. IP / Domain Hostname Edge Cases
    let invalid_hosts = vec![
        "[::1]",
        "[2001:db8::1]",
        "127.0.0.1:8080",
        "example.com:80",
        ".example.com",
        "example..com",
        "exKample.com", // Kelvin sign
        "example_domain.com",
        "example!domain.com",
        "example#domain.com",
    ];
    for h in invalid_hosts {
        let errs = TestModel {
            path: "safe/path".to_string(),
            host: h.to_string(),
            size: "1GB".to_string(),
            must_be_absolute: None,
        }
        .check()
        .unwrap_err();
        assert!(
            errs.errors()
                .iter()
                .any(|e| e.loc.to_string() == "host" && e.code() == "invalid_ip_or_domain"),
            "Expected failure for host: {}",
            h
        );
    }

    // Valid IP / Domain Hostname Edge Cases
    let valid_hosts = vec![
        "0.0.0.0",
        "255.255.255.255",
        "::",
        "2001:db8::1",
        "example.com.",
        "a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z.com",
    ];
    for h in valid_hosts {
        let res = TestModel {
            path: "safe/path".to_string(),
            host: h.to_string(),
            size: "1GB".to_string(),
            must_be_absolute: None,
        }
        .check();
        assert!(
            res.is_ok(),
            "Expected valid host to pass: {}, got error: {:?}",
            h,
            res
        );
    }

    // 3. Size Format Edge Cases
    let invalid_sizes = vec![
        "10 GB",
        "10\u{212A}B",
        "18446744073709551616B",
        "9999999999999999999999999999999999999999TB",
    ];
    for s in invalid_sizes {
        let errs = TestModel {
            path: "safe/path".to_string(),
            host: "localhost".to_string(),
            size: s.to_string(),
            must_be_absolute: None,
        }
        .check()
        .unwrap_err();
        assert!(
            errs.errors()
                .iter()
                .any(|e| e.loc.to_string() == "size" && e.code() == "invalid_size_format"),
            "Expected failure for size: {}",
            s
        );
    }

    // Valid Size Format Edge Cases
    let valid_sizes = vec!["00000000000000000010GB", "10gb", "10tb"];
    for s in valid_sizes {
        let res = TestModel {
            path: "safe/path".to_string(),
            host: "localhost".to_string(),
            size: s.to_string(),
            must_be_absolute: None,
        }
        .check();
        assert!(
            res.is_ok(),
            "Expected valid size to pass: {}, got error: {:?}",
            s,
            res
        );
    }
}

#[test]
fn test_more_extreme_adversarial() {
    struct TestModel {
        path: String,
        host: String,
        size: String,
        version: String,
        must_be_absolute: Option<bool>,
    }
    impl Validate for TestModel {
        fn validate(&self, v: &mut Validator) {
            v.check_path("path", &self.path, self.must_be_absolute);
            v.check_ip_or_domain("host", &self.host);
            v.check_size_format("size", &self.size);
            v.check_semver("version", &self.version);
        }
    }

    let check_cases =
        |path: &str, host: &str, size: &str, version: &str, must_be_absolute: Option<bool>| {
            let m = TestModel {
                path: path.to_string(),
                host: host.to_string(),
                size: size.to_string(),
                version: version.to_string(),
                must_be_absolute,
            };
            let _ = m.check();
        };

    // Verify no panic on control characters, special Unicode, and weird character sequences
    let weird_strings = vec![
        "\n\r\t",
        "\x00\x01\x02",
        " ",
        "   ",
        "C:\\foo\\bar\\..\\baz",
        "\\\\?\\C:\\foo",
        "\\\\.\\PhysicalDrive0",
        "127.0.0.1%lo0",
        "xn--hx0a611a.com",
        "Example.COM",
        "0B",
        "00000B",
        "0.0.0",
        "4294967295.4294967295.4294967295",
        "18446744073709551615B",
    ];

    for s1 in &weird_strings {
        for s2 in &weird_strings {
            check_cases(s1, s2, "1GB", "1.0.0", None);
            check_cases("safe/path", s1, "1GB", "1.0.0", None);
            check_cases("safe/path", "localhost", s1, "1.0.0", None);
            check_cases("safe/path", "localhost", "1GB", s1, None);
        }
    }

    // Explicit checks for Punycode domains and uppercase hosts
    assert!(TestModel {
        path: "safe/path".into(),
        host: "xn--hx0a611a.com".into(),
        size: "1GB".into(),
        version: "1.0.0".into(),
        must_be_absolute: None,
    }
    .check()
    .is_ok());

    assert!(TestModel {
        path: "safe/path".into(),
        host: "EXAMPLE.COM".into(),
        size: "1GB".into(),
        version: "1.0.0".into(),
        must_be_absolute: None,
    }
    .check()
    .is_ok());

    // Explicit checks for absolute path logic
    let unix_abs = "/absolute/path";

    let m = TestModel {
        path: unix_abs.to_string(),
        host: "localhost".to_string(),
        size: "1GB".to_string(),
        version: "1.0.0".to_string(),
        must_be_absolute: Some(true),
    };
    assert!(m.check().is_ok());

    let m_relative = TestModel {
        path: "relative/path".to_string(),
        host: "localhost".to_string(),
        size: "1GB".to_string(),
        version: "1.0.0".to_string(),
        must_be_absolute: Some(true),
    };
    let err = m_relative.check().unwrap_err();
    assert_eq!(err.errors()[0].code(), "invalid_path");
    assert!(err.errors()[0].msg.contains("path must be absolute"));
}
