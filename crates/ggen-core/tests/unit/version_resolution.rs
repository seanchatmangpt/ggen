//! Unit tests for version resolution and comparison

use semver::Version;

#[test]
fn test_version_parsing() {
    let v = Version::parse("1.2.3").unwrap();
    assert_eq!(v.major, 1);
    assert_eq!(v.minor, 2);
    assert_eq!(v.patch, 3);
}

#[test]
fn test_version_comparison() {
    let v1 = Version::parse("1.0.0").unwrap();
    let v2 = Version::parse("1.0.1").unwrap();
    let v3 = Version::parse("1.1.0").unwrap();
    let v4 = Version::parse("2.0.0").unwrap();

    assert!(v1 < v2);
    assert!(v2 < v3);
    assert!(v3 < v4);
    assert!(v1 < v4);
}

#[test]
fn test_version_equality() {
    let v1 = Version::parse("1.2.3").unwrap();
    let v2 = Version::parse("1.2.3").unwrap();

    assert_eq!(v1, v2);
}

#[test]
fn test_version_prerelease() {
    let stable = Version::parse("1.0.0").unwrap();
    let alpha = Version::parse("1.0.0-alpha").unwrap();
    let beta = Version::parse("1.0.0-beta").unwrap();
    let rc = Version::parse("1.0.0-rc.1").unwrap();

    assert!(stable > alpha);
    assert!(stable > beta);
    assert!(stable > rc);
    assert!(alpha < beta); // Lexicographic ordering
    assert!(beta < rc);

    assert!(!stable.pre.is_empty() == false);
    assert!(!alpha.pre.is_empty());
    assert!(!beta.pre.is_empty());
    assert!(!rc.pre.is_empty());
}

#[test]
fn test_version_build_metadata() {
    let v1 = Version::parse("1.0.0+build.123").unwrap();
    let v2 = Version::parse("1.0.0+build.456").unwrap();

    // Build metadata should not affect comparison
    assert_eq!(v1, v2);
    assert!(!v1.build.is_empty());
}

#[test]
fn test_version_invalid() {
    assert!(Version::parse("invalid").is_err());
    assert!(Version::parse("1").is_err());
    assert!(Version::parse("1.2").is_err());
    assert!(Version::parse("1.2.3.4").is_err());
    assert!(Version::parse("v1.2.3").is_err());
}

#[test]
fn test_version_leading_zeros() {
    // Leading zeros are not allowed in semver
    assert!(Version::parse("01.2.3").is_err());
    assert!(Version::parse("1.02.3").is_err());
    assert!(Version::parse("1.2.03").is_err());
}

#[test]
fn test_version_major_bumps() {
    let v1 = Version::parse("1.9.9").unwrap();
    let v2 = Version::parse("2.0.0").unwrap();

    assert!(v2 > v1);
    assert_eq!(v2.major, 2);
    assert_eq!(v2.minor, 0);
    assert_eq!(v2.patch, 0);
}

#[test]
fn test_version_minor_bumps() {
    let v1 = Version::parse("1.9.9").unwrap();
    let v2 = Version::parse("1.10.0").unwrap();

    assert!(v2 > v1);
    assert_eq!(v2.minor, 10);
}

#[test]
fn test_version_patch_bumps() {
    let v1 = Version::parse("1.0.9").unwrap();
    let v2 = Version::parse("1.0.10").unwrap();

    assert!(v2 > v1);
    assert_eq!(v2.patch, 10);
}

#[test]
fn test_version_zero_versions() {
    let v0_0_0 = Version::parse("0.0.0").unwrap();
    let v0_0_1 = Version::parse("0.0.1").unwrap();
    let v0_1_0 = Version::parse("0.1.0").unwrap();
    let v1_0_0 = Version::parse("1.0.0").unwrap();

    assert!(v0_0_0 < v0_0_1);
    assert!(v0_0_1 < v0_1_0);
    assert!(v0_1_0 < v1_0_0);
}

#[test]
fn test_version_sorting() {
    let mut versions = vec![
        Version::parse("2.0.0").unwrap(),
        Version::parse("1.0.0").unwrap(),
        Version::parse("1.1.0").unwrap(),
        Version::parse("1.0.1").unwrap(),
    ];

    versions.sort();

    assert_eq!(versions[0], Version::parse("1.0.0").unwrap());
    assert_eq!(versions[1], Version::parse("1.0.1").unwrap());
    assert_eq!(versions[2], Version::parse("1.1.0").unwrap());
    assert_eq!(versions[3], Version::parse("2.0.0").unwrap());
}

#[test]
fn test_version_prerelease_ordering() {
    let mut versions = vec![
        Version::parse("1.0.0").unwrap(),
        Version::parse("1.0.0-rc.2").unwrap(),
        Version::parse("1.0.0-beta").unwrap(),
        Version::parse("1.0.0-alpha").unwrap(),
        Version::parse("1.0.0-rc.1").unwrap(),
    ];

    versions.sort();

    // alpha < beta < rc.1 < rc.2 < 1.0.0
    assert_eq!(versions[0].to_string(), "1.0.0-alpha");
    assert_eq!(versions[1].to_string(), "1.0.0-beta");
    assert_eq!(versions[2].to_string(), "1.0.0-rc.1");
    assert_eq!(versions[3].to_string(), "1.0.0-rc.2");
    assert_eq!(versions[4].to_string(), "1.0.0");
}
