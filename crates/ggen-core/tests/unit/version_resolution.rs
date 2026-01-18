//! Unit tests for version resolution and comparison

use chicago_tdd_tools::prelude::*;
use semver::Version;

test!(test_version_parsing, {
    // Arrange & Act
    let v = Version::parse("1.2.3").unwrap();

    // Assert
    assert_eq!(v.major, 1);
    assert_eq!(v.minor, 2);
    assert_eq!(v.patch, 3);
});

test!(test_version_comparison, {
    // Arrange
    let v1 = Version::parse("1.0.0").unwrap();
    let v2 = Version::parse("1.0.1").unwrap();
    let v3 = Version::parse("1.1.0").unwrap();
    let v4 = Version::parse("2.0.0").unwrap();

    // Assert
    assert!(v1 < v2);
    assert!(v2 < v3);
    assert!(v3 < v4);
    assert!(v1 < v4);
});

test!(test_version_equality, {
    // Arrange
    let v1 = Version::parse("1.2.3").unwrap();
    let v2 = Version::parse("1.2.3").unwrap();

    // Assert
    assert_eq!(v1, v2);
});

test!(test_version_prerelease, {
    // Arrange
    let stable = Version::parse("1.0.0").unwrap();
    let alpha = Version::parse("1.0.0-alpha").unwrap();
    let beta = Version::parse("1.0.0-beta").unwrap();
    let rc = Version::parse("1.0.0-rc.1").unwrap();

    // Assert
    assert!(stable > alpha);
    assert!(stable > beta);
    assert!(stable > rc);
    assert!(alpha < beta);
    assert!(beta < rc);
    assert!(!stable.pre.is_empty() == false);
    assert!(!alpha.pre.is_empty());
    assert!(!beta.pre.is_empty());
    assert!(!rc.pre.is_empty());
});

test!(test_version_build_metadata, {
    // Arrange
    let v1 = Version::parse("1.0.0+build.123").unwrap();
    let v2 = Version::parse("1.0.0+build.456").unwrap();

    // Assert
    assert_eq!(v1, v2);
    assert!(!v1.build.is_empty());
});

test!(test_version_invalid, {
    // Arrange & Act & Assert
    assert_err!(&Version::parse("invalid"));
    assert_err!(&Version::parse("1"));
    assert_err!(&Version::parse("1.2"));
    assert_err!(&Version::parse("1.2.3.4"));
    assert_err!(&Version::parse("v1.2.3"));
});

test!(test_version_leading_zeros, {
    // Arrange & Act & Assert
    assert_err!(&Version::parse("01.2.3"));
    assert_err!(&Version::parse("1.02.3"));
    assert_err!(&Version::parse("1.2.03"));
});

test!(test_version_major_bumps, {
    // Arrange
    let v1 = Version::parse("1.9.9").unwrap();
    let v2 = Version::parse("2.0.0").unwrap();

    // Assert
    assert!(v2 > v1);
    assert_eq!(v2.major, 2);
    assert_eq!(v2.minor, 0);
    assert_eq!(v2.patch, 0);
});

test!(test_version_minor_bumps, {
    // Arrange
    let v1 = Version::parse("1.9.9").unwrap();
    let v2 = Version::parse("1.10.0").unwrap();

    // Assert
    assert!(v2 > v1);
    assert_eq!(v2.minor, 10);
});

test!(test_version_patch_bumps, {
    // Arrange
    let v1 = Version::parse("1.0.9").unwrap();
    let v2 = Version::parse("1.0.10").unwrap();

    // Assert
    assert!(v2 > v1);
    assert_eq!(v2.patch, 10);
});

test!(test_version_zero_versions, {
    // Arrange
    let v0_0_0 = Version::parse("0.0.0").unwrap();
    let v0_0_1 = Version::parse("0.0.1").unwrap();
    let v0_1_0 = Version::parse("0.1.0").unwrap();
    let v1_0_0 = Version::parse("1.0.0").unwrap();

    // Assert
    assert!(v0_0_0 < v0_0_1);
    assert!(v0_0_1 < v0_1_0);
    assert!(v0_1_0 < v1_0_0);
});

test!(test_version_sorting, {
    // Arrange
    let mut versions = vec![
        Version::parse("2.0.0").unwrap(),
        Version::parse("1.0.0").unwrap(),
        Version::parse("1.1.0").unwrap(),
        Version::parse("1.0.1").unwrap(),
    ];

    // Act
    versions.sort();

    // Assert
    assert_eq!(versions[0], Version::parse("1.0.0").unwrap());
    assert_eq!(versions[1], Version::parse("1.0.1").unwrap());
    assert_eq!(versions[2], Version::parse("1.1.0").unwrap());
    assert_eq!(versions[3], Version::parse("2.0.0").unwrap());
});

test!(test_version_prerelease_ordering, {
    // Arrange
    let mut versions = vec![
        Version::parse("1.0.0").unwrap(),
        Version::parse("1.0.0-rc.2").unwrap(),
        Version::parse("1.0.0-beta").unwrap(),
        Version::parse("1.0.0-alpha").unwrap(),
        Version::parse("1.0.0-rc.1").unwrap(),
    ];

    // Act
    versions.sort();

    // Assert
    assert_eq!(versions[0].to_string(), "1.0.0-alpha");
    assert_eq!(versions[1].to_string(), "1.0.0-beta");
    assert_eq!(versions[2].to_string(), "1.0.0-rc.1");
    assert_eq!(versions[3].to_string(), "1.0.0-rc.2");
    assert_eq!(versions[4].to_string(), "1.0.0");
});
