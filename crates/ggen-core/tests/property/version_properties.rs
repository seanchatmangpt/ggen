//! Property-based tests for version handling

#[cfg(feature = "proptest")]
mod tests {
    use proptest::prelude::*;
    use semver::Version;

    proptest! {
        #[test]
        fn version_comparison_is_transitive(
            major1 in 0..10u64,
            minor1 in 0..20u64,
            patch1 in 0..30u64,
            major2 in 0..10u64,
            minor2 in 0..20u64,
            patch2 in 0..30u64,
            major3 in 0..10u64,
            minor3 in 0..20u64,
            patch3 in 0..30u64,
        ) {
            let v1 = Version::new(major1, minor1, patch1);
            let v2 = Version::new(major2, minor2, patch2);
            let v3 = Version::new(major3, minor3, patch3);

            // Property: if v1 < v2 and v2 < v3, then v1 < v3
            if v1 < v2 && v2 < v3 {
                prop_assert!(v1 < v3, "Transitivity failed: {:?} < {:?} < {:?}", v1, v2, v3);
            }
        }

        #[test]
        fn version_comparison_is_antisymmetric(
            major1 in 0..10u64,
            minor1 in 0..20u64,
            patch1 in 0..30u64,
            major2 in 0..10u64,
            minor2 in 0..20u64,
            patch2 in 0..30u64,
        ) {
            let v1 = Version::new(major1, minor1, patch1);
            let v2 = Version::new(major2, minor2, patch2);

            // Property: if v1 < v2, then not v2 < v1
            if v1 < v2 {
                prop_assert!(!(v2 < v1), "Antisymmetry failed: {:?} and {:?}", v1, v2);
            }
        }

        #[test]
        fn version_equality_is_reflexive(
            major in 0..10u64,
            minor in 0..20u64,
            patch in 0..30u64,
        ) {
            let v = Version::new(major, minor, patch);

            // Property: v == v
            prop_assert_eq!(&v, &v);
        }

        #[test]
        fn version_equality_is_symmetric(
            major in 0..10u64,
            minor in 0..20u64,
            patch in 0..30u64,
        ) {
            let v1 = Version::new(major, minor, patch);
            let v2 = Version::new(major, minor, patch);

            // Property: if v1 == v2, then v2 == v1
            prop_assert_eq!(&v1, &v2);
            prop_assert_eq!(&v2, &v1);
        }

        #[test]
        fn version_parsing_roundtrip(
            major in 0..100u64,
            minor in 0..100u64,
            patch in 0..100u64,
        ) {
            let v = Version::new(major, minor, patch);
            let s = v.to_string();

            // Property: parsing and stringifying should roundtrip
            let parsed = Version::parse(&s).unwrap();

            prop_assert_eq!(parsed.major, major);
            prop_assert_eq!(parsed.minor, minor);
            prop_assert_eq!(parsed.patch, patch);
        }

        #[test]
        fn version_major_dominates(
            major1 in 0..10u64,
            major2 in 0..10u64,
            minor in 0..20u64,
            patch in 0..30u64,
        ) {
            if major1 != major2 {
                let v1 = Version::new(major1, minor, patch);
                let v2 = Version::new(major2, minor, patch);

                // Property: major version difference determines comparison
                if major1 < major2 {
                    prop_assert!(v1 < v2);
                } else {
                    prop_assert!(v1 > v2);
                }
            }
        }

        #[test]
        fn version_minor_when_major_equal(
            major in 0..10u64,
            minor1 in 0..20u64,
            minor2 in 0..20u64,
            patch in 0..30u64,
        ) {
            if minor1 != minor2 {
                let v1 = Version::new(major, minor1, patch);
                let v2 = Version::new(major, minor2, patch);

                // Property: when major is equal, minor determines comparison
                if minor1 < minor2 {
                    prop_assert!(v1 < v2);
                } else {
                    prop_assert!(v1 > v2);
                }
            }
        }

        #[test]
        fn version_patch_when_major_minor_equal(
            major in 0..10u64,
            minor in 0..20u64,
            patch1 in 0..30u64,
            patch2 in 0..30u64,
        ) {
            if patch1 != patch2 {
                let v1 = Version::new(major, minor, patch1);
                let v2 = Version::new(major, minor, patch2);

                // Property: when major and minor are equal, patch determines comparison
                if patch1 < patch2 {
                    prop_assert!(v1 < v2);
                } else {
                    prop_assert!(v1 > v2);
                }
            }
        }

        #[test]
        fn version_prerelease_less_than_stable(
            major in 0..10u64,
            minor in 0..20u64,
            patch in 0..30u64,
        ) {
            let stable = Version::new(major, minor, patch);
            let pre = Version::parse(&format!("{}.{}.{}-alpha", major, minor, patch)).unwrap();

            // Property: prerelease versions are less than stable versions
            prop_assert!(pre < stable);
            prop_assert!(!pre.pre.is_empty());
            prop_assert!(stable.pre.is_empty());
        }

        #[test]
        fn version_increment_increases(
            major in 0..10u64,
            minor in 0..20u64,
            patch in 0..30u64,
        ) {
            let v = Version::new(major, minor, patch);

            // Create incremented versions
            let v_patch = Version::new(major, minor, patch + 1);
            let v_minor = Version::new(major, minor + 1, 0);
            let v_major = Version::new(major + 1, 0, 0);

            // Property: incrementing any part creates a greater version
            prop_assert!(v < v_patch);
            prop_assert!(v < v_minor);
            prop_assert!(v < v_major);

            // Property: major > minor > patch in importance
            prop_assert!(v_major > v_minor);
            prop_assert!(v_major > v_patch);
            prop_assert!(v_minor > v_patch);
        }
    }
}
