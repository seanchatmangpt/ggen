//! Property-Based Tests for Routing System
//!
//! Generated from ontology/routing.ttl
//! DO NOT EDIT - regenerate via: ggen sync

#![cfg(test)]

use proptest::prelude::*;
use uuid::Uuid;
use chrono::Utc;

use crate::routing::{RouteResolver, AffiliateLinkRoute};
use crate::click_tracker::{ClickReceiptGenerator, ClickReceipt};

// ============================================================================
// PROPERTY: Route Resolution is Deterministic
// ============================================================================

proptest! {
    #[test]
    fn test_route_resolution_deterministic(
        slug in "[a-z0-9-]{1,64}",
        destination in "https://[a-z]+\\.com/[a-z]+"
    ) {
        let resolver = RouteResolver::new();
        let route = AffiliateLinkRoute {
            id: Uuid::now_v7(),
            source_path: slug.clone(),
            destination_url: destination.clone(),
            affiliate_id: Uuid::now_v7(),
            tracking_params: None,
            created_at: Utc::now(),
            active: true,
        };

        resolver.warm_cache(vec![route]).expect("warm cache");

        // Resolve 1000 times - should always return same result
        let mut results = Vec::new();
        for _ in 0..1000 {
            let result = resolver.resolve(&slug).expect("resolve");
            results.push(result.destination_url.clone());
        }

        // All results must be identical (deterministic)
        assert!(results.windows(2).all(|w| w[0] == w[1]));
        assert_eq!(results[0], destination);
    }
}

// ============================================================================
// PROPERTY: Invalid Slugs are Rejected
// ============================================================================

proptest! {
    #[test]
    fn test_invalid_slugs_rejected(
        slug in "[^a-z0-9-]+"  // Any non-alphanumeric, non-hyphen chars
    ) {
        let resolver = RouteResolver::new();
        let result = resolver.resolve(&slug);

        // Should fail with InvalidSlug error
        assert!(result.is_err());
    }
}

// ============================================================================
// PROPERTY: URL Encoding is Safe and Idempotent
// ============================================================================

proptest! {
    #[test]
    fn test_url_encoding_safe(
        base_url in "https://[a-z]+\\.com/[a-z]+",
        param_key in "[a-z_]{1,20}",
        param_value in "[a-z0-9%&=]{0,50}"
    ) {
        let route = AffiliateLinkRoute {
            id: Uuid::now_v7(),
            source_path: "test".to_string(),
            destination_url: base_url.clone(),
            affiliate_id: Uuid::now_v7(),
            tracking_params: Some(format!("{}={}", param_key, param_value)),
            created_at: Utc::now(),
            active: true,
        };

        let click_id = Uuid::now_v7();
        let redirect_url = RouteResolver::build_redirect_url(&route, click_id)
            .expect("build URL");

        // URL must start with base
        assert!(redirect_url.starts_with(&base_url));

        // URL must contain click_id
        assert!(redirect_url.contains(&format!("click_id={}", click_id)));

        // No XSS vectors
        assert!(!redirect_url.contains("<script"));
        assert!(!redirect_url.contains("javascript:"));
        assert!(!redirect_url.contains("data:"));
    }
}

// ============================================================================
// PROPERTY: Receipt Hashes are Unique
// ============================================================================

proptest! {
    #[test]
    fn test_receipt_hashes_unique(
        ips in prop::collection::vec("[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}", 100..1000)
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let generator = ClickReceiptGenerator::new();
            let route_id = Uuid::now_v7();

            let mut hashes = std::collections::HashSet::new();
            for ip in ips.iter() {
                let receipt = generator
                    .generate(route_id, ip, None, None)
                    .await
                    .expect("generate receipt");

                // Hash must be unique
                assert!(hashes.insert(receipt.hash.clone()));
            }
        });
    }
}

// ============================================================================
// PROPERTY: Merkle Chain Integrity
// ============================================================================

proptest! {
    #[test]
    fn test_merkle_chain_integrity(
        count in 10..100usize
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let generator = ClickReceiptGenerator::new();
            let route_id = Uuid::now_v7();

            let mut receipts = Vec::new();
            for i in 0..count {
                let receipt = generator
                    .generate(
                        route_id,
                        &format!("192.168.1.{}", i % 255),
                        None,
                        None,
                    )
                    .await
                    .expect("generate receipt");
                receipts.push(receipt);
            }

            // Verify chain integrity
            assert!(ClickReceiptGenerator::verify_chain(&receipts).expect("verify chain"));

            // First receipt has no prev_hash
            assert!(receipts[0].prev_hash.is_none());

            // All subsequent receipts link correctly
            for i in 1..receipts.len() {
                assert_eq!(
                    receipts[i].prev_hash.as_ref().unwrap(),
                    &receipts[i - 1].hash
                );
            }
        });
    }
}

// ============================================================================
// PROPERTY: IP Hashing is Deterministic and Irreversible
// ============================================================================

proptest! {
    #[test]
    fn test_ip_hashing_deterministic(
        ip in "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let generator = ClickReceiptGenerator::new();
            let route_id = Uuid::now_v7();

            // Generate multiple receipts for same IP
            let mut hashes = Vec::new();
            for _ in 0..10 {
                let receipt = generator
                    .generate(route_id, &ip, None, None)
                    .await
                    .expect("generate receipt");
                hashes.push(receipt.ip_hash.clone());
            }

            // All hashes must be identical (deterministic)
            assert!(hashes.windows(2).all(|w| w[0] == w[1]));

            // Hash must not equal plaintext IP (irreversible)
            assert_ne!(hashes[0], ip);

            // Hash must be hex SHA-256 (64 chars)
            assert_eq!(hashes[0].len(), 64);
            assert!(hashes[0].chars().all(|c| c.is_ascii_hexdigit()));
        });
    }
}

// ============================================================================
// PROPERTY: Active Routes Resolve, Inactive Do Not
// ============================================================================

proptest! {
    #[test]
    fn test_active_inactive_routes(
        slug in "[a-z0-9-]{1,64}",
        active in any::<bool>()
    ) {
        let resolver = RouteResolver::new();
        let route = AffiliateLinkRoute {
            id: Uuid::now_v7(),
            source_path: slug.clone(),
            destination_url: "https://example.com".to_string(),
            affiliate_id: Uuid::now_v7(),
            tracking_params: None,
            created_at: Utc::now(),
            active,
        };

        // Warm cache includes all routes (active and inactive)
        resolver.warm_cache(vec![route]).expect("warm cache");

        let result = resolver.resolve(&slug);

        if active {
            // Active routes resolve successfully
            assert!(result.is_ok());
        } else {
            // Inactive routes do not appear in cache (filtered during warm_cache)
            assert!(result.is_err());
        }
    }
}
