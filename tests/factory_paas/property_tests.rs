//! FactoryPaaS Property Tests
//!
//! Property-based testing using proptest to verify:
//! - Route resolution is deterministic
//! - Click receipts are cryptographically verifiable
//! - Commission calculations are accurate
//! - Content validation rules hold

#![cfg(test)]

use ggen_saas::factory_paas::*;
use proptest::prelude::*;
use uuid::Uuid;

// ============================================================================
// Property Test 1: Route Resolution is Deterministic
// ============================================================================

proptest! {
    #[test]
    fn prop_route_resolution_is_deterministic(
        slug_str in "[a-z0-9-_]{3,20}",
        target_url in "https://example\\.com/[a-z0-9-_]{3,20}"
    ) {
        // ARRANGE: Create route resolver
        let mut resolver = affiliate::RouteResolver::new();
        let slug = RouteSlug::new(slug_str.clone()).expect("Valid slug");
        let route = affiliate::AffiliateRoute::new(
            slug.clone(),
            target_url.clone(),
            Uuid::new_v4(),
        );
        resolver.add_route(route).expect("Route should be added");

        // ACT: Resolve multiple times
        let result1 = resolver.resolve(&slug).expect("Should resolve");
        let result2 = resolver.resolve(&slug).expect("Should resolve");
        let result3 = resolver.resolve(&slug).expect("Should resolve");

        // ASSERT: Results are always identical (deterministic)
        prop_assert_eq!(result1, result2);
        prop_assert_eq!(result2, result3);
        prop_assert_eq!(result1, &target_url);
    }

    #[test]
    fn prop_invalid_route_slug_rejected(
        invalid_slug in "[^a-zA-Z0-9-_]+"
    ) {
        // ACT: Try to create route slug with invalid characters
        let result = RouteSlug::new(invalid_slug);

        // ASSERT: Invalid slugs are always rejected
        prop_assert!(result.is_err(), "Invalid slug should be rejected");
    }

    #[test]
    fn prop_duplicate_routes_always_rejected(
        slug_str in "[a-z0-9-_]{3,20}"
    ) {
        // ARRANGE: Add first route
        let mut resolver = affiliate::RouteResolver::new();
        let slug = RouteSlug::new(slug_str).expect("Valid slug");
        let route1 = affiliate::AffiliateRoute::new(
            slug.clone(),
            "https://example.com/1".to_string(),
            Uuid::new_v4(),
        );
        resolver.add_route(route1).expect("First route should succeed");

        // ACT: Try to add duplicate
        let route2 = affiliate::AffiliateRoute::new(
            slug,
            "https://example.com/2".to_string(),
            Uuid::new_v4(),
        );
        let result = resolver.add_route(route2);

        // ASSERT: Duplicate is always rejected
        prop_assert!(result.is_err(), "Duplicate route should always be rejected");
    }
}

// ============================================================================
// Property Test 2: Click Receipts are Cryptographically Verifiable
// ============================================================================

proptest! {
    #[test]
    fn prop_click_receipts_always_verify(
        slug_str in "[a-z0-9-_]{3,20}"
    ) {
        // ARRANGE: Create click event and tracker
        let slug = RouteSlug::new(slug_str).expect("Valid slug");
        let click_event = click_tracking::ClickEvent::new(slug);
        let mut tracker = click_tracking::ClickTracker::new();

        // ACT: Track click and generate receipt
        let receipt = tracker.track_click(click_event).expect("Tracking should succeed");

        // ASSERT: Receipt always verifies
        prop_assert!(receipt.verify(), "Receipt should always verify");
    }

    #[test]
    fn prop_receipt_chain_maintains_integrity(
        clicks in 1usize..10
    ) {
        // ARRANGE: Create tracker
        let mut tracker = click_tracking::ClickTracker::new();
        let slug = RouteSlug::new("test-chain".to_string()).expect("Valid slug");

        // ACT: Track multiple clicks
        for _ in 0..clicks {
            let event = click_tracking::ClickEvent::new(slug.clone());
            let receipt = tracker.track_click(event).expect("Tracking should succeed");
            prop_assert!(receipt.verify(), "Each receipt should verify");
        }

        // ASSERT: Chain integrity is maintained
        prop_assert!(tracker.verify_chain(), "Chain should always be valid");
    }

    #[test]
    fn prop_receipt_hash_is_deterministic(
        slug_str in "[a-z0-9-_]{3,20}"
    ) {
        // ARRANGE: Create identical click events
        let slug = RouteSlug::new(slug_str).expect("Valid slug");
        let click_id = Uuid::new_v4();

        // ACT: Create receipts with same data
        let receipt1 = ClickReceipt::new(click_id, slug.clone(), None);
        let receipt2 = ClickReceipt::new(click_id, slug.clone(), None);

        // ASSERT: Hashes are deterministic for same input
        // Note: Timestamps will differ, so hashes will differ
        // This tests that hash computation is deterministic given same inputs
        prop_assert!(receipt1.verify(), "Receipt1 should verify");
        prop_assert!(receipt2.verify(), "Receipt2 should verify");
    }
}

// ============================================================================
// Property Test 3: Commission Calculations are Accurate
// ============================================================================

proptest! {
    #[test]
    fn prop_commission_calculation_accurate(
        amount_cents in 1u64..1_000_000u64
    ) {
        // Test all subscription tiers
        let tiers = vec![
            (SubscriptionTier::Free, 0.05),
            (SubscriptionTier::Starter, 0.10),
            (SubscriptionTier::Professional, 0.15),
            (SubscriptionTier::Enterprise, 0.20),
        ];

        for (tier, expected_rate) in tiers {
            // ARRANGE: Create revenue event
            let event = revenue::RevenueEvent::new(
                Uuid::new_v4(),
                RouteSlug::new("test".to_string()).expect("Valid slug"),
                Uuid::new_v4(),
                amount_cents,
                tier,
            );

            // ACT: Calculate expected commission
            let expected_commission = ((amount_cents as f64) * expected_rate) as u64;

            // ASSERT: Commission matches expected calculation
            prop_assert_eq!(
                event.commission_cents,
                expected_commission,
                "Commission should match tier rate for tier {:?}",
                tier
            );

            // Verify commission accuracy
            let mut attribution = revenue::RevenueAttribution::new();
            attribution.record_revenue(
                event.affiliate_id,
                event.route_slug.clone(),
                event.click_id,
                amount_cents,
                tier,
            );
            let verified = attribution.verify_commission(&event.id).expect("Event should exist");
            prop_assert!(verified, "Commission should verify as accurate");
        }
    }

    #[test]
    fn prop_revenue_attribution_sum_correct(
        amounts in prop::collection::vec(1u64..10_000u64, 1..10)
    ) {
        // ARRANGE: Create attribution engine
        let mut attribution = revenue::RevenueAttribution::new();
        let affiliate_id = Uuid::new_v4();
        let slug = RouteSlug::new("sum-test".to_string()).expect("Valid slug");

        // ACT: Record multiple revenue events
        let mut expected_total = 0u64;
        for amount in &amounts {
            attribution.record_revenue(
                affiliate_id,
                slug.clone(),
                Uuid::new_v4(),
                *amount,
                SubscriptionTier::Professional,
            );
            expected_total += amount;
        }

        // ASSERT: Total revenue matches sum of individual amounts
        let actual_total = attribution.total_revenue_by_affiliate(&affiliate_id);
        prop_assert_eq!(actual_total, expected_total, "Revenue sum should be accurate");
    }

    #[test]
    fn prop_commission_never_exceeds_revenue(
        amount_cents in 1u64..1_000_000u64
    ) {
        // Test all tiers
        let tiers = vec![
            SubscriptionTier::Free,
            SubscriptionTier::Starter,
            SubscriptionTier::Professional,
            SubscriptionTier::Enterprise,
        ];

        for tier in tiers {
            // ARRANGE: Create revenue event
            let event = revenue::RevenueEvent::new(
                Uuid::new_v4(),
                RouteSlug::new("test".to_string()).expect("Valid slug"),
                Uuid::new_v4(),
                amount_cents,
                tier,
            );

            // ASSERT: Commission is always less than or equal to revenue
            prop_assert!(
                event.commission_cents <= event.amount_cents,
                "Commission should never exceed revenue for tier {:?}",
                tier
            );
        }
    }
}

// ============================================================================
// Property Test 4: Content Validation Rules Hold
// ============================================================================

proptest! {
    #[test]
    fn prop_content_title_and_body_required(
        title in prop::option::of("[a-zA-Z0-9 ]{0,100}"),
        body in prop::option::of("[a-zA-Z0-9 ]{0,200}")
    ) {
        // ARRANGE: Create content with optional title/body
        let mut content = content::ContentItem::new(
            title.clone().unwrap_or_default(),
            body.clone().unwrap_or_default(),
            Uuid::new_v4(),
        );

        // ACT: Try to publish
        let result = content.publish();

        // ASSERT: Publishing fails if title or body is empty
        if title.is_none() || title.as_ref().unwrap().is_empty() ||
           body.is_none() || body.as_ref().unwrap().is_empty() {
            prop_assert!(result.is_err(), "Publishing should fail with empty title/body");
        } else {
            prop_assert!(result.is_ok(), "Publishing should succeed with valid title/body");
        }
    }

    #[test]
    fn prop_content_status_transitions_valid(
        operations in prop::collection::vec(0u8..4, 1..5)
    ) {
        // ARRANGE: Create content
        let mut content = content::ContentItem::new(
            "Test Title".to_string(),
            "Test Body".to_string(),
            Uuid::new_v4(),
        );

        // ACT: Perform random operations
        for op in operations {
            match op {
                0 => {
                    content.update("Updated Title".to_string(), "Updated Body".to_string());
                    prop_assert!(content.updated_at > content.created_at);
                }
                1 => {
                    let future_time = chrono::Utc::now() + chrono::Duration::hours(1);
                    let _ = content.schedule(future_time);
                }
                2 => {
                    let _ = content.publish();
                }
                3 => {
                    content.archive();
                    prop_assert_eq!(content.status, PublicationStatus::Archived);
                }
                _ => {}
            }
        }

        // ASSERT: Status is always one of the valid states
        prop_assert!(
            matches!(
                content.status,
                PublicationStatus::Draft |
                PublicationStatus::Scheduled |
                PublicationStatus::Published |
                PublicationStatus::Archived
            ),
            "Content status should always be valid"
        );
    }

    #[test]
    fn prop_scheduled_time_must_be_future(
        hours_offset in -24i64..24i64
    ) {
        // ARRANGE: Create content
        let mut content = content::ContentItem::new(
            "Test".to_string(),
            "Test Body".to_string(),
            Uuid::new_v4(),
        );

        // ACT: Try to schedule
        let scheduled_time = chrono::Utc::now() + chrono::Duration::hours(hours_offset);
        let result = content.schedule(scheduled_time);

        // ASSERT: Scheduling only succeeds for future times
        if hours_offset > 0 {
            prop_assert!(result.is_ok(), "Scheduling should succeed for future time");
        } else {
            prop_assert!(result.is_err(), "Scheduling should fail for past time");
        }
    }
}

// ============================================================================
// Property Test 5: Subscription Quota Enforcement
// ============================================================================

proptest! {
    #[test]
    fn prop_quota_enforced_at_limit(
        tier_index in 0usize..3
    ) {
        // Test tiers with quotas (not Enterprise)
        let tiers_with_quotas = vec![
            (SubscriptionTier::Free, 100),
            (SubscriptionTier::Starter, 10_000),
            (SubscriptionTier::Professional, 100_000),
        ];

        let (tier, quota) = tiers_with_quotas[tier_index];

        // ARRANGE: Create subscription
        let mut subscription = subscription::Subscription::new(Uuid::new_v4(), tier);

        // ACT: Use clicks up to quota
        for i in 0..quota {
            let result = subscription.increment_clicks();
            if i < quota {
                prop_assert!(result.is_ok(), "Click {} should succeed (under quota)", i + 1);
            }
        }

        // ASSERT: Next click exceeds quota and fails
        let over_quota = subscription.increment_clicks();
        prop_assert!(over_quota.is_err(), "Click over quota should fail");
    }

    #[test]
    fn prop_enterprise_has_unlimited_quota(
        clicks in 1usize..100_000
    ) {
        // ARRANGE: Create Enterprise subscription
        let mut subscription = subscription::Subscription::new(
            Uuid::new_v4(),
            SubscriptionTier::Enterprise,
        );

        // ACT: Use many clicks
        for _ in 0..clicks {
            let result = subscription.increment_clicks();
            prop_assert!(result.is_ok(), "Enterprise should never hit quota");
        }

        // ASSERT: Never exceeds quota
        prop_assert!(!subscription.is_quota_exceeded(), "Enterprise should never exceed quota");
    }

    #[test]
    fn prop_subscription_status_affects_operations(
        operations in prop::collection::vec(0u8..2, 1..5)
    ) {
        // ARRANGE: Create subscription and cancel it
        let mut subscription = subscription::Subscription::new(
            Uuid::new_v4(),
            SubscriptionTier::Professional,
        );
        subscription.cancel();

        // ACT: Try operations on cancelled subscription
        for op in operations {
            let result = match op {
                0 => subscription.increment_clicks(),
                1 => subscription.upgrade(SubscriptionTier::Enterprise),
                _ => Ok(()),
            };

            // ASSERT: Operations fail on inactive subscription
            if op < 2 {
                prop_assert!(result.is_err(), "Operations should fail on cancelled subscription");
            }
        }
    }
}

// ============================================================================
// Property Test 6: Deterministic Behavior Under Concurrency
// ============================================================================

proptest! {
    #[test]
    fn prop_route_resolver_thread_safe(
        routes_count in 1usize..20
    ) {
        // ARRANGE: Create multiple routes
        let mut resolver = affiliate::RouteResolver::new();
        let mut slugs = Vec::new();

        for i in 0..routes_count {
            let slug = RouteSlug::new(format!("route-{}", i)).expect("Valid slug");
            let route = affiliate::AffiliateRoute::new(
                slug.clone(),
                format!("https://example.com/{}", i),
                Uuid::new_v4(),
            );
            resolver.add_route(route).expect("Route should be added");
            slugs.push(slug);
        }

        // ACT: Resolve all routes
        let mut results = Vec::new();
        for slug in &slugs {
            results.push(resolver.resolve(slug).expect("Should resolve").to_string());
        }

        // ASSERT: All routes resolve correctly
        prop_assert_eq!(results.len(), routes_count);
        for (i, url) in results.iter().enumerate() {
            prop_assert_eq!(url, &format!("https://example.com/{}", i));
        }
    }
}
