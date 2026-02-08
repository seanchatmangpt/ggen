//! FactoryPaaS Integration Tests
//!
//! Chicago TDD Pattern: Arrange/Act/Assert with real collaborators
//! Tests verify end-to-end workflows with observable state changes

#![cfg(test)]

use super::TestContext;
use ggen_saas::factory_paas::*;
use uuid::Uuid;

// ============================================================================
// Test 1: Affiliate Link Routing End-to-End
// ============================================================================

#[tokio::test]
async fn test_affiliate_link_routing_end_to_end() {
    // ARRANGE: Set up route resolver with multiple routes
    let ctx = TestContext::new();
    let slugs = ctx
        .setup_sample_routes()
        .await
        .expect("Failed to setup routes");
    let test_slug = &slugs[0];

    // ACT: Resolve the route
    let resolver = ctx.route_resolver.read().await;
    let target_url = resolver.resolve(test_slug);

    // ASSERT: Route resolves to correct URL
    assert!(target_url.is_ok(), "Route should resolve successfully");
    assert!(
        target_url.unwrap().contains("example.com/affiliate"),
        "Target URL should be correct affiliate link"
    );
}

#[tokio::test]
async fn test_route_resolution_with_inactive_route() {
    // ARRANGE: Create route and deactivate it
    let ctx = TestContext::new();
    let slug = RouteSlug::new("test-route".to_string()).unwrap();
    let mut route = affiliate::AffiliateRoute::new(
        slug.clone(),
        "https://example.com/test".to_string(),
        Uuid::new_v4(),
    );
    route.deactivate();

    let mut resolver = ctx.route_resolver.write().await;
    resolver.add_route(route).expect("Failed to add route");

    // ACT: Try to resolve inactive route
    let result = resolver.resolve(&slug);

    // ASSERT: Inactive route should not resolve
    assert!(result.is_err(), "Inactive route should not resolve");
}

#[tokio::test]
async fn test_duplicate_route_slug_rejected() {
    // ARRANGE: Create route resolver with existing route
    let ctx = TestContext::new();
    let slug = RouteSlug::new("duplicate-test".to_string()).unwrap();
    let route1 = affiliate::AffiliateRoute::new(
        slug.clone(),
        "https://example.com/1".to_string(),
        Uuid::new_v4(),
    );

    let mut resolver = ctx.route_resolver.write().await;
    resolver
        .add_route(route1)
        .expect("First route should succeed");

    // ACT: Try to add duplicate route
    let route2 = affiliate::AffiliateRoute::new(
        slug.clone(),
        "https://example.com/2".to_string(),
        Uuid::new_v4(),
    );
    let result = resolver.add_route(route2);

    // ASSERT: Duplicate should be rejected
    assert!(result.is_err(), "Duplicate route slug should be rejected");
}

// ============================================================================
// Test 2: Click Tracking with Receipt Generation
// ============================================================================

#[tokio::test]
async fn test_click_tracking_generates_valid_receipt() {
    // ARRANGE: Create click event
    let ctx = TestContext::new();
    let slug = RouteSlug::new("test-click".to_string()).unwrap();
    let click_event = click_tracking::ClickEvent::new(slug.clone())
        .with_user_agent("Test Agent".to_string())
        .with_ip_address("192.168.1.1".to_string());

    // ACT: Track click and generate receipt
    let mut tracker = ctx.click_tracker.write().await;
    let receipt = tracker
        .track_click(click_event)
        .expect("Tracking should succeed");

    // ASSERT: Receipt is generated and verifiable
    assert!(
        receipt.verify(),
        "Receipt should be cryptographically valid"
    );
    assert_eq!(
        receipt.route_slug, slug,
        "Receipt should reference correct route"
    );
}

#[tokio::test]
async fn test_click_receipt_chain_integrity() {
    // ARRANGE: Track multiple clicks
    let ctx = TestContext::new();
    let slug = RouteSlug::new("chain-test".to_string()).unwrap();

    // ACT: Track 5 clicks to create receipt chain
    let mut tracker = ctx.click_tracker.write().await;
    for i in 0..5 {
        let click_event = click_tracking::ClickEvent::new(slug.clone())
            .with_referrer(format!("https://example.com/page{}", i));
        let receipt = tracker
            .track_click(click_event)
            .expect("Tracking should succeed");
        assert!(receipt.verify(), "Each receipt should be valid");
    }

    // ASSERT: Entire receipt chain is valid
    assert!(
        tracker.verify_chain(),
        "Receipt chain should have blockchain-like integrity"
    );
}

#[tokio::test]
async fn test_click_count_by_route_accurate() {
    // ARRANGE: Track clicks on multiple routes
    let ctx = TestContext::new();
    let slug1 = RouteSlug::new("route1".to_string()).unwrap();
    let slug2 = RouteSlug::new("route2".to_string()).unwrap();

    let mut tracker = ctx.click_tracker.write().await;

    // ACT: Track 3 clicks on route1, 2 clicks on route2
    for _ in 0..3 {
        let event = click_tracking::ClickEvent::new(slug1.clone());
        tracker.track_click(event).expect("Tracking should succeed");
    }
    for _ in 0..2 {
        let event = click_tracking::ClickEvent::new(slug2.clone());
        tracker.track_click(event).expect("Tracking should succeed");
    }

    // ASSERT: Click counts are accurate per route
    assert_eq!(
        tracker.clicks_by_route(&slug1),
        3,
        "Route1 should have 3 clicks"
    );
    assert_eq!(
        tracker.clicks_by_route(&slug2),
        2,
        "Route2 should have 2 clicks"
    );
    assert_eq!(tracker.total_clicks(), 5, "Total clicks should be 5");
}

// ============================================================================
// Test 3: SaaS Subscription Webhooks
// ============================================================================

#[tokio::test]
async fn test_subscription_creation_webhook() {
    // ARRANGE: Create subscription manager
    let ctx = TestContext::new();
    let user_id = Uuid::new_v4();

    // ACT: Create subscription
    let mut manager = ctx.subscription_manager.write().await;
    let subscription_id = manager.create_subscription(user_id, SubscriptionTier::Professional);

    // ASSERT: Webhook event is generated
    let events = manager.get_webhook_events(&subscription_id);
    assert_eq!(events.len(), 1, "Should have one webhook event");
    assert!(
        matches!(
            events[0].event_type,
            subscription::WebhookEventType::SubscriptionCreated
        ),
        "Event should be SubscriptionCreated"
    );
}

#[tokio::test]
async fn test_payment_succeeded_webhook_resets_usage() {
    // ARRANGE: Create subscription with used clicks
    let ctx = TestContext::new();
    let subscription_id = ctx
        .create_test_subscription(SubscriptionTier::Starter)
        .await;

    let mut manager = ctx.subscription_manager.write().await;
    let subscription = manager.get_subscription_mut(&subscription_id).unwrap();
    subscription.clicks_used = 50;

    // ACT: Process payment succeeded webhook
    let webhook = subscription::WebhookEvent::new(
        subscription::WebhookEventType::PaymentSucceeded,
        subscription_id,
        serde_json::json!({"amount": 1000}),
    );
    manager
        .process_webhook(webhook)
        .expect("Webhook should process successfully");

    // ASSERT: Usage is reset
    let subscription = manager.get_subscription(&subscription_id).unwrap();
    assert_eq!(
        subscription.clicks_used, 0,
        "Usage should be reset after payment"
    );
}

#[tokio::test]
async fn test_payment_failed_webhook_sets_past_due() {
    // ARRANGE: Create active subscription
    let ctx = TestContext::new();
    let subscription_id = ctx
        .create_test_subscription(SubscriptionTier::Professional)
        .await;

    // ACT: Process payment failed webhook
    let mut manager = ctx.subscription_manager.write().await;
    let webhook = subscription::WebhookEvent::new(
        subscription::WebhookEventType::PaymentFailed,
        subscription_id,
        serde_json::json!({"reason": "insufficient_funds"}),
    );
    manager
        .process_webhook(webhook)
        .expect("Webhook should process successfully");

    // ASSERT: Status is past due
    let subscription = manager.get_subscription(&subscription_id).unwrap();
    assert_eq!(
        subscription.status,
        subscription::SubscriptionStatus::PastDue,
        "Status should be past due after payment failure"
    );
}

// ============================================================================
// Test 4: Content Publishing Pipeline
// ============================================================================

#[tokio::test]
async fn test_content_draft_to_published_workflow() {
    // ARRANGE: Create draft content
    let ctx = TestContext::new();
    let author_id = Uuid::new_v4();
    let mut content = content::ContentItem::new(
        "Test Article".to_string(),
        "This is a test article body.".to_string(),
        author_id,
    );

    // ACT: Publish content
    content.publish().expect("Publishing should succeed");

    // ASSERT: Content is published
    assert_eq!(
        content.status,
        PublicationStatus::Published,
        "Status should be Published"
    );
    assert!(
        content.published_at.is_some(),
        "Published timestamp should be set"
    );
}

#[tokio::test]
async fn test_content_scheduled_publication() {
    // ARRANGE: Create content and schedule it
    let ctx = TestContext::new();
    let author_id = Uuid::new_v4();
    let mut content = content::ContentItem::new(
        "Scheduled Post".to_string(),
        "This will be published later.".to_string(),
        author_id,
    );

    let scheduled_time = chrono::Utc::now() + chrono::Duration::hours(1);
    content
        .schedule(scheduled_time)
        .expect("Scheduling should succeed");

    // ACT: Add to pipeline and process (won't publish yet)
    let mut pipeline = ctx.content_pipeline.write().await;
    let content_id = pipeline.add_content(content);
    let published_ids = pipeline.process_scheduled();

    // ASSERT: Content is scheduled but not yet published
    assert!(
        !published_ids.contains(&content_id),
        "Content should not be published yet"
    );
    let item = pipeline.get_content(&content_id).unwrap();
    assert_eq!(
        item.status,
        PublicationStatus::Scheduled,
        "Status should still be Scheduled"
    );
}

#[tokio::test]
async fn test_content_validation_rules() {
    // ARRANGE: Create pipeline
    let ctx = TestContext::new();
    let mut pipeline = ctx.content_pipeline.write().await;

    // Test empty title
    let content = content::ContentItem::new(String::new(), "Body text".to_string(), Uuid::new_v4());
    let id = pipeline.add_content(content);
    let result = pipeline.validate_content(&id);
    assert!(result.is_err(), "Empty title should fail validation");

    // Test empty body
    let content2 = content::ContentItem::new("Title".to_string(), String::new(), Uuid::new_v4());
    let id2 = pipeline.add_content(content2);
    let result2 = pipeline.validate_content(&id2);
    assert!(result2.is_err(), "Empty body should fail validation");

    // Test valid content
    let content3 = content::ContentItem::new(
        "Valid Title".to_string(),
        "Valid body text.".to_string(),
        Uuid::new_v4(),
    );
    let id3 = pipeline.add_content(content3);
    let result3 = pipeline.validate_content(&id3);
    assert!(result3.is_ok(), "Valid content should pass validation");
}

// ============================================================================
// Test 5: Revenue Attribution Calculations
// ============================================================================

#[tokio::test]
async fn test_revenue_attribution_accurate_commission() {
    // ARRANGE: Create revenue attribution engine
    let ctx = TestContext::new();
    let affiliate_id = Uuid::new_v4();
    let slug = RouteSlug::new("commission-test".to_string()).unwrap();
    let click_id = Uuid::new_v4();

    // ACT: Record revenue with Professional tier (15% commission)
    let mut attribution = ctx.revenue_attribution.write().await;
    let event = attribution.record_revenue(
        affiliate_id,
        slug.clone(),
        click_id,
        10_000, // $100.00
        SubscriptionTier::Professional,
    );

    // ASSERT: Commission is calculated correctly
    assert_eq!(
        event.amount_cents, 10_000,
        "Revenue amount should be $100.00"
    );
    assert_eq!(
        event.commission_cents, 1_500,
        "Commission should be $15.00 (15%)"
    );

    // Verify commission calculation
    let verified = attribution
        .verify_commission(&event.id)
        .expect("Event should exist");
    assert!(
        verified,
        "Commission calculation should be verified as accurate"
    );
}

#[tokio::test]
async fn test_revenue_attribution_by_affiliate() {
    // ARRANGE: Create multiple revenue events for different affiliates
    let ctx = TestContext::new();
    let affiliate1 = Uuid::new_v4();
    let affiliate2 = Uuid::new_v4();
    let slug = RouteSlug::new("multi-affiliate".to_string()).unwrap();

    let mut attribution = ctx.revenue_attribution.write().await;

    // ACT: Record revenue for affiliate1: $100, $200, $150
    attribution.record_revenue(
        affiliate1,
        slug.clone(),
        Uuid::new_v4(),
        10_000,
        SubscriptionTier::Professional,
    );
    attribution.record_revenue(
        affiliate1,
        slug.clone(),
        Uuid::new_v4(),
        20_000,
        SubscriptionTier::Professional,
    );
    attribution.record_revenue(
        affiliate1,
        slug.clone(),
        Uuid::new_v4(),
        15_000,
        SubscriptionTier::Professional,
    );

    // Record revenue for affiliate2: $50, $75
    attribution.record_revenue(
        affiliate2,
        slug.clone(),
        Uuid::new_v4(),
        5_000,
        SubscriptionTier::Starter,
    );
    attribution.record_revenue(
        affiliate2,
        slug.clone(),
        Uuid::new_v4(),
        7_500,
        SubscriptionTier::Starter,
    );

    // ASSERT: Total revenue is correct per affiliate
    assert_eq!(
        attribution.total_revenue_by_affiliate(&affiliate1),
        45_000,
        "Affiliate1 should have $450.00 total revenue"
    );
    assert_eq!(
        attribution.total_revenue_by_affiliate(&affiliate2),
        12_500,
        "Affiliate2 should have $125.00 total revenue"
    );

    // ASSERT: Total commission is correct (Professional=15%, Starter=10%)
    assert_eq!(
        attribution.total_commission_by_affiliate(&affiliate1),
        6_750,
        "Affiliate1 should have $67.50 commission (15%)"
    );
    assert_eq!(
        attribution.total_commission_by_affiliate(&affiliate2),
        1_250,
        "Affiliate2 should have $12.50 commission (10%)"
    );
}

#[tokio::test]
async fn test_average_commission_rate_calculation() {
    // ARRANGE: Create revenue events with varying tiers
    let ctx = TestContext::new();
    let affiliate_id = Uuid::new_v4();
    let slug = RouteSlug::new("avg-test".to_string()).unwrap();

    let mut attribution = ctx.revenue_attribution.write().await;

    // ACT: Record revenue with different tiers
    attribution.record_revenue(
        affiliate_id,
        slug.clone(),
        Uuid::new_v4(),
        10_000, // $100 at 10% = $10 commission
        SubscriptionTier::Starter,
    );
    attribution.record_revenue(
        affiliate_id,
        slug.clone(),
        Uuid::new_v4(),
        10_000, // $100 at 15% = $15 commission
        SubscriptionTier::Professional,
    );

    // ASSERT: Average commission rate is correct
    let avg_rate = attribution.avg_commission_rate(&affiliate_id);
    // Total revenue: $200, Total commission: $25, Avg rate: 12.5%
    assert!(
        (avg_rate - 0.125).abs() < 0.001,
        "Average commission rate should be 12.5%, got {}",
        avg_rate
    );
}

// ============================================================================
// Test 6: Subscription Quota Enforcement
// ============================================================================

#[tokio::test]
async fn test_subscription_quota_enforced() {
    // ARRANGE: Create Free tier subscription (100 clicks quota)
    let ctx = TestContext::new();
    let subscription_id = ctx.create_test_subscription(SubscriptionTier::Free).await;

    let mut manager = ctx.subscription_manager.write().await;
    let subscription = manager.get_subscription_mut(&subscription_id).unwrap();

    // ACT: Use 99 clicks (within quota)
    for _ in 0..99 {
        subscription.increment_clicks().expect("Should succeed");
    }

    // ASSERT: 100th click succeeds
    assert!(
        subscription.increment_clicks().is_ok(),
        "100th click should succeed (at quota)"
    );

    // ASSERT: 101st click fails (over quota)
    assert!(
        subscription.increment_clicks().is_err(),
        "101st click should fail (over quota)"
    );
}

#[tokio::test]
async fn test_subscription_upgrade_increases_quota() {
    // ARRANGE: Create Starter tier subscription
    let ctx = TestContext::new();
    let subscription_id = ctx
        .create_test_subscription(SubscriptionTier::Starter)
        .await;

    let mut manager = ctx.subscription_manager.write().await;
    let subscription = manager.get_subscription_mut(&subscription_id).unwrap();

    // ACT: Use 9,999 clicks
    subscription.clicks_used = 9_999;

    // Upgrade to Professional (100,000 quota)
    subscription
        .upgrade(SubscriptionTier::Professional)
        .expect("Upgrade should succeed");

    // ASSERT: Can continue clicking (higher quota)
    assert!(
        !subscription.is_quota_exceeded(),
        "Should not be over quota after upgrade"
    );
}

// ============================================================================
// Test 7: End-to-End Integration Test
// ============================================================================

#[tokio::test]
async fn test_full_workflow_affiliate_click_to_revenue() {
    // ARRANGE: Set up complete environment
    let ctx = TestContext::new();
    let affiliate_id = Uuid::new_v4();
    let slug = RouteSlug::new("e2e-test".to_string()).unwrap();

    // Step 1: Create affiliate route
    let route = affiliate::AffiliateRoute::new(
        slug.clone(),
        "https://example.com/product".to_string(),
        affiliate_id,
    );
    let mut resolver = ctx.route_resolver.write().await;
    resolver
        .add_route(route)
        .expect("Route creation should succeed");
    drop(resolver);

    // Step 2: Create subscription
    let subscription_id = ctx
        .create_test_subscription(SubscriptionTier::Professional)
        .await;

    // ACT: Execute full workflow
    // Step 3: Track click
    let click_event = click_tracking::ClickEvent::new(slug.clone())
        .with_user_agent("Mozilla/5.0".to_string())
        .with_ip_address("203.0.113.1".to_string());
    let mut tracker = ctx.click_tracker.write().await;
    let receipt = tracker
        .track_click(click_event)
        .expect("Click tracking should succeed");
    let click_id = receipt.click_id;
    drop(tracker);

    // Step 4: Record revenue
    let mut attribution = ctx.revenue_attribution.write().await;
    let revenue_event = attribution.record_revenue(
        affiliate_id,
        slug.clone(),
        click_id,
        25_000, // $250.00
        SubscriptionTier::Professional,
    );
    drop(attribution);

    // Step 5: Increment subscription usage
    let mut manager = ctx.subscription_manager.write().await;
    let subscription = manager.get_subscription_mut(&subscription_id).unwrap();
    subscription
        .increment_clicks()
        .expect("Click increment should succeed");

    // ASSERT: Verify complete workflow
    assert!(receipt.verify(), "Click receipt should be valid");
    assert_eq!(
        revenue_event.amount_cents, 25_000,
        "Revenue should be $250.00"
    );
    assert_eq!(
        revenue_event.commission_cents, 3_750,
        "Commission should be $37.50 (15%)"
    );
    assert_eq!(
        subscription.clicks_used, 1,
        "Subscription usage should be incremented"
    );
}
