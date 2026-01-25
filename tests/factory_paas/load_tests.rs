//! FactoryPaaS Load Tests
//!
//! Performance and load testing to verify:
//! - 10k requests/sec to /r/{route_slug}
//! - 1k concurrent click tracking
//! - Batch content publishing

#![cfg(test)]

use super::TestContext;
use ggen_saas::factory_paas::*;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;
use uuid::Uuid;

// ============================================================================
// Load Test 1: 10k Requests/Second to Route Resolution
// ============================================================================

#[tokio::test]
#[ignore] // Ignore by default - run with --ignored flag
async fn load_test_10k_route_resolutions_per_second() {
    // ARRANGE: Set up route resolver with 100 routes
    let ctx = TestContext::new();
    let mut resolver = ctx.route_resolver.write().await;

    let mut slugs = Vec::new();
    for i in 0..100 {
        let slug = RouteSlug::new(format!("route-{}", i)).expect("Valid slug");
        let route = affiliate::AffiliateRoute::new(
            slug.clone(),
            format!("https://example.com/target-{}", i),
            Uuid::new_v4(),
        );
        resolver.add_route(route).expect("Route should be added");
        slugs.push(slug);
    }
    drop(resolver);

    // ACT: Resolve routes 10,000 times in parallel
    let start = Instant::now();
    let tasks: Vec<_> = (0..10_000)
        .map(|i| {
            let ctx = ctx.clone();
            let slug = slugs[i % slugs.len()].clone();
            tokio::spawn(async move {
                let resolver = ctx.route_resolver.read().await;
                resolver.resolve(&slug).expect("Should resolve")
            })
        })
        .collect();

    // Wait for all tasks to complete
    for task in tasks {
        task.await.expect("Task should succeed");
    }
    let elapsed = start.elapsed();

    // ASSERT: All resolutions complete within 1 second
    assert!(
        elapsed < Duration::from_secs(1),
        "10k route resolutions should complete in < 1s, took {:?}",
        elapsed
    );

    println!(
        "✓ Load Test: 10,000 route resolutions completed in {:?} ({:.0} req/sec)",
        elapsed,
        10_000.0 / elapsed.as_secs_f64()
    );
}

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_route_resolution_latency() {
    // ARRANGE: Set up route resolver
    let ctx = TestContext::new();
    let slugs = ctx.setup_sample_routes().await.expect("Setup failed");
    let test_slug = &slugs[0];

    // ACT: Measure p50, p95, p99 latency
    let mut latencies = Vec::new();
    for _ in 0..1_000 {
        let start = Instant::now();
        let resolver = ctx.route_resolver.read().await;
        resolver.resolve(test_slug).expect("Should resolve");
        drop(resolver);
        latencies.push(start.elapsed());
    }

    // ASSERT: Calculate percentiles
    latencies.sort();
    let p50 = latencies[latencies.len() / 2];
    let p95 = latencies[(latencies.len() * 95) / 100];
    let p99 = latencies[(latencies.len() * 99) / 100];

    println!("Route resolution latency:");
    println!("  p50: {:?}", p50);
    println!("  p95: {:?}", p95);
    println!("  p99: {:?}", p99);

    assert!(
        p99 < Duration::from_millis(10),
        "p99 latency should be < 10ms, got {:?}",
        p99
    );
}

// ============================================================================
// Load Test 2: 1k Concurrent Click Tracking
// ============================================================================

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_1k_concurrent_click_tracking() {
    // ARRANGE: Set up click tracker
    let tracker = Arc::new(RwLock::new(click_tracking::ClickTracker::new()));
    let slug = RouteSlug::new("load-test-clicks".to_string()).expect("Valid slug");

    // ACT: Track 1,000 clicks concurrently
    let start = Instant::now();
    let tasks: Vec<_> = (0..1_000)
        .map(|i| {
            let tracker = Arc::clone(&tracker);
            let slug = slug.clone();
            tokio::spawn(async move {
                let event = click_tracking::ClickEvent::new(slug)
                    .with_user_agent(format!("Agent-{}", i))
                    .with_ip_address(format!("192.168.{}.{}", i / 256, i % 256));

                let mut tracker = tracker.write().await;
                tracker.track_click(event).expect("Tracking should succeed")
            })
        })
        .collect();

    // Wait for all tasks and collect receipts
    let mut receipts = Vec::new();
    for task in tasks {
        let receipt = task.await.expect("Task should succeed");
        receipts.push(receipt);
    }
    let elapsed = start.elapsed();

    // ASSERT: All clicks tracked and receipts valid
    assert_eq!(receipts.len(), 1_000, "Should have 1,000 receipts");

    let valid_count = receipts.iter().filter(|r| r.verify()).count();
    assert_eq!(valid_count, 1_000, "All receipts should be valid");

    println!(
        "✓ Load Test: 1,000 concurrent clicks tracked in {:?} ({:.0} clicks/sec)",
        elapsed,
        1_000.0 / elapsed.as_secs_f64()
    );

    // Verify total click count
    let tracker = tracker.read().await;
    assert_eq!(
        tracker.total_clicks(),
        1_000,
        "Should have exactly 1,000 clicks"
    );
}

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_click_receipt_chain_under_load() {
    // ARRANGE: Set up click tracker
    let tracker = Arc::new(RwLock::new(click_tracking::ClickTracker::new()));
    let slug = RouteSlug::new("chain-load-test".to_string()).expect("Valid slug");

    // ACT: Track 500 clicks sequentially (to maintain chain)
    let start = Instant::now();
    for i in 0..500 {
        let event = click_tracking::ClickEvent::new(slug.clone())
            .with_referrer(format!("https://example.com/page-{}", i));

        let mut tracker = tracker.write().await;
        let receipt = tracker.track_click(event).expect("Tracking should succeed");
        assert!(receipt.verify(), "Each receipt should verify");
        drop(tracker);
    }
    let elapsed = start.elapsed();

    // ASSERT: Chain integrity maintained under load
    let tracker = tracker.read().await;
    assert!(
        tracker.verify_chain(),
        "Receipt chain should maintain integrity under load"
    );

    println!(
        "✓ Load Test: 500 chained clicks tracked in {:?} (chain integrity verified)",
        elapsed
    );
}

// ============================================================================
// Load Test 3: Batch Content Publishing
// ============================================================================

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_batch_content_publishing() {
    // ARRANGE: Create content pipeline with 1,000 draft items
    let pipeline = Arc::new(RwLock::new(content::ContentPipeline::new()));
    let author_id = Uuid::new_v4();

    let mut content_ids = Vec::new();
    for i in 0..1_000 {
        let mut content = content::ContentItem::new(
            format!("Article {}", i),
            format!("This is the body of article {}.", i),
            author_id,
        );
        content.add_tags(vec![format!("tag-{}", i % 10)]);

        let mut pipeline_write = pipeline.write().await;
        let id = pipeline_write.add_content(content);
        content_ids.push(id);
        drop(pipeline_write);
    }

    // ACT: Publish all content in parallel
    let start = Instant::now();
    let tasks: Vec<_> = content_ids
        .into_iter()
        .map(|id| {
            let pipeline = Arc::clone(&pipeline);
            tokio::spawn(async move {
                let mut pipeline = pipeline.write().await;
                if let Some(content) = pipeline.get_content_mut(&id) {
                    content.publish()
                } else {
                    Err(content::ContentError::NotFound)
                }
            })
        })
        .collect();

    // Wait for all tasks
    let mut success_count = 0;
    for task in tasks {
        if task.await.expect("Task should complete").is_ok() {
            success_count += 1;
        }
    }
    let elapsed = start.elapsed();

    // ASSERT: All content published successfully
    assert_eq!(
        success_count, 1_000,
        "All content should publish successfully"
    );

    let pipeline = pipeline.read().await;
    let published = pipeline.list_by_status(PublicationStatus::Published);
    assert_eq!(published.len(), 1_000, "Should have 1,000 published items");

    println!(
        "✓ Load Test: 1,000 content items published in {:?} ({:.0} items/sec)",
        elapsed,
        1_000.0 / elapsed.as_secs_f64()
    );
}

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_scheduled_content_processing() {
    // ARRANGE: Create pipeline with scheduled content
    let pipeline = Arc::new(RwLock::new(content::ContentPipeline::new()));
    let author_id = Uuid::new_v4();

    // Add 100 items scheduled for immediate publication
    for i in 0..100 {
        let mut content = content::ContentItem::new(
            format!("Scheduled Article {}", i),
            format!("Body of scheduled article {}.", i),
            author_id,
        );

        // Schedule for 1 second in the past (will be published immediately)
        let scheduled_time = chrono::Utc::now() - chrono::Duration::seconds(1);
        content.scheduled_for = Some(scheduled_time);
        content.status = PublicationStatus::Scheduled;

        let mut pipeline_write = pipeline.write().await;
        pipeline_write.add_content(content);
        drop(pipeline_write);
    }

    // ACT: Process scheduled publications
    let start = Instant::now();
    let mut pipeline_write = pipeline.write().await;
    let published_ids = pipeline_write.process_scheduled();
    let elapsed = start.elapsed();

    // ASSERT: All scheduled content published
    assert_eq!(published_ids.len(), 100, "Should publish 100 items");

    println!(
        "✓ Load Test: 100 scheduled items processed in {:?}",
        elapsed
    );
}

// ============================================================================
// Load Test 4: Revenue Attribution Under Load
// ============================================================================

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_revenue_attribution_throughput() {
    // ARRANGE: Create revenue attribution engine
    let attribution = Arc::new(RwLock::new(revenue::RevenueAttribution::new()));
    let affiliate_id = Uuid::new_v4();
    let slug = RouteSlug::new("revenue-load-test".to_string()).expect("Valid slug");

    // ACT: Record 10,000 revenue events concurrently
    let start = Instant::now();
    let tasks: Vec<_> = (0..10_000)
        .map(|i| {
            let attribution = Arc::clone(&attribution);
            let slug = slug.clone();
            tokio::spawn(async move {
                let mut attribution = attribution.write().await;
                attribution.record_revenue(
                    affiliate_id,
                    slug,
                    Uuid::new_v4(),
                    (i as u64 + 1) * 100, // $1.00, $2.00, ..., $100.00
                    SubscriptionTier::Professional,
                )
            })
        })
        .collect();

    // Wait for all tasks
    for task in tasks {
        task.await.expect("Task should succeed");
    }
    let elapsed = start.elapsed();

    // ASSERT: All revenue recorded correctly
    let attribution = attribution.read().await;
    let total_revenue = attribution.total_revenue_by_affiliate(&affiliate_id);
    let expected_revenue: u64 = (1..=10_000).map(|i| i * 100).sum();
    assert_eq!(
        total_revenue, expected_revenue,
        "Total revenue should be accurate"
    );

    println!(
        "✓ Load Test: 10,000 revenue events recorded in {:?} ({:.0} events/sec)",
        elapsed,
        10_000.0 / elapsed.as_secs_f64()
    );
}

// ============================================================================
// Load Test 5: Subscription Management Under Load
// ============================================================================

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_subscription_operations() {
    // ARRANGE: Create subscription manager
    let manager = Arc::new(RwLock::new(subscription::SubscriptionManager::new()));

    // ACT: Create 1,000 subscriptions concurrently
    let start = Instant::now();
    let tasks: Vec<_> = (0..1_000)
        .map(|i| {
            let manager = Arc::clone(&manager);
            let tier = match i % 4 {
                0 => SubscriptionTier::Free,
                1 => SubscriptionTier::Starter,
                2 => SubscriptionTier::Professional,
                _ => SubscriptionTier::Enterprise,
            };
            tokio::spawn(async move {
                let mut manager = manager.write().await;
                manager.create_subscription(Uuid::new_v4(), tier)
            })
        })
        .collect();

    // Wait for all tasks
    let mut subscription_ids = Vec::new();
    for task in tasks {
        let id = task.await.expect("Task should succeed");
        subscription_ids.push(id);
    }
    let elapsed = start.elapsed();

    // ASSERT: All subscriptions created
    assert_eq!(
        subscription_ids.len(),
        1_000,
        "Should have 1,000 subscriptions"
    );

    println!(
        "✓ Load Test: 1,000 subscriptions created in {:?} ({:.0} subs/sec)",
        elapsed,
        1_000.0 / elapsed.as_secs_f64()
    );

    // Verify subscriptions exist
    let manager = manager.read().await;
    for id in subscription_ids.iter().take(10) {
        assert!(
            manager.get_subscription(id).is_some(),
            "Subscription should exist"
        );
    }
}

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_webhook_processing() {
    // ARRANGE: Create subscriptions
    let manager = Arc::new(RwLock::new(subscription::SubscriptionManager::new()));
    let mut subscription_ids = Vec::new();

    for _ in 0..100 {
        let mut manager_write = manager.write().await;
        let id = manager_write.create_subscription(Uuid::new_v4(), SubscriptionTier::Professional);
        subscription_ids.push(id);
        drop(manager_write);
    }

    // ACT: Process 1,000 webhooks (10 per subscription)
    let start = Instant::now();
    let tasks: Vec<_> = (0..1_000)
        .map(|i| {
            let manager = Arc::clone(&manager);
            let subscription_id = subscription_ids[i % subscription_ids.len()];
            let event_type = if i % 2 == 0 {
                subscription::WebhookEventType::PaymentSucceeded
            } else {
                subscription::WebhookEventType::SubscriptionUpdated
            };

            tokio::spawn(async move {
                let event = subscription::WebhookEvent::new(
                    event_type,
                    subscription_id,
                    serde_json::json!({"test": i}),
                );
                let mut manager = manager.write().await;
                manager.process_webhook(event)
            })
        })
        .collect();

    // Wait for all tasks
    let mut success_count = 0;
    for task in tasks {
        if task.await.expect("Task should complete").is_ok() {
            success_count += 1;
        }
    }
    let elapsed = start.elapsed();

    // ASSERT: All webhooks processed
    assert_eq!(
        success_count, 1_000,
        "All webhooks should process successfully"
    );

    println!(
        "✓ Load Test: 1,000 webhooks processed in {:?} ({:.0} webhooks/sec)",
        elapsed,
        1_000.0 / elapsed.as_secs_f64()
    );
}

// ============================================================================
// Load Test 6: End-to-End Load Test
// ============================================================================

#[tokio::test]
#[ignore] // Ignore by default
async fn load_test_end_to_end_workflow() {
    // ARRANGE: Set up complete system
    let ctx = TestContext::new();

    // Create 10 affiliate routes
    let mut resolver = ctx.route_resolver.write().await;
    let mut slugs = Vec::new();
    for i in 0..10 {
        let slug = RouteSlug::new(format!("e2e-route-{}", i)).expect("Valid slug");
        let route = affiliate::AffiliateRoute::new(
            slug.clone(),
            format!("https://example.com/e2e-{}", i),
            Uuid::new_v4(),
        );
        resolver.add_route(route).expect("Route should be added");
        slugs.push(slug);
    }
    drop(resolver);

    // ACT: Execute 100 complete workflows concurrently
    let start = Instant::now();
    let tasks: Vec<_> = (0..100)
        .map(|i| {
            let ctx_clone = ctx.clone();
            let slug = slugs[i % slugs.len()].clone();
            tokio::spawn(async move {
                // Step 1: Resolve route
                let resolver = ctx_clone.route_resolver.read().await;
                let _url = resolver.resolve(&slug).expect("Should resolve");
                drop(resolver);

                // Step 2: Track click
                let event = click_tracking::ClickEvent::new(slug.clone());
                let mut tracker = ctx_clone.click_tracker.write().await;
                let receipt = tracker.track_click(event).expect("Tracking should succeed");
                let click_id = receipt.click_id;
                drop(tracker);

                // Step 3: Record revenue
                let mut attribution = ctx_clone.revenue_attribution.write().await;
                attribution.record_revenue(
                    Uuid::new_v4(),
                    slug,
                    click_id,
                    (i as u64 + 1) * 1000,
                    SubscriptionTier::Professional,
                );

                receipt.verify()
            })
        })
        .collect();

    // Wait for all tasks
    let mut success_count = 0;
    for task in tasks {
        if task.await.expect("Task should complete") {
            success_count += 1;
        }
    }
    let elapsed = start.elapsed();

    // ASSERT: All workflows completed successfully
    assert_eq!(
        success_count, 100,
        "All workflows should complete successfully"
    );

    println!(
        "✓ Load Test: 100 end-to-end workflows completed in {:?} ({:.0} workflows/sec)",
        elapsed,
        100.0 / elapsed.as_secs_f64()
    );
}
