// Integration tests for innovative marketplace features
use ggen_marketplace::*;
use ndarray::Array1;
use std::path::PathBuf;

#[tokio::test]
async fn test_recommendation_engine_workflow() {
    let mut engine = RecommendationEngine::new();

    // Initialize with sample interactions
    let interactions = vec![
        ("alice".to_string(), "rust-web".to_string(), 1.0),
        ("alice".to_string(), "tokio-async".to_string(), 0.9),
        ("bob".to_string(), "rust-web".to_string(), 0.8),
        ("bob".to_string(), "axum-framework".to_string(), 1.0),
        ("carol".to_string(), "axum-framework".to_string(), 0.9),
    ];

    engine.initialize(interactions).unwrap();

    // Get recommendations for Alice
    let recommendations = engine.recommend_for_user("alice", 3).unwrap();
    assert!(!recommendations.is_empty());

    // Update package features for similarity
    engine.update_package_features(
        "rust-web".to_string(),
        Array1::from_vec(vec![1.0, 0.8, 0.5]),
    );
    engine.update_package_features(
        "axum-framework".to_string(),
        Array1::from_vec(vec![0.9, 0.7, 0.6]),
    );

    // Find similar packages
    let similar = engine.find_similar_packages("rust-web", 2).unwrap();
    assert!(!similar.is_empty());

    // Test complementary packages
    let complementary = engine.find_complementary_packages("rust-web", 3).unwrap();
    assert!(!complementary.is_empty());
}

#[tokio::test]
async fn test_quality_analyzer_comprehensive() {
    let analyzer = QualityAnalyzer::new();

    // Create temporary test directory
    let temp_dir = tempfile::tempdir().unwrap();
    let test_path = temp_dir.path();

    // Create minimal project structure
    tokio::fs::create_dir_all(test_path.join("src"))
        .await
        .unwrap();
    tokio::fs::create_dir_all(test_path.join("tests"))
        .await
        .unwrap();

    // Create sample source file
    let sample_code = r#"
        /// This is a documented function
        pub fn example() -> u32 {
            if true {
                42
            } else {
                0
            }
        }

        pub struct ExampleStruct {
            value: u32,
        }
    "#;

    tokio::fs::write(test_path.join("src/lib.rs"), sample_code)
        .await
        .unwrap();

    // Create sample test file
    let sample_test = r#"
        #[test]
        fn test_example() {
            assert_eq!(42, 42);
        }

        #[test]
        fn test_another() {
            assert!(true);
        }
    "#;

    tokio::fs::write(test_path.join("tests/test.rs"), sample_test)
        .await
        .unwrap();

    // Create README
    tokio::fs::write(test_path.join("README.md"), "# Test Package")
        .await
        .unwrap();

    // Analyze package quality
    let score = analyzer.analyze_package(test_path).await.unwrap();

    // Verify score components
    assert!(score.overall > 0.0);
    assert!(score.overall <= 100.0);
    assert!(score.components.code_quality > 0.0);
    assert!(score.components.documentation > 0.0);

    println!("Quality Score: {}", score.overall);
    println!("Grade: {:?}", score.grade);
}

#[tokio::test]
async fn test_content_addressed_storage_workflow() {
    let store = ContentAddressedStore::new();

    // Store different content
    let content1 = b"Hello, World!";
    let content2 = b"Different content";
    let content3 = b"Hello, World!"; // Same as content1

    let cid1 = store.store(content1).await.unwrap();
    let cid2 = store.store(content2).await.unwrap();
    let cid3 = store.store(content3).await.unwrap();

    // Same content should have same CID
    assert_eq!(cid1, cid3);
    assert_ne!(cid1, cid2);

    // Retrieve content
    let retrieved1 = store.retrieve(&cid1).await.unwrap();
    assert_eq!(retrieved1, content1);

    // Verify integrity
    assert!(store.verify(&cid1).await.unwrap());

    // Get metadata
    let metadata = store.get_metadata(&cid1).await.unwrap();
    assert_eq!(metadata.size, content1.len() as u64);

    // List stored content
    let list = store.list().await;
    assert_eq!(list.len(), 2); // Only 2 unique items

    // Test garbage collection
    let removed = store.garbage_collect(&[cid1]).await.unwrap();
    assert_eq!(removed, 1); // cid2 should be removed

    // Verify cid2 is gone
    assert!(!store.exists(&cid2).await);
}

#[tokio::test]
async fn test_content_addressed_storage_with_filesystem() {
    let temp_dir = tempfile::tempdir().unwrap();
    let store = ContentAddressedStore::with_base_path(temp_dir.path().to_path_buf());

    let content = b"Test data for filesystem storage";
    let cid = store.store(content).await.unwrap();

    // Content should be stored in filesystem
    assert!(store.exists(&cid).await);

    // Retrieve from filesystem
    let retrieved = store.retrieve(&cid).await.unwrap();
    assert_eq!(retrieved, content);

    // Pin content
    store.pin(&cid).await.unwrap();
    let metadata = store.get_metadata(&cid).await.unwrap();
    assert!(metadata.tags.contains(&"pinned".to_string()));

    // Unpin content
    store.unpin(&cid).await.unwrap();
    let metadata = store.get_metadata(&cid).await.unwrap();
    assert!(!metadata.tags.contains(&"pinned".to_string()));
}

#[tokio::test]
async fn test_plugin_manager_basic() {
    let manager = PluginManager::new().unwrap();

    // List plugins (should be empty initially)
    let plugins = manager.list_plugins().await;
    assert_eq!(plugins.len(), 0);

    // In a real scenario, you would load actual WASM plugins
    // For this test, we just verify the manager initializes correctly
}

#[tokio::test]
async fn test_smart_cache_workflow() {
    let cache = SmartCache::new();

    // Test package caching
    let package = cache::Package {
        id: "test-package".to_string(),
        name: "Test Package".to_string(),
        version: "1.0.0".to_string(),
        description: "A test package".to_string(),
        downloads: 1000,
    };

    cache
        .set_package("test-package".to_string(), package.clone())
        .await;

    let cached = cache.get_package(&"test-package".to_string()).await;
    assert!(cached.is_some());
    assert_eq!(cached.unwrap().name, "Test Package");

    // Test search caching
    let query = cache::SearchQuery {
        query: "rust web framework".to_string(),
        filters: vec!["category:web".to_string()],
        limit: 10,
    };

    let results = vec![package.clone()];
    cache
        .set_search_results(query.clone(), results.clone())
        .await;

    let cached_results = cache.get_search_results(&query).await;
    assert!(cached_results.is_some());
    assert_eq!(cached_results.unwrap().len(), 1);

    // Test download counter
    let count1 = cache
        .increment_download_count(&"test-package".to_string())
        .await;
    let count2 = cache
        .increment_download_count(&"test-package".to_string())
        .await;
    assert_eq!(count1, 1);
    assert_eq!(count2, 2);

    // Test cache stats
    let stats = cache.get_stats().await;
    assert!(stats.hits > 0);
    assert!(stats.entries > 0);

    // Test memory usage
    let memory = cache.memory_usage().await;
    assert!(memory.package_entries > 0);

    // Test cache invalidation
    cache.invalidate_package(&"test-package".to_string()).await;
    let cached_after_invalidation = cache.get_package(&"test-package".to_string()).await;
    assert!(cached_after_invalidation.is_none());
}

#[tokio::test]
async fn test_cache_get_or_insert() {
    let cache = SmartCache::new();

    // Test get_or_insert_with
    let package = cache
        .get_or_insert_with("new-package".to_string(), || async {
            Ok(cache::Package {
                id: "new-package".to_string(),
                name: "New Package".to_string(),
                version: "1.0.0".to_string(),
                description: "Dynamically fetched".to_string(),
                downloads: 500,
            })
        })
        .await
        .unwrap();

    assert_eq!(package.name, "New Package");

    // Second call should hit cache
    let cached_package = cache.get_package(&"new-package".to_string()).await;
    assert!(cached_package.is_some());
}

#[tokio::test]
async fn test_integrated_marketplace_features() {
    // Create all components
    let mut recommendation_engine = RecommendationEngine::new();
    let quality_analyzer = QualityAnalyzer::new();
    let storage = ContentAddressedStore::new();
    let cache = SmartCache::new();

    // Simulate a complete workflow

    // 1. Store package content
    let package_content = serde_json::to_vec(&cache::Package {
        id: "integrated-test".to_string(),
        name: "Integrated Test Package".to_string(),
        version: "1.0.0".to_string(),
        description: "Testing integration".to_string(),
        downloads: 100,
    })
    .unwrap();

    let content_id = storage.store(&package_content).await.unwrap();
    assert!(storage.exists(&content_id).await);

    // 2. Cache package
    let package = cache::Package {
        id: "integrated-test".to_string(),
        name: "Integrated Test Package".to_string(),
        version: "1.0.0".to_string(),
        description: "Testing integration".to_string(),
        downloads: 100,
    };

    cache.set_package(package.id.clone(), package.clone()).await;

    // 3. Initialize recommendations
    let interactions = vec![("user1".to_string(), "integrated-test".to_string(), 1.0)];
    recommendation_engine.initialize(interactions).unwrap();

    // 4. Get recommendations
    let recommendations = recommendation_engine
        .recommend_for_user("user1", 5)
        .unwrap();

    // Should work without errors even if no recommendations yet
    println!(
        "Integration test completed with {} recommendations",
        recommendations.len()
    );
}

#[tokio::test]
async fn test_quality_grade_calculation() {
    use ggen_marketplace::QualityGrade;

    assert_eq!(QualityGrade::from_score(95.0), QualityGrade::A);
    assert_eq!(QualityGrade::from_score(85.0), QualityGrade::B);
    assert_eq!(QualityGrade::from_score(75.0), QualityGrade::C);
    assert_eq!(QualityGrade::from_score(65.0), QualityGrade::D);
    assert_eq!(QualityGrade::from_score(50.0), QualityGrade::F);
}
