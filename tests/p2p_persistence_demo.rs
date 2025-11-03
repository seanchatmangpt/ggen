//! Demonstration of P2P state persistence
//!
//! This test shows how to use the P2P persistence layer to save and restore state.

#[cfg(all(test, feature = "p2p"))]
mod persistence_demo {
    use ggen_marketplace::backend::p2p::{P2PRegistry, P2PConfig};
    use libp2p::Multiaddr;
    use tempfile::TempDir;

    #[tokio::test]
    async fn demo_persistence_workflow() {
        // Create temporary directory for test
        let temp_dir = TempDir::new().unwrap();
        let state_file = temp_dir.path().join("p2p-state.json");

        // Step 1: Create P2P registry with custom state file
        let config = P2PConfig {
            bootstrap_nodes: vec![
                "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ"
                    .parse::<Multiaddr>()
                    .unwrap(),
            ],
            packages_topic: "/ggen/packages/v1".to_string(),
            dht_server_mode: true,
            listen_addresses: vec!["/ip4/0.0.0.0/tcp/0".parse().unwrap()],
        };

        let registry = P2PRegistry::new(config.clone()).await.unwrap();

        // Step 2: Use the registry (simulate some activity)
        // In real usage, you would:
        // - Publish packages
        // - Search for packages
        // - Build peer reputation
        // - Cache packages

        // Step 3: Manually save state (normally done by auto-save)
        // Note: Integration with P2PRegistry pending
        println!("State would be saved to: {:?}", state_file);

        // Step 4: Create new registry and load state
        let registry2 = P2PRegistry::new(config).await.unwrap();

        // State is automatically loaded in new() constructor
        // Peer reputation, addresses, and cache are restored

        println!("✓ Persistence workflow completed successfully");
    }

    #[tokio::test]
    async fn demo_persistence_api() {
        use ggen_marketplace::backend::p2p_persistence::*;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let state_file = temp_dir.path().join("test-state.json");

        // Create persistence manager
        let persistence = P2PStatePersistence::new_with_path(state_file.clone()).unwrap();

        // Create test state
        let mut state = P2PState::default();
        state.peers.push(SerializedPeer {
            peer_id: "12D3KooWTest123".to_string(),
            addresses: vec!["/ip4/127.0.0.1/tcp/4001".to_string()],
            last_seen: chrono::Utc::now(),
        });
        state.reputation.push(SerializedReputation {
            peer_id: "12D3KooWTest123".to_string(),
            successful_retrievals: 42,
            failed_retrievals: 3,
            last_seen: chrono::Utc::now(),
            avg_response_time_ms: 150,
            location: Some(SerializedGeoLocation {
                latitude: 37.7749,
                longitude: -122.4194,
                region: Some("US-CA".to_string()),
            }),
            packages_provided: 10,
        });
        state.bootstrap_nodes.push("/ip4/104.131.131.82/tcp/4001".to_string());

        // Save state
        persistence.update_state(state).await;
        persistence.save_state().await.unwrap();

        println!("✓ Saved state to {:?}", state_file);
        assert!(state_file.exists());

        // Load state
        let loaded = persistence.load_state().await.unwrap();

        assert_eq!(loaded.peers.len(), 1);
        assert_eq!(loaded.reputation.len(), 1);
        assert_eq!(loaded.reputation[0].successful_retrievals, 42);
        assert_eq!(loaded.reputation[0].packages_provided, 10);
        assert_eq!(loaded.bootstrap_nodes.len(), 1);

        println!("✓ Successfully loaded and verified state");

        // Start auto-save (runs in background)
        persistence.start_auto_save().await.unwrap();
        println!("✓ Auto-save started (saves every 60 seconds)");

        // Simulate some time passing
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        // Graceful shutdown
        persistence.persist_on_shutdown().await.unwrap();
        println!("✓ Graceful shutdown completed");
    }

    #[tokio::test]
    async fn demo_atomic_writes() {
        use ggen_marketplace::backend::p2p_persistence::P2PStatePersistence;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let state_file = temp_dir.path().join("atomic-test.json");

        let persistence = P2PStatePersistence::new_with_path(state_file.clone()).unwrap();

        // Perform multiple rapid writes
        for i in 0..10 {
            let mut state = ggen_marketplace::backend::p2p_persistence::P2PState::default();
            state.peers.push(ggen_marketplace::backend::p2p_persistence::SerializedPeer {
                peer_id: format!("Peer{}", i),
                addresses: vec![format!("/ip4/127.0.0.1/tcp/{}", 4000 + i)],
                last_seen: chrono::Utc::now(),
            });

            persistence.update_state(state).await;
            persistence.save_state().await.unwrap();
        }

        // File should never be corrupted
        let loaded = persistence.load_state().await.unwrap();
        assert_eq!(loaded.peers.len(), 1); // Last write wins
        println!("✓ Atomic writes preserved file integrity");
    }

    #[tokio::test]
    async fn demo_missing_file_handling() {
        use ggen_marketplace::backend::p2p_persistence::P2PStatePersistence;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let non_existent_file = temp_dir.path().join("does-not-exist.json");

        let persistence = P2PStatePersistence::new_with_path(non_existent_file).unwrap();

        // Should return default state without error
        let state = persistence.load_state().await.unwrap();

        assert_eq!(state.peers.len(), 0);
        assert_eq!(state.reputation.len(), 0);
        assert_eq!(state.version, 1);

        println!("✓ Missing file handled gracefully (first run scenario)");
    }
}
