//! Integration tests for quorum calculation

use ggen_consensus::{QuorumCalculator, QuorumConfig};

#[test]
fn test_minimum_replicas_for_fault_tolerance() {
    // Test 3f+1 rule for various fault tolerances
    let test_cases = vec![
        (1, 4),   // f=1 requires 4 replicas
        (2, 7),   // f=2 requires 7 replicas
        (3, 10),  // f=3 requires 10 replicas
        (5, 16),  // f=5 requires 16 replicas
        (10, 31), // f=10 requires 31 replicas
    ];

    for (faults, expected_replicas) in test_cases {
        // Arrange
        let config = QuorumConfig::new(expected_replicas, faults).unwrap();
        let calculator = QuorumCalculator::new(config);

        // Assert
        assert_eq!(calculator.total_replicas(), expected_replicas);
        assert_eq!(calculator.max_faults(), faults);
        assert_eq!(calculator.quorum_size(), 2 * faults + 1);
    }
}

#[test]
fn test_insufficient_replicas_rejected() {
    // Test that insufficient replicas are rejected
    let invalid_configs = vec![
        (3, 1), // 3 < 3*1+1=4
        (6, 2), // 6 < 3*2+1=7
        (9, 3), // 9 < 3*3+1=10
    ];

    for (replicas, faults) in invalid_configs {
        let result = QuorumConfig::new(replicas, faults);
        assert!(result.is_err());
    }
}

#[test]
fn test_quorum_guarantees_majority_honest() {
    // Test that quorum ensures majority of honest nodes
    let configs = vec![
        (4, 1),   // 4 replicas, 1 fault
        (7, 2),   // 7 replicas, 2 faults
        (10, 3),  // 10 replicas, 3 faults
        (100, 33), // 100 replicas, 33 faults
    ];

    for (total, faults) in configs {
        // Arrange
        let config = QuorumConfig::new(total, faults).unwrap();
        let calculator = QuorumCalculator::new(config);

        let quorum = calculator.quorum_size();
        let honest_min = total - faults;

        // Assert - With quorum votes, at least f+1 must be honest
        assert!(honest_min >= quorum - faults);
        assert_eq!(quorum, 2 * faults + 1);
    }
}

#[test]
fn test_prepare_vs_commit_quorum() {
    // Arrange
    let config = QuorumConfig::new(4, 1).unwrap();
    let calculator = QuorumCalculator::new(config);

    // Assert
    // Prepare needs 2f (primary already sent pre-prepare)
    assert_eq!(calculator.prepare_quorum(), 2);

    // Commit needs 2f+1 (including own commit)
    assert_eq!(calculator.commit_quorum(), 3);

    // Test quorum checking
    assert!(!calculator.has_prepare_quorum(1));
    assert!(calculator.has_prepare_quorum(2));

    assert!(!calculator.has_commit_quorum(2));
    assert!(calculator.has_commit_quorum(3));
}

#[test]
fn test_auto_calculate_faults_from_replicas() {
    // Test automatic fault calculation from total replicas
    let test_cases = vec![
        (4, 1),   // 4 replicas -> f=1
        (7, 2),   // 7 replicas -> f=2
        (10, 3),  // 10 replicas -> f=3
        (13, 4),  // 13 replicas -> f=4
        (100, 33), // 100 replicas -> f=33
    ];

    for (replicas, expected_faults) in test_cases {
        // Arrange & Act
        let config = QuorumConfig::from_total_replicas(replicas).unwrap();

        // Assert
        assert_eq!(config.max_faults, expected_faults);
        assert_eq!(config.total_replicas, replicas);
    }
}

#[test]
fn test_vote_count_validation() {
    // Arrange
    let config = QuorumConfig::new(4, 1).unwrap();
    let calculator = QuorumCalculator::new(config);

    // Assert
    assert!(calculator.validate_vote_count(0).is_ok());
    assert!(calculator.validate_vote_count(4).is_ok());
    assert!(calculator.validate_vote_count(5).is_err()); // More votes than replicas
    assert!(calculator.validate_vote_count(100).is_err());
}

#[test]
fn test_view_change_quorum() {
    // Arrange
    let config = QuorumConfig::new(7, 2).unwrap();
    let calculator = QuorumCalculator::new(config);

    // Assert - View change needs 2f+1
    assert_eq!(calculator.view_change_quorum(), 5);
    assert!(!calculator.has_view_change_quorum(4));
    assert!(calculator.has_view_change_quorum(5));
    assert!(calculator.has_view_change_quorum(6));
}

#[test]
fn test_edge_case_single_fault() {
    // Arrange - Minimum viable PBFT: 4 replicas, 1 fault
    let config = QuorumConfig::new(4, 1).unwrap();
    let calculator = QuorumCalculator::new(config);

    // Assert
    assert_eq!(calculator.total_replicas(), 4);
    assert_eq!(calculator.max_faults(), 1);
    assert_eq!(calculator.quorum_size(), 3);
    assert_eq!(calculator.prepare_quorum(), 2);
    assert_eq!(calculator.commit_quorum(), 3);

    // Verify 3 votes is enough (1 can be Byzantine)
    assert!(calculator.has_quorum(3));
}

#[test]
fn test_large_scale_deployment() {
    // Arrange - Large scale: 100 replicas, 33 faults
    let config = QuorumConfig::new(100, 33).unwrap();
    let calculator = QuorumCalculator::new(config);

    // Assert
    assert_eq!(calculator.quorum_size(), 67); // 2*33+1
    assert_eq!(calculator.prepare_quorum(), 66); // 2*33
    assert_eq!(calculator.commit_quorum(), 67); // 2*33+1

    // Verify fault tolerance
    let honest_min = 100 - 33;
    assert!(honest_min >= calculator.quorum_size());
}
