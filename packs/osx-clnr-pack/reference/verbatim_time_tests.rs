#[test]
fn test_snapshot_query_thin_parsing() {
    use osx_clnr::domain::time::{
        identify_thinned_snapshots, parse_snapshot_date, SnapshotThinReceipt,
    };

    // Test parse_snapshot_date
    assert_eq!(
        parse_snapshot_date("com.apple.TimeMachine.2026-05-26-135630.local"),
        Some("2026-05-26-135630".to_string())
    );
    assert_eq!(parse_snapshot_date("invalid-snapshot-name"), None);

    // Test identify_thinned_snapshots
    let before = vec!["snap1".to_string(), "snap2".to_string()];
    let after = vec!["snap2".to_string()];
    let thinned = identify_thinned_snapshots(&before, &after);
    assert_eq!(thinned, vec!["snap1".to_string()]);

    let thinned_none = identify_thinned_snapshots(&before, &before);
    assert!(thinned_none.is_empty());

    // Test SnapshotThinReceipt
    let receipt = SnapshotThinReceipt::new("/".to_string(), 1000000, 1716768000, before, after);
    assert_eq!(receipt.snapshots_thinned, vec!["snap1".to_string()]);
}

#[test]
fn test_size_parsing() {
    use osx_clnr::domain::time::parse_size_in_bytes;

    assert_eq!(parse_size_in_bytes("10GB").unwrap(), 10_000_000_000);
    assert_eq!(parse_size_in_bytes("500mb").unwrap(), 500_000_000);
    assert_eq!(parse_size_in_bytes("2048").unwrap(), 2048);
    assert_eq!(parse_size_in_bytes("2.5 GB").unwrap(), 2_500_000_000);
    assert_eq!(parse_size_in_bytes("100b").unwrap(), 100);

    assert!(parse_size_in_bytes("invalid_size").is_err());
    assert!(parse_size_in_bytes("").is_err());
    assert!(parse_size_in_bytes("abcGB").is_err());
}
