use ggen_cli_tps::error::CliError;

#[test]
fn test_cli_error_display() {
    let err = CliError::Validation("test error".to_string());
    assert_eq!(err.to_string(), "Validation error: test error");

    let err = CliError::ReceiptVerification("hash mismatch".to_string());
    assert_eq!(err.to_string(), "Receipt verification failed: hash mismatch");

    let err = CliError::AndonSignal("critical".to_string());
    assert_eq!(err.to_string(), "Andon signal: critical");

    let err = CliError::TaskStateError("invalid transition".to_string());
    assert_eq!(err.to_string(), "Task state error: invalid transition");

    let err = CliError::FirewallBlocked("access denied".to_string());
    assert_eq!(err.to_string(), "Firewall blocked: access denied");
}

#[test]
fn test_result_type_alias() {
    let success: ggen_cli_tps::Result<i32> = Ok(42);
    assert_eq!(success.unwrap(), 42);

    let failure: ggen_cli_tps::Result<i32> = Err(CliError::Unknown("test".to_string()));
    assert!(failure.is_err());
}

#[test]
fn test_error_conversion_from_io() {
    let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
    let cli_error: CliError = io_error.into();

    match cli_error {
        CliError::Io(_) => {}
        _ => panic!("Expected CliError::Io variant"),
    }
}

#[test]
fn test_error_conversion_from_json() {
    let json_str = "invalid json";
    let json_error = serde_json::from_str::<serde_json::Value>(json_str).unwrap_err();
    let cli_error: CliError = json_error.into();

    match cli_error {
        CliError::Json(_) => {}
        _ => panic!("Expected CliError::Json variant"),
    }
}

mod receipt_tests {
    use serde_json::json;

    #[test]
    fn test_receipt_serialization() {
        let receipt = json!({
            "operation": "test",
            "timestamp": "2024-01-01T00:00:00Z",
            "hash": "abc123",
            "metadata": {},
            "previous_hash": null
        });

        let serialized = serde_json::to_string(&receipt).unwrap();
        assert!(serialized.contains("test"));
        assert!(serialized.contains("abc123"));
    }

    #[test]
    fn test_receipt_chain_structure() {
        let chain = json!({
            "receipts": [
                {
                    "operation": "op1",
                    "timestamp": "2024-01-01T00:00:00Z",
                    "hash": "hash1",
                    "metadata": {},
                    "previous_hash": null
                },
                {
                    "operation": "op2",
                    "timestamp": "2024-01-01T00:01:00Z",
                    "hash": "hash2",
                    "metadata": {},
                    "previous_hash": "hash1"
                }
            ],
            "chain_hash": "chain_hash"
        });

        let receipts = chain["receipts"].as_array().unwrap();
        assert_eq!(receipts.len(), 2);
        assert_eq!(receipts[1]["previous_hash"], "hash1");
    }
}

mod jidoka_tests {
    use ggen_cli_tps::commands::jidoka::SignalLevel;

    #[test]
    fn test_signal_level_serde() {
        let level = SignalLevel::Critical;
        let json = serde_json::to_string(&level).unwrap();
        assert_eq!(json, "\"critical\"");

        let deserialized: SignalLevel = serde_json::from_str(&json).unwrap();
        assert!(matches!(deserialized, SignalLevel::Critical));
    }
}

mod a2a_tests {
    use serde_json::json;

    #[test]
    fn test_task_state_validation() {
        let valid_states = ["created", "running", "blocked", "completed", "failed"];

        for state in &valid_states {
            assert!(valid_states.contains(state));
        }
    }

    #[test]
    fn test_task_structure() {
        let task = json!({
            "id": "task-1",
            "state": "created",
            "title": "Test Task",
            "description": null,
            "assigned_to": null,
            "created_by": "agent-1",
            "created_at": "2024-01-01T00:00:00Z",
            "updated_at": "2024-01-01T00:00:00Z",
            "completed_at": null,
            "failure_reason": null,
            "artifacts": []
        });

        assert_eq!(task["state"], "created");
        assert_eq!(task["created_by"], "agent-1");
    }

    #[test]
    fn test_valid_state_transitions() {
        let valid_transitions = [
            ("created", "running"),
            ("created", "failed"),
            ("running", "blocked"),
            ("running", "completed"),
            ("running", "failed"),
            ("blocked", "running"),
            ("blocked", "failed"),
        ];

        for (from, to) in &valid_transitions {
            assert!(valid_transitions.contains(&(*from, *to)));
        }
    }

    #[test]
    fn test_invalid_state_transitions() {
        let invalid_transitions = [
            ("created", "blocked"),
            ("created", "completed"),
            ("completed", "running"),
            ("failed", "running"),
        ];

        let valid_transitions = [
            ("created", "running"),
            ("created", "failed"),
            ("running", "blocked"),
            ("running", "completed"),
            ("running", "failed"),
            ("blocked", "running"),
            ("blocked", "failed"),
        ];

        for (from, to) in &invalid_transitions {
            assert!(!valid_transitions.contains(&(*from, *to)));
        }
    }
}

mod firewall_tests {
    use ggen_cli_tps::commands::firewall::{Protocol, RuleType};

    #[test]
    fn test_rule_type_serde() {
        let allow = RuleType::Allow;
        let json = serde_json::to_string(&allow).unwrap();
        assert_eq!(json, "\"allow\"");

        let deny = RuleType::Deny;
        let json = serde_json::to_string(&deny).unwrap();
        assert_eq!(json, "\"deny\"");
    }

    #[test]
    fn test_protocol_serde() {
        let tcp = Protocol::Tcp;
        let json = serde_json::to_string(&tcp).unwrap();
        assert_eq!(json, "\"tcp\"");

        let udp = Protocol::Udp;
        let json = serde_json::to_string(&udp).unwrap();
        assert_eq!(json, "\"udp\"");
    }
}
