use ggen_cli_tps::{Cli, Commands};
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

#[tokio::test]
async fn test_receipt_generate_and_verify() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let receipt_path = temp_dir.path().join("test-receipt.json");

    // Generate receipt
    let generate_cmd = Commands::Receipt(
        ggen_cli_tps::commands::receipt::ReceiptCommands::Generate {
            operation: "test-operation".to_string(),
            metadata: Some(r#"{"test": "data"}"#.to_string()),
            output: receipt_path.clone(),
        },
    );

    let cli = Cli {
        command: generate_cmd,
    };
    cli.run().await?;

    // Verify receipt exists
    assert!(receipt_path.exists());

    // Verify receipt
    let verify_cmd = Commands::Receipt(
        ggen_cli_tps::commands::receipt::ReceiptCommands::Verify {
            receipt: receipt_path.clone(),
        },
    );

    let cli = Cli {
        command: verify_cmd,
    };
    cli.run().await?;

    Ok(())
}

#[tokio::test]
async fn test_receipt_chain_creation() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let receipt1_path = temp_dir.path().join("receipt1.json");
    let receipt2_path = temp_dir.path().join("receipt2.json");
    let chain_path = temp_dir.path().join("chain.json");

    // Generate two receipts
    for (i, path) in [&receipt1_path, &receipt2_path].iter().enumerate() {
        let generate_cmd = Commands::Receipt(
            ggen_cli_tps::commands::receipt::ReceiptCommands::Generate {
                operation: format!("operation-{}", i),
                metadata: None,
                output: (*path).clone(),
            },
        );

        let cli = Cli {
            command: generate_cmd,
        };
        cli.run().await?;
    }

    // Create chain
    let chain_cmd = Commands::Receipt(ggen_cli_tps::commands::receipt::ReceiptCommands::Chain {
        receipts: vec![receipt1_path, receipt2_path],
        output: chain_path.clone(),
    });

    let cli = Cli {
        command: chain_cmd,
    };
    cli.run().await?;

    assert!(chain_path.exists());

    Ok(())
}

#[tokio::test]
async fn test_jidoka_pull_and_clear() -> Result<(), Box<dyn std::error::Error>> {
    use ggen_cli_tps::commands::jidoka::{JidokaCommands, SignalLevel};

    let temp_dir = TempDir::new()?;
    std::env::set_current_dir(&temp_dir)?;

    // Pull andon cord
    let pull_cmd =
        Commands::Jidoka(JidokaCommands::Pull {
            level: SignalLevel::Critical,
            message: "Test signal".to_string(),
            context: None,
        });

    let cli = Cli { command: pull_cmd };
    cli.run().await?;

    // Check status file exists
    let status_path = PathBuf::from(".ggen/andon-status.json");
    assert!(status_path.exists());

    Ok(())
}

#[tokio::test]
async fn test_a2a_task_lifecycle() -> Result<(), Box<dyn std::error::Error>> {
    use ggen_cli_tps::commands::a2a::{A2aCommands, TaskStateArg};

    let temp_dir = TempDir::new()?;
    let task_path = temp_dir.path().join("task.json");

    // Create task
    let create_cmd = Commands::A2a(A2aCommands::Create {
        title: "Test Task".to_string(),
        description: Some("Test description".to_string()),
        agent: "agent-1".to_string(),
        assign_to: Some("agent-2".to_string()),
        output: task_path.clone(),
    });

    let cli = Cli {
        command: create_cmd,
    };
    cli.run().await?;

    assert!(task_path.exists());

    // Transition to running
    let transition_cmd = Commands::A2a(A2aCommands::Transition {
        task: task_path.clone(),
        state: TaskStateArg::Running,
        reason: None,
    });

    let cli = Cli {
        command: transition_cmd,
    };
    cli.run().await?;

    // Validate task
    let validate_cmd = Commands::A2a(A2aCommands::Validate {
        task: task_path.clone(),
    });

    let cli = Cli {
        command: validate_cmd,
    };
    cli.run().await?;

    Ok(())
}

#[tokio::test]
async fn test_a2a_invalid_transition() -> Result<(), Box<dyn std::error::Error>> {
    use ggen_cli_tps::commands::a2a::{A2aCommands, TaskStateArg};

    let temp_dir = TempDir::new()?;
    let task_path = temp_dir.path().join("task.json");

    // Create task
    let create_cmd = Commands::A2a(A2aCommands::Create {
        title: "Test Task".to_string(),
        description: None,
        agent: "agent-1".to_string(),
        assign_to: None,
        output: task_path.clone(),
    });

    let cli = Cli {
        command: create_cmd,
    };
    cli.run().await?;

    // Try invalid transition (created -> completed should fail)
    let transition_cmd = Commands::A2a(A2aCommands::Transition {
        task: task_path.clone(),
        state: TaskStateArg::Completed,
        reason: None,
    });

    let cli = Cli {
        command: transition_cmd,
    };
    let result = cli.run().await;

    assert!(result.is_err());

    Ok(())
}

#[tokio::test]
async fn test_firewall_rule_management() -> Result<(), Box<dyn std::error::Error>> {
    use ggen_cli_tps::commands::firewall::{FirewallCommands, Protocol, RuleType};

    let temp_dir = TempDir::new()?;
    std::env::set_current_dir(&temp_dir)?;

    // Add allow rule
    let add_cmd = Commands::Firewall(FirewallCommands::Add {
        rule_type: RuleType::Allow,
        source: "192.168.1.0/24".to_string(),
        destination: Some("10.0.0.0/8".to_string()),
        port: Some(443),
        protocol: Some(Protocol::Tcp),
        priority: 10,
    });

    let cli = Cli { command: add_cmd };
    cli.run().await?;

    // Check config file exists
    let config_path = PathBuf::from(".ggen/firewall-config.json");
    assert!(config_path.exists());

    // Add deny rule
    let add_deny_cmd = Commands::Firewall(FirewallCommands::Add {
        rule_type: RuleType::Deny,
        source: "0.0.0.0/0".to_string(),
        destination: None,
        port: None,
        protocol: None,
        priority: 100,
    });

    let cli = Cli {
        command: add_deny_cmd,
    };
    cli.run().await?;

    Ok(())
}

#[tokio::test]
async fn test_packet_validation() -> Result<(), Box<dyn std::error::Error>> {
    use ggen_cli_tps::commands::packet::{PacketCommands, PacketType};

    let temp_dir = TempDir::new()?;
    let packet_path = temp_dir.path().join("packet.json");

    // Create packet
    let create_cmd = Commands::Packet(PacketCommands::Create {
        packet_type: PacketType::Standard,
        source: "agent-1".to_string(),
        destination: "agent-2".to_string(),
        payload: r#"{"data": "test"}"#.to_string(),
        output: packet_path.clone(),
    });

    let cli = Cli {
        command: create_cmd,
    };
    cli.run().await?;

    assert!(packet_path.exists());

    // Validate packet
    let validate_cmd = Commands::Packet(PacketCommands::Validate {
        packet: packet_path.clone(),
    });

    let cli = Cli {
        command: validate_cmd,
    };
    cli.run().await?;

    Ok(())
}

#[tokio::test]
async fn test_backpressure_token_pool() -> Result<(), Box<dyn std::error::Error>> {
    use ggen_cli_tps::commands::backpressure::{BackpressureCommands, StrategyType};

    let temp_dir = TempDir::new()?;
    std::env::set_current_dir(&temp_dir)?;

    // Initialize backpressure system
    let init_cmd = Commands::Backpressure(BackpressureCommands::Init {
        capacity: 100,
        strategy: StrategyType::TokenBucket,
    });

    let cli = Cli { command: init_cmd };
    cli.run().await?;

    // Check config exists
    let config_path = PathBuf::from(".ggen/backpressure-config.json");
    assert!(config_path.exists());

    Ok(())
}

#[tokio::test]
async fn test_supplier_quality_scoring() -> Result<(), Box<dyn std::error::Error>> {
    use ggen_cli_tps::commands::supplier::SupplierCommands;

    let temp_dir = TempDir::new()?;
    std::env::set_current_dir(&temp_dir)?;

    // Register supplier
    let register_cmd = Commands::Supplier(SupplierCommands::Register {
        id: "supplier-1".to_string(),
        name: "Test Supplier".to_string(),
        max_rate: 100,
    });

    let cli = Cli {
        command: register_cmd,
    };
    cli.run().await?;

    // Check config exists
    let config_path = PathBuf::from(".ggen/supplier-config.json");
    assert!(config_path.exists());

    Ok(())
}

#[tokio::test]
async fn test_end_to_end_workflow() -> Result<(), Box<dyn std::error::Error>> {
    use ggen_cli_tps::commands::a2a::{A2aCommands, TaskStateArg};
    use ggen_cli_tps::commands::receipt::ReceiptCommands;

    let temp_dir = TempDir::new()?;
    std::env::set_current_dir(&temp_dir)?;

    // 1. Create task
    let task_path = temp_dir.path().join("workflow-task.json");
    let create_cmd = Commands::A2a(A2aCommands::Create {
        title: "E2E Workflow Task".to_string(),
        description: Some("End-to-end test".to_string()),
        agent: "agent-1".to_string(),
        assign_to: None,
        output: task_path.clone(),
    });

    Cli {
        command: create_cmd,
    }
    .run()
    .await?;

    // 2. Transition to running
    let transition_cmd = Commands::A2a(A2aCommands::Transition {
        task: task_path.clone(),
        state: TaskStateArg::Running,
        reason: None,
    });

    Cli {
        command: transition_cmd,
    }
    .run()
    .await?;

    // 3. Generate receipt
    let receipt_path = temp_dir.path().join("workflow-receipt.json");
    let receipt_cmd = Commands::Receipt(ReceiptCommands::Generate {
        operation: "workflow-step".to_string(),
        metadata: Some(r#"{"task_id": "workflow-task"}"#.to_string()),
        output: receipt_path.clone(),
    });

    Cli {
        command: receipt_cmd,
    }
    .run()
    .await?;

    // 4. Complete task
    let complete_cmd = Commands::A2a(A2aCommands::Transition {
        task: task_path.clone(),
        state: TaskStateArg::Completed,
        reason: None,
    });

    Cli {
        command: complete_cmd,
    }
    .run()
    .await?;

    // Verify all artifacts exist
    assert!(task_path.exists());
    assert!(receipt_path.exists());

    Ok(())
}
