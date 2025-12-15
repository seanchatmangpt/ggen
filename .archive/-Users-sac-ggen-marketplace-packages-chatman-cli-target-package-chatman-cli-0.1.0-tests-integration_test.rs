//! Integration tests for ChatMan CLI

use anyhow::Result;
use chatman_cli::{ChatManager, Config, Message};

#[tokio::test]
async fn test_basic_conversation() -> Result<()> {
    let config = Config::default();
    let mut manager = ChatManager::new(config)?;

    let response = manager.send_message(Message::user("Hello")).await?;

    assert_eq!(response.role, "assistant");
    assert!(!response.content.is_empty());
    assert!(response.content.contains("Echo"));

    Ok(())
}

#[tokio::test]
async fn test_conversation_history() -> Result<()> {
    let config = Config::default();
    let mut manager = ChatManager::new(config)?;

    manager.send_message(Message::user("First message")).await?;
    manager.send_message(Message::user("Second message")).await?;
    manager.send_message(Message::user("Third message")).await?;

    let history = manager.history();
    assert!(history.len() >= 3);

    Ok(())
}

#[tokio::test]
async fn test_history_limit() -> Result<()> {
    let config = Config {
        max_history: 2,
        ..Default::default()
    };
    let mut manager = ChatManager::new(config)?;

    manager.send_message(Message::user("msg1")).await?;
    manager.send_message(Message::user("msg2")).await?;
    manager.send_message(Message::user("msg3")).await?;

    assert!(manager.history().len() <= 2);

    Ok(())
}

#[tokio::test]
async fn test_clear_history() -> Result<()> {
    let config = Config::default();
    let mut manager = ChatManager::new(config)?;

    manager.send_message(Message::user("Test")).await?;
    assert!(!manager.history().is_empty());

    manager.clear_history();
    assert!(manager.history().is_empty());

    Ok(())
}

#[tokio::test]
async fn test_export_json() -> Result<()> {
    let config = Config::default();
    let mut manager = ChatManager::new(config)?;

    manager.send_message(Message::user("Test message")).await?;

    let json = manager.export_json()?;
    assert!(json.contains("Test message"));
    assert!(json.contains("user"));
    assert!(json.contains("assistant"));

    Ok(())
}

#[tokio::test]
async fn test_message_creation() {
    let user_msg = Message::user("test");
    assert_eq!(user_msg.role, "user");
    assert_eq!(user_msg.content, "test");
    assert!(user_msg.timestamp.is_some());

    let assistant_msg = Message::assistant("response");
    assert_eq!(assistant_msg.role, "assistant");

    let system_msg = Message::system("system");
    assert_eq!(system_msg.role, "system");
}

#[tokio::test]
async fn test_config_defaults() {
    let config = Config::default();
    assert_eq!(config.provider, "openai");
    assert_eq!(config.model, "gpt-3.5-turbo");
    assert_eq!(config.max_history, 100);
    assert_eq!(config.timeout_secs, 30);
    assert_eq!(config.retry_attempts, 3);
}

#[tokio::test]
async fn test_concurrent_conversations() -> Result<()> {
    let config = Config::default();

    let handles: Vec<_> = (0..5)
        .map(|i| {
            let config = config.clone();
            tokio::spawn(async move {
                let mut manager = ChatManager::new(config)?;
                manager
                    .send_message(Message::user(&format!("Message {}", i)))
                    .await
            })
        })
        .collect();

    for handle in handles {
        let result = handle.await??;
        assert!(!result.content.is_empty());
    }

    Ok(())
}
