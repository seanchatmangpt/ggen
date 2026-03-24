use ggen_transport::session::{ResumeCursor, Session, SessionId, SessionManager};
use std::collections::HashMap;

#[tokio::test]
async fn test_session_creation_and_retrieval() {
    let manager = SessionManager::new(3600);
    let session = manager.create_session().await;

    let retrieved = manager.get_session(&session.id).await.unwrap();
    assert_eq!(retrieved.id.as_str(), session.id.as_str());
    assert!(!retrieved.is_expired());
}

#[tokio::test]
async fn test_session_expiration() {
    let manager = SessionManager::new(1);
    let session = manager.create_session().await;

    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    let result = manager.get_session(&session.id).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_session_touch_extends_expiration() {
    let manager = SessionManager::new(2);
    let session = manager.create_session().await;

    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    manager.touch_session(&session.id).await.unwrap();

    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    let result = manager.get_session(&session.id).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_session_deletion() {
    let manager = SessionManager::new(3600);
    let session = manager.create_session().await;

    manager.delete_session(&session.id).await.unwrap();

    let result = manager.get_session(&session.id).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_session_cleanup_expired() {
    let manager = SessionManager::new(1);

    let _session1 = manager.create_session().await;
    let _session2 = manager.create_session().await;
    let _session3 = manager.create_session().await;

    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    let cleaned = manager.cleanup_expired().await;
    assert_eq!(cleaned, 3);
}

#[tokio::test]
async fn test_resume_cursor_creation() {
    let session_id = SessionId::new();
    let cursor = ResumeCursor::new(session_id.clone(), 42);

    assert_eq!(cursor.session_id.as_str(), session_id.as_str());
    assert_eq!(cursor.position, 42);
}

#[tokio::test]
async fn test_resume_cursor_with_metadata() {
    let session_id = SessionId::new();
    let cursor = ResumeCursor::new(session_id, 100)
        .with_metadata("key1".to_string(), "value1".to_string())
        .with_metadata("key2".to_string(), "value2".to_string());

    assert_eq!(cursor.metadata.get("key1").unwrap(), "value1");
    assert_eq!(cursor.metadata.get("key2").unwrap(), "value2");
}

#[tokio::test]
async fn test_resume_cursor_serialization() {
    let session_id = SessionId::new();
    let cursor = ResumeCursor::new(session_id, 42)
        .with_metadata("test".to_string(), "data".to_string());

    let serialized = cursor.to_string().unwrap();
    let deserialized = ResumeCursor::from_string(&serialized).unwrap();

    assert_eq!(deserialized.position, 42);
    assert_eq!(deserialized.metadata.get("test").unwrap(), "data");
}

#[tokio::test]
async fn test_session_cursor_management() {
    let manager = SessionManager::new(3600);
    let session = manager.create_session().await;

    let cursor = ResumeCursor::new(session.id.clone(), 100);
    manager.set_cursor(&session.id, cursor.clone()).await.unwrap();

    let retrieved_cursor = manager.get_cursor(&session.id).await.unwrap();
    assert!(retrieved_cursor.is_some());
    assert_eq!(retrieved_cursor.unwrap().position, 100);
}

#[tokio::test]
async fn test_session_update() {
    let manager = SessionManager::new(3600);
    let mut session = manager.create_session().await;

    session.metadata.insert("key".to_string(), "value".to_string());
    manager.update_session(&session.id, session.clone()).await.unwrap();

    let retrieved = manager.get_session(&session.id).await.unwrap();
    assert_eq!(retrieved.metadata.get("key").unwrap(), "value");
}

#[tokio::test]
async fn test_session_id_generation_uniqueness() {
    let id1 = SessionId::new();
    let id2 = SessionId::new();
    assert_ne!(id1.as_str(), id2.as_str());
}

#[tokio::test]
async fn test_session_id_from_string() {
    let id_str = "custom-session-id";
    let session_id = SessionId::from_string(id_str.to_string());
    assert_eq!(session_id.as_str(), id_str);
}

#[tokio::test]
async fn test_session_not_found_error() {
    let manager = SessionManager::new(3600);
    let nonexistent_id = SessionId::from_string("nonexistent".to_string());

    let result = manager.get_session(&nonexistent_id).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_session_multiple_cursors() {
    let manager = SessionManager::new(3600);
    let session = manager.create_session().await;

    let cursor1 = ResumeCursor::new(session.id.clone(), 10);
    manager.set_cursor(&session.id, cursor1).await.unwrap();

    let cursor2 = ResumeCursor::new(session.id.clone(), 20);
    manager.set_cursor(&session.id, cursor2).await.unwrap();

    let retrieved = manager.get_cursor(&session.id).await.unwrap();
    assert_eq!(retrieved.unwrap().position, 20);
}

#[tokio::test]
async fn test_session_concurrent_access() {
    let manager = SessionManager::new(3600);
    let session = manager.create_session().await;
    let session_id = session.id.clone();

    let manager_clone1 = manager.clone();
    let session_id_clone1 = session_id.clone();
    let handle1 = tokio::spawn(async move {
        for _ in 0..10 {
            manager_clone1.touch_session(&session_id_clone1).await.ok();
        }
    });

    let manager_clone2 = manager.clone();
    let session_id_clone2 = session_id.clone();
    let handle2 = tokio::spawn(async move {
        for _ in 0..10 {
            manager_clone2.get_session(&session_id_clone2).await.ok();
        }
    });

    handle1.await.unwrap();
    handle2.await.unwrap();

    let result = manager.get_session(&session_id).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_session_metadata() {
    let mut session = Session::new(3600);
    session.metadata.insert("user_id".to_string(), "123".to_string());
    session.metadata.insert("role".to_string(), "admin".to_string());

    assert_eq!(session.metadata.get("user_id").unwrap(), "123");
    assert_eq!(session.metadata.get("role").unwrap(), "admin");
}
