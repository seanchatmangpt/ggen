use bytes::Bytes;
use futures::StreamExt;
use ggen_transport::streaming::{StreamBuilder, StreamControl, StreamMessage};
use ggen_transport::SessionId;

#[tokio::test]
async fn test_streaming_basic_send_receive() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id);
    let (mut sender, mut stream) = builder.build();

    sender.send(Bytes::from("message1")).await.unwrap();
    sender.send(Bytes::from("message2")).await.unwrap();

    let msg1 = stream.next().await.unwrap().unwrap();
    let msg2 = stream.next().await.unwrap().unwrap();

    assert_eq!(msg1.sequence, 0);
    assert_eq!(msg1.payload, Bytes::from("message1"));
    assert_eq!(msg2.sequence, 1);
    assert_eq!(msg2.payload, Bytes::from("message2"));
}

#[tokio::test]
async fn test_streaming_sequence_numbering() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id);
    let (mut sender, mut stream) = builder.build();

    for i in 0..10 {
        sender
            .send(Bytes::from(format!("msg{}", i)))
            .await
            .unwrap();
    }

    for i in 0..10 {
        let msg = stream.next().await.unwrap().unwrap();
        assert_eq!(msg.sequence, i);
    }
}

#[tokio::test]
async fn test_streaming_with_metadata() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id);
    let (mut sender, mut stream) = builder.build();

    let metadata = serde_json::json!({"type": "test", "priority": "high"});
    sender
        .send_with_metadata(Bytes::from("test"), metadata.clone())
        .await
        .unwrap();

    let msg = stream.next().await.unwrap().unwrap();
    assert_eq!(msg.metadata.unwrap(), metadata);
}

#[tokio::test]
async fn test_streaming_final_message() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id);
    let (mut sender, mut stream) = builder.build();

    sender.send(Bytes::from("msg1")).await.unwrap();
    sender.send_final(Bytes::from("final")).await.unwrap();

    let msg1 = stream.next().await.unwrap().unwrap();
    let msg2 = stream.next().await.unwrap().unwrap();

    assert!(!msg1.is_final);
    assert!(msg2.is_final);
    assert_eq!(msg2.payload, Bytes::from("final"));
}

#[tokio::test]
async fn test_streaming_resume_from_position() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id).resume_from(100);
    let (mut sender, mut stream) = builder.build();

    sender.send(Bytes::from("test")).await.unwrap();

    let msg = stream.next().await.unwrap().unwrap();
    assert_eq!(msg.sequence, 100);
}

#[tokio::test]
async fn test_streaming_buffer_size() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id).with_buffer_size(5);
    let (mut sender, _stream) = builder.build();

    for i in 0..5 {
        sender
            .send(Bytes::from(format!("msg{}", i)))
            .await
            .unwrap();
    }
}

#[tokio::test]
async fn test_streaming_cursor_tracking() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id);
    let (mut sender, mut stream) = builder.build();

    sender.send(Bytes::from("msg1")).await.unwrap();
    sender.send(Bytes::from("msg2")).await.unwrap();

    stream.next().await.unwrap().unwrap();
    let cursor1 = stream.get_cursor();
    assert_eq!(cursor1.position, 0);

    stream.next().await.unwrap().unwrap();
    let cursor2 = stream.get_cursor();
    assert_eq!(cursor2.position, 1);
}

#[tokio::test]
async fn test_streaming_control_pause_resume() {
    let session_id = SessionId::new();

    let pause_control = StreamControl::pause(session_id.clone());
    assert!(matches!(pause_control.action, ggen_transport::streaming::StreamAction::Pause));

    let resume_control = StreamControl::resume(session_id.clone(), Some(42));
    if let ggen_transport::streaming::StreamAction::Resume { from_position } = resume_control.action {
        assert_eq!(from_position, Some(42));
    } else {
        panic!("Expected Resume action");
    }
}

#[tokio::test]
async fn test_streaming_control_cancel() {
    let session_id = SessionId::new();
    let cancel_control = StreamControl::cancel(session_id);
    assert!(matches!(cancel_control.action, ggen_transport::streaming::StreamAction::Cancel));
}

#[tokio::test]
async fn test_streaming_control_acknowledge() {
    let session_id = SessionId::new();
    let ack_control = StreamControl::acknowledge(session_id, 100);
    if let ggen_transport::streaming::StreamAction::Acknowledge { sequence } = ack_control.action {
        assert_eq!(sequence, 100);
    } else {
        panic!("Expected Acknowledge action");
    }
}

#[tokio::test]
async fn test_streaming_error_propagation() {
    use ggen_transport::TransportError;

    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id);
    let (sender, mut stream) = builder.build();

    let error = TransportError::StreamError("Test error".to_string());
    sender.send_error(error).await.unwrap();

    let result = stream.next().await.unwrap();
    assert!(result.is_err());
}

#[tokio::test]
async fn test_streaming_multiple_messages_concurrent() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id);
    let (mut sender, mut stream) = builder.build();

    tokio::spawn(async move {
        for i in 0..100 {
            sender
                .send(Bytes::from(format!("msg{}", i)))
                .await
                .unwrap();
        }
    });

    let mut count = 0;
    while let Some(Ok(msg)) = stream.next().await {
        assert_eq!(msg.sequence, count);
        count += 1;
        if count == 100 {
            break;
        }
    }
    assert_eq!(count, 100);
}

#[tokio::test]
async fn test_streaming_message_serialization() {
    let session_id = SessionId::new();
    let message = StreamMessage::new(42, session_id, Bytes::from("test"))
        .with_metadata(serde_json::json!({"key": "value"}))
        .mark_final();

    let serialized = serde_json::to_string(&message).unwrap();
    let deserialized: StreamMessage = serde_json::from_str(&serialized).unwrap();

    assert_eq!(deserialized.sequence, 42);
    assert_eq!(deserialized.payload, Bytes::from("test"));
    assert!(deserialized.is_final);
    assert!(deserialized.metadata.is_some());
}

#[tokio::test]
async fn test_streaming_get_current_sequence() {
    let session_id = SessionId::new();
    let builder = StreamBuilder::new(session_id);
    let (mut sender, _stream) = builder.build();

    assert_eq!(sender.get_current_sequence(), 0);

    sender.send(Bytes::from("msg1")).await.unwrap();
    assert_eq!(sender.get_current_sequence(), 1);

    sender.send(Bytes::from("msg2")).await.unwrap();
    assert_eq!(sender.get_current_sequence(), 2);
}
