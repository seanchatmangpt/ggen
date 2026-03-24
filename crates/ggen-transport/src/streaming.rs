use crate::error::{Result, TransportError};
use crate::session::{ResumeCursor, SessionId};
use bytes::Bytes;
use futures::stream::Stream;
use serde::{Deserialize, Serialize};
use std::pin::Pin;
use std::task::{Context, Poll};
use tokio::sync::mpsc;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StreamMessage {
    pub sequence: u64,
    pub session_id: SessionId,
    pub payload: Bytes,
    pub is_final: bool,
    pub metadata: Option<serde_json::Value>,
}

impl StreamMessage {
    pub fn new(sequence: u64, session_id: SessionId, payload: Bytes) -> Self {
        Self {
            sequence,
            session_id,
            payload,
            is_final: false,
            metadata: None,
        }
    }

    pub fn with_metadata(mut self, metadata: serde_json::Value) -> Self {
        self.metadata = Some(metadata);
        self
    }

    pub fn mark_final(mut self) -> Self {
        self.is_final = true;
        self
    }
}

pub struct MessageStream {
    receiver: mpsc::Receiver<Result<StreamMessage>>,
    session_id: SessionId,
    current_position: u64,
}

impl MessageStream {
    pub fn new(receiver: mpsc::Receiver<Result<StreamMessage>>, session_id: SessionId) -> Self {
        Self {
            receiver,
            session_id,
            current_position: 0,
        }
    }

    pub fn get_cursor(&self) -> ResumeCursor {
        ResumeCursor::new(self.session_id.clone(), self.current_position)
    }

    pub async fn next(&mut self) -> Option<Result<StreamMessage>> {
        match self.receiver.recv().await {
            Some(Ok(msg)) => {
                self.current_position = msg.sequence;
                Some(Ok(msg))
            }
            Some(Err(e)) => Some(Err(e)),
            None => None,
        }
    }
}

impl Stream for MessageStream {
    type Item = Result<StreamMessage>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.receiver.poll_recv(cx)
    }
}

pub struct StreamBuilder {
    session_id: SessionId,
    buffer_size: usize,
    resume_from: Option<u64>,
}

impl StreamBuilder {
    pub fn new(session_id: SessionId) -> Self {
        Self {
            session_id,
            buffer_size: 100,
            resume_from: None,
        }
    }

    pub fn with_buffer_size(mut self, size: usize) -> Self {
        self.buffer_size = size;
        self
    }

    pub fn resume_from(mut self, position: u64) -> Self {
        self.resume_from = Some(position);
        self
    }

    pub fn build(self) -> (StreamSender, MessageStream) {
        let (tx, rx) = mpsc::channel(self.buffer_size);
        let sender = StreamSender {
            sender: tx,
            session_id: self.session_id.clone(),
            next_sequence: self.resume_from.unwrap_or(0),
        };
        let stream = MessageStream::new(rx, self.session_id);
        (sender, stream)
    }
}

pub struct StreamSender {
    sender: mpsc::Sender<Result<StreamMessage>>,
    session_id: SessionId,
    next_sequence: u64,
}

impl StreamSender {
    pub async fn send(&mut self, payload: Bytes) -> Result<()> {
        let msg = StreamMessage::new(self.next_sequence, self.session_id.clone(), payload);
        self.next_sequence += 1;
        self.sender
            .send(Ok(msg))
            .await
            .map_err(|_| TransportError::StreamError("Channel closed".to_string()))
    }

    pub async fn send_with_metadata(
        &mut self,
        payload: Bytes,
        metadata: serde_json::Value,
    ) -> Result<()> {
        let msg = StreamMessage::new(self.next_sequence, self.session_id.clone(), payload)
            .with_metadata(metadata);
        self.next_sequence += 1;
        self.sender
            .send(Ok(msg))
            .await
            .map_err(|_| TransportError::StreamError("Channel closed".to_string()))
    }

    pub async fn send_final(&mut self, payload: Bytes) -> Result<()> {
        let msg = StreamMessage::new(self.next_sequence, self.session_id.clone(), payload)
            .mark_final();
        self.next_sequence += 1;
        self.sender
            .send(Ok(msg))
            .await
            .map_err(|_| TransportError::StreamError("Channel closed".to_string()))
    }

    pub async fn send_error(&self, error: TransportError) -> Result<()> {
        self.sender
            .send(Err(error))
            .await
            .map_err(|_| TransportError::StreamError("Channel closed".to_string()))
    }

    pub fn get_current_sequence(&self) -> u64 {
        self.next_sequence
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StreamControl {
    pub session_id: SessionId,
    pub action: StreamAction,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StreamAction {
    Pause,
    Resume { from_position: Option<u64> },
    Cancel,
    Acknowledge { sequence: u64 },
}

impl StreamControl {
    pub fn pause(session_id: SessionId) -> Self {
        Self {
            session_id,
            action: StreamAction::Pause,
        }
    }

    pub fn resume(session_id: SessionId, from_position: Option<u64>) -> Self {
        Self {
            session_id,
            action: StreamAction::Resume { from_position },
        }
    }

    pub fn cancel(session_id: SessionId) -> Self {
        Self {
            session_id,
            action: StreamAction::Cancel,
        }
    }

    pub fn acknowledge(session_id: SessionId, sequence: u64) -> Self {
        Self {
            session_id,
            action: StreamAction::Acknowledge { sequence },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_stream_basic() {
        let session_id = SessionId::new();
        let builder = StreamBuilder::new(session_id);
        let (mut sender, mut stream) = builder.build();

        sender.send(Bytes::from("test")).await.unwrap();
        let msg = stream.next().await.unwrap().unwrap();
        assert_eq!(msg.sequence, 0);
        assert_eq!(msg.payload, Bytes::from("test"));
    }

    #[tokio::test]
    async fn test_stream_sequence() {
        let session_id = SessionId::new();
        let builder = StreamBuilder::new(session_id);
        let (mut sender, mut stream) = builder.build();

        sender.send(Bytes::from("1")).await.unwrap();
        sender.send(Bytes::from("2")).await.unwrap();
        sender.send(Bytes::from("3")).await.unwrap();

        let msg1 = stream.next().await.unwrap().unwrap();
        let msg2 = stream.next().await.unwrap().unwrap();
        let msg3 = stream.next().await.unwrap().unwrap();

        assert_eq!(msg1.sequence, 0);
        assert_eq!(msg2.sequence, 1);
        assert_eq!(msg3.sequence, 2);
    }

    #[tokio::test]
    async fn test_stream_final() {
        let session_id = SessionId::new();
        let builder = StreamBuilder::new(session_id);
        let (mut sender, mut stream) = builder.build();

        sender.send_final(Bytes::from("final")).await.unwrap();
        let msg = stream.next().await.unwrap().unwrap();
        assert!(msg.is_final);
    }

    #[tokio::test]
    async fn test_stream_resume() {
        let session_id = SessionId::new();
        let builder = StreamBuilder::new(session_id).resume_from(10);
        let (mut sender, mut stream) = builder.build();

        sender.send(Bytes::from("test")).await.unwrap();
        let msg = stream.next().await.unwrap().unwrap();
        assert_eq!(msg.sequence, 10);
    }
}
