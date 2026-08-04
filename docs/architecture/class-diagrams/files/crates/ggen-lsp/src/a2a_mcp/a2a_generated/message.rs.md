# `crates/ggen-lsp/src/a2a_mcp/a2a_generated/message.rs`

Source SHA-256: `c92b42af5bb2efcafdd0e7d1bdf081859a1c056a9ae0fbf251bccea05a2b5172`

```mermaid
classDiagram
    class struct_Message {
      <<struct>>
      +"id: String"
      +"message_type: MessageType"
      +"source: String"
      +"target: Option~String~"
      +"payload: MessagePayload"
      +"metadata: HashMap~String"
      +"timestamp: chrono::DateTime~chrono::Utc~"
      +"priority: MessagePriority"
      +"status: MessageStatus"
    }
    class enum_MessageType {
      <<enum>>
    }
    class struct_MessagePayload {
      <<struct>>
      +"content: serde_json::Value"
      +"schema_version: String"
      +"content_type: String"
    }
    class enum_MessagePriority {
      <<enum>>
    }
    class enum_MessageStatus {
      <<enum>>
    }
    class trait_MessageHandler {
      <<trait>>
      +"handle_message(
        &self, message: Message,
    ) -~ std::pin::Pin~"
      +"can_handle(&self, message_type: &MessageType) -~ bool"
      +"capabilities(&self) -~ MessageCapabilities"
    }
    class struct_MessageResponse {
      <<struct>>
      +"status: ResponseStatus"
      +"payload: Option~MessagePayload~"
      +"metadata: HashMap~String"
      +"in_reply_to: String"
    }
    class enum_ResponseStatus {
      <<enum>>
    }
    class struct_MessageError {
      <<struct>>
      +"message: String"
      +"error_type: MessageErrorType"
      +"message_id: Option~String~"
    }
    class enum_MessageErrorType {
      <<enum>>
    }
    class struct_MessageCapabilities {
      <<struct>>
      +"supported_types: Vec~MessageType~"
      +"max_message_size: usize"
      +"requires_ack: bool"
    }
    class struct_MessageBroker {
      <<struct>>
      +"handlers: HashMap~String"
    }
    class struct_DefaultMessageHandler {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for MessageBroker"
    note "Message"
    note "MessageBroker"
    note "MessageError"
    note "MessageHandler for DefaultMessageHandler"
    note "MessagePayload"
    note "MessageResponse"
    note "std::error::Error for MessageError"
    note "std::fmt::Display for MessageError"
    note "std::fmt::Display for MessageErrorType"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
