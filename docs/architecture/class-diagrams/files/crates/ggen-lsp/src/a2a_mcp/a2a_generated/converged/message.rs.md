# `crates/ggen-lsp/src/a2a_mcp/a2a_generated/converged/message.rs`

Source SHA-256: `d7c73b3e7404590f124fced654a2ca942101a1be5ee4734dbaf7324a75157ffc`

```mermaid
classDiagram
    class struct_ConvergedMessage {
      <<struct>>
      +"message_id: String"
      +"source: String"
      +"target: Option~String~"
      +"envelope: MessageEnvelope"
      +"payload: ConvergedPayload"
      +"routing: MessageRouting"
      +"lifecycle: MessageLifecycle"
      +"extensions: Option~HashMap~String"
    }
    class struct_MessageEnvelope {
      <<struct>>
      +"message_type: ConvergedMessageType"
      +"priority: MessagePriority"
      +"timestamp: DateTime~Utc~"
      +"schema_version: String"
      +"content_type: String"
      +"correlation_id: Option~String~"
      +"causation_chain: Option~Vec~String~~"
    }
    class struct_ConvergedPayload {
      <<struct>>
      +"content: UnifiedContent"
      +"context: Option~UnifiedContext~"
      +"hints: Option~MessageHints~"
      +"integrity: Option~MessageIntegrity~"
    }
    class enum_UnifiedContent {
      <<enum>>
    }
    class struct_UnifiedFileContent {
      <<struct>>
      +"name: Option~String~"
      +"mime_type: Option~String~"
      +"bytes: Option~String~"
      +"uri: Option~String~"
      +"size: Option~u64~"
      +"hash: Option~String~"
    }
    class struct_UnifiedContext {
      <<struct>>
      +"tasks: Option~Vec~TaskContext~~"
      +"conversation: Option~ConversationContext~"
      +"domain: Option~DomainContext~"
      +"temporal: Option~TemporalContext~"
    }
    class struct_TaskContext {
      <<struct>>
      +"task_id: String"
      +"status: Option~TaskStatus~"
      +"priority: Option~MessagePriority~"
      +"parent_id: Option~String~"
      +"children: Option~Vec~String~~"
    }
    class struct_ConversationContext {
      <<struct>>
      +"conversation_id: String"
      +"turn_index: usize"
      +"previous_id: Option~String~"
      +"next_id: Option~String~"
    }
    class struct_DomainContext {
      <<struct>>
      +"namespace: String"
      +"version: String"
      +"properties: Option~HashMap~String"
    }
    class struct_TemporalContext {
      <<struct>>
      +"event_time: DateTime~Utc~"
      +"expires_at: Option~DateTime~Utc~~"
      +"valid_for: Option~std::time::Duration~"
    }
    class struct_MessageHints {
      <<struct>>
      +"required_capabilities: Vec~String~"
      +"processing: Option~ProcessingHints~"
      +"security: Option~SecurityHints~"
    }
    class struct_ProcessingHints {
      <<struct>>
      +"parallel_allowed: bool"
      +"estimated_duration: Option~std::time::Duration~"
      +"resources: Option~ResourceRequirements~"
    }
    class struct_SecurityHints {
      <<struct>>
      +"classification: SecurityClassification"
      +"access_control: Vec~String~"
      +"encryption: Option~EncryptionRequirements~"
    }
    class struct_ResourceRequirements {
      <<struct>>
      +"memory: Option~u64~"
      +"cpu: Option~f64~"
      +"storage: Option~u64~"
      +"network_bandwidth: Option~u64~"
    }
    class enum_SecurityClassification {
      <<enum>>
    }
    class struct_EncryptionRequirements {
      <<struct>>
      +"algorithm: String"
      +"key_length: usize"
      +"mode: Option~String~"
    }
    class struct_MessageIntegrity {
      <<struct>>
      +"hash: String"
      +"algorithm: String"
      +"signature: Option~String~"
      +"certificates: Option~Vec~String~~"
    }
    class struct_MessageRouting {
      <<struct>>
      +"path: Vec~String~"
      +"metadata: Option~HashMap~String"
      +"qos: QoSRequirements"
    }
    class struct_QoSRequirements {
      <<struct>>
      +"reliability: ReliabilityLevel"
      +"latency: Option~LatencyRequirements~"
      +"throughput: Option~ThroughputRequirements~"
    }
    class enum_ReliabilityLevel {
      <<enum>>
    }
    class struct_LatencyRequirements {
      <<struct>>
      +"max_latency_ms: u64"
      +"target_latency_ms: u64"
    }
    class struct_ThroughputRequirements {
      <<struct>>
      +"min_mps: f64"
      +"target_mps: f64"
      +"max_mps: f64"
    }
    class struct_MessageLifecycle {
      <<struct>>
      +"state: MessageState"
      +"history: Vec~MessageStateTransition~"
      +"timeout: Option~MessageTimeout~"
    }
    class enum_MessageState {
      <<enum>>
    }
    class struct_MessageStateTransition {
      <<struct>>
      +"from: MessageState"
      +"to: MessageState"
      +"timestamp: DateTime~Utc~"
      +"reason: Option~String~"
      +"metadata: Option~HashMap~String"
    }
    class struct_MessageTimeout {
      <<struct>>
      +"duration: std::time::Duration"
      +"timeout_type: TimeoutType"
      +"expires_at: DateTime~Utc~"
    }
    class enum_TimeoutType {
      <<enum>>
    }
    class enum_ConvergedMessageType {
      <<enum>>
    }
    class enum_MessagePriority {
      <<enum>>
    }
    class enum_TaskStatus {
      <<enum>>
    }
    class struct_ConvergedMessageBuilder {
      <<struct>>
      +"message_id: String"
      +"source: String"
      +"target: Option~String~"
      +"envelope: Option~MessageEnvelope~"
      +"payload: Option~ConvergedPayload~"
      +"routing: Option~MessageRouting~"
      +"lifecycle: Option~MessageLifecycle~"
      +"extensions: Option~HashMap~String"
    }
    class mod_tests {
      <<mod>>
    }
    class enum_HandlerAction {
      <<enum>>
    }
    class enum_HandlerPriority {
      <<enum>>
    }
    class enum_HandlerStatus {
      <<enum>>
    }
    class struct_MessageRouter {
      <<struct>>
      +"id: String"
      +"name: String"
      +"rules: Vec~RoutingRule~"
      +"default_route: Option~Route~"
      +"metadata: Option~HashMap~String"
    }
    class struct_RoutingRule {
      <<struct>>
      +"id: String"
      +"name: String"
      +"condition: RoutingCondition"
      +"action: RouteAction"
      +"priority: i32"
      +"enabled: bool"
      +"metadata: Option~HashMap~String"
    }
    class enum_RoutingCondition {
      <<enum>>
    }
    class enum_RouteAction {
      <<enum>>
    }
    class struct_Route {
      <<struct>>
      +"target: String"
      +"metadata: Option~HashMap~String"
    }
    class fn_default_true {
      <<fn>>
    }
    note "ConvergedMessage"
    note "ConvergedMessageBuilder"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
