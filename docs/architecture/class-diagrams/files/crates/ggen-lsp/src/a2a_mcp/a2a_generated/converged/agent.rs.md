# `crates/ggen-lsp/src/a2a_mcp/a2a_generated/converged/agent.rs`

Source SHA-256: `f942146b069c67d1deaad28530cb834ed03b65565d8c910d7b1e95557513e9ae`

```mermaid
classDiagram
    class struct_AuthenticationRequirements {
      <<struct>>
      +"methods: Vec~String~"
      +"min_security_level: Option~u32~"
    }
    class struct_AuthorizationRequirements {
      <<struct>>
      +"roles: Vec~String~"
      +"permissions: Vec~String~"
    }
    class struct_UnifiedAgent {
      <<struct>>
      +"identity: AgentIdentity"
      +"capabilities: AgentCapabilities"
      +"lifecycle: AgentLifecycle"
      +"communication: AgentCommunication"
      +"execution: AgentExecution"
      +"security: AgentSecurity"
      +"extensions: Option~HashMap~String"
    }
    class struct_AgentIdentity {
      <<struct>>
      +"id: String"
      +"name: String"
      +"agent_type: String"
      +"version: String"
      +"namespace: String"
      +"tags: Option~Vec~String~~"
    }
    class struct_AgentCapabilities {
      <<struct>>
      +"primary: Vec~Capability~"
      +"secondary: Option~Vec~Capability~~"
      +"protocols: Vec~AgentProtocol~"
      +"formats: Vec~DataFormat~"
      +"message_types: Vec~ConvergedMessageType~"
      +"qos_levels: Vec~QoSLevel~"
      +"constraints: Option~ResourceConstraints~"
    }
    class struct_Capability {
      <<struct>>
      +"name: String"
      +"version: String"
      +"description: Option~String~"
      +"requirements: Option~HashMap~String"
      +"metadata: Option~HashMap~String"
    }
    class enum_AgentProtocol {
      <<enum>>
    }
    class enum_DataFormat {
      <<enum>>
    }
    class enum_QoSLevel {
      <<enum>>
    }
    class struct_ResourceConstraints {
      <<struct>>
      +"memory: Option~ResourceLimit~"
      +"cpu: Option~ResourceLimit~"
      +"storage: Option~ResourceLimit~"
      +"network: Option~ResourceLimit~"
      +"concurrent_ops: Option~ResourceLimit~"
    }
    class struct_ResourceLimit {
      <<struct>>
      +"min: Option~u64~"
      +"max: Option~u64~"
      +"default: Option~u64~"
      +"unit: ResourceUnit"
    }
    class enum_ResourceUnit {
      <<enum>>
    }
    class struct_AgentLifecycle {
      <<struct>>
      +"state: AgentState"
      +"state_history: Vec~AgentStateTransition~"
      +"health: AgentHealth"
      +"metrics: Option~AgentMetrics~"
      +"configuration: AgentConfiguration"
      +"dependencies: Option~AgentDependencies~"
      +"timeouts: Option~AgentTimeouts~"
    }
    class enum_AgentState {
      <<enum>>
    }
    class struct_AgentHealth {
      <<struct>>
      +"status: HealthStatus"
      +"last_check: DateTime~Utc~"
      +"check_interval: std::time::Duration"
      +"metrics: Option~HealthMetrics~"
      +"warnings: Option~Vec~HealthWarning~~"
      +"errors: Option~Vec~HealthError~~"
    }
    class enum_HealthStatus {
      <<enum>>
    }
    class struct_HealthMetrics {
      <<struct>>
      +"response_time: ResponseTimeMetrics"
      +"error_rate: ErrorRateMetrics"
      +"resource_usage: ResourceUsageMetrics"
      +"throughput: Option~ThroughputMetrics~"
    }
    class struct_ResponseTimeMetrics {
      <<struct>>
      +"average: std::time::Duration"
      +"p95: std::time::Duration"
      +"p99: std::time::Duration"
      +"maximum: std::time::Duration"
      +"minimum: std::time::Duration"
    }
    class struct_ErrorRateMetrics {
      <<struct>>
      +"rate: f64"
      +"total: u64"
      +"errors: u64"
      +"trend: Option~ErrorTrend~"
    }
    class enum_ErrorTrend {
      <<enum>>
    }
    class struct_ResourceUsageMetrics {
      <<struct>>
      +"memory_percent: f64"
      +"cpu_percent: f64"
      +"disk_percent: f64"
      +"network_bytes: u64"
    }
    class struct_ThroughputMetrics {
      <<struct>>
      +"requests_per_second: f64"
      +"messages_per_second: f64"
      +"processing_rate: Option~f64~"
      +"queued: Option~u64~"
    }
    class struct_HealthWarning {
      <<struct>>
      +"message: String"
      +"timestamp: DateTime~Utc~"
      +"severity: WarningSeverity"
      +"category: String"
      +"details: Option~HashMap~String"
    }
    class enum_WarningSeverity {
      <<enum>>
    }
    class struct_HealthError {
      <<struct>>
      +"message: String"
      +"timestamp: DateTime~Utc~"
      +"error_type: String"
      +"severity: ErrorSeverity"
      +"stack_trace: Option~String~"
      +"details: Option~HashMap~String"
    }
    class enum_ErrorSeverity {
      <<enum>>
    }
    class struct_AgentStateTransition {
      <<struct>>
      +"from: AgentState"
      +"to: AgentState"
      +"timestamp: DateTime~Utc~"
      +"reason: Option~String~"
      +"details: Option~HashMap~String"
    }
    class struct_AgentMetrics {
      <<struct>>
      +"performance: PerformanceMetrics"
      +"reliability: ReliabilityMetrics"
      +"efficiency: Option~EfficiencyMetrics~"
      +"scalability: Option~ScalabilityMetrics~"
      +"business: Option~BusinessMetrics~"
    }
    class struct_PerformanceMetrics {
      <<struct>>
      +"processing_time: ProcessingMetrics"
      +"response_time: ResponseMetrics"
      +"throughput: ThroughputMetrics"
      +"resource_utilization: ResourceUtilizationMetrics"
    }
    class struct_ProcessingMetrics {
      <<struct>>
      +"average: std::time::Duration"
      +"maximum: std::time::Duration"
      +"minimum: std::time::Duration"
      +"throughput: f64"
    }
    class struct_ResponseMetrics {
      <<struct>>
      +"average: std::time::Duration"
      +"median: std::time::Duration"
      +"p95: std::time::Duration"
      +"p99: std::time::Duration"
      +"maximum: std::time::Duration"
    }
    class struct_ResourceUtilizationMetrics {
      <<struct>>
      +"cpu: f64"
      +"memory: f64"
      +"disk: f64"
      +"network: f64"
    }
    class struct_ReliabilityMetrics {
      <<struct>>
      +"uptime: f64"
      +"error_rate: f64"
      +"success_rate: f64"
      +"mtbf: Option~std::time::Duration~"
      +"mttr: Option~std::time::Duration~"
    }
    class struct_EfficiencyMetrics {
      <<struct>>
      +"cost_efficiency: f64"
      +"resource_efficiency: f64"
      +"time_efficiency: f64"
      +"operational_efficiency: f64"
    }
    class struct_ScalabilityMetrics {
      <<struct>>
      +"horizontal: f64"
      +"vertical: f64"
      +"elasticity: f64"
      +"load_distribution: f64"
    }
    class struct_BusinessMetrics {
      <<struct>>
      +"user_satisfaction: f64"
      +"business_value: f64"
      +"roi: f64"
      +"customer_satisfaction: f64"
    }
    class struct_AgentConfiguration {
      <<struct>>
      +"parameters: HashMap~String"
      +"version: String"
      +"timestamp: DateTime~Utc~"
      +"source: Option~String~"
      +"validation: Option~ConfigurationValidation~"
    }
    class struct_ConfigurationValidation {
      <<struct>>
      +"valid: bool"
      +"errors: Option~Vec~ValidationError~~"
      +"warnings: Option~Vec~ValidationWarning~~"
    }
    class struct_ValidationError {
      <<struct>>
      +"message: String"
      +"field: String"
      +"error_type: String"
      +"details: Option~HashMap~String"
    }
    class struct_ValidationWarning {
      <<struct>>
      +"message: String"
      +"field: String"
      +"warning_type: String"
      +"details: Option~HashMap~String"
    }
    class struct_AgentDependencies {
      <<struct>>
      +"required: Vec~Dependency~"
      +"optional: Option~Vec~Dependency~~"
      +"resolution_status: DependencyResolutionStatus"
    }
    class struct_Dependency {
      <<struct>>
      +"name: String"
      +"version: String"
      +"dependency_type: DependencyType"
      +"source: Option~String~"
      +"status: DependencyStatus"
    }
    class enum_DependencyType {
      <<enum>>
    }
    class enum_DependencyStatus {
      <<enum>>
    }
    class enum_DependencyResolutionStatus {
      <<enum>>
    }
    class struct_AgentTimeouts {
      <<struct>>
      +"initialization: Option~std::time::Duration~"
      +"processing: Option~std::time::Duration~"
      +"response: Option~std::time::Duration~"
      +"health_check: Option~std::time::Duration~"
      +"communication: Option~std::time::Duration~"
    }
    class struct_AgentCommunication {
      <<struct>>
      +"endpoints: Vec~CommunicationEndpoint~"
      +"protocols: Vec~AgentProtocol~"
      +"handlers: Option~MessageHandlers~"
      +"security: Option~CommunicationSecurity~"
      +"qos: CommunicationQoS"
    }
    class struct_CommunicationEndpoint {
      <<struct>>
      +"url: String"
      +"endpoint_type: EndpointType"
      +"authentication: Option~EndpointAuthentication~"
      +"metadata: Option~HashMap~String"
    }
    class enum_EndpointType {
      <<enum>>
    }
    class enum_EndpointAuthentication {
      <<enum>>
    }
    class struct_MessageHandlers {
      <<struct>>
      +"request_handlers: Vec~MessageHandler~"
      +"response_handlers: Vec~MessageHandler~"
      +"event_handlers: Vec~MessageHandler~"
      +"error_handlers: Vec~MessageHandler~"
    }
    class struct_MessageHandler {
      <<struct>>
      +"name: String"
      +"handler_type: HandlerType"
      +"priority: HandlerPriority"
      +"configuration: HashMap~String"
      +"metadata: Option~HashMap~String"
    }
    class enum_HandlerType {
      <<enum>>
    }
    class enum_HandlerPriority {
      <<enum>>
    }
    class struct_CommunicationSecurity {
      <<struct>>
      +"protocols: Vec~SecurityProtocol~"
      +"encryption: EncryptionRequirements"
      +"authentication: AuthenticationRequirements"
      +"authorization: AuthorizationRequirements"
    }
    class enum_SecurityProtocol {
      <<enum>>
    }
    class struct_CommunicationQoS {
      <<struct>>
      +"reliability: ReliabilityLevel"
      +"latency: Option~LatencyRequirements~"
      +"throughput: Option~ThroughputRequirements~"
      +"ordering: Option~OrderingRequirements~"
      +"flow_control: Option~FlowControlRequirements~"
    }
    class struct_OrderingRequirements {
      <<struct>>
      +"guarantee: OrderingGuarantee"
      +"max_out_of_order: Option~usize~"
      +"reordering_window_size: Option~usize~"
    }
    class enum_OrderingGuarantee {
      <<enum>>
    }
    class struct_FlowControlRequirements {
      <<struct>>
      +"control_type: FlowControlType"
      +"window_size: Option~usize~"
      +"timeout: Option~std::time::Duration~"
      +"backpressure: Option~BackpressureHandling~"
    }
    class enum_FlowControlType {
      <<enum>>
    }
    class enum_BackpressureHandling {
      <<enum>>
    }
    class struct_AgentExecution {
      <<struct>>
      +"mode: ExecutionMode"
      +"parameters: HashMap~String"
      +"context: Option~ExecutionContext~"
      +"strategy: Option~ExecutionStrategy~"
      +"monitoring: Option~ExecutionMonitoring~"
      +"policies: Option~ExecutionPolicies~"
    }
    class enum_ExecutionMode {
      <<enum>>
    }
    class struct_ExecutionContext {
      <<struct>>
      +"data: HashMap~String"
      +"metadata: Option~HashMap~String"
      +"lifecycle: Option~ContextLifecycle~"
      +"validation: Option~ContextValidation~"
    }
    class enum_ContextLifecycle {
      <<enum>>
    }
    class struct_ContextValidation {
      <<struct>>
      +"rules: Vec~ValidationRule~"
      +"mode: ValidationMode"
      +"results: Option~ValidationResults~"
    }
    class struct_ValidationRule {
      <<struct>>
      +"name: String"
      +"rule_type: ValidationRuleType"
      +"condition: String"
      +"action: ValidationAction"
      +"severity: ValidationSeverity"
    }
    class enum_ValidationRuleType {
      <<enum>>
    }
    class enum_ValidationAction {
      <<enum>>
    }
    class enum_ValidationSeverity {
      <<enum>>
    }
    class enum_ValidationMode {
      <<enum>>
    }
    class struct_ValidationResults {
      <<struct>>
      +"status: ValidationStatus"
      +"errors: Option~Vec~ValidationError~~"
      +"warnings: Option~Vec~ValidationWarning~~"
      +"metadata: Option~HashMap~String"
    }
    class enum_ValidationStatus {
      <<enum>>
    }
    class struct_ExecutionStrategy {
      <<struct>>
      +"strategy_type: StrategyType"
      +"configuration: HashMap~String"
      +"parameters: Option~HashMap~String"
      +"metadata: Option~HashMap~String"
    }
    class enum_StrategyType {
      <<enum>>
    }
    class struct_ExecutionMonitoring {
      <<struct>>
      +"metrics: MonitoringMetrics"
      +"thresholds: MonitoringThresholds"
      +"alerts: Option~Vec~MonitoringAlert~~"
      +"dashboards: Option~Vec~MonitoringDashboard~~"
    }
    class struct_MonitoringMetrics {
      <<struct>>
      +"performance: PerformanceMetrics"
      +"resources: ResourceUsageMetrics"
      +"business: Option~BusinessMetrics~"
      +"custom: Option~HashMap~String"
    }
    class struct_MonitoringThresholds {
      <<struct>>
      +"performance: Thresholds"
      +"resources: Thresholds"
      +"business: Option~Thresholds~"
      +"custom: Option~HashMap~String"
    }
    class struct_Thresholds {
      <<struct>>
      +"warning: f64"
      +"critical: f64"
      +"unit: String"
      +"operator: ComparisonOperator"
    }
    class enum_ComparisonOperator {
      <<enum>>
    }
    class struct_MonitoringAlert {
      <<struct>>
      +"name: String"
      +"alert_type: String"
      +"severity: AlertSeverity"
      +"message: String"
      +"timestamp: DateTime~Utc~"
      +"details: Option~HashMap~String"
      +"metadata: Option~HashMap~String"
    }
    class enum_AlertSeverity {
      <<enum>>
    }
    class struct_MonitoringDashboard {
      <<struct>>
      +"name: String"
      +"dashboard_type: String"
      +"description: Option~String~"
      +"widgets: Vec~DashboardWidget~"
      +"metadata: Option~HashMap~String"
    }
    class struct_DashboardWidget {
      <<struct>>
      +"name: String"
      +"widget_type: String"
      +"configuration: HashMap~String"
      +"position: WidgetPosition"
      +"size: WidgetSize"
    }
    class struct_WidgetPosition {
      <<struct>>
      +"x: u32"
      +"y: u32"
      +"z: u32"
    }
    class struct_WidgetSize {
      <<struct>>
      +"width: u32"
      +"height: u32"
      +"unit: String"
    }
    class struct_ExecutionPolicies {
      <<struct>>
      +"retry: RetryPolicy"
      +"timeout: TimeoutPolicy"
      +"circuit_breaker: CircuitBreakerPolicy"
      +"bulkhead: BulkheadPolicy"
      +"custom: Option~HashMap~String"
    }
    class struct_RetryPolicy {
      <<struct>>
      +"max_retries: u32"
      +"delay: std::time::Duration"
      +"backoff: BackoffStrategy"
      +"conditions: Option~Vec~RetryCondition~~"
    }
    class enum_BackoffStrategy {
      <<enum>>
    }
    class struct_RetryCondition {
      <<struct>>
      +"condition_type: ConditionType"
      +"expression: String"
      +"action: RetryAction"
      +"metadata: Option~HashMap~String"
    }
    class enum_ConditionType {
      <<enum>>
    }
    class enum_RetryAction {
      <<enum>>
    }
    class struct_TimeoutPolicy {
      <<struct>>
      +"duration: std::time::Duration"
      +"timeout_type: TimeoutType"
      +"behavior: TimeoutBehavior"
      +"metadata: Option~HashMap~String"
    }
    class enum_TimeoutType {
      <<enum>>
    }
    class enum_TimeoutBehavior {
      <<enum>>
    }
    class struct_CircuitBreakerPolicy {
      <<struct>>
      +"failure_threshold: f64"
      +"recovery_timeout: std::time::Duration"
      +"half_open_requests: u32"
      +"status: CircuitBreakerStatus"
      +"metadata: Option~HashMap~String"
    }
    class enum_CircuitBreakerStatus {
      <<enum>>
    }
    class struct_BulkheadPolicy {
      <<struct>>
      +"max_concurrent_ops: u32"
      +"max_pending_ops: u32"
      +"queue_timeout: Option~std::time::Duration~"
      +"metadata: Option~HashMap~String"
    }
    class struct_AgentSecurity {
      <<struct>>
      +"authentication: AuthenticationConfig"
      +"authorization: AuthorizationConfig"
      +"encryption: EncryptionConfig"
      +"audit: AuditConfig"
      +"compliance: Option~ComplianceConfig~"
      +"policies: Option~SecurityPolicies~"
    }
    class struct_AuthenticationConfig {
      <<struct>>
      +"methods: Vec~AuthenticationMethod~"
      +"providers: Option~Vec~AuthenticationProvider~~"
      +"metadata: Option~HashMap~String"
    }
    class enum_AuthenticationMethod {
      <<enum>>
    }
    class struct_AuthenticationProvider {
      <<struct>>
      +"name: String"
      +"provider_type: ProviderType"
      +"configuration: HashMap~String"
      +"metadata: Option~HashMap~String"
    }
    class enum_ProviderType {
      <<enum>>
    }
    class struct_AuthorizationConfig {
      <<struct>>
      +"model: AuthorizationModel"
      +"policies: Vec~AuthorizationPolicy~"
      +"roles: Option~Vec~AuthorizationRole~~"
      +"metadata: Option~HashMap~String"
    }
    class enum_AuthorizationModel {
      <<enum>>
    }
    class struct_AuthorizationPolicy {
      <<struct>>
      +"name: String"
      +"policy_type: PolicyType"
      +"rules: Vec~PolicyRule~"
      +"effect: PolicyEffect"
      +"metadata: Option~HashMap~String"
    }
    class enum_PolicyType {
      <<enum>>
    }
    class struct_PolicyRule {
      <<struct>>
      +"name: String"
      +"condition: String"
      +"actions: Vec~String~"
      +"resources: Vec~String~"
      +"metadata: Option~HashMap~String"
    }
    class enum_PolicyEffect {
      <<enum>>
    }
    class struct_AuthorizationRole {
      <<struct>>
      +"name: String"
      +"description: Option~String~"
      +"permissions: Vec~Permission~"
      +"metadata: Option~HashMap~String"
    }
    class struct_Permission {
      <<struct>>
      +"name: String"
      +"permission_type: PermissionType"
      +"scope: PermissionScope"
      +"metadata: Option~HashMap~String"
    }
    class enum_PermissionType {
      <<enum>>
    }
    class enum_PermissionScope {
      <<enum>>
    }
    class struct_EncryptionConfig {
      <<struct>>
      +"algorithms: Vec~EncryptionAlgorithm~"
      +"keys: Vec~EncryptionKey~"
      +"modes: Vec~EncryptionMode~"
      +"metadata: Option~HashMap~String"
    }
    class enum_EncryptionAlgorithm {
      <<enum>>
    }
    class struct_EncryptionKey {
      <<struct>>
      +"name: String"
      +"key_type: KeyType"
      +"material: String"
      +"metadata: Option~HashMap~String"
    }
    class enum_KeyType {
      <<enum>>
    }
    class enum_EncryptionMode {
      <<enum>>
    }
    class struct_AuditConfig {
      <<struct>>
      +"events: Vec~AuditEvent~"
      +"destinations: Vec~AuditDestination~"
      +"retention: AuditRetention"
      +"metadata: Option~HashMap~String"
    }
    class enum_AuditEvent {
      <<enum>>
    }
    class struct_AuditDestination {
      <<struct>>
      +"name: String"
      +"destination_type: DestinationType"
      +"configuration: HashMap~String"
      +"metadata: Option~HashMap~String"
    }
    class enum_DestinationType {
      <<enum>>
    }
    class struct_AuditRetention {
      <<struct>>
      +"period: std::time::Duration"
      +"policy: RetentionPolicy"
      +"metadata: Option~HashMap~String"
    }
    class enum_RetentionPolicy {
      <<enum>>
    }
    class struct_ComplianceConfig {
      <<struct>>
      +"frameworks: Vec~ComplianceFramework~"
      +"standards: Vec~ComplianceStandard~"
      +"requirements: Vec~ComplianceRequirement~"
      +"metadata: Option~HashMap~String"
    }
    class enum_ComplianceFramework {
      <<enum>>
    }
    class struct_ComplianceStandard {
      <<struct>>
      +"name: String"
      +"version: String"
      +"requirements: Vec~String~"
      +"metadata: Option~HashMap~String"
    }
    class struct_ComplianceRequirement {
      <<struct>>
      +"name: String"
      +"requirement_type: RequirementType"
      +"controls: Vec~ComplianceControl~"
      +"metadata: Option~HashMap~String"
    }
    class enum_RequirementType {
      <<enum>>
    }
    class struct_ComplianceControl {
      <<struct>>
      +"name: String"
      +"description: String"
      +"implementation: String"
      +"metadata: Option~HashMap~String"
    }
    class struct_SecurityPolicies {
      <<struct>>
      +"access_control: Vec~SecurityPolicy~"
      +"data_protection: Vec~SecurityPolicy~"
      +"network_security: Vec~SecurityPolicy~"
      +"system_security: Vec~SecurityPolicy~"
      +"custom: Option~HashMap~String"
    }
    class struct_SecurityPolicy {
      <<struct>>
      +"name: String"
      +"description: Option~String~"
      +"rules: Vec~SecurityRule~"
      +"priority: PolicyPriority"
      +"metadata: Option~HashMap~String"
    }
    class struct_SecurityRule {
      <<struct>>
      +"name: String"
      +"condition: String"
      +"actions: Vec~String~"
      +"targets: Vec~String~"
      +"metadata: Option~HashMap~String"
    }
    class enum_PolicyPriority {
      <<enum>>
    }
    class struct_UnifiedAgentBuilder {
      <<struct>>
      +"identity: AgentIdentity"
      +"capabilities: AgentCapabilities"
      +"lifecycle: AgentLifecycle"
      +"communication: AgentCommunication"
      +"execution: AgentExecution"
      +"security: AgentSecurity"
      +"extensions: Option~HashMap~String"
    }
    class mod_tests {
      <<mod>>
    }
    class enum_AgentError {
      <<enum>>
    }
    note "UnifiedAgent"
    note "UnifiedAgentBuilder"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`
- `super::message::{ ConvergedMessageType, EncryptionRequirements, LatencyRequirements, ReliabilityLevel, ThroughputRequirements, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
