# `crates/ggen-config/src/domain/mcp_config.rs`

Source SHA-256: `4caa5ed831248a6abc53563a1cddac662ae2c716a5bb9abc17854e103838aa36`

```mermaid
classDiagram
    class struct_McpConfigFile {
      <<struct>>
      +"mcp_servers: HashMap~String"
      +"metadata: McpMetadata"
      +"description: Option~String~"
      +"version: String"
    }
    class fn_default_mcp_version {
      <<fn>>
    }
    class struct_McpMetadata {
      <<struct>>
      +"project: Option~String~"
      +"version: Option~String~"
      +"purpose: Option~String~"
      +"updated_at: Option~String~"
    }
    class struct_McpServerConfig {
      <<struct>>
      +"command: String"
      +"args: Vec~String~"
      +"env: HashMap~String"
      +"cwd: Option~String~"
      +"timeout: u64"
      +"enabled: bool"
      +"max_restarts: u32"
      +"server_type: Option~String~"
    }
    class fn_default_server_timeout {
      <<fn>>
    }
    class fn_default_server_enabled {
      <<fn>>
    }
    class fn_default_max_restarts {
      <<fn>>
    }
    class struct_A2aConfig {
      <<struct>>
      +"server: A2aServerConfig"
      +"agents: HashMap~String"
      +"workflows: HashMap~String"
      +"metadata: A2aMetadata"
    }
    class struct_A2aServerConfig {
      <<struct>>
      +"host: String"
      +"port: u16"
      +"tls_enabled: bool"
      +"tls_cert_path: Option~String~"
      +"tls_key_path: Option~String~"
      +"timeout: u64"
      +"max_connections: usize"
    }
    class fn_default_a2a_host {
      <<fn>>
    }
    class fn_default_a2a_port {
      <<fn>>
    }
    class fn_default_a2a_timeout {
      <<fn>>
    }
    class fn_default_max_connections {
      <<fn>>
    }
    class struct_A2aAgentConfig {
      <<struct>>
      +"agent_type: String"
      +"name: String"
      +"description: Option~String~"
      +"enabled: bool"
      +"config: HashMap~String"
    }
    class fn_default_agent_enabled {
      <<fn>>
    }
    class struct_A2aWorkflowConfig {
      <<struct>>
      +"spec_file: String"
      +"name: String"
      +"auto_start: bool"
    }
    class struct_A2aMetadata {
      <<struct>>
      +"version: String"
      +"environment: Option~String~"
      +"updated_at: Option~String~"
    }
    class fn_default_a2a_config_version {
      <<fn>>
    }
    class enum_McpValidationError {
      <<enum>>
    }
    class enum_A2aValidationError {
      <<enum>>
    }
    class enum_ConfigPriority {
      <<enum>>
    }
    class struct_ResolvedConfig {
      <<struct>>
      +"mcp: Option~McpConfigFile~"
      +"a2a: Option~A2aConfig~"
      +"sources: HashMap~String"
    }
    class fn_load_config {
      <<fn>>
    }
    class fn_load_mcp_from_file {
      <<fn>>
    }
    class fn_load_a2a_from_file {
      <<fn>>
    }
    class fn_load_mcp_from_env {
      <<fn>>
    }
    class fn_load_a2a_from_env {
      <<fn>>
    }
    class fn_init_mcp_config {
      <<fn>>
    }
    class fn_init_a2a_config {
      <<fn>>
    }
    class fn_write_mcp_config {
      <<fn>>
    }
    class fn_write_a2a_config {
      <<fn>>
    }
    class fn_validate_mcp_config {
      <<fn>>
    }
    class struct_ValidationResult {
      <<struct>>
      +"server_name: String"
      +"is_valid: bool"
      +"errors: Vec~String~"
      +"warnings: Vec~String~"
    }
    class struct_ServerStatus {
      <<struct>>
      +"is_running: bool"
      +"pid: Option~u32~"
      +"uptime_secs: Option~u64~"
      +"config_file: Option~String~"
      +"address: Option~String~"
      +"last_start_time: Option~String~"
    }
    class fn_get_server_status {
      <<fn>>
    }
    class fn_is_process_running {
      <<fn>>
    }
    class fn_is_process_running {
      <<fn>>
    }
    class fn_get_process_uptime {
      <<fn>>
    }
    class fn_get_process_uptime {
      <<fn>>
    }
    class fn_stop_server {
      <<fn>>
    }
    class fn_terminate_process {
      <<fn>>
    }
    class fn_terminate_process {
      <<fn>>
    }
    class fn_write_pid_file {
      <<fn>>
    }
    class fn_timestamp_now {
      <<fn>>
    }
    class fn_format_timestamp {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "A2aConfig"
    note "Default for A2aMetadata"
    note "Default for A2aServerConfig"
    note "Default for McpConfigFile"
    note "McpServerConfig"
    note "std::fmt::Display for ConfigPriority"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `crate::config_lib::{ConfigError, Result}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::env`
- `std::fs`
- `std::path::{Path, PathBuf}`
- `std::process::Command`
- `std::time::{Duration, SystemTime, UNIX_EPOCH}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
