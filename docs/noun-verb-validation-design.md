# Noun-Verb Validation Design

**System Architect**: Claude (Hive Mind Swarm Phase 2)
**Date**: 2025-11-19
**Version**: 1.0.0
**Status**: Security & Validation Design

---

## Executive Summary

This document defines the security and validation architecture for noun-verb command patterns in ggen, integrating `clap-noun-verb` with ggen.toml configuration to enforce IO permissions, audit logging, and constitution-based invariants.

---

## Overview

### Design Goals

1. **Permission Model**: Define read/write/delete operations per verb
2. **Security by Default**: Explicit permissions required for destructive operations
3. **Audit Trail**: Log all command executions with metadata
4. **Constitution Enforcement**: Hard invariants checked at runtime
5. **Type-Safe Validation**: Compile-time checks where possible

### Architecture Layers

```
┌─────────────────────────────────────────────────────────┐
│                 User Command Input                       │
│          ggen graph update --insert "..."                │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│         Noun-Verb Parser (clap-noun-verb)               │
│  • Extract noun: "graph"                                │
│  • Extract verb: "update"                               │
│  • Extract args: {insert: "..."}                        │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│         Permission Validator                            │
│  • Load permissions from ggen.toml                      │
│  • Check: graph.update → ["read", "write"]              │
│  • Validate IO operation is allowed                     │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│         Constitution Checker                            │
│  • Verify hard invariants                               │
│  • Check preconditions                                  │
│  • Validate state consistency                           │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│         Audit Logger                                    │
│  • Record command execution                             │
│  • Log timestamp, user, noun, verb                      │
│  • Store in .ggen/audit.log                             │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│         Command Execution                               │
│  • Execute validated command                            │
│  • Return result                                        │
└─────────────────────────────────────────────────────────┘
```

---

## Permission Model

### IO Operations Taxonomy

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum IOOperation {
    /// Read from filesystem or database
    Read,

    /// Write to filesystem or database (non-destructive)
    Write,

    /// Delete/remove data (destructive)
    Delete,

    /// Network operations (HTTP, gRPC, etc.)
    Network,

    /// Execute external commands
    Execute,

    /// Modify system state (environment, config)
    SystemModify,
}

impl IOOperation {
    pub fn is_destructive(&self) -> bool {
        matches!(self, Self::Delete | Self::Execute | Self::SystemModify)
    }

    pub fn requires_confirmation(&self) -> bool {
        self.is_destructive()
    }
}
```

### Permission Configuration in ggen.toml

```toml
[commands.graph]
noun = "graph"
description = "RDF graph operations"
verbs = ["query", "update", "export", "validate", "delete"]

# Permission model: define allowed IO operations per verb
[commands.graph.io_operations]
query = ["read"]                        # Read-only operation
update = ["read", "write"]              # Read and write, no delete
export = ["read"]                       # Read-only export
validate = ["read"]                     # Read-only validation
delete = ["read", "delete"]             # Destructive operation

# Security configuration
[commands.graph.security]
destructive_verbs = ["delete"]          # Require confirmation
require_confirmation = true              # Prompt before destructive ops
audit_log_path = ".ggen/audit.log"      # Audit log location

# Constitution: Hard invariants that must hold
[commands.graph.constitution]
invariants = [
    { name = "output_writable", check = "output_path_is_writable" },
    { name = "input_readable", check = "input_path_is_readable" },
    { name = "valid_format", check = "format_is_supported" },
]
```

---

## Permission Validator Implementation

### Core Validator

```rust
// crates/ggen-cli/src/validation.rs

use std::collections::HashMap;
use serde::{Deserialize, Serialize};

pub struct PermissionValidator {
    permissions: HashMap<String, Vec<IOOperation>>,
    security_config: SecurityConfig,
}

#[derive(Debug, Deserialize)]
pub struct SecurityConfig {
    pub destructive_verbs: Vec<String>,
    pub require_confirmation: bool,
    pub audit_log_path: String,
}

impl PermissionValidator {
    /// Load permissions from ggen.toml
    pub fn from_config(config: &GgenTomlConfig) -> Result<Self> {
        let mut permissions = HashMap::new();

        for (noun, cmd_config) in &config.commands {
            if let Some(io_ops) = &cmd_config.io_operations {
                for (verb, ops) in io_ops {
                    let key = format!("{}.{}", noun, verb);
                    let parsed_ops = ops.iter()
                        .map(|s| IOOperation::from_str(s))
                        .collect::<Result<Vec<_>>>()?;
                    permissions.insert(key, parsed_ops);
                }
            }
        }

        let security_config = config.commands
            .values()
            .next()
            .and_then(|c| c.security.clone())
            .unwrap_or_default();

        Ok(Self {
            permissions,
            security_config,
        })
    }

    /// Validate an IO operation for a noun.verb command
    pub fn validate(&self, noun: &str, verb: &str, operation: IOOperation) -> Result<()> {
        let key = format!("{}.{}", noun, verb);

        let allowed = self.permissions.get(&key)
            .ok_or_else(|| Error::UnknownCommand {
                noun: noun.to_string(),
                verb: verb.to_string(),
            })?;

        if !allowed.contains(&operation) {
            return Err(Error::OperationNotAllowed {
                noun: noun.to_string(),
                verb: verb.to_string(),
                operation,
                allowed: allowed.clone(),
            });
        }

        // Check if destructive and requires confirmation
        if operation.is_destructive() && self.security_config.require_confirmation {
            self.require_user_confirmation(noun, verb, operation)?;
        }

        Ok(())
    }

    /// Prompt user for confirmation on destructive operations
    fn require_user_confirmation(
        &self,
        noun: &str,
        verb: &str,
        operation: IOOperation
    ) -> Result<()> {
        use std::io::{self, Write};

        println!("⚠️  Warning: Destructive operation");
        println!("   Command: {} {}", noun, verb);
        println!("   Operation: {:?}", operation);
        print!("   Continue? [y/N]: ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        if !input.trim().eq_ignore_ascii_case("y") {
            return Err(Error::OperationCancelled);
        }

        Ok(())
    }

    /// Validate all operations for a command
    pub fn validate_all(&self, noun: &str, verb: &str, operations: &[IOOperation]) -> Result<()> {
        for op in operations {
            self.validate(noun, verb, *op)?;
        }
        Ok(())
    }
}

impl FromStr for IOOperation {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s.to_lowercase().as_str() {
            "read" => Ok(Self::Read),
            "write" => Ok(Self::Write),
            "delete" => Ok(Self::Delete),
            "network" => Ok(Self::Network),
            "execute" => Ok(Self::Execute),
            "system_modify" => Ok(Self::SystemModify),
            _ => Err(Error::UnknownIOOperation(s.to_string())),
        }
    }
}
```

---

## Constitution Checker Implementation

### Constitution Model

```rust
// crates/ggen-core/src/ontology/constitution.rs

pub struct Constitution {
    invariants: Vec<Invariant>,
}

pub struct Invariant {
    pub name: String,
    pub check: InvariantCheck,
}

pub enum InvariantCheck {
    /// Custom Rust function
    Function(Box<dyn Fn(&CommandContext) -> Result<()> + Send + Sync>),

    /// SHACL shape validation
    ShaclShape { shape_uri: String },

    /// SPARQL ASK query
    SparqlAsk { query: String },
}

pub struct CommandContext {
    pub noun: String,
    pub verb: String,
    pub args: HashMap<String, String>,
    pub config: GgenTomlConfig,
}

impl Constitution {
    /// Load constitution from ggen.toml
    pub fn from_config(config: &GgenTomlConfig) -> Result<Self> {
        let mut invariants = vec![];

        for (noun, cmd_config) in &config.commands {
            if let Some(constitution) = &cmd_config.constitution {
                for inv_config in &constitution.invariants {
                    let check = match inv_config.check.as_str() {
                        "output_path_is_writable" => {
                            InvariantCheck::Function(Box::new(check_output_writable))
                        }
                        "input_path_is_readable" => {
                            InvariantCheck::Function(Box::new(check_input_readable))
                        }
                        "format_is_supported" => {
                            InvariantCheck::Function(Box::new(check_format_supported))
                        }
                        custom => {
                            InvariantCheck::Function(
                                Self::load_custom_function(custom)?
                            )
                        }
                    };

                    invariants.push(Invariant {
                        name: inv_config.name.clone(),
                        check,
                    });
                }
            }
        }

        Ok(Self { invariants })
    }

    /// Check all invariants hold for a command
    pub fn check(&self, context: &CommandContext) -> Result<()> {
        for invariant in &self.invariants {
            match &invariant.check {
                InvariantCheck::Function(f) => {
                    f(context).map_err(|e| Error::ConstitutionViolation {
                        invariant: invariant.name.clone(),
                        reason: e.to_string(),
                    })?;
                }
                InvariantCheck::ShaclShape { shape_uri } => {
                    self.check_shacl_shape(context, shape_uri)?;
                }
                InvariantCheck::SparqlAsk { query } => {
                    self.check_sparql_ask(context, query)?;
                }
            }
        }
        Ok(())
    }

    fn check_shacl_shape(&self, context: &CommandContext, shape_uri: &str) -> Result<()> {
        // Use SHACL validator from ggen-core
        let validator = ShaclValidator::new()?;
        validator.validate(shape_uri, &context.args)?;
        Ok(())
    }

    fn check_sparql_ask(&self, context: &CommandContext, query: &str) -> Result<()> {
        // Execute SPARQL ASK query
        use oxigraph::sparql::QueryResults;

        let store = context.config.graph_store()?;
        let results = store.query(query)?;

        match results {
            QueryResults::Boolean(true) => Ok(()),
            QueryResults::Boolean(false) => Err(Error::SparqlAskFailed {
                query: query.to_string(),
            }),
            _ => Err(Error::InvalidQueryType),
        }
    }

    fn load_custom_function(name: &str) -> Result<Box<dyn Fn(&CommandContext) -> Result<()> + Send + Sync>> {
        // Load custom invariant functions from user-defined modules
        // This could use dynamic loading or a registry pattern
        Err(Error::CustomFunctionNotFound { name: name.to_string() })
    }
}

// Built-in invariant checks

fn check_output_writable(ctx: &CommandContext) -> Result<()> {
    if let Some(output) = ctx.args.get("output") {
        let path = PathBuf::from(output);
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                return Err(Error::PathNotFound { path: parent.to_path_buf() });
            }
            if !parent.metadata()?.permissions().readonly() {
                return Ok(());
            }
        }
        return Err(Error::PathNotWritable { path });
    }
    Ok(())
}

fn check_input_readable(ctx: &CommandContext) -> Result<()> {
    if let Some(input) = ctx.args.get("input") {
        let path = PathBuf::from(input);
        if !path.exists() {
            return Err(Error::PathNotFound { path });
        }
        if !path.is_file() {
            return Err(Error::PathNotFile { path });
        }
        // Try to open for reading
        std::fs::File::open(&path)
            .map_err(|e| Error::PathNotReadable { path: path.clone(), source: e })?;
    }
    Ok(())
}

fn check_format_supported(ctx: &CommandContext) -> Result<()> {
    if let Some(format) = ctx.args.get("format") {
        const SUPPORTED: &[&str] = &["turtle", "ntriples", "jsonld", "rdfxml"];
        if !SUPPORTED.contains(&format.as_str()) {
            return Err(Error::UnsupportedFormat {
                format: format.clone(),
                supported: SUPPORTED.iter().map(|s| s.to_string()).collect(),
            });
        }
    }
    Ok(())
}
```

---

## Audit Logger Implementation

### Audit Log Structure

```rust
// crates/ggen-cli/src/audit.rs

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEntry {
    pub timestamp: DateTime<Utc>,
    pub noun: String,
    pub verb: String,
    pub args: HashMap<String, String>,
    pub user: Option<String>,
    pub hostname: Option<String>,
    pub result: AuditResult,
    pub duration_ms: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AuditResult {
    Success,
    Failure { error: String },
    Cancelled,
}

pub struct AuditLogger {
    log_path: PathBuf,
}

impl AuditLogger {
    pub fn new(log_path: PathBuf) -> Result<Self> {
        // Ensure parent directory exists
        if let Some(parent) = log_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        Ok(Self { log_path })
    }

    pub fn from_config(config: &GgenTomlConfig) -> Result<Self> {
        let log_path = config.commands
            .values()
            .next()
            .and_then(|c| c.security.as_ref())
            .map(|s| PathBuf::from(&s.audit_log_path))
            .unwrap_or_else(|| PathBuf::from(".ggen/audit.log"));

        Self::new(log_path)
    }

    /// Record a command execution
    pub fn record(&self, entry: &AuditEntry) -> Result<()> {
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.log_path)?;

        let json = serde_json::to_string(entry)?;
        writeln!(file, "{}", json)?;
        file.flush()?;

        Ok(())
    }

    /// Record a successful command
    pub fn record_success(
        &self,
        noun: &str,
        verb: &str,
        args: &HashMap<String, String>,
        duration_ms: u64,
    ) -> Result<()> {
        self.record(&AuditEntry {
            timestamp: Utc::now(),
            noun: noun.to_string(),
            verb: verb.to_string(),
            args: args.clone(),
            user: std::env::var("USER").ok(),
            hostname: hostname::get().ok().and_then(|h| h.into_string().ok()),
            result: AuditResult::Success,
            duration_ms,
        })
    }

    /// Record a failed command
    pub fn record_failure(
        &self,
        noun: &str,
        verb: &str,
        args: &HashMap<String, String>,
        error: &str,
    ) -> Result<()> {
        self.record(&AuditEntry {
            timestamp: Utc::now(),
            noun: noun.to_string(),
            verb: verb.to_string(),
            args: args.clone(),
            user: std::env::var("USER").ok(),
            hostname: hostname::get().ok().and_then(|h| h.into_string().ok()),
            result: AuditResult::Failure {
                error: error.to_string(),
            },
            duration_ms: 0,
        })
    }

    /// Query audit log entries
    pub fn query(&self, filter: AuditFilter) -> Result<Vec<AuditEntry>> {
        use std::io::{BufRead, BufReader};

        let file = File::open(&self.log_path)?;
        let reader = BufReader::new(file);

        let mut entries = vec![];
        for line in reader.lines() {
            let line = line?;
            if let Ok(entry) = serde_json::from_str::<AuditEntry>(&line) {
                if filter.matches(&entry) {
                    entries.push(entry);
                }
            }
        }

        Ok(entries)
    }

    /// Get statistics from audit log
    pub fn stats(&self) -> Result<AuditStats> {
        let entries = self.query(AuditFilter::all())?;

        let total_commands = entries.len();
        let successful = entries.iter().filter(|e| matches!(e.result, AuditResult::Success)).count();
        let failed = entries.iter().filter(|e| matches!(e.result, AuditResult::Failure { .. })).count();
        let cancelled = entries.iter().filter(|e| matches!(e.result, AuditResult::Cancelled)).count();

        let avg_duration = if !entries.is_empty() {
            entries.iter().map(|e| e.duration_ms).sum::<u64>() / entries.len() as u64
        } else {
            0
        };

        Ok(AuditStats {
            total_commands,
            successful,
            failed,
            cancelled,
            avg_duration_ms: avg_duration,
        })
    }
}

#[derive(Debug, Default)]
pub struct AuditFilter {
    pub noun: Option<String>,
    pub verb: Option<String>,
    pub user: Option<String>,
    pub since: Option<DateTime<Utc>>,
    pub until: Option<DateTime<Utc>>,
    pub result: Option<AuditResult>,
}

impl AuditFilter {
    pub fn all() -> Self {
        Self::default()
    }

    pub fn matches(&self, entry: &AuditEntry) -> bool {
        if let Some(noun) = &self.noun {
            if &entry.noun != noun {
                return false;
            }
        }

        if let Some(verb) = &self.verb {
            if &entry.verb != verb {
                return false;
            }
        }

        if let Some(user) = &self.user {
            if entry.user.as_ref() != Some(user) {
                return false;
            }
        }

        if let Some(since) = self.since {
            if entry.timestamp < since {
                return false;
            }
        }

        if let Some(until) = self.until {
            if entry.timestamp > until {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Serialize)]
pub struct AuditStats {
    pub total_commands: usize,
    pub successful: usize,
    pub failed: usize,
    pub cancelled: usize,
    pub avg_duration_ms: u64,
}
```

---

## Integration with clap-noun-verb

### Middleware Pattern

```rust
// crates/ggen-cli/src/middleware.rs

pub struct ValidationMiddleware {
    permission_validator: PermissionValidator,
    constitution: Constitution,
    audit_logger: AuditLogger,
}

impl ValidationMiddleware {
    pub fn from_config(config: &GgenTomlConfig) -> Result<Self> {
        Ok(Self {
            permission_validator: PermissionValidator::from_config(config)?,
            constitution: Constitution::from_config(config)?,
            audit_logger: AuditLogger::from_config(config)?,
        })
    }

    /// Validate and execute a command
    pub async fn execute<F, R>(
        &self,
        noun: &str,
        verb: &str,
        args: HashMap<String, String>,
        handler: F,
    ) -> Result<R>
    where
        F: FnOnce(&CommandContext) -> Result<R>,
    {
        let start = std::time::Instant::now();

        // 1. Validate permissions
        let io_operations = self.detect_io_operations(noun, verb, &args)?;
        for op in &io_operations {
            self.permission_validator.validate(noun, verb, *op)?;
        }

        // 2. Check constitution
        let context = CommandContext {
            noun: noun.to_string(),
            verb: verb.to_string(),
            args: args.clone(),
            config: load_config()?,
        };
        self.constitution.check(&context)?;

        // 3. Execute command
        let result = handler(&context);

        // 4. Audit log
        let duration_ms = start.elapsed().as_millis() as u64;
        match &result {
            Ok(_) => {
                self.audit_logger.record_success(noun, verb, &args, duration_ms)?;
            }
            Err(e) => {
                self.audit_logger.record_failure(noun, verb, &args, &e.to_string())?;
            }
        }

        result
    }

    fn detect_io_operations(
        &self,
        noun: &str,
        verb: &str,
        args: &HashMap<String, String>,
    ) -> Result<Vec<IOOperation>> {
        // Heuristic detection of IO operations based on verb and args
        let mut ops = vec![];

        // Check if verb implies read
        if ["query", "export", "validate", "show"].contains(&verb) {
            ops.push(IOOperation::Read);
        }

        // Check if verb implies write
        if ["update", "create", "insert"].contains(&verb) {
            ops.push(IOOperation::Write);
        }

        // Check if verb implies delete
        if ["delete", "remove", "drop"].contains(&verb) {
            ops.push(IOOperation::Delete);
        }

        // Check args for IO hints
        if args.contains_key("output") || args.contains_key("file") {
            ops.push(IOOperation::Write);
        }

        if args.contains_key("input") || args.contains_key("source") {
            ops.push(IOOperation::Read);
        }

        Ok(ops)
    }
}
```

### Usage in CLI

```rust
// crates/ggen-cli/src/commands/graph.rs

use crate::middleware::ValidationMiddleware;

pub async fn execute_graph_command(
    cmd: GraphCommands,
    config: &GgenTomlConfig,
) -> Result<()> {
    let middleware = ValidationMiddleware::from_config(config)?;

    match cmd.action {
        GraphAction::Query(args) => {
            let mut arg_map = HashMap::new();
            arg_map.insert("sparql".to_string(), args.sparql);
            arg_map.insert("format".to_string(), args.format);

            middleware.execute("graph", "query", arg_map, |ctx| {
                // Actual query execution
                execute_query_impl(ctx)
            }).await?;
        }

        GraphAction::Update(args) => {
            let mut arg_map = HashMap::new();
            arg_map.insert("insert".to_string(), args.insert);

            middleware.execute("graph", "update", arg_map, |ctx| {
                // Actual update execution
                execute_update_impl(ctx)
            }).await?;
        }

        GraphAction::Delete(args) => {
            let mut arg_map = HashMap::new();
            arg_map.insert("where".to_string(), args.where_clause);

            middleware.execute("graph", "delete", arg_map, |ctx| {
                // Actual delete execution
                execute_delete_impl(ctx)
            }).await?;
        }
    }

    Ok(())
}
```

---

## Error Handling

### Validation Errors

```rust
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Unknown command: {noun}.{verb}")]
    UnknownCommand { noun: String, verb: String },

    #[error("Operation {operation:?} not allowed for {noun}.{verb}. Allowed: {allowed:?}")]
    OperationNotAllowed {
        noun: String,
        verb: String,
        operation: IOOperation,
        allowed: Vec<IOOperation>,
    },

    #[error("Constitution violation: {invariant} - {reason}")]
    ConstitutionViolation { invariant: String, reason: String },

    #[error("Operation cancelled by user")]
    OperationCancelled,

    #[error("Unknown IO operation: {0}")]
    UnknownIOOperation(String),

    #[error("Path not found: {path}")]
    PathNotFound { path: PathBuf },

    #[error("Path not writable: {path}")]
    PathNotWritable { path: PathBuf },

    #[error("Path not readable: {path}")]
    PathNotReadable { path: PathBuf, source: std::io::Error },

    #[error("Unsupported format: {format}. Supported: {supported:?}")]
    UnsupportedFormat { format: String, supported: Vec<String> },
}
```

---

## Testing Strategy

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_permission_validation_read_allowed() {
        let config = create_test_config();
        let validator = PermissionValidator::from_config(&config).unwrap();

        assert!(validator.validate("graph", "query", IOOperation::Read).is_ok());
    }

    #[test]
    fn test_permission_validation_write_denied() {
        let config = create_test_config();
        let validator = PermissionValidator::from_config(&config).unwrap();

        assert!(validator.validate("graph", "query", IOOperation::Write).is_err());
    }

    #[test]
    fn test_constitution_check_output_writable() {
        let ctx = CommandContext {
            noun: "graph".to_string(),
            verb: "export".to_string(),
            args: vec![
                ("output".to_string(), "/tmp/test.ttl".to_string()),
            ].into_iter().collect(),
            config: create_test_config(),
        };

        assert!(check_output_writable(&ctx).is_ok());
    }

    #[test]
    fn test_audit_log_success() {
        let temp_dir = tempfile::tempdir().unwrap();
        let log_path = temp_dir.path().join("audit.log");
        let logger = AuditLogger::new(log_path.clone()).unwrap();

        let args = vec![("sparql".to_string(), "SELECT * WHERE {?s ?p ?o}".to_string())]
            .into_iter()
            .collect();

        logger.record_success("graph", "query", &args, 100).unwrap();

        // Verify entry exists
        let entries = logger.query(AuditFilter::all()).unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].noun, "graph");
        assert_eq!(entries[0].verb, "query");
    }
}
```

### Integration Tests

```rust
#[tokio::test]
async fn test_middleware_integration() {
    let config = load_test_config();
    let middleware = ValidationMiddleware::from_config(&config).unwrap();

    let args = vec![
        ("sparql".to_string(), "SELECT * WHERE {?s ?p ?o}".to_string()),
    ].into_iter().collect();

    let result = middleware.execute("graph", "query", args, |ctx| {
        // Mock execution
        Ok(())
    }).await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_destructive_operation_requires_confirmation() {
    let config = load_test_config();
    let middleware = ValidationMiddleware::from_config(&config).unwrap();

    let args = vec![
        ("where".to_string(), "?s ?p ?o".to_string()),
    ].into_iter().collect();

    // Without user confirmation, should fail
    let result = middleware.execute("graph", "delete", args, |ctx| {
        Ok(())
    }).await;

    assert!(result.is_err());
}
```

---

## Security Considerations

### 1. Audit Log Tampering

**Risk**: Attacker modifies audit log to hide malicious activity
**Mitigation**:
- Write-only permissions on audit log
- Cryptographic signatures on entries (future)
- Remote logging to immutable storage

```rust
pub struct SignedAuditLogger {
    logger: AuditLogger,
    signing_key: ed25519_dalek::Keypair,
}

impl SignedAuditLogger {
    pub fn record_signed(&self, entry: &AuditEntry) -> Result<()> {
        let signature = self.signing_key.sign(&serde_json::to_vec(entry)?);
        let signed_entry = SignedAuditEntry {
            entry: entry.clone(),
            signature: signature.to_bytes().to_vec(),
        };
        self.logger.record_raw(&signed_entry)
    }
}
```

### 2. Permission Escalation

**Risk**: User bypasses permission checks
**Mitigation**:
- Validate at multiple layers (middleware + handler)
- Constitution checks as defense in depth
- Audit all operations, even failed ones

### 3. Race Conditions

**Risk**: TOCTOU (time-of-check-time-of-use) in file operations
**Mitigation**:
- Atomic file operations
- Lock files during validation
- Re-validate in handler

```rust
pub fn atomic_write(path: &Path, content: &[u8]) -> Result<()> {
    use std::io::Write;

    let temp_path = path.with_extension("tmp");
    let mut file = File::create(&temp_path)?;
    file.write_all(content)?;
    file.sync_all()?;

    std::fs::rename(temp_path, path)?;
    Ok(())
}
```

---

## Performance Optimization

### Caching Validators

```rust
use std::sync::Arc;
use once_cell::sync::OnceCell;

static VALIDATOR: OnceCell<Arc<ValidationMiddleware>> = OnceCell::new();

pub fn get_validator() -> Arc<ValidationMiddleware> {
    VALIDATOR.get_or_init(|| {
        let config = load_config().expect("Failed to load config");
        Arc::new(ValidationMiddleware::from_config(&config).unwrap())
    }).clone()
}
```

### Lazy Constitution Checks

```rust
impl Constitution {
    pub fn check_lazy(&self, context: &CommandContext) -> Result<()> {
        // Only check invariants relevant to the command
        for invariant in &self.invariants {
            if self.is_relevant(invariant, context) {
                // Check only relevant invariants
                match &invariant.check {
                    InvariantCheck::Function(f) => f(context)?,
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn is_relevant(&self, invariant: &Invariant, context: &CommandContext) -> bool {
        // Heuristic: check if invariant name relates to context
        invariant.name.contains(&context.noun) || invariant.name.contains(&context.verb)
    }
}
```

---

## CLI Commands for Audit Management

### View Audit Log

```bash
ggen audit show --last 10
ggen audit show --noun graph --verb delete
ggen audit show --since "2025-11-01" --until "2025-11-19"
```

### Audit Statistics

```bash
ggen audit stats
# Output:
# Total Commands: 1,234
# Successful: 1,200 (97.2%)
# Failed: 30 (2.4%)
# Cancelled: 4 (0.3%)
# Avg Duration: 45ms
```

### Export Audit Log

```bash
ggen audit export --format json --output audit-2025-11.json
ggen audit export --format csv --output audit-2025-11.csv
```

---

## Success Metrics

1. ✅ All destructive operations require explicit confirmation
2. ✅ 100% of commands logged in audit trail
3. ✅ Constitution violations blocked before execution
4. ✅ Clear error messages for permission denials
5. ✅ Zero false positives in permission checks
6. ✅ <10ms overhead for validation per command
7. ✅ Tamper-evident audit logs
8. ✅ Comprehensive test coverage (>95%)

---

**Document Status**: ✅ Complete
**Related Documents**:
- clap-ggen-integration-design.md
- ggen-cli-macro-design.md
