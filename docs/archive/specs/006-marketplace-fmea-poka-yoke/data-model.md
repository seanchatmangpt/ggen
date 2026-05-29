# Data Model: FMEA & Poka-Yoke Marketplace Framework

**Branch**: `006-marketplace-fmea-poka-yoke`
**Date**: 2025-12-14

## Entity Relationship Diagram

```text
┌─────────────────────────────────────────────────────────────────┐
│                          Package                                 │
├─────────────────────────────────────────────────────────────────┤
│ name: String                                                     │
│ version: String                                                  │
│ enterprise: Option<EnterpriseConfig>                            │
│ generation: Option<GenerationConfig>                            │
│ poka_yoke: Option<PokaYokeConfig>                               │
│ fmea: Option<FmeaConfig>                                        │
└─────────────────────────────────────────────────────────────────┘
         │              │              │              │
         ▼              ▼              ▼              ▼
┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
│ Enterprise  │ │ Generation  │ │ PokaYoke    │ │ Fmea        │
│ Config      │ │ Config      │ │ Config      │ │ Config      │
├─────────────┤ ├─────────────┤ ├─────────────┤ ├─────────────┤
│fortune_500_ │ │protected_   │ │generated_   │ │enabled      │
│  ready      │ │  paths[]    │ │  file_header│ │min_coverage │
│fmea_controls│ │regenerate_  │ │gitignore_   │ │controls[]   │
│poka_yoke_   │ │  paths[]    │ │  patterns[] │ │             │
│  enabled    │ │             │ │gitattributes│ │             │
│domain_      │ │             │ │  _patterns[]│ │             │
│  protection │ │             │ │             │ │             │
└─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘
                      │                               │
                      ▼                               ▼
              ┌─────────────┐               ┌─────────────────┐
              │ PathPattern │               │  FailureMode    │
              ├─────────────┤               ├─────────────────┤
              │pattern:     │               │id: String       │
              │  String     │               │mode: String     │
              │compiled:    │               │severity: u8     │
              │  GlobSet    │               │occurrence: u8   │
              └─────────────┘               │detection: u8    │
                                            │rpn: RpnScore    │
                                            │control: Option  │
                                            └─────────────────┘
                                                    │
                                                    ▼
                                            ┌─────────────┐
                                            │  RpnScore   │
                                            ├─────────────┤
                                            │value: u16   │
                                            │level: Enum  │
                                            └─────────────┘
```

## Type Definitions

### Core Types (ggen-core)

```rust
//! crates/ggen-core/src/types/fmea.rs

use serde::{Deserialize, Serialize};

/// FMEA Severity rating (1-10)
/// Higher = more severe impact if failure occurs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Severity(u8);

impl Severity {
    pub fn new(value: u8) -> Result<Self, ValidationError> {
        if value < 1 || value > 10 {
            return Err(ValidationError::InvalidSeverity(value));
        }
        Ok(Self(value))
    }

    pub fn value(&self) -> u8 {
        self.0
    }
}

/// FMEA Occurrence rating (1-10)
/// Higher = more likely to occur
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Occurrence(u8);

impl Occurrence {
    pub fn new(value: u8) -> Result<Self, ValidationError> {
        if value < 1 || value > 10 {
            return Err(ValidationError::InvalidOccurrence(value));
        }
        Ok(Self(value))
    }

    pub fn value(&self) -> u8 {
        self.0
    }
}

/// FMEA Detection rating (1-10)
/// Higher = harder to detect before release
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Detection(u8);

impl Detection {
    pub fn new(value: u8) -> Result<Self, ValidationError> {
        if value < 1 || value > 10 {
            return Err(ValidationError::InvalidDetection(value));
        }
        Ok(Self(value))
    }

    pub fn value(&self) -> u8 {
        self.0
    }
}

/// Risk Priority Number = Severity × Occurrence × Detection
/// Range: 1-1000
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RpnScore(u16);

impl RpnScore {
    pub fn calculate(severity: Severity, occurrence: Occurrence, detection: Detection) -> Self {
        let rpn = (severity.value() as u16)
            * (occurrence.value() as u16)
            * (detection.value() as u16);
        Self(rpn)
    }

    pub fn value(&self) -> u16 {
        self.0
    }

    pub fn level(&self) -> RpnLevel {
        match self.0 {
            201..=1000 => RpnLevel::Critical,
            100..=200 => RpnLevel::High,
            _ => RpnLevel::Medium,
        }
    }
}

/// RPN risk classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RpnLevel {
    /// RPN >200: MUST have control, blocks validation
    Critical,
    /// RPN 100-200: SHOULD have control, warning if missing
    High,
    /// RPN <100: MAY have control, informational
    Medium,
}

/// Single FMEA failure mode entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureMode {
    /// Unique identifier (e.g., "F1", "F2")
    pub id: String,
    /// Description of the failure mode
    pub mode: String,
    /// Severity rating (1-10)
    pub severity: Severity,
    /// Occurrence rating (1-10)
    pub occurrence: Occurrence,
    /// Detection rating (1-10)
    pub detection: Detection,
    /// Calculated RPN score
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rpn: Option<RpnScore>,
    /// Mitigation control (required for Critical, recommended for High)
    pub control: Option<String>,
}

impl FailureMode {
    pub fn calculate_rpn(&self) -> RpnScore {
        RpnScore::calculate(self.severity, self.occurrence, self.detection)
    }

    pub fn is_mitigated(&self) -> bool {
        self.control.is_some() && !self.control.as_ref().unwrap().is_empty()
    }

    pub fn requires_control(&self) -> bool {
        self.calculate_rpn().level() == RpnLevel::Critical
    }
}
```

```rust
//! crates/ggen-core/src/types/enterprise.rs

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Path protection strategy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DomainProtectionStrategy {
    /// Trait boundary: generated code defines traits, domain implements
    TraitBoundary,
    /// Module boundary: separate module directories
    ModuleBoundary,
    /// File boundary: separate files, no regeneration
    FileBoundary,
}

impl Default for DomainProtectionStrategy {
    fn default() -> Self {
        Self::TraitBoundary
    }
}

/// Protected path pattern (NEVER overwrite)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ProtectedPath(pub String);

/// Regeneratable path pattern (safe to overwrite)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RegeneratePath(pub String);

/// Generation configuration for path protection
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GenerationConfig {
    /// Paths that MUST NOT be modified by generation
    #[serde(default)]
    pub protected_paths: Vec<ProtectedPath>,
    /// Paths that CAN be regenerated freely
    #[serde(default)]
    pub regenerate_paths: Vec<RegeneratePath>,
}

/// Enterprise-grade configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EnterpriseConfig {
    /// Ready for Fortune 500 deployment
    #[serde(default)]
    pub fortune_500_ready: bool,
    /// FMEA controls enabled
    #[serde(default)]
    pub fmea_controls: bool,
    /// Poka-Yoke error proofing enabled
    #[serde(default)]
    pub poka_yoke_enabled: bool,
    /// Domain protection strategy
    #[serde(default)]
    pub domain_protection: DomainProtectionStrategy,
}

/// Poka-Yoke (error proofing) configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PokaYokeConfig {
    /// Header to inject into generated files
    pub generated_file_header: String,
    /// Patterns to add to .gitignore
    #[serde(default)]
    pub gitignore_patterns: Vec<String>,
    /// Patterns to mark in .gitattributes
    #[serde(default)]
    pub gitattributes_patterns: Vec<String>,
}

impl Default for PokaYokeConfig {
    fn default() -> Self {
        Self {
            generated_file_header: "⚠️ DO NOT EDIT - This file is auto-generated by ggen".into(),
            gitignore_patterns: vec!["src/generated/".into()],
            gitattributes_patterns: vec!["src/generated/** linguist-generated=true".into()],
        }
    }
}

/// FMEA configuration section
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FmeaConfig {
    /// Enable FMEA validation
    #[serde(default)]
    pub enabled: bool,
    /// Minimum coverage percentage (0-100)
    #[serde(default = "default_min_coverage")]
    pub min_coverage: u8,
    /// List of failure mode controls
    #[serde(default)]
    pub controls: Vec<FailureMode>,
}

fn default_min_coverage() -> u8 {
    100 // All critical modes must have controls
}
```

### Package Extensions (ggen-domain)

```rust
//! crates/ggen-domain/src/package/mod.rs (extension)

use ggen_core::types::{EnterpriseConfig, GenerationConfig, PokaYokeConfig, FmeaConfig};

/// Extended Package struct with enterprise sections
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    // ... existing fields ...

    /// Enterprise configuration
    #[serde(default)]
    pub enterprise: Option<EnterpriseConfig>,

    /// Generation path configuration
    #[serde(default)]
    pub generation: Option<GenerationConfig>,

    /// Poka-Yoke configuration
    #[serde(default)]
    pub poka_yoke: Option<PokaYokeConfig>,

    /// FMEA configuration
    #[serde(default)]
    pub fmea: Option<FmeaConfig>,
}
```

## Validation Rules

### Path Protection Rules

| Rule | Validation | Error |
|------|------------|-------|
| No overlap | protected ∩ regenerate = ∅ | `PathOverlapError` |
| Protected exists | protected paths exist in template | Warning only |
| Regenerate writable | regenerate paths are files | `PathTypeError` |

### FMEA Validation Rules

| Rule | Threshold | Action |
|------|-----------|--------|
| Critical unmitigated | RPN >200, no control | ERROR (block) |
| High unmitigated | RPN 100-200, no control | WARNING |
| Invalid ratings | Any rating <1 or >10 | ERROR |
| Duplicate IDs | Same ID used twice | ERROR |
| Coverage below min | Controls < min_coverage | ERROR |

### Poka-Yoke Validation Rules

| Rule | Check | Action |
|------|-------|--------|
| Header present | Generated files start with header | ERROR if missing |
| Gitignore valid | Patterns are valid glob syntax | ERROR if invalid |
| Gitattributes valid | Patterns follow gitattributes format | ERROR if invalid |

## State Transitions

### Generation State Machine

```text
                    ┌──────────────────┐
                    │   Initial State  │
                    │   (no files)     │
                    └────────┬─────────┘
                             │
                             │ ggen generate (first time)
                             ▼
┌───────────────────────────────────────────────────────┐
│                   First Generation                     │
│  - Create regenerate_paths with headers               │
│  - Create protected_paths with unimplemented!() stubs │
│  - Generate .gitignore, .gitattributes                │
└───────────────────────┬───────────────────────────────┘
                        │
                        │ User implements domain logic
                        ▼
┌───────────────────────────────────────────────────────┐
│                   Implemented State                    │
│  - protected_paths: User code (NEVER touch)           │
│  - regenerate_paths: Generated code (safe to update)  │
└───────────────────────┬───────────────────────────────┘
                        │
                        │ ggen generate --force
                        ▼
┌───────────────────────────────────────────────────────┐
│                   Regeneration                         │
│  - Overwrite regenerate_paths ONLY                    │
│  - Skip protected_paths entirely                      │
│  - Verify domain still implements traits              │
└───────────────────────┬───────────────────────────────┘
                        │
                        │ If trait signature changed
                        ▼
┌───────────────────────────────────────────────────────┐
│              Compilation Error (Expected)              │
│  - Domain code doesn't match new trait                │
│  - User must update domain implementations            │
│  - NOT a ggen bug - intentional design                │
└───────────────────────────────────────────────────────┘
```

## Sample package.toml

```toml
[package]
name = "enterprise-ops"
version = "1.0.0"
description = "Enterprise operations CLI"

[enterprise]
fortune_500_ready = true
fmea_controls = true
poka_yoke_enabled = true
domain_protection = "trait-boundary"

[generation]
protected_paths = [
    "src/domain/**",
    "src/infrastructure/**",
    "src/main.rs",
    "Cargo.toml",
]
regenerate_paths = [
    "src/generated/**",
]

[poka_yoke]
generated_file_header = """
// ⚠️ DO NOT EDIT - This file is auto-generated
// @generated by ggen from RDF ontology
//
// Regenerate with:
//   ggen generate --template clap-noun-verb --domain cli.ttl
//
// Changes will be overwritten. Place code in src/domain/ instead.
"""
gitignore_patterns = ["src/generated/"]
gitattributes_patterns = ["src/generated/** linguist-generated=true"]

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "F1"
mode = "Developer edits generated file"
severity = 9
occurrence = 6
detection = 4
control = "⚠️ DO NOT EDIT header + .gitignore"

[[fmea.controls]]
id = "F2"
mode = "Regenerate overwrites domain logic"
severity = 10
occurrence = 7
detection = 2
control = "Trait boundary separation - domain in protected_paths"

[[fmea.controls]]
id = "F3"
mode = "Merge conflict in ontology"
severity = 4
occurrence = 5
detection = 4
control = "One file per verb pattern"

[[fmea.controls]]
id = "F4"
mode = "Wrong team modifies noun"
severity = 6
occurrence = 4
detection = 3
control = "CODEOWNERS per noun directory"

[[fmea.controls]]
id = "F5"
mode = "Missing error handling"
severity = 8
occurrence = 6
detection = 5
control = "Trait requires Result<T, DomainError>"
```
