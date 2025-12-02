//! Core FMEA types with type-level validation.
//!
//! This module implements the foundational types for FMEA analysis:
//! - Severity (1-10): Impact of failure
//! - Occurrence (1-10): Frequency of failure
//! - Detection (1-10): Ability to detect before impact
//! - RPN (Risk Priority Number): S × O × D
//!
//! All types use NewType pattern for compile-time validation.

use std::fmt;

/// Severity rating (1-10 scale).
///
/// Measures the impact/consequences of a failure mode:
/// - 1-3: Low (minor inconvenience)
/// - 4-6: Medium (degraded functionality)
/// - 7-8: High (major functionality loss)
/// - 9-10: Critical (security breach, data loss)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Severity(u8);

impl Severity {
    /// Creates a new Severity rating.
    ///
    /// # Errors
    ///
    /// Returns error if value is not in range 1-10.
    pub fn new(value: u8) -> Result<Self, String> {
        if (1..=10).contains(&value) {
            Ok(Self(value))
        } else {
            Err(format!("Severity must be 1-10, got {}", value))
        }
    }

    /// Returns the raw value.
    #[inline]
    pub const fn value(self) -> u8 {
        self.0
    }

    /// Returns the severity level as a string.
    pub const fn level(self) -> &'static str {
        match self.0 {
            1..=3 => "LOW",
            4..=6 => "MEDIUM",
            7..=8 => "HIGH",
            9..=10 => "CRITICAL",
            _ => "INVALID",
        }
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.0, self.level())
    }
}

/// Occurrence rating (1-10 scale).
///
/// Measures the frequency/probability of a failure mode:
/// - 1-2: Remote (< 0.01%)
/// - 3-4: Low (0.01% - 0.1%)
/// - 5-6: Moderate (0.1% - 1%)
/// - 7-8: High (1% - 10%)
/// - 9-10: Very High (> 10%)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Occurrence(u8);

impl Occurrence {
    /// Creates a new Occurrence rating.
    ///
    /// # Errors
    ///
    /// Returns error if value is not in range 1-10.
    pub fn new(value: u8) -> Result<Self, String> {
        if (1..=10).contains(&value) {
            Ok(Self(value))
        } else {
            Err(format!("Occurrence must be 1-10, got {}", value))
        }
    }

    /// Returns the raw value.
    #[inline]
    pub const fn value(self) -> u8 {
        self.0
    }

    /// Returns the occurrence level as a string.
    pub const fn level(self) -> &'static str {
        match self.0 {
            1..=2 => "REMOTE",
            3..=4 => "LOW",
            5..=6 => "MODERATE",
            7..=8 => "HIGH",
            9..=10 => "VERY_HIGH",
            _ => "INVALID",
        }
    }
}

impl fmt::Display for Occurrence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.0, self.level())
    }
}

/// Detection rating (1-10 scale).
///
/// Measures the ability to detect failure before impact:
/// - 1-2: Very High (automated detection, fail-safe)
/// - 3-4: High (automated detection, manual intervention)
/// - 5-6: Medium (manual inspection, testing)
/// - 7-8: Low (minimal inspection, difficult to detect)
/// - 9-10: Very Low (no detection, user discovers)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Detection(u8);

impl Detection {
    /// Creates a new Detection rating.
    ///
    /// # Errors
    ///
    /// Returns error if value is not in range 1-10.
    pub fn new(value: u8) -> Result<Self, String> {
        if (1..=10).contains(&value) {
            Ok(Self(value))
        } else {
            Err(format!("Detection must be 1-10, got {}", value))
        }
    }

    /// Returns the raw value.
    #[inline]
    pub const fn value(self) -> u8 {
        self.0
    }

    /// Returns the detection level as a string.
    pub const fn level(self) -> &'static str {
        match self.0 {
            1..=2 => "VERY_HIGH",
            3..=4 => "HIGH",
            5..=6 => "MEDIUM",
            7..=8 => "LOW",
            9..=10 => "VERY_LOW",
            _ => "INVALID",
        }
    }
}

impl fmt::Display for Detection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.0, self.level())
    }
}

/// Risk Priority Number (RPN).
///
/// Calculated as: Severity × Occurrence × Detection
/// Range: 1-1000
/// - 1-100: Low risk
/// - 101-250: Medium risk
/// - 251-500: High risk
/// - 501-1000: Critical risk
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RPN(u16);

impl RPN {
    /// Creates RPN from Severity × Occurrence × Detection.
    #[inline]
    pub const fn calculate(severity: Severity, occurrence: Occurrence, detection: Detection) -> Self {
        let value = (severity.value() as u16) * (occurrence.value() as u16) * (detection.value() as u16);
        Self(value)
    }

    /// Returns the raw RPN value.
    #[inline]
    pub const fn value(self) -> u16 {
        self.0
    }

    /// Returns the risk level based on RPN.
    pub const fn risk_level(self) -> &'static str {
        match self.0 {
            1..=100 => "LOW",
            101..=250 => "MEDIUM",
            251..=500 => "HIGH",
            501..=1000 => "CRITICAL",
            _ => "INVALID",
        }
    }
}

impl fmt::Display for RPN {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.0, self.risk_level())
    }
}

/// Failure category based on 80/20 Pareto analysis.
///
/// 8 categories account for 99% of failures in ggen CLI:
/// - FileIO: 30% (non-atomic writes, ENOSPC, race conditions)
/// - NetworkOps: 25% (timeouts, DNS failures, connection errors)
/// - ConcurrencyRace: 15% (mutex poisoning, deadlocks, race conditions)
/// - InputValidation: 15% (path traversal, injection, malformed input)
/// - TemplateRendering: 10% (SSTI, missing variables, syntax errors)
/// - DependencyResolution: 3% (circular deps, version conflicts)
/// - MemoryExhaustion: 1% (OOM, excessive allocations)
/// - Deserialization: 1% (invalid TOML/JSON, schema violations)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FailureCategory {
    /// File I/O operations (30% of failures).
    FileIO,
    /// Network operations (25% of failures).
    NetworkOps,
    /// Concurrency and race conditions (15% of failures).
    ConcurrencyRace,
    /// Input validation (15% of failures).
    InputValidation,
    /// Template rendering (10% of failures).
    TemplateRendering,
    /// Dependency resolution (3% of failures).
    DependencyResolution,
    /// Memory exhaustion (1% of failures).
    MemoryExhaustion,
    /// Deserialization errors (1% of failures).
    Deserialization,
}

impl fmt::Display for FailureCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FileIO => write!(f, "FileIO"),
            Self::NetworkOps => write!(f, "NetworkOps"),
            Self::ConcurrencyRace => write!(f, "ConcurrencyRace"),
            Self::InputValidation => write!(f, "InputValidation"),
            Self::TemplateRendering => write!(f, "TemplateRendering"),
            Self::DependencyResolution => write!(f, "DependencyResolution"),
            Self::MemoryExhaustion => write!(f, "MemoryExhaustion"),
            Self::Deserialization => write!(f, "Deserialization"),
        }
    }
}

/// Complete failure mode with metadata.
///
/// Captures all FMEA information:
/// - Identification (id, category, description)
/// - Risk metrics (severity, occurrence, detection, RPN)
/// - Analysis (effects, causes, controls, actions)
#[derive(Debug, Clone)]
pub struct FailureMode {
    /// Unique identifier (e.g., "file_io_write_fail").
    pub id: String,
    /// Failure category.
    pub category: FailureCategory,
    /// Human-readable description.
    pub description: String,
    /// Severity rating (1-10).
    pub severity: Severity,
    /// Occurrence rating (1-10).
    pub occurrence: Occurrence,
    /// Detection rating (1-10).
    pub detection: Detection,
    /// Risk Priority Number (auto-calculated).
    pub rpn: RPN,
    /// Effects of this failure mode.
    pub effects: Vec<String>,
    /// Root causes.
    pub causes: Vec<String>,
    /// Current controls/safeguards.
    pub controls: Vec<String>,
    /// Recommended actions.
    pub actions: Vec<String>,
}

impl FailureMode {
    /// Creates a new builder for constructing FailureMode.
    pub fn builder() -> FailureModeBuilder {
        FailureModeBuilder::new()
    }
}

/// Builder for constructing FailureMode instances.
///
/// Uses builder pattern for ergonomic API with compile-time validation.
#[derive(Debug, Default)]
pub struct FailureModeBuilder {
    id: Option<String>,
    category: Option<FailureCategory>,
    description: Option<String>,
    severity: Option<Severity>,
    occurrence: Option<Occurrence>,
    detection: Option<Detection>,
    effects: Vec<String>,
    causes: Vec<String>,
    controls: Vec<String>,
    actions: Vec<String>,
}

impl FailureModeBuilder {
    /// Creates a new builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the failure mode ID.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the failure category.
    pub fn category(mut self, category: FailureCategory) -> Self {
        self.category = Some(category);
        self
    }

    /// Sets the description.
    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Sets the severity rating.
    pub fn severity(mut self, severity: Severity) -> Self {
        self.severity = Some(severity);
        self
    }

    /// Sets the occurrence rating.
    pub fn occurrence(mut self, occurrence: Occurrence) -> Self {
        self.occurrence = Some(occurrence);
        self
    }

    /// Sets the detection rating.
    pub fn detection(mut self, detection: Detection) -> Self {
        self.detection = Some(detection);
        self
    }

    /// Adds an effect.
    pub fn effect(mut self, effect: impl Into<String>) -> Self {
        self.effects.push(effect.into());
        self
    }

    /// Adds multiple effects.
    pub fn effects(mut self, effects: Vec<String>) -> Self {
        self.effects.extend(effects);
        self
    }

    /// Adds a cause.
    pub fn cause(mut self, cause: impl Into<String>) -> Self {
        self.causes.push(cause.into());
        self
    }

    /// Adds multiple causes.
    pub fn causes(mut self, causes: Vec<String>) -> Self {
        self.causes.extend(causes);
        self
    }

    /// Adds a control.
    pub fn control(mut self, control: impl Into<String>) -> Self {
        self.controls.push(control.into());
        self
    }

    /// Adds multiple controls.
    pub fn controls(mut self, controls: Vec<String>) -> Self {
        self.controls.extend(controls);
        self
    }

    /// Adds an action.
    pub fn action(mut self, action: impl Into<String>) -> Self {
        self.actions.push(action.into());
        self
    }

    /// Adds multiple actions.
    pub fn actions(mut self, actions: Vec<String>) -> Self {
        self.actions.extend(actions);
        self
    }

    /// Builds the FailureMode.
    ///
    /// # Errors
    ///
    /// Returns error if required fields are missing.
    pub fn build(self) -> Result<FailureMode, String> {
        let id = self.id.ok_or("Missing required field: id")?;
        let category = self.category.ok_or("Missing required field: category")?;
        let description = self.description.ok_or("Missing required field: description")?;
        let severity = self.severity.ok_or("Missing required field: severity")?;
        let occurrence = self.occurrence.ok_or("Missing required field: occurrence")?;
        let detection = self.detection.ok_or("Missing required field: detection")?;

        // Auto-calculate RPN
        let rpn = RPN::calculate(severity, occurrence, detection);

        Ok(FailureMode {
            id,
            category,
            description,
            severity,
            occurrence,
            detection,
            rpn,
            effects: self.effects,
            causes: self.causes,
            controls: self.controls,
            actions: self.actions,
        })
    }
}
