//! Dead Man's Switch
//!
//! Automatic kill activation if heartbeat not received within timeout.
//! Prevents runaway contracts and ensures human oversight.
//!
//! # Design
//!
//! The dead man's switch provides a fail-safe mechanism for contract execution:
//! - When armed, the switch starts counting down from the configured timeout
//! - Each heartbeat resets the countdown timer
//! - If no heartbeat is received before timeout + grace period, the switch triggers
//! - Triggering can kill a specific contract, family, or activate global kill
//!
//! # Example
//!
//! ```rust,no_run
//! use mcp_contract::deadman::{DeadManConfig, DeadManSwitch, KillScope};
//! use std::time::Duration;
//!
//! let config = DeadManConfig {
//!     timeout: Duration::from_secs(300),
//!     grace_period: Duration::from_secs(60),
//!     notify_before_kill: true,
//!     scope: KillScope::Global,
//! };
//!
//! let switch = DeadManSwitch::new(config);
//! switch.arm();
//!
//! // In your main loop, send heartbeats
//! switch.heartbeat();
//! ```

use crate::state::KillSwitchState;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::RwLock;
use std::time::{Duration, Instant};

/// Dead man's switch configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeadManConfig {
    /// Timeout before auto-kill (default 5 minutes)
    #[serde(with = "duration_serde")]
    pub timeout: Duration,
    /// Grace period after first missed heartbeat
    #[serde(with = "duration_serde")]
    pub grace_period: Duration,
    /// Whether to notify before kill
    pub notify_before_kill: bool,
    /// Contracts to kill on timeout
    pub scope: KillScope,
}

impl Default for DeadManConfig {
    fn default() -> Self {
        Self {
            timeout: Duration::from_secs(300), // 5 minutes
            grace_period: Duration::from_secs(60), // 1 minute
            notify_before_kill: true,
            scope: KillScope::Global,
        }
    }
}

impl DeadManConfig {
    /// Create a config for a specific contract
    pub fn for_contract(contract_id: impl Into<String>) -> Self {
        Self {
            scope: KillScope::Contract(contract_id.into()),
            ..Default::default()
        }
    }

    /// Create a config for a contract family
    pub fn for_family(family: impl Into<String>) -> Self {
        Self {
            scope: KillScope::Family(family.into()),
            ..Default::default()
        }
    }

    /// Create a global kill config
    pub fn global() -> Self {
        Self {
            scope: KillScope::Global,
            ..Default::default()
        }
    }

    /// Set the timeout duration
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
        self
    }

    /// Set the grace period
    pub fn with_grace_period(mut self, grace_period: Duration) -> Self {
        self.grace_period = grace_period;
        self
    }

    /// Set whether to notify before kill
    pub fn with_notify(mut self, notify: bool) -> Self {
        self.notify_before_kill = notify;
        self
    }

    /// Calculate total time before kill (timeout + grace period)
    pub fn total_deadline(&self) -> Duration {
        self.timeout + self.grace_period
    }
}

/// Scope of the kill action
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum KillScope {
    /// Kill specific contract
    Contract(String),
    /// Kill entire family
    Family(String),
    /// Global kill
    Global,
}

impl std::fmt::Display for KillScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KillScope::Contract(id) => write!(f, "Contract({})", id),
            KillScope::Family(family) => write!(f, "Family({})", family),
            KillScope::Global => write!(f, "Global"),
        }
    }
}

/// Dead man's switch controller
pub struct DeadManSwitch {
    config: DeadManConfig,
    last_heartbeat: RwLock<Instant>,
    armed: AtomicBool,
    triggered: AtomicBool,
    in_grace_period: AtomicBool,
}

impl DeadManSwitch {
    /// Create a new dead man's switch
    pub fn new(config: DeadManConfig) -> Self {
        Self {
            config,
            last_heartbeat: RwLock::new(Instant::now()),
            armed: AtomicBool::new(false),
            triggered: AtomicBool::new(false),
            in_grace_period: AtomicBool::new(false),
        }
    }

    /// Create with default config (5 min timeout, 1 min grace, global scope)
    pub fn with_defaults() -> Self {
        Self::new(DeadManConfig::default())
    }

    /// Get the configuration
    pub fn config(&self) -> &DeadManConfig {
        &self.config
    }

    /// Send heartbeat to reset timer
    pub fn heartbeat(&self) {
        if let Ok(mut last) = self.last_heartbeat.write() {
            *last = Instant::now();
            // Reset grace period flag on successful heartbeat
            self.in_grace_period.store(false, Ordering::SeqCst);
        }
    }

    /// Arm the switch (starts counting)
    pub fn arm(&self) {
        self.heartbeat(); // Reset timer when arming
        self.armed.store(true, Ordering::SeqCst);
        self.triggered.store(false, Ordering::SeqCst);
        self.in_grace_period.store(false, Ordering::SeqCst);
    }

    /// Disarm the switch
    pub fn disarm(&self) {
        self.armed.store(false, Ordering::SeqCst);
        self.in_grace_period.store(false, Ordering::SeqCst);
    }

    /// Check if the switch is armed
    pub fn is_armed(&self) -> bool {
        self.armed.load(Ordering::SeqCst)
    }

    /// Check if the switch has been triggered
    pub fn is_triggered(&self) -> bool {
        self.triggered.load(Ordering::SeqCst)
    }

    /// Check if currently in grace period
    pub fn is_in_grace_period(&self) -> bool {
        self.in_grace_period.load(Ordering::SeqCst)
    }

    /// Time since last heartbeat
    pub fn time_since_heartbeat(&self) -> Duration {
        self.last_heartbeat
            .read()
            .map(|last| last.elapsed())
            .unwrap_or(Duration::ZERO)
    }

    /// Time remaining before auto-kill
    ///
    /// Returns Duration::ZERO if already past deadline
    pub fn time_remaining(&self) -> Duration {
        if !self.is_armed() {
            return Duration::MAX;
        }

        let elapsed = self.time_since_heartbeat();
        let deadline = self.config.total_deadline();

        if elapsed >= deadline {
            Duration::ZERO
        } else {
            deadline - elapsed
        }
    }

    /// Get the current status of the switch
    pub fn status(&self) -> SwitchStatus {
        if !self.is_armed() {
            return SwitchStatus::Disarmed;
        }

        if self.is_triggered() {
            return SwitchStatus::Triggered;
        }

        let elapsed = self.time_since_heartbeat();

        if elapsed >= self.config.total_deadline() {
            SwitchStatus::DeadlineExceeded
        } else if elapsed >= self.config.timeout {
            SwitchStatus::GracePeriod
        } else {
            SwitchStatus::Armed
        }
    }

    /// Check if timeout exceeded, trigger kill if needed
    ///
    /// Returns Some(KillEvent) if the switch triggered, None otherwise.
    /// Should be called periodically (e.g., in a monitoring loop).
    pub fn check(&self, kill_switch: &mut KillSwitchState) -> Option<KillEvent> {
        // Only check if armed and not already triggered
        if !self.is_armed() || self.is_triggered() {
            return None;
        }

        let elapsed = self.time_since_heartbeat();

        // Check if we're in grace period (past timeout but not past deadline)
        if elapsed >= self.config.timeout && elapsed < self.config.total_deadline() {
            self.in_grace_period.store(true, Ordering::SeqCst);
            return None;
        }

        // Check if past total deadline
        if elapsed >= self.config.total_deadline() {
            self.triggered.store(true, Ordering::SeqCst);

            // Execute kill based on scope
            match &self.config.scope {
                KillScope::Contract(id) => {
                    // For single contract, we add it to disabled families as a workaround
                    // In a real impl, this would target the specific contract
                    kill_switch.kill_family(format!("__contract_{}", id));
                }
                KillScope::Family(family) => {
                    kill_switch.kill_family(family);
                }
                KillScope::Global => {
                    kill_switch.activate_global();
                }
            }

            return Some(KillEvent {
                timestamp: Utc::now(),
                scope: self.config.scope.clone(),
                reason: format!(
                    "Dead man's switch triggered: no heartbeat for {:?}",
                    elapsed
                ),
                last_heartbeat_ago: elapsed,
                notified: self.config.notify_before_kill,
            });
        }

        None
    }

    /// Force trigger the switch immediately
    pub fn force_trigger(&self, kill_switch: &mut KillSwitchState) -> KillEvent {
        self.triggered.store(true, Ordering::SeqCst);

        let elapsed = self.time_since_heartbeat();

        // Execute kill based on scope
        match &self.config.scope {
            KillScope::Contract(id) => {
                kill_switch.kill_family(format!("__contract_{}", id));
            }
            KillScope::Family(family) => {
                kill_switch.kill_family(family);
            }
            KillScope::Global => {
                kill_switch.activate_global();
            }
        }

        KillEvent {
            timestamp: Utc::now(),
            scope: self.config.scope.clone(),
            reason: "Dead man's switch force triggered".to_string(),
            last_heartbeat_ago: elapsed,
            notified: self.config.notify_before_kill,
        }
    }

    /// Reset the switch (disarm, clear triggered state, reset timer)
    pub fn reset(&self) {
        self.disarm();
        self.triggered.store(false, Ordering::SeqCst);
        self.in_grace_period.store(false, Ordering::SeqCst);
        self.heartbeat();
    }
}

/// Status of the dead man's switch
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SwitchStatus {
    /// Switch is not armed
    Disarmed,
    /// Switch is armed and timer is running
    Armed,
    /// Past timeout, in grace period
    GracePeriod,
    /// Past deadline, ready to trigger
    DeadlineExceeded,
    /// Switch has triggered
    Triggered,
}

impl std::fmt::Display for SwitchStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SwitchStatus::Disarmed => write!(f, "Disarmed"),
            SwitchStatus::Armed => write!(f, "Armed"),
            SwitchStatus::GracePeriod => write!(f, "Grace Period"),
            SwitchStatus::DeadlineExceeded => write!(f, "Deadline Exceeded"),
            SwitchStatus::Triggered => write!(f, "Triggered"),
        }
    }
}

/// Event emitted when dead man's switch triggers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KillEvent {
    /// When the kill was triggered
    pub timestamp: DateTime<Utc>,
    /// Scope of the kill
    pub scope: KillScope,
    /// Reason for the kill
    pub reason: String,
    /// Time since last heartbeat when kill triggered
    #[serde(with = "duration_serde")]
    pub last_heartbeat_ago: Duration,
    /// Whether notification was sent before kill
    pub notified: bool,
}

impl KillEvent {
    /// Check if this was a global kill
    pub fn is_global(&self) -> bool {
        matches!(self.scope, KillScope::Global)
    }

    /// Get the contract ID if this was a contract-specific kill
    pub fn contract_id(&self) -> Option<&str> {
        match &self.scope {
            KillScope::Contract(id) => Some(id),
            _ => None,
        }
    }

    /// Get the family if this was a family kill
    pub fn family(&self) -> Option<&str> {
        match &self.scope {
            KillScope::Family(family) => Some(family),
            _ => None,
        }
    }
}

/// Serde support for Duration
mod duration_serde {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::time::Duration;

    #[derive(Serialize, Deserialize)]
    struct DurationHelper {
        secs: u64,
        nanos: u32,
    }

    pub fn serialize<S>(duration: &Duration, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        DurationHelper {
            secs: duration.as_secs(),
            nanos: duration.subsec_nanos(),
        }
        .serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Duration, D::Error>
    where
        D: Deserializer<'de>,
    {
        let helper = DurationHelper::deserialize(deserializer)?;
        Ok(Duration::new(helper.secs, helper.nanos))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_default_config() {
        let config = DeadManConfig::default();

        assert_eq!(config.timeout, Duration::from_secs(300));
        assert_eq!(config.grace_period, Duration::from_secs(60));
        assert!(config.notify_before_kill);
        assert_eq!(config.scope, KillScope::Global);
    }

    #[test]
    fn test_config_builders() {
        let config = DeadManConfig::for_contract("contract-001")
            .with_timeout(Duration::from_secs(60))
            .with_grace_period(Duration::from_secs(10))
            .with_notify(false);

        assert_eq!(config.timeout, Duration::from_secs(60));
        assert_eq!(config.grace_period, Duration::from_secs(10));
        assert!(!config.notify_before_kill);
        assert_eq!(config.scope, KillScope::Contract("contract-001".to_string()));
    }

    #[test]
    fn test_config_for_family() {
        let config = DeadManConfig::for_family("gpt-4");

        assert_eq!(config.scope, KillScope::Family("gpt-4".to_string()));
    }

    #[test]
    fn test_total_deadline() {
        let config = DeadManConfig::default()
            .with_timeout(Duration::from_secs(100))
            .with_grace_period(Duration::from_secs(50));

        assert_eq!(config.total_deadline(), Duration::from_secs(150));
    }

    #[test]
    fn test_switch_creation() {
        let switch = DeadManSwitch::with_defaults();

        assert!(!switch.is_armed());
        assert!(!switch.is_triggered());
        assert!(!switch.is_in_grace_period());
        assert_eq!(switch.status(), SwitchStatus::Disarmed);
    }

    #[test]
    fn test_arm_and_disarm() {
        let switch = DeadManSwitch::with_defaults();

        switch.arm();
        assert!(switch.is_armed());
        assert_eq!(switch.status(), SwitchStatus::Armed);

        switch.disarm();
        assert!(!switch.is_armed());
        assert_eq!(switch.status(), SwitchStatus::Disarmed);
    }

    #[test]
    fn test_heartbeat_resets_timer() {
        let config = DeadManConfig::default()
            .with_timeout(Duration::from_millis(100))
            .with_grace_period(Duration::from_millis(50));

        let switch = DeadManSwitch::new(config);
        switch.arm();

        // Wait a bit
        thread::sleep(Duration::from_millis(30));
        let elapsed1 = switch.time_since_heartbeat();

        // Send heartbeat
        switch.heartbeat();
        let elapsed2 = switch.time_since_heartbeat();

        // Timer should have reset
        assert!(elapsed2 < elapsed1);
    }

    #[test]
    fn test_time_remaining_decreases() {
        let config = DeadManConfig::default()
            .with_timeout(Duration::from_millis(200))
            .with_grace_period(Duration::from_millis(100));

        let switch = DeadManSwitch::new(config);
        switch.arm();

        let remaining1 = switch.time_remaining();
        thread::sleep(Duration::from_millis(50));
        let remaining2 = switch.time_remaining();

        assert!(remaining2 < remaining1);
    }

    #[test]
    fn test_time_remaining_when_disarmed() {
        let switch = DeadManSwitch::with_defaults();

        // When disarmed, time remaining should be MAX
        assert_eq!(switch.time_remaining(), Duration::MAX);
    }

    #[test]
    fn test_check_does_not_trigger_before_deadline() {
        let config = DeadManConfig::default()
            .with_timeout(Duration::from_secs(100))
            .with_grace_period(Duration::from_secs(50));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        switch.arm();

        // Check should return None (not triggered)
        let result = switch.check(&mut kill_state);
        assert!(result.is_none());
        assert!(!switch.is_triggered());
        assert!(!kill_state.global_active);
    }

    #[test]
    fn test_check_triggers_after_deadline() {
        let config = DeadManConfig::global()
            .with_timeout(Duration::from_millis(10))
            .with_grace_period(Duration::from_millis(10));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        switch.arm();

        // Wait past deadline
        thread::sleep(Duration::from_millis(30));

        // Check should trigger
        let result = switch.check(&mut kill_state);
        assert!(result.is_some());
        assert!(switch.is_triggered());
        assert!(kill_state.global_active);

        let event = result.unwrap();
        assert!(event.is_global());
        assert!(event.notified);
    }

    #[test]
    fn test_check_triggers_family_kill() {
        let config = DeadManConfig::for_family("test-family")
            .with_timeout(Duration::from_millis(10))
            .with_grace_period(Duration::from_millis(10));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        switch.arm();
        thread::sleep(Duration::from_millis(30));

        let result = switch.check(&mut kill_state);
        assert!(result.is_some());

        let event = result.unwrap();
        assert_eq!(event.family(), Some("test-family"));
        assert!(kill_state.is_family_killed("test-family"));
        assert!(!kill_state.global_active);
    }

    #[test]
    fn test_check_triggers_contract_kill() {
        let config = DeadManConfig::for_contract("contract-001")
            .with_timeout(Duration::from_millis(10))
            .with_grace_period(Duration::from_millis(10));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        switch.arm();
        thread::sleep(Duration::from_millis(30));

        let result = switch.check(&mut kill_state);
        assert!(result.is_some());

        let event = result.unwrap();
        assert_eq!(event.contract_id(), Some("contract-001"));
    }

    #[test]
    fn test_check_only_triggers_once() {
        let config = DeadManConfig::global()
            .with_timeout(Duration::from_millis(10))
            .with_grace_period(Duration::from_millis(10));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        switch.arm();
        thread::sleep(Duration::from_millis(30));

        // First check triggers
        let result1 = switch.check(&mut kill_state);
        assert!(result1.is_some());

        // Second check should not trigger again
        let result2 = switch.check(&mut kill_state);
        assert!(result2.is_none());
    }

    #[test]
    fn test_check_not_triggered_when_disarmed() {
        let config = DeadManConfig::global()
            .with_timeout(Duration::from_millis(10))
            .with_grace_period(Duration::from_millis(10));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        // Don't arm, wait past deadline
        thread::sleep(Duration::from_millis(30));

        // Should not trigger because not armed
        let result = switch.check(&mut kill_state);
        assert!(result.is_none());
        assert!(!kill_state.global_active);
    }

    #[test]
    fn test_force_trigger() {
        let switch = DeadManSwitch::with_defaults();
        let mut kill_state = KillSwitchState::default();

        switch.arm();

        // Force trigger immediately (no wait)
        let event = switch.force_trigger(&mut kill_state);

        assert!(switch.is_triggered());
        assert!(kill_state.global_active);
        assert!(event.is_global());
        assert_eq!(event.reason, "Dead man's switch force triggered");
    }

    #[test]
    fn test_reset() {
        let config = DeadManConfig::global()
            .with_timeout(Duration::from_millis(10))
            .with_grace_period(Duration::from_millis(10));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        switch.arm();
        thread::sleep(Duration::from_millis(30));
        switch.check(&mut kill_state);

        assert!(switch.is_triggered());

        // Reset the switch
        switch.reset();

        assert!(!switch.is_armed());
        assert!(!switch.is_triggered());
        assert!(!switch.is_in_grace_period());
        assert_eq!(switch.status(), SwitchStatus::Disarmed);
    }

    #[test]
    fn test_grace_period_detection() {
        let config = DeadManConfig::global()
            .with_timeout(Duration::from_millis(20))
            .with_grace_period(Duration::from_millis(100));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        switch.arm();

        // Wait past timeout but not past grace period
        thread::sleep(Duration::from_millis(30));

        // Should be in grace period, not triggered
        let result = switch.check(&mut kill_state);
        assert!(result.is_none());
        assert!(switch.is_in_grace_period());
        assert_eq!(switch.status(), SwitchStatus::GracePeriod);
        assert!(!kill_state.global_active);
    }

    #[test]
    fn test_heartbeat_exits_grace_period() {
        let config = DeadManConfig::global()
            .with_timeout(Duration::from_millis(20))
            .with_grace_period(Duration::from_millis(100));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        switch.arm();
        thread::sleep(Duration::from_millis(30));

        // Enter grace period
        switch.check(&mut kill_state);
        assert!(switch.is_in_grace_period());

        // Heartbeat should exit grace period
        switch.heartbeat();
        assert!(!switch.is_in_grace_period());
        assert_eq!(switch.status(), SwitchStatus::Armed);
    }

    #[test]
    fn test_status_transitions() {
        let config = DeadManConfig::global()
            .with_timeout(Duration::from_millis(15))
            .with_grace_period(Duration::from_millis(15));

        let switch = DeadManSwitch::new(config);
        let mut kill_state = KillSwitchState::default();

        // Initially disarmed
        assert_eq!(switch.status(), SwitchStatus::Disarmed);

        // Arm
        switch.arm();
        assert_eq!(switch.status(), SwitchStatus::Armed);

        // Wait to enter grace period
        thread::sleep(Duration::from_millis(20));
        switch.check(&mut kill_state);
        assert_eq!(switch.status(), SwitchStatus::GracePeriod);

        // Wait past deadline
        thread::sleep(Duration::from_millis(20));
        switch.check(&mut kill_state);
        assert_eq!(switch.status(), SwitchStatus::Triggered);
    }

    #[test]
    fn test_kill_scope_display() {
        assert_eq!(
            KillScope::Contract("c1".to_string()).to_string(),
            "Contract(c1)"
        );
        assert_eq!(
            KillScope::Family("gpt-4".to_string()).to_string(),
            "Family(gpt-4)"
        );
        assert_eq!(KillScope::Global.to_string(), "Global");
    }

    #[test]
    fn test_switch_status_display() {
        assert_eq!(SwitchStatus::Disarmed.to_string(), "Disarmed");
        assert_eq!(SwitchStatus::Armed.to_string(), "Armed");
        assert_eq!(SwitchStatus::GracePeriod.to_string(), "Grace Period");
        assert_eq!(SwitchStatus::DeadlineExceeded.to_string(), "Deadline Exceeded");
        assert_eq!(SwitchStatus::Triggered.to_string(), "Triggered");
    }

    #[test]
    fn test_kill_event_accessors() {
        let event = KillEvent {
            timestamp: Utc::now(),
            scope: KillScope::Contract("test".to_string()),
            reason: "test".to_string(),
            last_heartbeat_ago: Duration::from_secs(100),
            notified: true,
        };

        assert!(!event.is_global());
        assert_eq!(event.contract_id(), Some("test"));
        assert_eq!(event.family(), None);

        let global_event = KillEvent {
            timestamp: Utc::now(),
            scope: KillScope::Global,
            reason: "test".to_string(),
            last_heartbeat_ago: Duration::from_secs(100),
            notified: true,
        };

        assert!(global_event.is_global());
        assert_eq!(global_event.contract_id(), None);

        let family_event = KillEvent {
            timestamp: Utc::now(),
            scope: KillScope::Family("gpt-4".to_string()),
            reason: "test".to_string(),
            last_heartbeat_ago: Duration::from_secs(100),
            notified: true,
        };

        assert_eq!(family_event.family(), Some("gpt-4"));
    }

    #[test]
    fn test_config_serialization() {
        let config = DeadManConfig::for_family("test")
            .with_timeout(Duration::from_secs(120))
            .with_grace_period(Duration::from_secs(30));

        let json = serde_json::to_string(&config).unwrap();
        let restored: DeadManConfig = serde_json::from_str(&json).unwrap();

        assert_eq!(restored.timeout, Duration::from_secs(120));
        assert_eq!(restored.grace_period, Duration::from_secs(30));
        assert_eq!(restored.scope, KillScope::Family("test".to_string()));
    }

    #[test]
    fn test_kill_event_serialization() {
        let event = KillEvent {
            timestamp: Utc::now(),
            scope: KillScope::Global,
            reason: "Test kill".to_string(),
            last_heartbeat_ago: Duration::from_secs(300),
            notified: true,
        };

        let json = serde_json::to_string(&event).unwrap();
        let restored: KillEvent = serde_json::from_str(&json).unwrap();

        assert_eq!(restored.scope, KillScope::Global);
        assert_eq!(restored.reason, "Test kill");
        assert_eq!(restored.last_heartbeat_ago, Duration::from_secs(300));
        assert!(restored.notified);
    }
}
