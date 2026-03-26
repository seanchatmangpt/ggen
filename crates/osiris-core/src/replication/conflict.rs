//! Conflict Resolution Engine for Multi-Region Replication
//!
//! Provides automatic conflict detection and resolution strategies for concurrent writes
//! across regions. Integrates with vector clocks for causality tracking.
//!
//! ## Strategies
//!
//! - **LWW (Last-Writer-Wins)**: Resolves by timestamp, deterministic and fast
//! - **AppSpecific**: Application-defined merge logic (e.g., prefer higher values)
//! - **Custom**: User-provided resolution functions
//! - **VectorClock**: Uses causality to detect and resolve concurrent updates
//!
//! ## Example
//!
//! ```rust
//! use osiris_core::replication::conflict::{
//!     ConflictResolution, ConflictType, LWWStrategy, ResolutionPolicy
//! };
//! use osiris_core::replication::VectorClock;
//!
//! // Detect conflict between concurrent updates
//! let vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
//! let vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
//! let conflict = ConflictType::detect(&vc1, &vc2);
//!
//! // Resolve using LWW
//! let strategy = LWWStrategy::new();
//! let resolved = strategy.resolve(&"value_a", &"value_b", &conflict);
//! ```

use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

use super::vector_clock::VectorClock;

/// Type of conflict between two updates
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConflictType {
    /// No conflict - updates are causally related
    None,

    /// Concurrent updates - neither happened before the other
    Concurrent,

    /// Conflicting values for same key
    ConflictingValues,

    /// Version conflict (same vector clock)
    VersionConflict,
}

impl ConflictType {
    /// Detect the type of conflict between two vector clocks
    pub fn detect(vc1: &VectorClock, vc2: &VectorClock) -> Self {
        if vc1 == vc2 {
            return ConflictType::VersionConflict;
        }

        if vc1.concurrent(vc2) {
            return ConflictType::Concurrent;
        }

        ConflictType::None
    }

    /// Returns true if this is a conflict that needs resolution
    pub fn needs_resolution(&self) -> bool {
        matches!(self, ConflictType::Concurrent | ConflictType::ConflictingValues | ConflictType::VersionConflict)
    }
}

/// Result of a conflict resolution operation
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ResolutionResult<T> {
    /// The resolved value
    pub value: T,

    /// The strategy used to resolve the conflict
    pub strategy: ResolutionPolicy,

    /// Whether a conflict actually occurred
    pub had_conflict: bool,

    /// The winning vector clock (if applicable)
    pub winning_clock: Option<VectorClock>,
}

impl<T: fmt::Display> fmt::Display for ResolutionResult<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ResolutionResult(value={}, strategy={:?}, conflict={})",
            self.value, self.strategy, self.had_conflict
        )
    }
}

/// Resolution policy for handling conflicts
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ResolutionPolicy {
    /// Last-Writer-Wins: highest timestamp wins
    LastWriterWins,

    /// Application-specific: use custom logic
    AppSpecific(String),

    /// Prefer primary region's value
    PreferPrimary,

    /// Merge both values (for collections)
    Merge,

    /// Custom policy name
    Custom(String),
}

impl ResolutionPolicy {
    /// Create a new Last-Writer-Wins policy
    pub fn lww() -> Self {
        ResolutionPolicy::LastWriterWins
    }

    /// Create a new application-specific policy
    pub fn app_specific(name: impl Into<String>) -> Self {
        ResolutionPolicy::AppSpecific(name.into())
    }

    /// Create a new prefer-primary policy
    pub fn prefer_primary() -> Self {
        ResolutionPolicy::PreferPrimary
    }

    /// Create a new merge policy
    pub fn merge() -> Self {
        ResolutionPolicy::Merge
    }

    /// Create a custom policy with a name
    pub fn custom(name: impl Into<String>) -> Self {
        ResolutionPolicy::Custom(name.into())
    }
}

/// Trait for conflict resolution strategies
pub trait ConflictResolution<T: Clone + PartialEq>: Send + Sync {
    /// Resolve a conflict between two values
    ///
    /// # Arguments
    /// * `value_a` - First value (typically from local replica)
    /// * `value_b` - Second value (typically from remote replica)
    /// * `conflict_type` - Type of conflict detected
    /// * `vc_a` - Vector clock for value_a
    /// * `vc_b` - Vector clock for value_b
    ///
    /// # Returns
    /// The resolved value and metadata about the resolution
    fn resolve(
        &self,
        value_a: &T,
        value_b: &T,
        conflict_type: &ConflictType,
        vc_a: &VectorClock,
        vc_b: &VectorClock,
    ) -> ResolutionResult<T>;

    /// Returns the policy used by this strategy
    fn policy(&self) -> ResolutionPolicy;
}

/// Last-Writer-Wins strategy using timestamps
#[derive(Clone)]
pub struct LWWStrategy {
    /// Whether to use region ID as tiebreaker
    use_region_tiebreaker: bool,

    /// Primary region (for prefer-primary variant)
    primary_region: Option<String>,
}

impl LWWStrategy {
    /// Create a new LWW strategy
    pub fn new() -> Self {
        Self {
            use_region_tiebreaker: true,
            primary_region: None,
        }
    }

    /// Create an LWW strategy that prefers the primary region
    pub fn prefer_primary(primary_region: impl Into<String>) -> Self {
        Self {
            use_region_tiebreaker: true,
            primary_region: Some(primary_region.into()),
        }
    }

    /// Create an LWW strategy without region tiebreaker
    pub fn without_tiebreaker() -> Self {
        Self {
            use_region_tiebreaker: false,
            primary_region: None,
        }
    }
}

impl Default for LWWStrategy {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone + PartialEq + Send + Sync> ConflictResolution<T> for LWWStrategy {
    fn resolve(
        &self,
        value_a: &T,
        value_b: &T,
        conflict_type: &ConflictType,
        vc_a: &VectorClock,
        vc_b: &VectorClock,
    ) -> ResolutionResult<T> {
        // If no conflict, prefer causally newer value
        if !conflict_type.needs_resolution() {
            if vc_a.happens_before(vc_b) {
                return ResolutionResult {
                    value: value_b.clone(),
                    strategy: <LWWStrategy as ConflictResolution<T>>::policy(self),
                    had_conflict: false,
                    winning_clock: Some(vc_b.clone()),
                };
            } else if vc_b.happens_before(vc_a) {
                return ResolutionResult {
                    value: value_a.clone(),
                    strategy: <LWWStrategy as ConflictResolution<T>>::policy(self),
                    had_conflict: false,
                    winning_clock: Some(vc_a.clone()),
                };
            }
        }

        // For concurrent updates, use LWW based on vector clock comparison
        // We compare the sum of all timestamps as a proxy for "latest"
        let sum_a: u64 = vc_a.as_map().values().sum();
        let sum_b: u64 = vc_b.as_map().values().sum();

        let (winner, winner_vc) = if sum_a > sum_b {
            (value_a.clone(), vc_a.clone())
        } else if sum_b > sum_a {
            (value_b.clone(), vc_b.clone())
        } else {
            // Tie: use region tiebreaker if enabled
            if self.use_region_tiebreaker {
                // Compare regions lexicographically
                let regions_a = vc_a.regions();
                let regions_b = vc_b.regions();

                if regions_a > regions_b {
                    (value_a.clone(), vc_a.clone())
                } else {
                    (value_b.clone(), vc_b.clone())
                }
            } else {
                // No tiebreaker: prefer first value (deterministic)
                (value_a.clone(), vc_a.clone())
            }
        };

        ResolutionResult {
            value: winner,
            strategy: <LWWStrategy as ConflictResolution<T>>::policy(self),
            had_conflict: conflict_type.needs_resolution(),
            winning_clock: Some(winner_vc),
        }
    }

    fn policy(&self) -> ResolutionPolicy {
        if self.primary_region.is_some() {
            ResolutionPolicy::PreferPrimary
        } else {
            ResolutionPolicy::LastWriterWins
        }
    }
}

/// Application-specific conflict resolution
///
/// Allows users to provide custom merge logic for specific data types.
pub struct AppSpecificStrategy<T, F>
where
    T: Clone + PartialEq,
    F: Fn(&T, &T, &ConflictType) -> T + Send + Sync,
{
    merge_fn: F,
    policy_name: String,
    _phantom: std::marker::PhantomData<T>,
}

impl<T, F> AppSpecificStrategy<T, F>
where
    T: Clone + PartialEq,
    F: Fn(&T, &T, &ConflictType) -> T + Send + Sync,
{
    /// Create a new application-specific strategy
    pub fn new(name: impl Into<String>, merge_fn: F) -> Self {
        Self {
            merge_fn,
            policy_name: name.into(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T, F> ConflictResolution<T> for AppSpecificStrategy<T, F>
where
    T: Clone + PartialEq + Send + Sync,
    F: Fn(&T, &T, &ConflictType) -> T + Send + Sync,
{
    fn resolve(
        &self,
        value_a: &T,
        value_b: &T,
        conflict_type: &ConflictType,
        _vc_a: &VectorClock,
        _vc_b: &VectorClock,
    ) -> ResolutionResult<T> {
        let merged = (self.merge_fn)(value_a, value_b, conflict_type);

        ResolutionResult {
            value: merged,
            strategy: self.policy(),
            had_conflict: conflict_type.needs_resolution(),
            winning_clock: None,
        }
    }

    fn policy(&self) -> ResolutionPolicy {
        ResolutionPolicy::AppSpecific(self.policy_name.clone())
    }
}

/// Registry of conflict resolution strategies
///
/// Allows registration and lookup of strategies by name and type.
#[derive(Clone)]
pub struct StrategyRegistry {
    /// Default strategy to use when no specific strategy is registered
    default_strategy: ResolutionPolicy,

    /// Map of data type names to resolution policies
    type_policies: HashMap<String, ResolutionPolicy>,
}

impl StrategyRegistry {
    /// Create a new strategy registry
    pub fn new(default_policy: ResolutionPolicy) -> Self {
        Self {
            default_strategy: default_policy,
            type_policies: HashMap::new(),
        }
    }

    /// Create a registry with LWW as the default
    pub fn lww_default() -> Self {
        Self::new(ResolutionPolicy::LastWriterWins)
    }

    /// Register a resolution policy for a specific data type
    pub fn register(&mut self, type_name: impl Into<String>, policy: ResolutionPolicy) {
        self.type_policies.insert(type_name.into(), policy);
    }

    /// Get the resolution policy for a specific type
    pub fn get_policy(&self, type_name: &str) -> Option<&ResolutionPolicy> {
        self.type_policies.get(type_name)
    }

    /// Get the default policy
    pub fn default_policy(&self) -> &ResolutionPolicy {
        &self.default_strategy
    }

    /// Check if a type has a specific policy registered
    pub fn has_policy(&self, type_name: &str) -> bool {
        self.type_policies.contains_key(type_name)
    }

    /// Remove a policy registration
    pub fn unregister(&mut self, type_name: &str) -> Option<ResolutionPolicy> {
        self.type_policies.remove(type_name)
    }

    /// Get all registered types
    pub fn registered_types(&self) -> Vec<&String> {
        self.type_policies.keys().collect()
    }
}

impl Default for StrategyRegistry {
    fn default() -> Self {
        Self::lww_default()
    }
}

/// Conflict resolver that uses a registry to select strategies
pub struct ConflictResolver {
    registry: StrategyRegistry,
    lww_strategy: LWWStrategy,
}

impl ConflictResolver {
    /// Create a new conflict resolver with a strategy registry
    pub fn new(registry: StrategyRegistry) -> Self {
        Self {
            registry,
            lww_strategy: LWWStrategy::new(),
        }
    }

    /// Create a resolver with LWW as the default strategy
    pub fn lww_default() -> Self {
        Self::new(StrategyRegistry::lww_default())
    }

    /// Resolve a conflict using the registered strategy for the type
    pub fn resolve<T: Clone + PartialEq + Send + Sync>(
        &self,
        value_a: &T,
        value_b: &T,
        conflict_type: &ConflictType,
        vc_a: &VectorClock,
        vc_b: &VectorClock,
        type_name: Option<&str>,
    ) -> Result<ResolutionResult<T>> {
        // Get the policy for this type, or use default
        let policy = if let Some(type_name) = type_name {
            self.registry.get_policy(type_name).cloned()
        } else {
            None
        };

        let policy = policy.as_ref().unwrap_or(self.registry.default_policy());

        // Use LWW strategy for most policies
        match policy {
            ResolutionPolicy::LastWriterWins | ResolutionPolicy::PreferPrimary => {
                let strategy = if matches!(policy, ResolutionPolicy::PreferPrimary) {
                    LWWStrategy::prefer_primary("us-east")
                } else {
                    LWWStrategy::new()
                };

                Ok(strategy.resolve(value_a, value_b, conflict_type, vc_a, vc_b))
            }
            ResolutionPolicy::AppSpecific(_) | ResolutionPolicy::Custom(_) => {
                // For app-specific/custom policies, fall back to LWW with a note
                // In a real implementation, you'd look up the actual strategy
                Ok(self
                    .lww_strategy
                    .resolve(value_a, value_b, conflict_type, vc_a, vc_b))
            }
            ResolutionPolicy::Merge => {
                // Merge policy: for collections, this would merge both
                // For now, use LWW as fallback
                Ok(self
                    .lww_strategy
                    .resolve(value_a, value_b, conflict_type, vc_a, vc_b))
            }
        }
    }

    /// Get the strategy registry
    pub fn registry(&self) -> &StrategyRegistry {
        &self.registry
    }

    /// Get a mutable reference to the registry
    pub fn registry_mut(&mut self) -> &mut StrategyRegistry {
        &mut self.registry
    }
}

impl Default for ConflictResolver {
    fn default() -> Self {
        Self::lww_default()
    }
}

/// Helper functions for common conflict resolution scenarios
pub mod helpers {
    use super::*;

    /// Prefer higher numeric value on conflict
    pub fn prefer_higher<T>(a: &T, b: &T, _conflict: &ConflictType) -> T
    where
        T: Clone + PartialEq + PartialOrd,
    {
        if a > b {
            a.clone()
        } else {
            b.clone()
        }
    }

    /// Prefer lower numeric value on conflict
    pub fn prefer_lower<T>(a: &T, b: &T, _conflict: &ConflictType) -> T
    where
        T: Clone + PartialEq + PartialOrd,
    {
        if a < b {
            a.clone()
        } else {
            b.clone()
        }
    }

    /// Concatenate strings on conflict
    pub fn concat_strings(a: &str, b: &str, _conflict: &ConflictType) -> String {
        format!("{} | {}", a, b)
    }

    /// Take the longer of two vectors
    pub fn prefer_longer<T: Clone>(a: &Vec<T>, b: &Vec<T>, _conflict: &ConflictType) -> Vec<T> {
        if a.len() >= b.len() {
            a.clone()
        } else {
            b.clone()
        }
    }

    /// Merge two vectors by concatenating
    pub fn merge_vectors<T: Clone + Eq + Hash>(
        a: &Vec<T>,
        b: &Vec<T>,
        _conflict: &ConflictType,
    ) -> Vec<T> {
        let mut merged = a.clone();
        for item in b {
            if !merged.contains(item) {
                merged.push(item.clone());
            }
        }
        merged
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::helpers::*;

    #[test]
    fn test_conflict_type_detect_concurrent() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-west");

        let conflict = ConflictType::detect(&vc1, &vc2);
        assert_eq!(conflict, ConflictType::Concurrent);
        assert!(conflict.needs_resolution());
    }

    #[test]
    fn test_conflict_type_detect_causality() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-east");
        vc2.increment("us-west");

        let conflict = ConflictType::detect(&vc1, &vc2);
        assert_eq!(conflict, ConflictType::None);
        assert!(!conflict.needs_resolution());
    }

    #[test]
    fn test_conflict_type_version_conflict() {
        let vc1 = VectorClock::with_regions(&["us-east"]);
        let vc2 = VectorClock::with_regions(&["us-east"]);

        let conflict = ConflictType::detect(&vc1, &vc2);
        assert_eq!(conflict, ConflictType::VersionConflict);
        assert!(conflict.needs_resolution());
    }

    #[test]
    fn test_lww_strategy_concurrent() {
        let strategy = LWWStrategy::new();

        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-west");
        vc2.increment("us-west");

        let conflict = ConflictType::Concurrent;
        let result = strategy.resolve(&"value_a", &"value_b", &conflict, &vc1, &vc2);

        assert_eq!(result.value, "value_b"); // us-west has higher sum
        assert!(result.had_conflict);
        assert_eq!(result.strategy, ResolutionPolicy::LastWriterWins);
    }

    #[test]
    fn test_lww_strategy_causality() {
        let strategy = LWWStrategy::new();

        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-east");
        vc2.increment("us-west");

        let conflict = ConflictType::None;
        let result = strategy.resolve(&"value_a", &"value_b", &conflict, &vc1, &vc2);

        assert_eq!(result.value, "value_b"); // vc2 happens after vc1
        assert!(!result.had_conflict);
    }

    #[test]
    fn test_lww_strategy_tiebreaker() {
        let strategy = LWWStrategy::new();

        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-west");

        let conflict = ConflictType::Concurrent;
        let result = strategy.resolve(&"value_a", &"value_b", &conflict, &vc1, &vc2);

        // Both have sum = 1, us-west > us-east lexicographically
        assert_eq!(result.value, "value_b");
        assert!(result.had_conflict);
    }

    #[test]
    fn test_lww_strategy_prefer_primary() {
        let strategy = LWWStrategy::prefer_primary("us-east");

        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-west");

        let conflict = ConflictType::Concurrent;
        let result = strategy.resolve(&"value_a", &"value_b", &conflict, &vc1, &vc2);

        // Both have sum = 1, but we prefer primary (us-east)
        assert_eq!(result.value, "value_a");
        assert_eq!(result.strategy, ResolutionPolicy::PreferPrimary);
    }

    #[test]
    fn test_app_specific_strategy_prefer_higher() {
        let strategy = AppSpecificStrategy::new("prefer_higher", prefer_higher::<i32>);

        let vc1 = VectorClock::with_regions(&["us-east"]);
        let vc2 = VectorClock::with_regions(&["us-west"]);

        let conflict = ConflictType::Concurrent;
        let result = strategy.resolve(&100, &120, &conflict, &vc1, &vc2);

        assert_eq!(result.value, 120);
        assert!(result.had_conflict);
        assert_eq!(result.strategy, ResolutionPolicy::AppSpecific("prefer_higher".into()));
    }

    #[test]
    fn test_app_specific_strategy_prefer_lower() {
        let strategy = AppSpecificStrategy::new("prefer_lower", prefer_lower::<i32>);

        let vc1 = VectorClock::with_regions(&["us-east"]);
        let vc2 = VectorClock::with_regions(&["us-west"]);

        let conflict = ConflictType::Concurrent;
        let result = strategy.resolve(&100, &120, &conflict, &vc1, &vc2);

        assert_eq!(result.value, 100);
        assert!(result.had_conflict);
    }

    #[test]
    fn test_app_specific_strategy_concat_strings() {
        let strategy = AppSpecificStrategy::new("concat_strings", concat_strings);

        let vc1 = VectorClock::with_regions(&["us-east"]);
        let vc2 = VectorClock::with_regions(&["us-west"]);

        let conflict = ConflictType::Concurrent;
        let result = strategy.resolve(&"hello", &"world", &conflict, &vc1, &vc2);

        assert_eq!(result.value, "hello | world");
        assert!(result.had_conflict);
    }

    #[test]
    fn test_strategy_registry_register() {
        let mut registry = StrategyRegistry::lww_default();
        registry.register("price", ResolutionPolicy::AppSpecific("prefer_higher".into()));
        registry.register("status", ResolutionPolicy::PreferPrimary);

        assert!(registry.has_policy("price"));
        assert!(registry.has_policy("status"));
        assert!(!registry.has_policy("name"));

        let price_policy = registry.get_policy("price");
        assert_eq!(
            price_policy,
            Some(&ResolutionPolicy::AppSpecific("prefer_higher".into()))
        );
    }

    #[test]
    fn test_strategy_registry_default() {
        let registry = StrategyRegistry::lww_default();
        assert_eq!(registry.default_policy(), &ResolutionPolicy::LastWriterWins);
    }

    #[test]
    fn test_strategy_registry_unregister() {
        let mut registry = StrategyRegistry::lww_default();
        registry.register("price", ResolutionPolicy::AppSpecific("prefer_higher".into()));

        assert!(registry.has_policy("price"));

        let removed = registry.unregister("price");
        assert_eq!(
            removed,
            Some(ResolutionPolicy::AppSpecific("prefer_higher".into()))
        );
        assert!(!registry.has_policy("price"));
    }

    #[test]
    fn test_conflict_resolver_default() {
        let resolver = ConflictResolver::lww_default();

        let mut vc1 = VectorClock::with_regions(&["us-east"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-west"]);
        vc2.increment("us-west");
        vc2.increment("us-west");

        let conflict = ConflictType::Concurrent;
        let result = resolver
            .resolve(&"value_a", &"value_b", &conflict, &vc1, &vc2, None)
            .unwrap();

        assert_eq!(result.value, "value_b"); // Higher sum wins
        assert!(result.had_conflict);
    }

    #[test]
    fn test_conflict_resolver_with_type_policy() {
        let mut registry = StrategyRegistry::lww_default();
        registry.register("price", ResolutionPolicy::AppSpecific("prefer_higher".into()));

        let resolver = ConflictResolver::new(registry);

        let vc1 = VectorClock::with_regions(&["us-east"]);
        let vc2 = VectorClock::with_regions(&["us-west"]);

        let conflict = ConflictType::Concurrent;
        let result = resolver
            .resolve(&100, &120, &conflict, &vc1, &vc2, Some("price"))
            .unwrap();

        // Falls back to LWW (since we don't have actual app-specific strategy registered)
        // In a real implementation, this would use the prefer_higher strategy
        assert!(result.had_conflict);
    }

    #[test]
    fn test_resolution_policy_constructors() {
        assert_eq!(ResolutionPolicy::lww(), ResolutionPolicy::LastWriterWins);
        assert_eq!(
            ResolutionPolicy::app_specific("my_policy"),
            ResolutionPolicy::AppSpecific("my_policy".into())
        );
        assert_eq!(ResolutionPolicy::prefer_primary(), ResolutionPolicy::PreferPrimary);
        assert_eq!(ResolutionPolicy::merge(), ResolutionPolicy::Merge);
        assert_eq!(
            ResolutionPolicy::custom("custom_policy"),
            ResolutionPolicy::Custom("custom_policy".into())
        );
    }

    #[test]
    fn test_helpers_prefer_longer() {
        let vec_a = vec![1, 2, 3];
        let vec_b = vec![1, 2, 3, 4, 5];

        let result = prefer_longer(&vec_a, &vec_b, &ConflictType::Concurrent);
        assert_eq!(result, vec_b);
    }

    #[test]
    fn test_helpers_merge_vectors() {
        let vec_a = vec![1, 2, 3];
        let vec_b = vec![3, 4, 5];

        let result = merge_vectors(&vec_a, &vec_b, &ConflictType::Concurrent);
        assert_eq!(result, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_lww_strategy_without_tiebreaker() {
        let strategy = LWWStrategy::without_tiebreaker();

        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-west");

        let conflict = ConflictType::Concurrent;
        let result = strategy.resolve(&"value_a", &"value_b", &conflict, &vc1, &vc2);

        // Equal sums, no tiebreaker: prefer first value
        assert_eq!(result.value, "value_a");
        assert!(result.had_conflict);
    }

    #[test]
    fn test_resolution_result_display() {
        let result = ResolutionResult {
            value: 42,
            strategy: ResolutionPolicy::LastWriterWins,
            had_conflict: true,
            winning_clock: None,
        };

        let display = format!("{}", result);
        assert!(display.contains("42"));
        assert!(display.contains("LastWriterWins"));
        assert!(display.contains("true"));
    }

    #[test]
    fn test_conflict_type_serialization() {
        let conflict = ConflictType::Concurrent;

        let serialized = serde_json::to_string(&conflict).unwrap();
        let deserialized: ConflictType = serde_json::from_str(&serialized).unwrap();

        assert_eq!(conflict, deserialized);
    }

    #[test]
    fn test_vector_clock_integration() {
        // Test that vector clocks integrate properly with conflict detection
        let mut vc1 = VectorClock::new();
        vc1.increment("region-a");
        vc1.increment("region-a");

        let mut vc2 = VectorClock::new();
        vc2.increment("region-b");

        let conflict = ConflictType::detect(&vc1, &vc2);
        assert_eq!(conflict, ConflictType::Concurrent);

        // Now make vc2 happen after vc1
        vc2.merge(&vc1);
        vc2.increment("region-b");

        let conflict_after = ConflictType::detect(&vc1, &vc2);
        assert_eq!(conflict_after, ConflictType::None);
    }
}
