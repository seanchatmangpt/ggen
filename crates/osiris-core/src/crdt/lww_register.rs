//! Last-Writer-Wins (LWW) Register CRDT
//!
//! A conflict-free replicated register where concurrent writes are resolved
//! by timestamp: the value with the highest timestamp wins.
//!
//! **Merge Semantics**: Takes the value from whichever write has the higher timestamp.
//! **Use Case**: System-controlled values where time is authoritative (e.g., health status, domain state).
//! **Caveat**: Requires reasonably synchronized clocks. In extreme clock skew scenarios, the "lost" write
//! cannot be recovered.

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

/// A Last-Writer-Wins register that resolves concurrent writes by timestamp.
///
/// # Examples
///
/// ```
/// use osiris_core::crdt::LWWRegister;
///
/// let mut register = LWWRegister::new("initial_value".to_string(), 100, "region-1");
/// register.set("new_value".to_string(), 105);
/// assert_eq!(register.get(), "new_value");
///
/// // Merge with older timestamp is ignored
/// let mut other = LWWRegister::new("older_value".to_string(), 50, "region-2");
/// register.merge(&other);
/// assert_eq!(register.get(), "new_value");
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LWWRegister<T: Clone + PartialEq> {
    /// The current value stored in this register
    value: T,
    /// Timestamp of the last write (highest timestamp wins on merge)
    timestamp: u64,
    /// Region ID that last wrote to this register (for debugging)
    region_id: String,
}

impl<T: Clone + PartialEq> LWWRegister<T> {
    /// Creates a new LWW register with an initial value and timestamp.
    ///
    /// # Arguments
    /// * `value` - The initial value
    /// * `timestamp` - The timestamp of this initial write (typically current time)
    /// * `region_id` - Identifier of the region making this write
    pub fn new(value: T, timestamp: u64, region_id: impl Into<String>) -> Self {
        Self {
            value,
            timestamp,
            region_id: region_id.into(),
        }
    }

    /// Returns a reference to the current value.
    pub fn get(&self) -> &T {
        &self.value
    }

    /// Returns a clone of the current value.
    pub fn get_cloned(&self) -> T {
        self.value.clone()
    }

    /// Updates the register with a new value if the provided timestamp is higher.
    ///
    /// # Arguments
    /// * `value` - The new value to store
    /// * `timestamp` - The timestamp of this write
    ///
    /// # Returns
    /// `true` if the value was updated, `false` if this write was older than the current state
    pub fn set(&mut self, value: T, timestamp: u64) -> bool {
        match timestamp.cmp(&self.timestamp) {
            Ordering::Greater => {
                self.value = value;
                self.timestamp = timestamp;
                true
            }
            Ordering::Equal => {
                // Tie-break: equal timestamps are allowed but don't update
                false
            }
            Ordering::Less => {
                // Older write: ignore (already have newer data)
                false
            }
        }
    }

    /// Updates the region ID (mainly for debugging/tracking)
    pub fn set_region_id(&mut self, region_id: impl Into<String>) {
        self.region_id = region_id.into();
    }

    /// Returns the current timestamp
    pub fn timestamp(&self) -> u64 {
        self.timestamp
    }

    /// Returns the region ID that last wrote to this register
    pub fn region_id(&self) -> &str {
        &self.region_id
    }

    /// Merges another register into this one (CRDT merge operation).
    ///
    /// The result is deterministic: whichever value has the higher timestamp wins.
    /// This operation is commutative, associative, and idempotent.
    ///
    /// # Arguments
    /// * `other` - The other register to merge
    ///
    /// # Returns
    /// `true` if this register's state changed, `false` if `other` was older
    pub fn merge(&mut self, other: &Self) -> bool {
        match other.timestamp.cmp(&self.timestamp) {
            Ordering::Greater => {
                self.value = other.value.clone();
                self.timestamp = other.timestamp;
                self.region_id = other.region_id.clone();
                true
            }
            Ordering::Equal => {
                // Equal timestamps: no update (already in sync or conflicting)
                false
            }
            Ordering::Less => {
                // Other is older: no change needed
                false
            }
        }
    }

    /// Returns true if this register and another have identical state
    pub fn is_identical(&self, other: &Self) -> bool {
        self.value == other.value && self.timestamp == other.timestamp
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lww_register_creation() {
        let reg = LWWRegister::new("hello".to_string(), 100, "region-1");
        assert_eq!(reg.get(), "hello");
        assert_eq!(reg.timestamp(), 100);
        assert_eq!(reg.region_id(), "region-1");
    }

    #[test]
    fn test_lww_register_set_newer() {
        let mut reg = LWWRegister::new("old".to_string(), 100, "region-1");
        let updated = reg.set("new".to_string(), 105);
        assert!(updated);
        assert_eq!(reg.get(), "new");
        assert_eq!(reg.timestamp(), 105);
    }

    #[test]
    fn test_lww_register_set_older() {
        let mut reg = LWWRegister::new("new".to_string(), 105, "region-1");
        let updated = reg.set("old".to_string(), 100);
        assert!(!updated);
        assert_eq!(reg.get(), "new");
    }

    #[test]
    fn test_lww_register_set_equal_timestamp() {
        let mut reg = LWWRegister::new("first".to_string(), 100, "region-1");
        let updated = reg.set("second".to_string(), 100);
        assert!(!updated);
        assert_eq!(reg.get(), "first");
    }

    #[test]
    fn test_lww_register_merge_newer() {
        let mut reg_a = LWWRegister::new("value_a".to_string(), 100, "region-a");
        let reg_b = LWWRegister::new("value_b".to_string(), 110, "region-b");

        let changed = reg_a.merge(&reg_b);
        assert!(changed);
        assert_eq!(reg_a.get(), "value_b");
        assert_eq!(reg_a.timestamp(), 110);
        assert_eq!(reg_a.region_id(), "region-b");
    }

    #[test]
    fn test_lww_register_merge_older() {
        let mut reg_a = LWWRegister::new("value_a".to_string(), 110, "region-a");
        let reg_b = LWWRegister::new("value_b".to_string(), 100, "region-b");

        let changed = reg_a.merge(&reg_b);
        assert!(!changed);
        assert_eq!(reg_a.get(), "value_a");
    }

    #[test]
    fn test_lww_register_merge_commutative() {
        let mut reg1 = LWWRegister::new("val1".to_string(), 100, "r1");
        let reg2 = LWWRegister::new("val2".to_string(), 120, "r2");

        let reg1_copy = LWWRegister::new("val1".to_string(), 100, "r1");
        let mut reg2_copy = LWWRegister::new("val2".to_string(), 120, "r2");

        // merge(A, B)
        reg1.merge(&reg2);

        // merge(B, A)
        reg2_copy.merge(&reg1_copy);

        // Both should be identical
        assert_eq!(reg1, reg2_copy);
    }

    #[test]
    fn test_lww_register_merge_idempotent() {
        let mut reg = LWWRegister::new("value".to_string(), 100, "region");
        let other = LWWRegister::new("value".to_string(), 100, "region");

        let first_merge = reg.merge(&other);
        let second_merge = reg.merge(&other);

        assert!(!first_merge); // No change on first merge
        assert!(!second_merge); // No change on second merge
    }

    #[test]
    fn test_lww_register_is_identical() {
        let reg1 = LWWRegister::new("value".to_string(), 100, "region");
        let reg2 = LWWRegister::new("value".to_string(), 100, "region");
        let reg3 = LWWRegister::new("value".to_string(), 101, "region");

        assert!(reg1.is_identical(&reg2));
        assert!(!reg1.is_identical(&reg3));
    }

    #[test]
    fn test_lww_register_get_cloned() {
        let reg = LWWRegister::new(vec![1, 2, 3], 100, "region");
        let cloned = reg.get_cloned();
        assert_eq!(cloned, vec![1, 2, 3]);
    }
}
