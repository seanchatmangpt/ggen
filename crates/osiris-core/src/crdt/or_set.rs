//! Observed-Remove Set (OR-Set) CRDT
//!
//! A conflict-free replicated set where concurrent add/remove operations commute.
//! Each element is tagged with (region_id, unique_id) to distinguish concurrent adds.
//!
//! **Merge Semantics**: Union of all elements ever added, minus those removed.
//! **Use Case**: Subscriptions, membership lists, tags where concurrency is high.
//! **Key Property**: Add/remove operations commute—order doesn't matter.

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Unique tag for an element added to the OR-Set.
/// Combines region ID and a sequence number to ensure uniqueness across regions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ElementTag {
    /// ID of the region that added this element
    pub region_id: String,
    /// Unique sequence number within the region
    pub sequence: u64,
}

/// An Observed-Remove Set (OR-Set) CRDT.
///
/// Elements can be added and removed concurrently. If an element is added in one region
/// and removed in another before they sync, the add wins (add happens before remove).
/// Subsequent removes must reference the original add's tag.
///
/// # Examples
///
/// ```
/// use osiris_core::crdt::OrSet;
///
/// let mut set = OrSet::new("region-1", 1);
/// set.add("alice");
/// set.add("bob");
/// assert!(set.contains("alice"));
///
/// // Remove alice (by tag)
/// let tag = set.get_tag("alice").unwrap().clone();
/// set.remove(&tag);
/// assert!(!set.contains("alice"));
/// assert!(set.contains("bob"));
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrSet<T: Clone + PartialEq + Eq + std::hash::Hash> {
    /// Map from element to set of tags that have added it (elements present)
    added: HashMap<T, HashSet<ElementTag>>,
    /// Map from element to set of tags that have been removed
    removed: HashMap<T, HashSet<ElementTag>>,
    /// Current region ID (for tagging new adds)
    region_id: String,
    /// Next sequence number to assign in this region
    next_sequence: u64,
}

impl<T: Clone + PartialEq + Eq + std::hash::Hash> OrSet<T> {
    /// Creates a new OR-Set in a given region.
    ///
    /// # Arguments
    /// * `region_id` - Identifier for this region
    /// * `initial_sequence` - Starting sequence number (should be unique per region)
    pub fn new(region_id: impl Into<String>, initial_sequence: u64) -> Self {
        Self {
            added: HashMap::new(),
            removed: HashMap::new(),
            region_id: region_id.into(),
            next_sequence: initial_sequence,
        }
    }

    /// Adds an element to the set in this region.
    ///
    /// # Returns
    /// The tag assigned to this add (used later for removal)
    pub fn add(&mut self, element: T) -> ElementTag {
        let tag = ElementTag {
            region_id: self.region_id.clone(),
            sequence: self.next_sequence,
        };
        self.next_sequence += 1;

        self.added.entry(element).or_insert_with(HashSet::new).insert(tag.clone());
        tag
    }

    /// Removes a specific add operation (by tag) from the set.
    ///
    /// This only removes the specific add identified by the tag. If an element
    /// has multiple tags (concurrent adds), other tags remain.
    ///
    /// # Arguments
    /// * `element` - The element to remove
    /// * `tag` - The tag of the add operation to remove
    pub fn remove(&mut self, element: &T, tag: &ElementTag) {
        self.removed.entry(element.clone()).or_insert_with(HashSet::new).insert(tag.clone());
    }

    /// Removes all instances of an element from the set.
    ///
    /// This removes all tags associated with the element.
    pub fn remove_all(&mut self, element: &T) {
        if let Some(tags) = self.added.get(element) {
            let removed = self.removed.entry(element.clone()).or_insert_with(HashSet::new);
            for tag in tags.iter() {
                removed.insert(tag.clone());
            }
        }
    }

    /// Checks if an element is in the set.
    ///
    /// An element is in the set if it has at least one add tag that hasn't been removed.
    pub fn contains(&self, element: &T) -> bool {
        if let Some(added_tags) = self.added.get(element) {
            if let Some(removed_tags) = self.removed.get(element) {
                added_tags.iter().any(|tag| !removed_tags.contains(tag))
            } else {
                !added_tags.is_empty()
            }
        } else {
            false
        }
    }

    /// Returns all tags that have been added for an element (even if removed later).
    pub fn get_tags(&self, element: &T) -> Option<HashSet<ElementTag>> {
        self.added.get(element).cloned()
    }

    /// Returns a single tag for an element (if it exists).
    pub fn get_tag(&self, element: &T) -> Option<&ElementTag> {
        self.added.get(element).and_then(|tags| tags.iter().next())
    }

    /// Returns all elements currently in the set.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.added
            .iter()
            .filter_map(move |(elem, tags)| {
                let is_present = tags.iter().any(|tag| {
                    self.removed
                        .get(elem)
                        .map(|removed_tags| !removed_tags.contains(tag))
                        .unwrap_or(true)
                });
                if is_present {
                    Some(elem)
                } else {
                    None
                }
            })
    }

    /// Returns the current size of the set.
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    /// Returns true if the set is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Merges another OR-Set into this one (CRDT merge operation).
    ///
    /// Takes the union of added elements and removed tags. This is commutative,
    /// associative, and idempotent.
    pub fn merge(&mut self, other: &Self) {
        // Merge all added tags
        for (element, other_tags) in &other.added {
            let our_tags = self.added.entry(element.clone()).or_insert_with(HashSet::new);
            for tag in other_tags {
                our_tags.insert(tag.clone());
            }
        }

        // Merge all removed tags
        for (element, other_removed) in &other.removed {
            let our_removed = self.removed.entry(element.clone()).or_insert_with(HashSet::new);
            for tag in other_removed {
                our_removed.insert(tag.clone());
            }
        }
    }

    /// Returns true if this set and another have identical state.
    pub fn is_identical(&self, other: &Self) -> bool {
        // Check if added maps are identical
        if self.added.len() != other.added.len() {
            return false;
        }
        for (elem, tags) in &self.added {
            if other.added.get(elem) != Some(tags) {
                return false;
            }
        }

        // Check if removed maps are identical
        if self.removed.len() != other.removed.len() {
            return false;
        }
        for (elem, tags) in &self.removed {
            if other.removed.get(elem) != Some(tags) {
                return false;
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_or_set_add_and_contains() {
        let mut set = OrSet::new("region-1", 1);
        set.add("alice".to_string());
        set.add("bob".to_string());

        assert!(set.contains(&"alice".to_string()));
        assert!(set.contains(&"bob".to_string()));
        assert!(!set.contains(&"charlie".to_string()));
    }

    #[test]
    fn test_or_set_remove() {
        let mut set = OrSet::new("region-1", 1);
        let tag = set.add("alice".to_string());
        assert!(set.contains(&"alice".to_string()));

        set.remove(&"alice".to_string(), &tag);
        assert!(!set.contains(&"alice".to_string()));
    }

    #[test]
    fn test_or_set_remove_all() {
        let mut set = OrSet::new("region-1", 1);
        set.add("alice".to_string());
        set.add("alice".to_string());
        assert!(set.contains(&"alice".to_string()));

        set.remove_all(&"alice".to_string());
        assert!(!set.contains(&"alice".to_string()));
    }

    #[test]
    fn test_or_set_concurrent_add() {
        let mut set = OrSet::new("region-1", 1);
        set.add("alice".to_string());
        assert!(set.contains(&"alice".to_string()));

        let tags = set.get_tags(&"alice".to_string()).unwrap();
        assert_eq!(tags.len(), 1);
    }

    #[test]
    fn test_or_set_merge_union() {
        let mut set1 = OrSet::new("region-1", 1);
        set1.add("alice".to_string());
        set1.add("bob".to_string());

        let mut set2 = OrSet::new("region-2", 100);
        set2.add("charlie".to_string());
        set2.add("diana".to_string());

        set1.merge(&set2);

        assert!(set1.contains(&"alice".to_string()));
        assert!(set1.contains(&"bob".to_string()));
        assert!(set1.contains(&"charlie".to_string()));
        assert!(set1.contains(&"diana".to_string()));
    }

    #[test]
    fn test_or_set_merge_respects_removes() {
        let mut set1 = OrSet::new("region-1", 1);
        let tag_alice = set1.add("alice".to_string());
        let tag_bob = set1.add("bob".to_string());

        let mut set2 = OrSet::new("region-2", 100);
        set2.add("charlie".to_string());
        set2.added.insert("alice".to_string(), {
            let mut tags = HashSet::new();
            tags.insert(tag_alice.clone());
            tags
        });
        set2.removed.insert("alice".to_string(), {
            let mut tags = HashSet::new();
            tags.insert(tag_alice.clone());
            tags
        });

        set1.merge(&set2);

        // Alice was removed in set2, so after merge it should be gone
        assert!(!set1.contains(&"alice".to_string()));
        assert!(set1.contains(&"bob".to_string()));
        assert!(set1.contains(&"charlie".to_string()));
    }

    #[test]
    fn test_or_set_merge_commutative() {
        let mut set1 = OrSet::new("region-1", 1);
        set1.add("alice".to_string());
        set1.add("bob".to_string());

        let mut set2 = OrSet::new("region-2", 100);
        set2.add("charlie".to_string());

        let mut set1_copy = OrSet::new("region-1", 1);
        set1_copy.add("alice".to_string());
        set1_copy.add("bob".to_string());

        let mut set2_copy = OrSet::new("region-2", 100);
        set2_copy.add("charlie".to_string());

        // merge(set1, set2)
        set1.merge(&set2);

        // merge(set2_copy, set1_copy)
        set2_copy.merge(&set1_copy);

        // Both should have the same elements
        assert_eq!(set1.is_identical(&set2_copy), true);
    }

    #[test]
    fn test_or_set_len_and_is_empty() {
        let mut set = OrSet::new("region-1", 1);
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);

        set.add("alice".to_string());
        assert!(!set.is_empty());
        assert_eq!(set.len(), 1);

        set.add("bob".to_string());
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn test_or_set_iter() {
        let mut set = OrSet::new("region-1", 1);
        set.add("alice".to_string());
        set.add("bob".to_string());
        set.add("charlie".to_string());

        let elements: Vec<_> = set.iter().collect();
        assert_eq!(elements.len(), 3);
        assert!(elements.contains(&&"alice".to_string()));
        assert!(elements.contains(&&"bob".to_string()));
        assert!(elements.contains(&&"charlie".to_string()));
    }

    #[test]
    fn test_or_set_add_idempotence_after_merge() {
        let mut set1 = OrSet::new("region-1", 1);
        set1.add("alice".to_string());

        let set2 = set1.clone();
        set1.merge(&set2);

        // After merging with itself, alice should still be in the set
        assert!(set1.contains(&"alice".to_string()));
        assert_eq!(set1.len(), 1);
    }
}
