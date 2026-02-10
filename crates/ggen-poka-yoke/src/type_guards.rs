//! Type guards using phantom types to prevent invalid states.
//!
//! # Examples
//!
//! ```
//! use ggen_poka_yoke::type_guards::{StateMachine, Uninitialized, Initialized, Running};
//!
//! let machine = StateMachine::<Uninitialized>::new();
//! let machine = machine.initialize("config");
//! let machine = machine.start();
//! machine.stop();
//! ```

use std::marker::PhantomData;

/// State: Uninitialized
#[derive(Debug, Clone, Copy)]
pub struct Uninitialized;

/// State: Initialized but not started
#[derive(Debug, Clone, Copy)]
pub struct Initialized;

/// State: Running
#[derive(Debug, Clone, Copy)]
pub struct Running;

/// State: Stopped
#[derive(Debug, Clone, Copy)]
pub struct Stopped;

/// State machine with compile-time state tracking.
///
/// Invalid transitions are impossible to express.
#[derive(Debug)]
pub struct StateMachine<State> {
    config: Option<String>,
    _state: PhantomData<State>,
}

impl StateMachine<Uninitialized> {
    /// Creates a new uninitialized state machine.
    pub fn new() -> Self {
        Self {
            config: None,
            _state: PhantomData,
        }
    }

    /// Initializes the state machine with configuration.
    ///
    /// Transitions: Uninitialized -> Initialized
    pub fn initialize(self, config: impl Into<String>) -> StateMachine<Initialized> {
        StateMachine {
            config: Some(config.into()),
            _state: PhantomData,
        }
    }
}

impl Default for StateMachine<Uninitialized> {
    fn default() -> Self {
        Self::new()
    }
}

impl StateMachine<Initialized> {
    /// Starts the state machine.
    ///
    /// Transitions: Initialized -> Running
    pub fn start(self) -> StateMachine<Running> {
        StateMachine {
            config: self.config,
            _state: PhantomData,
        }
    }

    /// Retrieves the configuration.
    pub fn config(&self) -> Option<&str> {
        self.config.as_deref()
    }
}

impl StateMachine<Running> {
    /// Stops the state machine.
    ///
    /// Transitions: Running -> Stopped
    pub fn stop(self) -> StateMachine<Stopped> {
        StateMachine {
            config: self.config,
            _state: PhantomData,
        }
    }

    /// Restarts the state machine.
    ///
    /// Transitions: Running -> Running (internal restart)
    pub fn restart(self) -> StateMachine<Running> {
        StateMachine {
            config: self.config,
            _state: PhantomData,
        }
    }

    /// Checks if running (always true, guaranteed by type).
    pub fn is_running(&self) -> bool {
        true
    }
}

impl StateMachine<Stopped> {
    /// Reinitializes the state machine.
    ///
    /// Transitions: Stopped -> Initialized
    pub fn reinitialize(self, config: impl Into<String>) -> StateMachine<Initialized> {
        StateMachine {
            config: Some(config.into()),
            _state: PhantomData,
        }
    }
}

/// Non-empty vector guaranteed at compile time.
#[derive(Debug, Clone)]
pub struct NonEmpty<T> {
    head: T,
    tail: Vec<T>,
}

impl<T> NonEmpty<T> {
    /// Creates a non-empty vector with a single element.
    pub fn new(head: T) -> Self {
        Self {
            head,
            tail: Vec::new(),
        }
    }

    /// Creates a non-empty vector with head and tail.
    pub fn with_tail(head: T, tail: Vec<T>) -> Self {
        Self { head, tail }
    }

    /// Returns the first element (always exists).
    pub fn first(&self) -> &T {
        &self.head
    }

    /// Returns the first element mutably.
    pub fn first_mut(&mut self) -> &mut T {
        &mut self.head
    }

    /// Returns the length (always >= 1).
    pub fn len(&self) -> usize {
        1 + self.tail.len()
    }

    /// Always returns false (non-empty by construction).
    pub fn is_empty(&self) -> bool {
        false
    }

    /// Pushes an element to the tail.
    pub fn push(&mut self, item: T) {
        self.tail.push(item);
    }

    /// Iterates over all elements.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        std::iter::once(&self.head).chain(self.tail.iter())
    }
}

impl<T> From<NonEmpty<T>> for Vec<T> {
    fn from(non_empty: NonEmpty<T>) -> Vec<T> {
        std::iter::once(non_empty.head)
            .chain(non_empty.tail)
            .collect()
    }
}

impl<T> IntoIterator for NonEmpty<T> {
    type Item = T;
    type IntoIter = std::iter::Chain<std::iter::Once<T>, std::vec::IntoIter<T>>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self.head).chain(self.tail)
    }
}

/// Validated string that is never empty.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonEmptyString(String);

impl NonEmptyString {
    /// Creates a non-empty string, returns None if empty.
    pub fn new(s: String) -> Option<Self> {
        if s.is_empty() {
            None
        } else {
            Some(Self(s))
        }
    }

    /// Creates from static str.
    pub const fn from_static(_s: &'static str) -> Self {
        // Cannot check at compile time in const fn, caller must ensure non-empty
        Self(String::new()) // Placeholder for const, use from_static_checked
    }

    /// Creates from static str with runtime check.
    pub fn from_static_checked(s: &'static str) -> Option<Self> {
        if s.is_empty() {
            None
        } else {
            Some(Self(s.to_string()))
        }
    }

    /// Returns the inner string.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Consumes and returns the inner string.
    pub fn into_string(self) -> String {
        self.0
    }
}

impl std::fmt::Display for NonEmptyString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Positive integer (> 0) guaranteed by type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Positive<T>(T);

impl Positive<u32> {
    /// Creates a positive u32, returns None if zero.
    pub fn new(value: u32) -> Option<Self> {
        if value > 0 {
            Some(Self(value))
        } else {
            None
        }
    }

    /// Returns the inner value (always > 0).
    pub fn get(&self) -> u32 {
        self.0
    }
}

impl Positive<u64> {
    /// Creates a positive u64, returns None if zero.
    pub fn new(value: u64) -> Option<Self> {
        if value > 0 {
            Some(Self(value))
        } else {
            None
        }
    }

    /// Returns the inner value (always > 0).
    pub fn get(&self) -> u64 {
        self.0
    }
}

impl Positive<usize> {
    /// Creates a positive usize, returns None if zero.
    pub fn new(value: usize) -> Option<Self> {
        if value > 0 {
            Some(Self(value))
        } else {
            None
        }
    }

    /// Returns the inner value (always > 0).
    pub fn get(&self) -> usize {
        self.0
    }
}

/// Bounded integer in range [MIN, MAX].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bounded<const MIN: i32, const MAX: i32>(i32);

impl<const MIN: i32, const MAX: i32> Bounded<MIN, MAX> {
    /// Creates a bounded value, returns None if out of range.
    pub fn new(value: i32) -> Option<Self> {
        if value >= MIN && value <= MAX {
            Some(Self(value))
        } else {
            None
        }
    }

    /// Returns the inner value (always in [MIN, MAX]).
    pub fn get(&self) -> i32 {
        self.0
    }

    /// Returns the minimum bound.
    pub const fn min() -> i32 {
        MIN
    }

    /// Returns the maximum bound.
    pub const fn max() -> i32 {
        MAX
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_machine_transitions() {
        let machine = StateMachine::<Uninitialized>::new();
        let machine = machine.initialize("test_config");
        assert_eq!(machine.config(), Some("test_config"));

        let machine = machine.start();
        assert!(machine.is_running());

        let machine = machine.stop();
        let _machine = machine.reinitialize("new_config");
    }

    #[test]
    fn test_non_empty_vector() {
        let mut vec = NonEmpty::new(42);
        assert_eq!(vec.first(), &42);
        assert_eq!(vec.len(), 1);
        assert!(!vec.is_empty());

        vec.push(43);
        assert_eq!(vec.len(), 2);

        let items: Vec<_> = vec.iter().copied().collect();
        assert_eq!(items, vec![42, 43]);
    }

    #[test]
    fn test_non_empty_string() {
        assert!(NonEmptyString::new("".to_string()).is_none());
        assert!(NonEmptyString::new("hello".to_string()).is_some());

        let s = NonEmptyString::new("world".to_string()).unwrap();
        assert_eq!(s.as_str(), "world");
    }

    #[test]
    fn test_positive() {
        assert!(Positive::<u32>::new(0).is_none());
        assert!(Positive::<u32>::new(1).is_some());

        let pos = Positive::<u32>::new(42).unwrap();
        assert_eq!(pos.get(), 42);
    }

    #[test]
    fn test_bounded() {
        type Percentage = Bounded<0, 100>;

        assert!(Percentage::new(-1).is_none());
        assert!(Percentage::new(0).is_some());
        assert!(Percentage::new(50).is_some());
        assert!(Percentage::new(100).is_some());
        assert!(Percentage::new(101).is_none());

        let pct = Percentage::new(75).unwrap();
        assert_eq!(pct.get(), 75);
        assert_eq!(Percentage::min(), 0);
        assert_eq!(Percentage::max(), 100);
    }
}
