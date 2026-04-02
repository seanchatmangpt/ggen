//! Integration tests for type guards.

use ggen_poka_yoke::type_guards::*;

#[test]
fn test_state_machine_full_lifecycle() {
    // Uninitialized -> Initialized -> Running -> Stopped -> Initialized
    let machine = StateMachine::<Uninitialized>::new();
    let machine = machine.initialize("initial_config");

    assert_eq!(machine.config(), Some("initial_config"));

    let machine = machine.start();
    assert!(machine.is_running());

    let machine = machine.restart();
    assert!(machine.is_running());

    let machine = machine.stop();
    let machine = machine.reinitialize("new_config");

    assert_eq!(machine.config(), Some("new_config"));
}

#[test]
fn test_non_empty_vector_operations() {
    let mut vec = NonEmpty::new(1);
    assert_eq!(vec.len(), 1);
    assert!(!vec.is_empty());
    assert_eq!(vec.first(), &1);

    vec.push(2);
    vec.push(3);
    assert_eq!(vec.len(), 3);

    let items: Vec<_> = vec.iter().copied().collect();
    assert_eq!(items, vec![1, 2, 3]);

    *vec.first_mut() = 10;
    assert_eq!(vec.first(), &10);

    let standard_vec: Vec<_> = vec.into();
    assert_eq!(standard_vec, vec![10, 2, 3]);
}

#[test]
fn test_non_empty_string_validation() {
    let empty_result = NonEmptyString::new(String::new());
    assert!(empty_result.is_none());

    let valid = NonEmptyString::new("hello".to_string()).unwrap();
    assert_eq!(valid.as_str(), "hello");

    let cloned = valid.clone();
    assert_eq!(cloned.as_str(), "hello");

    let s = valid.into_string();
    assert_eq!(s, "hello");
}

#[test]
fn test_positive_numbers() {
    // u32
    assert!(Positive::<u32>::new(0).is_none());
    assert!(Positive::<u32>::new(1).is_some());

    let pos = Positive::<u32>::new(42).unwrap();
    assert_eq!(pos.get(), 42);

    // u64
    let pos64 = Positive::<u64>::new(1000).unwrap();
    assert_eq!(pos64.get(), 1000);

    // usize
    let pos_size = Positive::<usize>::new(100).unwrap();
    assert_eq!(pos_size.get(), 100);
}

#[test]
fn test_bounded_values() {
    type Percentage = Bounded<0, 100>;
    type Temperature = Bounded<-40, 50>;

    // Valid percentages
    let pct = Percentage::new(0).unwrap();
    assert_eq!(pct.get(), 0);

    let pct = Percentage::new(50).unwrap();
    assert_eq!(pct.get(), 50);

    let pct = Percentage::new(100).unwrap();
    assert_eq!(pct.get(), 100);

    // Invalid percentages
    assert!(Percentage::new(-1).is_none());
    assert!(Percentage::new(101).is_none());

    // Valid temperatures
    let temp = Temperature::new(-40).unwrap();
    assert_eq!(temp.get(), -40);

    let temp = Temperature::new(20).unwrap();
    assert_eq!(temp.get(), 20);

    let temp = Temperature::new(50).unwrap();
    assert_eq!(temp.get(), 50);

    // Invalid temperatures
    assert!(Temperature::new(-41).is_none());
    assert!(Temperature::new(51).is_none());

    // Check bounds
    assert_eq!(Percentage::min(), 0);
    assert_eq!(Percentage::max(), 100);
    assert_eq!(Temperature::min(), -40);
    assert_eq!(Temperature::max(), 50);
}

#[test]
fn test_non_empty_with_multiple_elements() {
    let vec = NonEmpty::with_tail(1, vec![2, 3, 4]);
    assert_eq!(vec.len(), 4);
    assert_eq!(vec.first(), &1);

    let items: Vec<_> = vec.into_iter().collect();
    assert_eq!(items, vec![1, 2, 3, 4]);
}

#[test]
fn test_bounded_ordering() {
    type Score = Bounded<0, 100>;

    let score1 = Score::new(50).unwrap();
    let score2 = Score::new(75).unwrap();
    let score3 = Score::new(50).unwrap();

    assert!(score1 < score2);
    assert!(score2 > score1);
    assert_eq!(score1, score3);
}
