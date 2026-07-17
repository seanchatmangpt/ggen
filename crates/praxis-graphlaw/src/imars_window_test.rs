#![cfg(test)]

use crate::imars_window::{ImarsWindow, SimpleWindowConsumer};
use std::cell::RefCell;
use std::rc::Rc;

#[test]
fn test_new_window() {
    let window: ImarsWindow<i32> = ImarsWindow::new(5, 2);
    assert_eq!(window.len(), 0);
}

#[test]
fn test_add_to_window() {
    let mut window: ImarsWindow<i32> = ImarsWindow::new(5, 2);
    window.add(100, 0);
    assert_eq!(window.content.front(), Some(&(0, Rc::from(100))));
}

#[test]
fn test_window_shift() {
    let mut window: ImarsWindow<i32> = ImarsWindow::new(2, 2);
    window.add_to_list_and_index(Rc::from(100), 0);
    window.add_to_list_and_index(Rc::from(101), 1);
    window.add_to_list_and_index(Rc::from(102), 2);
    window.add_to_list_and_index(Rc::from(103), 3);
    window.progress_time_and_delete_old(&3);
    assert_eq!(window.content.front(), Some(&(2, Rc::from(102))));
}

#[test]
fn test_window_bound_calculation() {
    let mut window: ImarsWindow<i32> = ImarsWindow::new(3, 2);
    assert!(!(window.does_window_trigger(2)));
    assert!(!(window.does_window_trigger(3)));
    assert!(window.does_window_trigger(4));
    assert!(window.does_window_trigger(5));
    window.update_window_open_time(5);
    assert!(!(window.does_window_trigger(5)));
    assert!(window.does_window_trigger(6));
    assert!(window.does_window_trigger(7));
    window.update_window_open_time(8);
    assert!(!(window.does_window_trigger(9)));
    assert!(window.does_window_trigger(10));

    let mut window: ImarsWindow<i32> = ImarsWindow::new(5, 3);
    assert!(!(window.does_window_trigger(2)));
    assert!(window.does_window_trigger(6));
    window.update_window_open_time(6);
    assert!(!(window.does_window_trigger(7)));
    assert!(window.does_window_trigger(9));
    window.update_window_open_time(10);
    assert!(!(window.does_window_trigger(11)));
    assert!(window.does_window_trigger(12));
}

#[test]
fn test_consumer() {
    let mut window: ImarsWindow<i32> = ImarsWindow::new(2, 2);
    let consumer = Rc::new(RefCell::new(SimpleWindowConsumer::new()));
    window.register_consumer(consumer.clone());
    assert_eq!(0, consumer.borrow_mut().new.len());
    window.add(100, 0);
    window.add(101, 1);
    window.add(102, 2);
    window.add(103, 3);

    assert_eq!(4, consumer.borrow_mut().new.len());
    assert_eq!(2, consumer.borrow_mut().old.len());
}

#[test]
fn test_delete() {
    let mut window: ImarsWindow<i32> = ImarsWindow::new(2, 2);
    let consumer = Rc::new(RefCell::new(SimpleWindowConsumer::new()));
    window.register_consumer(consumer.clone());
    assert_eq!(0, consumer.borrow_mut().new.len());
    window.add(100, 0);
    window.add(101, 1);
    window.add(102, 2);
    window.add(103, 3);
    assert_eq!(2, window.content.len());
    assert_eq!(2, window.index.len());

    assert_eq!(4, consumer.borrow_mut().new.len());
    assert_eq!(2, consumer.borrow_mut().old.len());
}

#[test]
fn test_update() {
    let mut window: ImarsWindow<i32> = ImarsWindow::new(4, 2);
    let consumer = Rc::new(RefCell::new(SimpleWindowConsumer::new()));
    window.register_consumer(consumer.clone());
    assert_eq!(0, consumer.borrow_mut().new.len());
    window.add(100, 0);
    window.add(101, 1);
    window.add(102, 2);
    window.add(103, 3);
    assert_eq!(4, window.content.len());
    assert_eq!(4, window.index.len());
    window.add(100, 4);
    assert_eq!(4, window.content.len());
    assert_eq!(4, window.index.len());
}

#[test]
fn test_get_timestamp() {
    let mut window: ImarsWindow<i32> = ImarsWindow::new(4, 2);
    let consumer = Rc::new(RefCell::new(SimpleWindowConsumer::new()));
    window.register_consumer(consumer.clone());
    assert_eq!(0, consumer.borrow_mut().new.len());
    window.add(100, 0);
    window.add(101, 1);
    window.add(102, 2);
    window.add(103, 3);
    assert_eq!(4, window.content.len());
    assert_eq!(4, window.index.len());
    window.add(100, 4);
    let item = Rc::new(100);
    assert_eq!(4, window.get_time_stamp(item).unwrap());
}
