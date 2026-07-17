#![cfg(test)]

use super::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;

#[test]
fn test_window() {
    let mut report = Report::new();
    report.add(ReportStrategy::OnWindowClose);
    let mut window = CSPARQLWindow {
        width: 10,
        slide: 2,
        app_time: 0,
        t_0: 0,
        active_windows: HashMap::new(),
        report,
        tick: Tick::TimeDriven,
        consumer: None,
        call_back: None,
    };
    let receiver = window.register();
    let consumer = Consumer::new();
    consumer.start(receiver);

    for i in 0..10 {
        let triple = WindowTriple {
            s: format!("s{}", i),
            p: "p".to_string(),
            o: "o".to_string(),
        };
        window.add_to_window(triple, i);
    }

    window.stop();
    thread::sleep(Duration::from_secs(1));
    assert_eq!(5, consumer.len());
}
#[test]
fn test_window_with_call_back() {
    let mut report = Report::new();
    report.add(ReportStrategy::OnWindowClose);
    let mut window = CSPARQLWindow {
        width: 10,
        slide: 2,
        app_time: 0,
        t_0: 0,
        active_windows: HashMap::new(),
        report,
        tick: Tick::TimeDriven,
        consumer: None,
        call_back: None,
    };
    let recieved_data = Rc::new(RefCell::new(Vec::new()));
    let data_clone = recieved_data.clone();
    let call_back = move |content| {
        println!("Content: {:?}", content);
        recieved_data.borrow_mut().push(content);
    };
    window.register_callback(Box::new(call_back));

    for i in 0..10 {
        let triple = WindowTriple {
            s: format!("s{}", i),
            p: "p".to_string(),
            o: "o".to_string(),
        };
        window.add_to_window(triple, i);
    }

    window.stop();
    assert_eq!(5, (*data_clone.borrow_mut()).len());
}
