// Positive fixture for CHEAT-T03 no-assertion-test: nothing in this body
// can ever fail -- no assert*!/.unwrap()/.expect()/panic call anywhere.
fn build_widget(name: &str) -> String {
    format!("widget:{name}")
}

#[test]
fn test_builds_widget() {
    let widget = build_widget("gear");
    let _ = widget.len();
    println!("{widget}");
}
