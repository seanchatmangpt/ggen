// Clean fixture for CHEAT-T03: a real assertion is present.
fn build_widget(name: &str) -> String {
    format!("widget:{name}")
}

#[test]
fn test_builds_widget_with_check() {
    let widget = build_widget("gear");
    assert_eq!(widget, "widget:gear");
}
