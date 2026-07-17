// Clean fixture for CHEAT-T04: a real testcontainer that happens to be
// named "Mock..." but does not import mockall, is not #[automock], and does
// not substitute for a trait real production code also implements -- one of
// the explicit false-positive shapes named in the task brief.
struct MockApiContainer {
    port: u16,
}

impl MockApiContainer {
    fn new(port: u16) -> Self {
        Self { port }
    }

    fn base_url(&self) -> String {
        format!("http://localhost:{}", self.port)
    }
}

#[test]
fn test_real_container_boots() {
    let container = MockApiContainer::new(8080);
    assert_eq!(container.base_url(), "http://localhost:8080");
}
