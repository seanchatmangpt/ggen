// NEGATIVE (historical FP class): a real testcontainer merely named
// MockApiContainer. Its trait has no other implementer here, so it is not a
// collaborator substitute. Expected verdict: [] (clean) under both
// scan_source and collect_impls/find_mock_substitutes.
pub trait ApiContainer {
    fn start(&self) -> bool;
}

pub struct MockApiContainer {
    pub port: u16,
}

impl ApiContainer for MockApiContainer {
    fn start(&self) -> bool {
        self.port != 0
    }
}

#[test]
fn container_reaches_the_wire() {
    let c = MockApiContainer { port: 5432 };
    assert!(c.start());
}
