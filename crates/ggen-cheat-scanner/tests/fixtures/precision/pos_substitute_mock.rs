// POSITIVE (cross-file pair, mock half): MockStorage implements the same
// domain trait RealStorage does -- a collaborator substitute.
// Expected verdict: [CHEAT-T04] via find_mock_substitutes.
pub trait Storage {
    fn put(&mut self, k: String, v: String);
}

pub struct MockStorage {
    pub last_key: Option<String>,
}

impl Storage for MockStorage {
    fn put(&mut self, k: String, _v: String) {
        self.last_key = Some(k);
    }
}
