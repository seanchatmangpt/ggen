// POSITIVE (cross-file pair, real half): production implementer of Storage.
// Expected verdict (via find_mock_substitutes with pos_substitute_mock.rs): [CHEAT-T04]
// anchored to the mock file.
pub trait Storage {
    fn put(&mut self, k: String, v: String);
}

pub struct RealStorage {
    pub backing: std::collections::BTreeMap<String, String>,
}

impl Storage for RealStorage {
    fn put(&mut self, k: String, v: String) {
        self.backing.insert(k, v);
    }
}
