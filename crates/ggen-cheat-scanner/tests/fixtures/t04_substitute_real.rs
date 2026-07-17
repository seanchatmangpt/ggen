// Half of a CHEAT-T04 cross-file positive pair: real production code
// implementing `Storage`. Paired with t04_substitute_mock.rs, which defines
// `MockStorage` implementing the same trait -- a genuine collaborator
// substitute.
trait Storage {
    fn get(&self, key: &str) -> Option<String>;
}

struct RealStorage;

impl Storage for RealStorage {
    fn get(&self, key: &str) -> Option<String> {
        Some(key.to_string())
    }
}
