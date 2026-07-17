// See t04_substitute_real.rs -- this MockStorage substitutes for the same
// `Storage` trait that real production code (`RealStorage`) implements.
trait Storage {
    fn get(&self, key: &str) -> Option<String>;
}

struct MockStorage;

impl Storage for MockStorage {
    fn get(&self, _key: &str) -> Option<String> {
        None
    }
}
