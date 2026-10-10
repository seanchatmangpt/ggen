// POSITIVE: mockall import (unconditional T04 half). Expected verdict: [CHEAT-T04]
// (one finding, for the `use` item; the `mock!` invocation itself is not scanned).
use mockall::mock;

mock! {
    pub Storage {
        fn get(&self, k: &str) -> Option<String>;
    }
}
