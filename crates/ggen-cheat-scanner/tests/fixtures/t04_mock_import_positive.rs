// Positive fixture for CHEAT-T04 mock-import (unconditional half): a
// direct mockall import plus an #[automock] trait -- forbidden London-TDD
// mocking per this repo's Chicago-TDD-only testing policy.
use mockall::mock;

#[mockall::automock]
trait HttpClient {
    fn get(&self, url: &str) -> Result<String, ()>;
}
