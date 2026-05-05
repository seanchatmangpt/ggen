pub mod config_policy;
pub mod evidence_policy;
pub mod test_policy;

#[derive(Debug)]
pub struct Violation {
    pub pattern: String,
    pub location: String,
    pub rule: String,
}
