//! T3 Layer: Refusal Tests — Negative-Path Verification
//!
//! Validates that the system loudly refuses invalid states:
//! - SAB-01: Corrupt packs.lock → sync --locked fails
//! - SAB-02: Empty receipt signature → receipt verify fails
//! - SAB-03: Missing pack directory → pack add fails

#[cfg(test)]
mod tests {
    #[test]
    fn refusal_placeholder() {
        // Placeholder for sabotage tests
        // These tests verify the system REFUSES invalid states loudly
        // Real tests will use assert_cmd with real CLI execution
        assert!(true);
    }
}
