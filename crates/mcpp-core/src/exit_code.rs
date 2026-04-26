//! Process exit codes — promoted from v2 `speckit-ralph::exitcode`.
//! Mirrors `control-pack.yaml::exit_codes`.

pub const SUCCESS: i32 = 0;
pub const COMMAND_FAILED: i32 = 1;
pub const INVALID_USAGE: i32 = 2;
pub const GATE_FAILED: i32 = 3;
pub const LINE_STOPPED: i32 = 4;
pub const MISSING_REQUIRED_STATE: i32 = 5;
pub const POLICY_VIOLATION: i32 = 6;
pub const RECEIPT_INVALID: i32 = 7;
pub const EXTERNAL_TOOL_FAILURE: i32 = 8;
pub const INVOCATION_DEFECT: i32 = 9;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn receipt_invalid_is_seven() {
        // Cross-checks the v2 abnormality-path contract: a missing or
        // unverifiable receipt exits with code 7.
        assert_eq!(RECEIPT_INVALID, 7);
    }

    #[test]
    fn invocation_defect_is_nine() {
        assert_eq!(INVOCATION_DEFECT, 9);
    }
}
