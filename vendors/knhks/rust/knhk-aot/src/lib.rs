// rust/knhk-aot/src/lib.rs
// Ahead-Of-Time (AOT) Compilation Guard
// Validates IR before execution to enforce Chatman Constant (≤8 ticks)

#![no_std]
extern crate alloc;

use alloc::vec::Vec;
use alloc::string::String;

/// Hook IR validation result
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationResult {
    Valid,
    ExceedsTickBudget,
    InvalidOperation,
    InvalidRunLength,
}

/// AOT guard for validating hooks before execution
pub struct AotGuard;

impl AotGuard {
    /// Validate hook IR before execution
    /// Returns Ok(()) if valid, Err(ValidationResult) if invalid
    pub fn validate_ir(op: u32, run_len: u64, k: u64) -> Result<(), ValidationResult> {
        // Check run length ≤ 8 (Chatman Constant constraint)
        if run_len > 8 {
            return Err(ValidationResult::InvalidRunLength);
        }
        
        // Validate operation is in hot path set
        if !Self::is_hot_path_op(op) {
            return Err(ValidationResult::InvalidOperation);
        }
        
        // Check operation-specific constraints
        match op {
            // ASK operations - always valid if run_len ≤ 8
            1 | 3 | 7 => Ok(()),
            // COUNT operations - check k threshold
            2 | 5 | 6 | 9 | 10 | 11 => {
                if k > run_len {
                    return Err(ValidationResult::ExceedsTickBudget);
                }
                Ok(())
            }
            // UNIQUE - run_len must be ≤ 1
            8 => {
                if run_len > 1 {
                    return Err(ValidationResult::ExceedsTickBudget);
                }
                Ok(())
            }
            // COMPARE operations - always valid
            12 | 13 | 14 | 15 | 16 => Ok(()),
            // CONSTRUCT8 - check template size ≤ 8
            32 => {
                if run_len > 8 {
                    return Err(ValidationResult::ExceedsTickBudget);
                }
                Ok(())
            }
            _ => Err(ValidationResult::InvalidOperation),
        }
    }
    
    /// Check if operation is in hot path set
    fn is_hot_path_op(op: u32) -> bool {
        matches!(op, 
            1 | 2 | 3 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 32
        )
    }
    
    /// Get validation error message
    pub fn error_message(result: &ValidationResult) -> String {
        match result {
            ValidationResult::Valid => String::from("Valid"),
            ValidationResult::ExceedsTickBudget => String::from("Operation exceeds 8-tick budget"),
            ValidationResult::InvalidOperation => String::from("Operation not in hot path set"),
            ValidationResult::InvalidRunLength => String::from("Run length must be ≤ 8"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_validate_valid_ask_sp() {
        assert!(AotGuard::validate_ir(1, 8, 0).is_ok());
    }
    
    #[test]
    fn test_validate_invalid_run_length() {
        assert_eq!(
            AotGuard::validate_ir(1, 9, 0),
            Err(ValidationResult::InvalidRunLength)
        );
    }
    
    #[test]
    fn test_validate_unique_sp() {
        assert!(AotGuard::validate_ir(8, 1, 0).is_ok());
        assert_eq!(
            AotGuard::validate_ir(8, 2, 0),
            Err(ValidationResult::ExceedsTickBudget)
        );
    }
}

