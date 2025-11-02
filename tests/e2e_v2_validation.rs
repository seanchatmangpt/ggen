// E2E Validation Test Suite for ggen v2.0.0
// Main test runner that executes all end-to-end validation scenarios

mod e2e_v2;

// Re-export all test modules to ensure they're compiled and run
#[cfg(test)]
mod tests {
    // All tests are in the individual module files
    // This file just ensures they're discovered by cargo test
}
