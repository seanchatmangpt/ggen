# Chicago TDD Tools Procedural Macros

Procedural macros for the Chicago TDD Tools framework.

## Features

- `#[tdd_test]`: Automatically detects AAA sections, generates test metadata, and validates AAA pattern at compile time.
- `#[fixture]`: Generates fixture setup/teardown code and provides type-safe fixture state management.
- `#[derive(TestBuilder)]`: Generates a fluent builder pattern for test data structures.

## Usage

This crate is an internal dependency of `chicago-tdd-tools`. Most users should use the macros through the main crate.

```rust
use chicago_tdd_tools::tdd_test;

#[tdd_test]
fn my_test() {
    // Arrange
    let x = 1;
    // Act
    let y = x + 1;
    // Assert
    assert_eq!(y, 2);
}
```

## License

MIT
