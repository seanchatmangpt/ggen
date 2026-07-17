//! Macros Module
//!
//! Re-exports macros from `core::macros` for backward compatibility.
//! Macros are exported at crate root via `#[macro_export]` in their definitions.

// Re-export macro submodules - macros use #[macro_export] so they're already at crate root
// Note: The wildcard imports appear unused but are necessary for macro re-export
#[allow(unused_imports)]
#[macro_use]
/// Assertion macros module
///
/// Re-exports assertion macros from `core::macros::assert` for backward compatibility.
pub mod assert {
    pub use crate::core::macros::assert::*;
}

#[macro_use]
/// Test macros module
///
/// Re-exports test macros from `core::macros::test` for backward compatibility.
pub mod test {
    pub use crate::core::macros::test::*;
}
