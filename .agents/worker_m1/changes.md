# Changes Made by Worker M1 - Pack & Projection Core Model Implementation

We refactored `ggen-projection`'s core models from inline definitions in `lib.rs` into separate module files and declared/re-exported them from the crate root.

## Files Created

1. **`crates/ggen-projection/src/descriptor.rs`**
   - Implemented `PackTemplateDescriptor` and `PackDescriptor`.
   - Implemented TOML deserialization and validation logic (`PackDescriptor::from_toml`, `PackDescriptor::validate`).

2. **`crates/ggen-projection/src/plan.rs`**
   - Implemented `PackPlan`.
   - Implemented error types: `DependencyCycleError`, `DependencyNotFoundError`, and `VersionConflictError`.
   - Implemented topological resolution and semver compatibility checking (`PackPlan::resolve`, `is_compatible`).

3. **`crates/ggen-projection/src/mapping.rs`**
   - Implemented `ProjectionMapping`, `ProjectionMap`, and `CustomizationMap`.
   - Implemented mapping addition, customization point validation, incomplete slot check, and state sync validation.

4. **`crates/ggen-projection/src/receipt.rs`**
   - Implemented `Receipt` (alias `CryptographicReceipt`) and `ReceiptIndex`.
   - Implemented receipt addition, index hashing, JSON parsing, and disk persistence.

5. **`crates/ggen-projection/src/pipeline.rs`**
   - Implemented `StagingGate` and `sync`.
   - Implemented dirty local checks and atomic writes of the projection/customization maps and receipt index to output directories.

## Files Modified

1. **`crates/ggen-projection/src/lib.rs`**
   - Declared modules: `descriptor`, `plan`, `mapping`, `receipt`, and `pipeline`.
   - Re-exported core structures and functions to maintain backward compatibility.
   - Removed duplicate inline definitions from the file.
