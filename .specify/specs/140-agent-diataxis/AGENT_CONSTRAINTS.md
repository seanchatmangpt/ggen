# Agent Constraints and Error Codes

## `E0001`
**RULE:** Template merging must not fail due to invalid block structure.
**REASON:** The generator requires valid block syntax to safely merge templates.
**FIX:** Ensure all template tags are correctly closed and formatted.

## `E0002`
**RULE:** Template variables must be defined before use.
**REASON:** Undefined variables lead to generation failures or empty outputs.
**FIX:** Add the required variable declarations in the query bindings or manifest.

## `E0003`
**RULE:** Template file paths must be absolute or resolve within project root.
**REASON:** File access outside target directories violates security constraints.
**FIX:** Adjust the path configuration to reside within the workspace boundaries.

## `E0004`
**RULE:** Target directory must be writable.
**REASON:** The generator cannot write projected files if permissions are blocked.
**FIX:** Verify and set directory write permissions.

## `E0005`
**RULE:** Output file names must not contain forbidden characters.
**REASON:** Operating system path limitations prevent file creation.
**FIX:** Rename the output file path to use only valid alphanumeric characters.

## `E0006`
**RULE:** SPARQL query syntax must be valid.
**REASON:** Oxigraph cannot execute invalid queries during sync pipeline.
**FIX:** Validate the query query structure and syntax.

## `E0007`
**RULE:** RDF ontology syntax must be valid.
**REASON:** Invalid RDF triples cannot be parsed into memory store.
**FIX:** Check Turtle or N-Quads formatting for syntax compliance.

## `E0008`
**RULE:** Query aliases must map to valid queries.
**REASON:** Missing target query definitions cause resolution failures.
**FIX:** Define the query associated with the alias in pack.toml.

## `E0009`
**RULE:** Pack dependencies must form an acyclic graph.
**REASON:** Cyclic dependencies cause stack overflow and build failures.
**FIX:** Remove the circular reference between packages.

## `E0010`
**RULE:** Project version must follow semantic versioning rules.
**REASON:** Invalid version formats disrupt dependency resolution.
**FIX:** Change version to a valid semver format like 1.0.0.

## `E0011`
**RULE:** Project name is a required field.
**REASON:** Identifiers are needed to coordinate packs and projection.
**FIX:** Specify a non-empty name field in the manifest configuration.

## `E0012`
**RULE:** Rule execution timeout must be positive.
**REASON:** Timeout values must allow sufficient execution time.
**FIX:** Increase the timeout duration setting in the manifest.

## `E0013`
**RULE:** Reserved error code E0013.
**REASON:** Internal reservation.
**FIX:** No action required.

## `E0014`
**RULE:** Reserved error code E0014.
**REASON:** Internal reservation.
**FIX:** No action required.

## `E0015`
**RULE:** Reserved error code E0015.
**REASON:** Internal reservation.
**FIX:** No action required.

## `E0016`
**RULE:** Reserved error code E0016.
**REASON:** Internal reservation.
**FIX:** No action required.

## `E0017`
**RULE:** Reserved error code E0017.
**REASON:** Internal reservation.
**FIX:** No action required.

## `E0018`
**RULE:** Reserved error code E0018.
**REASON:** Internal reservation.
**FIX:** No action required.

## `E0019`
**RULE:** Reserved error code E0019.
**REASON:** Internal reservation.
**FIX:** No action required.

## `E0020`
**RULE:** Preflight validation checks must pass.
**REASON:** Preflight validation prevents corrupted states in target directory.
**FIX:** Resolve preflight warnings shown in check logs.

## `E0021`
**RULE:** Invariant checks on ontologies must be satisfied.
**REASON:** Inconsistent ontologies result in incorrect code projection.
**FIX:** Correct inconsistencies or validation violations in target ontology files.

## `E0022`
**RULE:** Customization points must match template variables.
**REASON:** Unmatched customization points lead to uninitialized variables.
**FIX:** Match the customization point name with the variable name.

## `E0023`
**RULE:** Cryptographic signatures must be valid if present.
**REASON:** Invalid signatures indicate corrupted or tampered receipts.
**FIX:** Re-run sync to generate fresh valid signatures and receipts.

## `E0024`
**RULE:** Single-file compilation must succeed.
**REASON:** Code syntax errors block further analysis.
**FIX:** Fix compilation errors in the source document.

## `E0025`
**RULE:** Workspace validation constraints must be met.
**REASON:** Violated constraints block downstream projection checks.
**FIX:** Ensure workspace layout matches setup standards.

## `E0026`
**RULE:** Reserved error code E0026.
**REASON:** Internal reservation.
**FIX:** No action required.

## `E0027`
**RULE:** Output directory configuration must exist.
**REASON:** Target directory must be specified for projected files.
**FIX:** Define output_dir in the generation section.

## `E0028`
**RULE:** Determinism salt must be alphanumeric.
**REASON:** Non-alphanumeric salts may cause generation differences.
**FIX:** Set salt to a clean alphanumeric string.

## `E0029`
**RULE:** Preflight rules must not contain duplicate names.
**REASON:** Duplicate rule names cause configuration conflicts.
**FIX:** Ensure all preflight rules have unique identifiers.
