# Pseudocode: Feature Flag Resolution Logic

**SPARC Phase**: Pseudocode
**Algorithm**: Workspace Feature Flag Composition
**Complexity**: O(n * m) where n = crates, m = features per crate
**Status**: Formal specification complete

---

## Algorithm 1: Feature Flag Extraction & Validation

### Function Signature
```
FUNCTION extract_and_validate_features(workspace: Manifest) -> Result<FeatureRegistry>
    Precondition: workspace.Cargo.toml is valid TOML
    Postcondition: Returns validated feature map
    Returns: FeatureRegistry with all features and constraints
```

### Pseudocode (Detailed)

```
FUNCTION extract_and_validate_features(workspace: Manifest) -> Result<FeatureRegistry>
    registry = FeatureRegistry::new()
    errors = Vec<String>::new()

    // Step 1: Iterate all crates in workspace
    FOR EACH crate IN workspace.members:  // O(n)
        cargo_toml = load_cargo_toml(crate.path)

        IF cargo_toml.features == null:
            log_debug("Crate {} has no features", crate.name)
            CONTINUE
        END IF

        // Step 2: Extract features for this crate
        FOR EACH (feature_name, feature_deps) IN cargo_toml.features:  // O(m)
            feature = Feature {
                name: feature_name,
                crate: crate.name,
                dependencies: feature_deps,
                is_default: feature_name IN cargo_toml.default_features
            }

            // Step 3: Validate feature dependencies
            FOR EACH dep IN feature_deps:  // O(d) where d = deps per feature
                IF NOT is_valid_dependency(crate, dep):
                    errors.push(format!("Invalid dependency in {}/{}: {}",
                                       crate.name, feature_name, dep))
                END IF
            END FOR

            registry.add_feature(feature)
        END FOR

        // Step 4: Extract default features for this crate
        IF cargo_toml.default_features != null:
            for_default_features = Feature {
                name: "__default__",
                crate: crate.name,
                dependencies: cargo_toml.default_features
            }
            registry.set_default_features(crate.name, for_default_features)
        END IF
    END FOR

    // Step 5: Validate cross-crate feature references
    FOR EACH feature IN registry.all_features():  // O(features)
        FOR EACH dep IN feature.dependencies:
            IF dep.is_crate_feature():  // E.g., "crate-name/feature-name"
                target_crate = dep.crate_name
                target_feature = dep.feature_name

                IF NOT registry.has_feature(target_crate, target_feature):
                    errors.push(format!("Feature {} references missing feature {}/{}",
                                       feature.full_name, target_crate, target_feature))
                END IF
            END IF
        END FOR
    END FOR

    IF errors.is_not_empty():
        RETURN Err(FeatureValidationError(errors))
    END IF

    RETURN Ok(registry)
END FUNCTION

STRUCT Feature:
    name: String                    // E.g., "otel", "async-http"
    crate: String                   // E.g., "ggen-core"
    dependencies: Vec<String>       // E.g., ["tokio/macros", "opentelemetry"]
    is_default: bool               // Enabled by default?

STRUCT FeatureRegistry:
    features: Map<String, Vec<Feature>>  // Organized by crate
    dependencies: Map<String, Vec<String>>  // Cross-crate dependencies
```

### Complexity Analysis

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Load & parse Cargo.toml | O(n * k) | n=crates, k=file_size |
| Extract features | O(n * m) | n=crates, m=avg features |
| Validate dependencies | O(features * d) | d=avg deps per feature |
| Validate cross-references | O(features * cross_refs) | Cross-crate validation |
| **Total** | **O(n * m * d)** | Typically small (n~30, m~5, d~3) |

---

## Algorithm 2: Feature Flag Composition for Profiles

### Purpose
Determine which features to enable based on build profile (dev/test/release)

### Function Signature
```
FUNCTION compose_features_for_profile(registry: FeatureRegistry, profile: BuildProfile) -> Result<FeatureSet>
    Precondition: registry is validated, profile in [Dev, Test, Release]
    Postcondition: Returns resolved feature set
    Returns: FeatureSet with enabled flags for profile
```

### Pseudocode

```
FUNCTION compose_features_for_profile(registry: FeatureRegistry, profile: BuildProfile) -> Result<FeatureSet>
    // Step 1: Get feature rules for this profile
    rules = get_profile_rules(profile)

    feature_set = FeatureSet::new(profile)
    enabled_features = Set<String>::new()
    pending_features = Queue<String>::new()

    // Step 2: Start with mandatory features (always enabled)
    FOR EACH mandatory_feature IN rules.mandatory:  // O(mandatory_count)
        pending_features.enqueue(mandatory_feature)
    END FOR

    // Step 3: Process features with dependency resolution (BFS)
    WHILE pending_features.is_not_empty():  // O(features)
        current_feature = pending_features.dequeue()

        IF enabled_features.contains(current_feature):
            CONTINUE  // Already processed
        END IF

        // Check if feature should be enabled for this profile
        IF should_enable_feature(current_feature, profile, rules):
            enabled_features.insert(current_feature)
            feature_set.enable(current_feature)

            // Enqueue transitive dependencies
            feature = registry.get_feature(current_feature)
            FOR EACH dep IN feature.dependencies:  // O(deps)
                IF dep.is_feature_reference() AND NOT enabled_features.contains(dep):
                    pending_features.enqueue(dep)
                END IF
            END FOR
        ELSE:
            // Feature not applicable to this profile
            log_debug("Skipping feature {} for profile {}", current_feature, profile)
        END IF
    END WHILE

    // Step 4: Validate feature consistency (no conflicts)
    IF has_feature_conflicts(feature_set):
        RETURN Err("Feature conflict detected")
    END IF

    RETURN Ok(feature_set)
END FUNCTION

STRUCT BuildProfile:
    name: String                    // "dev", "test", "release"
    optimization_level: u32        // 0, 1, 2, 3
    debug_symbols: bool
    strip_binary: bool

STRUCT FeatureSet:
    profile: BuildProfile
    enabled: Set<String>
    disabled: Set<String>
    timestamp: Timestamp
```

### Profile-Specific Rules

```
PROFILE Dev (Development):
├─ Mandatory: [] (no forced features)
├─ Recommended: ["logging", "metrics"]
├─ Forbidden: ["optimized", "strip-symbols"]
├─ Rationale: Fast iteration, debugging, feature exploration
├─ Typical compile time: 45s (ggen-core only)

PROFILE Test (Unit & Integration Tests):
├─ Mandatory: ["test-utils", "mock-data"]
├─ Recommended: ["debug-assertions", "logging"]
├─ Forbidden: ["production-only"]
├─ Rationale: Comprehensive coverage, test support
├─ Typical compile time: 60s (ggen-core + test crates)

PROFILE Release (Production):
├─ Mandatory: ["release-optimizations", "security-hardening"]
├─ Recommended: ["performance-profiling", "security-audit"]
├─ Forbidden: ["test-only", "debug-symbols"]
├─ Rationale: Performance, binary size, security
├─ Typical compile time: 90s (all crates)
```

### Example Resolution

```
Scenario: ggen-core feature composition for TEST profile

Requested profile: Test
Rules:
  Mandatory: test-utils, mock-data
  Optional based on dependencies: logging, metrics

Process:
├─ Enqueue: test-utils
├─ Process test-utils:
│  ├─ Enable: test-utils
│  └─ Enqueue transitive: serde, tokio/rt
├─ Process mock-data:
│  ├─ Enable: mock-data
│  └─ Enqueue transitive: rand
├─ Process logging (optional):
│  ├─ Enable: logging
│  └─ Enqueue transitive: log, tracing
├─ Process serde (from test-utils):
│  ├─ Enable: serde
│  └─ No further dependencies
└─ Continue until queue empty

Result (Features enabled for TEST profile):
✓ test-utils
✓ mock-data
✓ logging
✓ metrics
✓ serde
✓ tokio/rt
✓ rand
✓ log
✓ tracing

Disabled:
✗ release-optimizations (not applicable to test)
✗ security-hardening (not applicable to test)
```

---

## Algorithm 3: Feature Flag Impact Analysis

### Purpose
Analyze build time and binary size impact of feature combinations

### Function Signature
```
FUNCTION analyze_feature_impact(features: FeatureSet, crate_name: String) -> ImpactAnalysis
    Precondition: features are validated and enabled
    Postcondition: Returns compile-time and size impact
    Returns: ImpactAnalysis with metrics
```

### Pseudocode

```
FUNCTION analyze_feature_impact(features: FeatureSet, crate_name: String) -> ImpactAnalysis
    analysis = ImpactAnalysis::new()

    // Baseline: compile without any optional features
    baseline_time = benchmark_compile(crate_name, features=[])  // ~5s for ggen-core
    baseline_size = measure_binary_size(crate_name, features=[])  // ~2MB

    analysis.baseline = CompileMetrics {
        time_ms: baseline_time,
        size_bytes: baseline_size
    }

    // Step 1: Measure impact of each enabled feature
    FOR EACH enabled_feature IN features.enabled:  // O(features)
        // Compile with only this feature
        with_feature_time = benchmark_compile(crate_name, features=[enabled_feature])
        with_feature_size = measure_binary_size(crate_name, features=[enabled_feature])

        impact = {
            time_delta_ms: with_feature_time - baseline_time,
            size_delta_bytes: with_feature_size - baseline_size,
            time_percent: (with_feature_time / baseline_time) * 100
        }

        analysis.add_feature_impact(enabled_feature, impact)
    END FOR

    // Step 2: Identify slow features (> 2s compilation impact)
    slow_features = filter(analysis.impacts,
                          i => i.time_delta_ms > 2000)

    IF slow_features.is_not_empty():
        analysis.slow_features = slow_features
        analysis.optimization_suggestion = "Consider lazy-loading or compile-time feature gating"
    END IF

    // Step 3: Measure feature interaction (some combinations slow)
    key_combinations = [
        [ggen-ai, knhk-otel],
        [ggen-marketplace-v2, async-http]
    ]

    FOR EACH combo IN key_combinations:
        combo_time = benchmark_compile(crate_name, features=combo)
        // If time is more than sum of individual times, there's interaction
        interaction_overhead = combo_time - sum([impacts[f].time_delta_ms for f in combo])
        IF interaction_overhead > 500ms:
            analysis.add_interaction(combo, interaction_overhead)
        END IF
    END FOR

    RETURN analysis
END FUNCTION

STRUCT ImpactAnalysis:
    crate_name: String
    baseline: CompileMetrics
    impacts: Vec<FeatureImpact>
    slow_features: Vec<(String, u64)>  // (feature, time_delta_ms)
    interactions: Vec<(Vec<String>, u64)>  // (features, interaction_overhead)
    optimization_suggestion: String
```

---

## Algorithm 4: Feature Flag CLI Resolution

### Purpose
Convert CLI flags into enabled feature set

### Function Signature
```
FUNCTION resolve_cli_flags(args: CliArgs) -> Result<FeatureSet>
    Precondition: args contain --features and --no-default-features flags
    Postcondition: Returns resolved feature set
    Returns: FeatureSet with CLI-specified features
```

### Pseudocode

```
FUNCTION resolve_cli_flags(args: CliArgs) -> Result<FeatureSet>
    feature_set = FeatureSet::new()

    // Step 1: Determine default feature behavior
    IF args.has_flag("--no-default-features"):
        // Start with empty feature set
        feature_set.use_defaults = false
    ELSE:
        // Start with crate's default features
        feature_set.use_defaults = true
    END IF

    // Step 2: Parse --features flag (comma-separated)
    // Example: --features "async-http,logging,metrics"
    IF args.has_flag("--features"):
        features_str = args.get_value("--features")
        user_features = split(features_str, ",")  // Vec<&str>

        FOR EACH feature IN user_features:
            feature = trim(feature)

            // Check for negation prefix: --features "-otel" disables otel
            IF feature.starts_with("-"):
                disabled_feature = feature[1..]
                feature_set.disable(disabled_feature)
                log_debug("Disabled feature: {}", disabled_feature)
            ELSE:
                feature_set.enable(feature)
                log_debug("Enabled feature: {}", feature)
            END IF
        END FOR
    END IF

    // Step 3: Validate requested features exist
    FOR EACH enabled IN feature_set.enabled:
        IF NOT registry.has_feature(enabled):
            RETURN Err(format!("Unknown feature: {}", enabled))
        END IF
    END FOR

    // Step 4: Resolve dependencies transitively
    resolved = resolve_transitive_dependencies(feature_set)

    RETURN Ok(resolved)
END FUNCTION
```

### CLI Examples

```
Example 1: Default features (fastest dev build)
$ cargo build
→ Uses crate's default features
→ Build time: ~5s for ggen-core

Example 2: Core only (minimal)
$ cargo build --no-default-features
→ No features enabled
→ Build time: ~2s for ggen-core
→ Binary size: ~1.5MB

Example 3: Add specific features
$ cargo build --features "logging,metrics"
→ Uses crate defaults + adds logging + metrics
→ Build time: ~8s

Example 4: Custom feature set (test)
$ cargo build --no-default-features --features "test-utils,mock-data,logging"
→ Only specified features
→ Build time: ~10s

Example 5: Release build (all features)
$ cargo build --release --features "all"
→ All optional features enabled
→ Build time: ~90s (workspace)
```

---

## Impact on ggen Build Times

### Current Workspace: 30 crates

```
Feature Composition Impact:

Core Crates (Always built):
├─ ggen-core: 20s (baseline)
├─ ggen-cli: 5s
├─ ggen-domain: 3s
├─ ggen-utils: 1s
├─ ggen-config: 1s
└─ Subtotal: 30s

Optional Crates (Feature-gated):
├─ ggen-ai (2.6M) ────────────────→ +30s (+100%)
├─ knhk-otel (704K) ──────────────→ +15s (+50%)
├─ knhk-connectors ───────────────→ +5s
├─ ggen-marketplace-v2 (596K) ────→ +10s
├─ ggen-api ──────────────────────→ +5s
└─ Others ────────────────────────→ +15s

Total workspace (all features): 90s
Minimal workspace (core only): 30s

Speedup from feature-gating:
└─ Development: 90s / 30s = 3x faster ✓
```

---

## Testing Strategy

### Unit Tests
```
TEST resolve_features_deterministic:
    args1 = CliArgs { features: "logging,metrics" }
    args2 = CliArgs { features: "metrics,logging" }
    set1 = resolve_cli_flags(args1)
    set2 = resolve_cli_flags(args2)
    ASSERT set1.enabled == set2.enabled  // Order-independent

TEST feature_conflict_detection:
    // Some features might conflict (e.g., tokio/multi-thread + tokio/single-thread)
    conflicting = resolve_cli_flags("tokio/multi-thread,tokio/single-thread")
    ASSERT conflicting.is_err()

TEST transitive_dependency_resolution:
    features = resolve_cli_flags("otel")
    ASSERT features.contains("tokio/rt")  // Transitive dependency
```

### Integration Tests
```
TEST build_times_with_feature_gates:
    time_baseline = benchmark_build(features=[])
    time_with_ai = benchmark_build(features=["ggen-ai"])
    ASSERT time_with_ai > time_baseline + 25s  // ~30s overhead for ggen-ai

TEST cli_feature_parsing:
    result = resolve_cli_flags("--features 'async-http,logging' --no-default-features")
    ASSERT result.enabled.contains("async-http")
    ASSERT result.enabled.contains("logging")
    ASSERT NOT result.use_defaults
```

---

## References

- **Cargo Features Documentation**: https://doc.rust-lang.org/cargo/reference/features.html
- **Feature Design Patterns**: https://docs.rs/paste/latest/paste/
- **Minimal Versions**: https://doc.rust-lang.org/nightly/cargo/reference/msrv.html
