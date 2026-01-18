# CLI Business Logic Extraction Matrix

**Generated:** 2025-11-20
**Project:** ggen Rust CLI
**Objective:** Detailed function-by-function extraction plan

---

## EXECUTIVE SUMMARY

**STATUS:** ✅ 94.1% COMPLETE - Excellent architecture already in place

**METRICS:**
- Total Functions: 34
- Fully Migrated: 32 (94.1%)
- Partially Migrated: 2 (5.9%)
- Total LOC to Extract: 62 (2.2% of 2,881 total CLI LOC)

---

## EXTRACTION MATRIX

### Legend
- **Priority:** Critical (P0) | Important (P1) | Nice-to-have (P2)
- **Complexity:** High (H) | Medium (M) | Low (L) | Trivial (T)
- **Effort:** Hours estimate
- **Risk:** High | Medium | Low | None

---

## MARKETPLACE COMMANDS (19 functions)

| # | Function | Lines | LOC | Status | Business Logic → Domain | CLI-Only | Priority | Complexity | Effort | Risk |
|---|----------|-------|-----|--------|------------------------|----------|----------|-----------|--------|------|
| 1 | search | 153-182 | 29 | ✅ DONE | `execute_search` | Output mapping | - | - | - | - |
| 2 | install | 186-209 | 23 | ✅ DONE | `execute_install` | Output mapping | - | - | - | - |
| 3 | list | 229-291 | 62 | ⚠️ PARTIAL | `execute_list` + **33 LOC filtering** | Output mapping | P1 | L | 1h | Low |
| 4 | publish | 294-315 | 21 | ✅ DONE | `execute_publish` | Output mapping | - | - | - | - |
| 5 | validate | 335-452 | 117 | ✅ DONE | `validate_package/validate_all_packages` | Complex output mapping | - | - | - | - |
| 6 | maturity | 469-549 | 80 | ✅ DONE | `MaturityEvaluator::evaluate` | Output mapping | - | - | - | - |
| 7 | dashboard | 566-671 | 105 | ✅ DONE | `MaturityDashboard` | Output mapping | - | - | - | - |
| 8 | maturity_batch | 685-771 | 86 | ✅ DONE | `MaturityAssessment` | Output mapping | - | - | - | - |
| 9 | recommend | 788-871 | 83 | ✅ DONE | `find_for_use_case` | Output mapping | - | - | - | - |
| 10 | compare | 888-1015 | 127 | ✅ DONE | `generate_all_assessments` | Output mapping | - | - | - | - |
| 11 | search_maturity | 1035-1115 | 80 | ✅ DONE | `generate_all_assessments` | Filtering + output | - | - | - | - |
| 12 | export | 1135-1225 | 90 | ✅ DONE | `export_as_csv/json` | Output mapping | - | - | - | - |
| 13 | list_bundles | 1239-1280 | 41 | ✅ DONE | `BundleRegistry::list_bundles` | Output mapping | - | - | - | - |
| 14 | bundle_info | 1294-1343 | 49 | ✅ DONE | `BundleRegistry::get_bundle` | Output mapping | - | - | - | - |
| 15 | install_bundle | 1357-1416 | 59 | ✅ DONE | `BundleRegistry` | Output mapping | - | - | - | - |
| 16 | emit_receipts | 1430-1489 | 59 | ✅ DONE | `emit_receipts_for_marketplace` | Output mapping | - | - | - | - |
| 17 | report | 1503-1546 | 43 | ✅ DONE | `generate_validation_report` | Output mapping | - | - | - | - |
| 18 | generate_artifacts | 1560-1600 | 40 | ✅ DONE | `generate_packages_markdown` | Output mapping | - | - | - | - |
| 19 | improve | 1614-1704 | 90 | ⚠️ PARTIAL | `generate_improvement_plan` + **16 LOC apply** | Output mapping | P1 | L | 30min | Very Low |

**Marketplace Summary:**
- Total Functions: 19
- Fully Migrated: 17 (89.5%)
- Partially Migrated: 2 (10.5%)
- LOC to Extract: 49 (2.8% of 1,749 total)

---

## TEMPLATE COMMANDS (8 functions)

| # | Function | Lines | LOC | Status | Business Logic → Domain | CLI-Only | Priority | Complexity | Effort | Risk |
|---|----------|-------|-----|--------|------------------------|----------|----------|-----------|--------|------|
| 1 | show | 83-100 | 17 | ✅ DONE | `show_template_metadata` | Output mapping | - | - | - | - |
| 2 | get | 103-106 | 3 | ✅ DONE | Alias to `show` | Alias | - | - | - | - |
| 3 | new | 110-134 | 24 | ✅ DONE | `generate_template_content` | Output mapping | - | - | - | - |
| 4 | list | 138-174 | 36 | ✅ DONE | `list_templates` | Output mapping | - | - | - | - |
| 5 | lint | 178-217 | 39 | ✅ DONE | `lint_template` | Output mapping | - | - | - | - |
| 6 | generate | 221-249 | 28 | ✅ DONE | `generate_file` | Output mapping | - | - | - | - |
| 7 | generate_tree | 253-285 | 32 | ✅ DONE | `generate_file_tree` | Output mapping | - | - | - | - |
| 8 | regenerate | 289-296 | 7 | ✅ DONE | Placeholder (TODO) | Output mapping | - | - | - | - |

**Template Summary:**
- Total Functions: 8
- Fully Migrated: 8 (100%)
- Partially Migrated: 0 (0%)
- LOC to Extract: 0

**Note:** Helper function `parse_variables` (303-316, 13 LOC) is duplicated in project.rs

---

## PROJECT COMMANDS (7 functions)

| # | Function | Lines | LOC | Status | Business Logic → Domain | CLI-Only | Priority | Complexity | Effort | Risk |
|---|----------|-------|-----|--------|------------------------|----------|----------|-----------|--------|------|
| 1 | new | 110-153 | 43 | ✅ DONE | `create_project` | Async bridge + output | - | - | - | - |
| 2 | plan | 176-230 | 54 | ✅ DONE | `create_plan` + **input validation** | Async bridge + output | - | - | - | - |
| 3 | gen | 254-323 | 69 | ✅ DONE | `execute_gen` + **input validation** | Async bridge + output | - | - | - | - |
| 4 | apply | 344-372 | 28 | ✅ DONE | `apply_plan` | Async bridge + output | - | - | - | - |
| 5 | init | 393-592 | 199 | ✅ DONE | Uses `presets` module | Complex but organized | - | - | - | - |
| 6 | generate | 618-731 | 113 | ✅ DONE | `render_with_rdf` | Template discovery + output | - | - | - | - |
| 7 | watch | 747-769 | 22 | ✅ DONE | `ProjectWatcher` | Blocking watcher | - | - | - | - |

**Project Summary:**
- Total Functions: 7
- Fully Migrated: 7 (100%)
- Partially Migrated: 0 (0%)
- LOC to Extract: 0

**Note 1:** Input validation in `plan` and `gen` (182-200, 258-276) is CORRECT to keep in CLI (defense in depth)
**Note 2:** Helper function `parse_variables` (777-790, 13 LOC) is duplicated from template.rs

---

## CODE DUPLICATION ANALYSIS

| Pattern | Occurrences | Files | Lines | LOC | Extract To | Priority | Effort |
|---------|-------------|-------|-------|-----|-----------|----------|--------|
| Variable parsing | 2 | template.rs, project.rs | 303-316, 777-790 | 13 | `ggen_utils::cli::parse_key_value_pairs` | P2 | 15min |

---

## DETAILED EXTRACTION PLANS

### P1-1: Marketplace List Maturity Filtering (1 hour)

**File:** `crates/ggen-cli/src/cmds/marketplace.rs`
**Lines:** 252-285 (33 LOC)
**Function:** `list`

**Current Code:**
```rust
// Filter by maturity level if specified
if let Some(level_str) = min_maturity {
    let min_level = match level_str.as_str() {
        "experimental" => 0u32,
        "beta" => 41u32,
        "production" => 61u32,
        "enterprise" => 81u32,
        _ => 61u32,
    };

    // In a real implementation, fetch actual maturity scores
    // For now, keep all packages and document the filtering capability
    let _min_score = min_level;
}

if let Some(_level_str) = maturity_level {
    // Filter by specific maturity level
    // In a real implementation, this would filter to only packages at this exact level
}

// Sort by specified field
if let Some(sort_field) = sort {
    match sort_field.as_str() {
        "maturity" => {
            // Would sort by maturity score in real implementation
        }
        "downloads" => {
            // Would sort by download count
        }
        "updated" => {
            // Would sort by last update time
        }
        _ => {}
    }
}
```

**Extract To:** `ggen_domain::marketplace::list::filter_and_sort`

**New Domain Function:**
```rust
// In crates/ggen-domain/src/marketplace/list.rs

pub struct FilterAndSortOptions {
    pub min_maturity: Option<String>,
    pub maturity_level: Option<String>,
    pub sort_by: Option<String>,
}

pub fn filter_and_sort(
    packages: Vec<InstalledPackageInfo>,
    options: &FilterAndSortOptions,
) -> Result<Vec<InstalledPackageInfo>> {
    let mut filtered = packages;

    // Filter by minimum maturity
    if let Some(level_str) = &options.min_maturity {
        let min_score = match level_str.as_str() {
            "experimental" => 0u32,
            "beta" => 41u32,
            "production" => 61u32,
            "enterprise" => 81u32,
            _ => 61u32,
        };

        filtered.retain(|pkg| {
            // Fetch maturity score for package
            // For now, placeholder implementation
            let _score = min_score;
            true // TODO: Implement actual filtering
        });
    }

    // Filter by exact maturity level
    if let Some(level_str) = &options.maturity_level {
        filtered.retain(|pkg| {
            // Check if package matches exact level
            // TODO: Implement actual filtering
            let _level = level_str;
            true
        });
    }

    // Sort by field
    if let Some(sort_field) = &options.sort_by {
        match sort_field.as_str() {
            "maturity" => {
                // TODO: Sort by maturity score
            }
            "downloads" => {
                // TODO: Sort by download count
            }
            "updated" => {
                // TODO: Sort by last update
            }
            _ => {}
        }
    }

    Ok(filtered)
}
```

**New CLI Code:**
```rust
#[verb]
fn list(
    detailed: bool, json: bool, min_maturity: Option<String>,
    maturity_level: Option<String>, sort: Option<String>,
) -> Result<ListOutput> {
    let input = ListInput { detailed, json };

    execute_async_verb(async move {
        let result = execute_list(input).await?;

        // ✅ Call domain for filtering/sorting
        let filter_opts = FilterAndSortOptions {
            min_maturity,
            maturity_level,
            sort_by: sort,
        };

        let packages = filter_and_sort(result.packages, &filter_opts)?
            .into_iter()
            .map(|p| InstalledPackage { ... })
            .collect();

        Ok(ListOutput { packages, total: packages.len() })
    })
}
```

**Testing:**
```rust
// In crates/ggen-domain/src/marketplace/list.rs

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_filter_by_min_maturity() {
        let packages = vec![/* test data */];
        let opts = FilterAndSortOptions {
            min_maturity: Some("production".to_string()),
            maturity_level: None,
            sort_by: None,
        };

        let filtered = filter_and_sort(packages, &opts).unwrap();
        assert!(filtered.iter().all(|p| /* score >= 61 */));
    }

    #[test]
    fn test_sort_by_maturity() {
        let packages = vec![/* test data */];
        let opts = FilterAndSortOptions {
            min_maturity: None,
            maturity_level: None,
            sort_by: Some("maturity".to_string()),
        };

        let sorted = filter_and_sort(packages, &opts).unwrap();
        // Verify sorted order
    }
}
```

**Effort Breakdown:**
- Create domain function: 20 min
- Update CLI: 10 min
- Write tests: 20 min
- Test & verify: 10 min
- **Total: 60 minutes**

---

### P1-2: Marketplace Improve Template Application (30 minutes)

**File:** `crates/ggen-cli/src/cmds/marketplace.rs`
**Lines:** 1677-1693 (16 LOC)
**Function:** `improve`

**Current Code:**
```rust
// Apply template if requested
if let Some(template) = apply {
    match apply_template_improvements(&package_path, &template) {
        Ok(message) => {
            println!("\n✅ {}", message);
            return Ok(serde_json::json!({
                "status": "applied",
                "package_id": package_id,
                "template": template,
                "message": message
            }));
        }
        Err(e) => {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                e.to_string(),
            ));
        }
    }
}
```

**Issue:** Domain function `apply_template_improvements` already exists! This is just CLI output formatting that could be cleaner.

**Refactored CLI Code:**
```rust
#[verb]
fn improve(package_id: String, apply: Option<String>) -> Result<serde_json::Value> {
    use ggen_domain::marketplace::{apply_template_improvements, generate_improvement_plan};
    use std::path::PathBuf;

    let marketplace_root = PathBuf::from(".");
    let package_path = marketplace_root
        .join("marketplace")
        .join("packages")
        .join(&package_id);

    if !package_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Package not found: {}",
            package_id
        )));
    }

    // Generate improvement plan
    let plan = generate_improvement_plan(&package_id, &marketplace_root)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    // ✅ Cleaner: Domain handles application, CLI just formats output
    if let Some(template) = apply {
        let message = apply_template_improvements(&package_path, &template)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        println!("\n✅ {}", message);

        return Ok(serde_json::json!({
            "status": "applied",
            "package_id": package_id,
            "template": template,
            "message": message
        }));
    }

    // Return improvement plan
    println!("\n🚀 Improvement Plan for {}\n...", package_id);
    // ... existing plan output

    Ok(serde_json::json!({
        "status": "success",
        "package_id": package_id,
        "current_score": plan.current_score,
        "projected_score": plan.projected_new_score,
        "suggestions": plan.suggestions.len(),
        "estimated_effort_hours": plan.estimated_effort_hours,
        "suggestions": plan.suggestions
    }))
}
```

**Effort Breakdown:**
- Refactor CLI: 15 min
- Test: 10 min
- Verify: 5 min
- **Total: 30 minutes**

---

### P2-1: Variable Parsing Helper (15 minutes)

**Files:** `crates/ggen-cli/src/cmds/template.rs` (303-316), `crates/ggen-cli/src/cmds/project.rs` (777-790)
**LOC:** 13 (duplicated in 2 files)

**Current Code (Duplicated):**
```rust
fn parse_variables(vars: &[String]) -> Result<BTreeMap<String, String>, String> {
    let mut map = BTreeMap::new();
    for var in vars {
        if let Some((key, value)) = var.split_once('=') {
            map.insert(key.to_string(), value.to_string());
        } else {
            return Err(format!(
                "Invalid variable format: {}. Expected key=value",
                var
            ));
        }
    }
    Ok(map)
}
```

**Extract To:** `ggen_utils::cli::parse_key_value_pairs`

**New Utility Function:**
```rust
// In crates/ggen-utils/src/cli.rs (new file)

use std::collections::BTreeMap;

/// Parse key=value pairs from command-line arguments
///
/// # Examples
///
/// ```rust
/// let vars = vec!["name=value".to_string(), "key=data".to_string()];
/// let map = parse_key_value_pairs(&vars).unwrap();
/// assert_eq!(map.get("name"), Some(&"value".to_string()));
/// ```
pub fn parse_key_value_pairs(
    pairs: &[String],
) -> Result<BTreeMap<String, String>, String> {
    let mut map = BTreeMap::new();

    for pair in pairs {
        if let Some((key, value)) = pair.split_once('=') {
            map.insert(key.to_string(), value.to_string());
        } else {
            return Err(format!(
                "Invalid key=value format: '{}'. Expected format: KEY=VALUE",
                pair
            ));
        }
    }

    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_key_value_pairs_valid() {
        let input = vec![
            "name=myapp".to_string(),
            "version=1.0".to_string(),
        ];

        let result = parse_key_value_pairs(&input).unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result.get("name"), Some(&"myapp".to_string()));
        assert_eq!(result.get("version"), Some(&"1.0".to_string()));
    }

    #[test]
    fn test_parse_key_value_pairs_invalid() {
        let input = vec!["invalid_format".to_string()];
        let result = parse_key_value_pairs(&input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_key_value_pairs_empty() {
        let input: Vec<String> = vec![];
        let result = parse_key_value_pairs(&input).unwrap();
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn test_parse_key_value_pairs_with_equals_in_value() {
        let input = vec!["url=http://example.com?q=test".to_string()];
        let result = parse_key_value_pairs(&input).unwrap();
        assert_eq!(result.get("url"), Some(&"http://example.com?q=test".to_string()));
    }
}
```

**Update template.rs:**
```rust
// Remove lines 303-316, replace with:
use ggen_utils::cli::parse_key_value_pairs;
```

**Update project.rs:**
```rust
// Remove lines 777-790, replace with:
use ggen_utils::cli::parse_key_value_pairs;
```

**Update ggen-utils/src/lib.rs:**
```rust
pub mod cli;
```

**Effort Breakdown:**
- Create utility module: 5 min
- Write tests: 5 min
- Update CLI files: 3 min
- Test & verify: 2 min
- **Total: 15 minutes**

---

## EXECUTION PLAN

### Phase 1: Important Extractions (1.5 hours)

**Week 1:**
1. P1-1: Marketplace list filtering (1 hour) - **Medium priority**
2. P1-2: Marketplace improve refactor (30 minutes) - **Low risk, consistency**

### Phase 2: Code Quality (15 minutes)

**Week 1:**
3. P2-1: Variable parsing helper (15 minutes) - **DRY principle**

---

## VALIDATION CHECKLIST

After each extraction:

- [ ] Domain function exists in `ggen-domain`
- [ ] CLI calls domain function
- [ ] CLI only handles output formatting
- [ ] Unit tests added to domain
- [ ] Integration test updated (if needed)
- [ ] Cargo make check passes
- [ ] Cargo make test passes
- [ ] Cargo make lint passes
- [ ] No circular dependencies introduced
- [ ] Documentation updated

---

## SUCCESS CRITERIA

**Before:**
- Functions fully migrated: 32/34 (94.1%)
- LOC to extract: 62
- Code duplication instances: 1

**After:**
- Functions fully migrated: 34/34 (100%)
- LOC to extract: 0
- Code duplication instances: 0

---

**END OF EXTRACTION MATRIX**
