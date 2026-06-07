use ggen_projection::{
    DependencyCycleError, DependencyNotFoundError, PackDescriptor, PackPlan,
    PackTemplateDescriptor, VersionConflictError,
};
use std::collections::BTreeMap;
use std::path::PathBuf;

#[test]
fn test_f1_t1_parse_valid_toml() {
    let toml_content = r#"
        id = "pack_a"
        name = "Pack A"
        version = "26.6.6"
        description = "A valid pack"
        license = "MIT"
        dependencies = { "pack_b" = "^2.0.0" }
        query_aliases = {}
        [[templates]]
        path = "src/main.rs"
        description = "main template"
        variables = ["var1"]
    "#;
    let desc = PackDescriptor::from_toml(toml_content).unwrap();
    assert_eq!(desc.id, "pack_a");
    assert_eq!(desc.name, "Pack A");
    assert_eq!(desc.version, "26.6.6");
    assert_eq!(desc.dependencies.get("pack_b").unwrap(), "^2.0.0");
    assert_eq!(desc.templates.len(), 1);
    assert_eq!(desc.templates[0].path, PathBuf::from("src/main.rs"));
}

#[test]
fn test_f1_t1_resolve_linear_dependency() {
    let p_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: {
            let mut map = BTreeMap::new();
            map.insert("pack_b".to_string(), "^2.0.0".to_string());
            map
        },
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let p_b = PackDescriptor {
        id: "pack_b".to_string(),
        name: "Pack B".to_string(),
        version: "2.1.0".to_string(),
        description: "B".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl2.rs"),
            description: "desc2".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let plan = PackPlan::resolve(&[p_a, p_b]).unwrap();
    assert_eq!(plan.resolution_order, vec!["pack_b", "pack_a"]);
}

#[test]
fn test_f1_t1_resolve_multi_dependency() {
    let p_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: {
            let mut map = BTreeMap::new();
            map.insert("pack_b".to_string(), "*".to_string());
            map.insert("pack_c".to_string(), "*".to_string());
            map
        },
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let p_b = PackDescriptor {
        id: "pack_b".to_string(),
        name: "Pack B".to_string(),
        version: "26.6.6".to_string(),
        description: "B".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl2.rs"),
            description: "desc2".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let p_c = PackDescriptor {
        id: "pack_c".to_string(),
        name: "Pack C".to_string(),
        version: "26.6.6".to_string(),
        description: "C".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl3.rs"),
            description: "desc3".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let plan = PackPlan::resolve(&[p_a, p_b, p_c]).unwrap();
    assert!(plan.resolution_order.contains(&"pack_b".to_string()));
    assert!(plan.resolution_order.contains(&"pack_c".to_string()));
    assert_eq!(*plan.resolution_order.last().unwrap(), "pack_a".to_string());
}

#[test]
fn test_f1_t1_validate_metadata_fields() {
    // missing id
    let p_no_id = PackDescriptor {
        id: "".to_string(),
        name: "A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    assert!(p_no_id.validate().is_err());

    // missing name
    let p_no_name = PackDescriptor {
        id: "pack_a".to_string(),
        name: "".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    assert!(p_no_name.validate().is_err());

    // missing version
    let p_no_version = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    assert!(p_no_version.validate().is_err());

    // missing templates
    let p_no_templates = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![],
        query_aliases: BTreeMap::new(),
    };
    assert!(p_no_templates.validate().is_err());
}

#[test]
fn test_f1_t1_check_version_compatibility() {
    let p_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: {
            let mut map = BTreeMap::new();
            map.insert("pack_b".to_string(), "^1.2.0".to_string());
            map
        },
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let p_b = PackDescriptor {
        id: "pack_b".to_string(),
        name: "Pack B".to_string(),
        version: "1.3.4".to_string(),
        description: "B".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl2.rs"),
            description: "desc2".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let plan = PackPlan::resolve(&[p_a, p_b]);
    assert!(plan.is_ok());
}

#[test]
fn test_f1_t2_dependency_cycle() {
    let p_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: {
            let mut map = BTreeMap::new();
            map.insert("pack_b".to_string(), "*".to_string());
            map
        },
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let p_b = PackDescriptor {
        id: "pack_b".to_string(),
        name: "Pack B".to_string(),
        version: "26.6.6".to_string(),
        description: "B".to_string(),
        license: "MIT".to_string(),
        dependencies: {
            let mut map = BTreeMap::new();
            map.insert("pack_a".to_string(), "*".to_string());
            map
        },
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl2.rs"),
            description: "desc2".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let plan_err = PackPlan::resolve(&[p_a, p_b]).unwrap_err();
    assert!(plan_err.is::<DependencyCycleError>());
}

#[test]
fn test_f1_t2_missing_dependency() {
    let p_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: {
            let mut map = BTreeMap::new();
            map.insert("pack_missing".to_string(), "*".to_string());
            map
        },
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let plan_err = PackPlan::resolve(&[p_a]).unwrap_err();
    assert!(plan_err.is::<DependencyNotFoundError>());
}

#[test]
fn test_f1_t2_invalid_toml_syntax() {
    let invalid_toml = r#"
        id = "pack_a"
        name = "Pack A"
        version = "26.6.6"
        license = MIT
    "#; // MIT without quotes is syntax error
    let res = PackDescriptor::from_toml(invalid_toml);
    assert!(res.is_err());
}

#[test]
fn test_f1_t2_duplicate_dependency_name() {
    let toml_content = r#"
        id = "pack_a"
        name = "Pack A"
        version = "26.6.6"
        description = "A valid pack"
        license = "MIT"
        dependencies = { "pack_b" = "^26.6.6", "pack_b" = "^2.0.0" }
        query_aliases = {}
        [[templates]]
        path = "src/main.rs"
        description = "main template"
        variables = ["var1"]
    "#;
    let res = PackDescriptor::from_toml(toml_content);
    assert!(res.is_err());
}

#[test]
fn test_f1_t2_incompatible_version_conflict() {
    let p_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: {
            let mut map = BTreeMap::new();
            map.insert("pack_b".to_string(), "^2.0.0".to_string());
            map
        },
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let p_b = PackDescriptor {
        id: "pack_b".to_string(),
        name: "Pack B".to_string(),
        version: "26.6.6".to_string(),
        description: "B".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl2.rs"),
            description: "desc2".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };
    let plan_err = PackPlan::resolve(&[p_a, p_b]).unwrap_err();
    assert!(plan_err.is::<VersionConflictError>());
}
