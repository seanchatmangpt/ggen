fn validate_format(
    root: &Path,
    capability: &Capability,
    bytes: &[u8],
) -> Result<Vec<&'static str>, EvidenceError> {
    let text = utf8(capability.path, bytes)?;
    match capability.validator {
        Validator::RustLibrary => {
            let file = syn::parse_file(text).map_err(|error| EvidenceError::InvalidData {
                identity: capability.id.to_owned(),
                message: error.to_string(),
            })?;
            if public_item_count(&file) == 0 {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "Rust module exposes no public item".to_owned(),
                });
            }
            Ok(vec!["rust-parse", "public-api"])
        }
        Validator::RustBinary => {
            let file = syn::parse_file(text).map_err(|error| EvidenceError::InvalidData {
                identity: capability.id.to_owned(),
                message: error.to_string(),
            })?;
            let has_main = file.items.iter().any(
                |item| matches!(item, syn::Item::Fn(function) if function.sig.ident == "main"),
            );
            if !has_main {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "Rust binary has no main function".to_owned(),
                });
            }
            Ok(vec!["rust-parse", "binary-entrypoint"])
        }
        Validator::WorkflowYaml => {
            let value: serde_yaml::Value =
                serde_yaml::from_str(text).map_err(|error| EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: error.to_string(),
                })?;
            if value
                .get("jobs")
                .and_then(serde_yaml::Value::as_mapping)
                .is_none()
            {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "workflow has no jobs mapping".to_owned(),
                });
            }
            Ok(vec!["yaml-parse", "workflow-jobs"])
        }
        Validator::TargetList => {
            let targets: Vec<_> = text
                .lines()
                .map(str::trim)
                .filter(|line| !line.is_empty() && !line.starts_with('#'))
                .collect();
            let unique: BTreeSet<_> = targets.iter().copied().collect();
            if targets.len() < 8 || unique.len() != targets.len() {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "target list is too small or contains duplicates".to_owned(),
                });
            }
            Ok(vec!["target-list", "unique-targets"])
        }
        Validator::DebianControl => {
            require_fields(
                capability.id,
                text,
                &["Package:", "Version:", "Architecture:"],
            )?;
            Ok(vec!["debian-control", "required-fields"])
        }
        Validator::RpmSpec => {
            require_fields(
                capability.id,
                text,
                &["Name:", "Version:", "Release:", "Summary:"],
            )?;
            Ok(vec!["rpm-spec", "required-fields"])
        }
        Validator::NpmPackage => {
            let value: JsonValue =
                serde_json::from_str(text).map_err(|error| EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: error.to_string(),
                })?;
            for key in ["name", "version", "license", "files"] {
                if value.get(key).is_none() {
                    return Err(EvidenceError::InvalidData {
                        identity: capability.id.to_owned(),
                        message: format!("npm package missing {key}"),
                    });
                }
            }
            Ok(vec!["json-parse", "npm-contract"])
        }
        Validator::NugetPackage => {
            let document = roxmltree::Document::parse(text).map_err(|error| {
                EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: error.to_string(),
                }
            })?;
            for tag in ["package", "metadata", "id", "version", "authors"] {
                if !document.descendants().any(|node| node.has_tag_name(tag)) {
                    return Err(EvidenceError::InvalidData {
                        identity: capability.id.to_owned(),
                        message: format!("NuGet package missing <{tag}>"),
                    });
                }
            }
            Ok(vec!["xml-parse", "nuget-contract"])
        }
        Validator::CycloneDx => {
            let value: JsonValue =
                serde_json::from_str(text).map_err(|error| EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: error.to_string(),
                })?;
            if value.get("bomFormat").and_then(JsonValue::as_str) != Some("CycloneDX") {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "not a CycloneDX BOM".to_owned(),
                });
            }
            Ok(vec!["json-parse", "cyclonedx-contract"])
        }
        Validator::Spdx => {
            let value: JsonValue =
                serde_json::from_str(text).map_err(|error| EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: error.to_string(),
                })?;
            let version = value
                .get("spdxVersion")
                .and_then(JsonValue::as_str)
                .unwrap_or_default();
            if !version.starts_with("SPDX-") {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "not an SPDX document".to_owned(),
                });
            }
            Ok(vec!["json-parse", "spdx-contract"])
        }
        Validator::InTotoJsonLines => {
            let mut statements = 0_usize;
            for (line_number, line) in text.lines().enumerate() {
                if line.trim().is_empty() {
                    continue;
                }
                let value: JsonValue = serde_json::from_str(line).map_err(|error| {
                    EvidenceError::InvalidData {
                        identity: capability.id.to_owned(),
                        message: format!("line {}: {error}", line_number + 1),
                    }
                })?;
                if value.get("_type").is_none() && value.get("predicateType").is_none() {
                    return Err(EvidenceError::InvalidData {
                        identity: capability.id.to_owned(),
                        message: format!("line {} is not an in-toto statement", line_number + 1),
                    });
                }
                statements += 1;
            }
            if statements == 0 {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "no in-toto statements".to_owned(),
                });
            }
            Ok(vec!["jsonl-parse", "in-toto-contract"])
        }
        Validator::SourceManifest => {
            let entries = parse_manifest(text)?;
            if entries.len() != EXPECTED_MANIFEST_ENTRIES {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: format!(
                        "expected {EXPECTED_MANIFEST_ENTRIES} entries, found {}",
                        entries.len()
                    ),
                });
            }
            Ok(vec!["manifest-parse", "unique-sha256-paths"])
        }
        Validator::PythonSyntax => {
            let path = root.join(capability.path);
            let status = Command::new("python3")
                .args([
                    "-c",
                    "import pathlib,sys; p=pathlib.Path(sys.argv[1]); compile(p.read_text(encoding='utf-8'), str(p), 'exec')",
                ])
                .arg(&path)
                .status()
                .map_err(|error| EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: format!("python3 unavailable: {error}"),
                })?;
            if !status.success() {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "Python syntax compilation failed".to_owned(),
                });
            }
            Ok(vec!["python-compile", "lifecycle-entrypoint"])
        }
        Validator::ShellSyntax => {
            let status = Command::new("bash")
                .arg("-n")
                .arg(root.join(capability.path))
                .status()
                .map_err(|error| EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: format!("bash unavailable: {error}"),
                })?;
            if !status.success() {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "shell syntax check failed".to_owned(),
                });
            }
            Ok(vec!["bash-parse", "reproducible-build-entrypoint"])
        }
        Validator::GgenLock => {
            validate_ggen_lock(text)?;
            Ok(vec!["toml-parse", "complete-pack-lock"])
        }
    }
}
