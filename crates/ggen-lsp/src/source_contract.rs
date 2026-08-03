//! GGEN-SRC-004: generated Rust module declarations must have generation authority.
//!
//! This detector closes the gap between a successful `ggen sync` and a later
//! Rust compiler failure caused by `mod child;` when no generation rule owns
//! either legal module path.

use std::collections::HashSet;
use std::path::{Component, Path, PathBuf};

use lsp_max::lsp_types_max::DiagnosticSeverity;
use lsp_max_protocol::{LawAxis, MaxDiagnostic};

use crate::analyzers::diag::{self, Span};
use crate::project_index::{BufferOverlay, ProjectIndex};

pub use crate::analyzers::source_law_analyzer::GGEN_SRC_004;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModuleDeclaration {
    name: String,
    line: u32,
    start_col: u32,
    end_col: u32,
}

#[derive(Debug, Default)]
struct LexState {
    block_comment_depth: usize,
    raw_string_hashes: Option<usize>,
    quote: Option<u8>,
    escaped: bool,
}

/// Detect semicolon module declarations in generated Rust outputs whose legal
/// module paths are not owned by any generation rule.
///
/// Dynamic output patterns and URL outputs are excluded because their concrete
/// filesystem identity is not admitted until generation time.
#[must_use]
pub fn detect(
    project: &ProjectIndex, overlay: &BufferOverlay,
) -> Vec<(PathBuf, Vec<MaxDiagnostic>)> {
    let generated: HashSet<PathBuf> = project
        .rule_entries
        .iter()
        .filter_map(|entry| static_output_path(&project.root, &entry.output_file))
        .collect();

    let mut inspected = HashSet::new();
    let mut groups = Vec::new();

    for source_path in &generated {
        if source_path.extension().and_then(|ext| ext.to_str()) != Some("rs")
            || !inspected.insert(source_path.clone())
        {
            continue;
        }

        let Some(source) = read_overlay_or_disk(overlay, source_path) else {
            continue;
        };
        let mut diagnostics = Vec::new();

        for declaration in module_declarations(&source) {
            let candidates = module_candidates(source_path, &declaration.name);
            if candidates.iter().any(|candidate| generated.contains(candidate)) {
                continue;
            }

            let expected = candidates
                .iter()
                .map(|path| display_relative(&project.root, path))
                .collect::<Vec<_>>()
                .join(" or ");
            diagnostics.push(diag::max(
                Span {
                    start_line: declaration.line,
                    start_col: declaration.start_col,
                    end_line: declaration.line,
                    end_col: declaration.end_col,
                },
                DiagnosticSeverity::ERROR,
                Some(GGEN_SRC_004),
                format!(
                    "GGEN-SRC-004 UNOWNED_MODULE: generated source declares module `{}` \
                     but no generation rule produces {expected}. Add a generation rule \
                     for the module or remove the declaration from the generating template.",
                    declaration.name
                ),
                LawAxis::Domain,
            ));
        }

        if !diagnostics.is_empty() {
            groups.push((source_path.clone(), diagnostics));
        }
    }

    groups.sort_by(|left, right| left.0.cmp(&right.0));
    groups
}

fn read_overlay_or_disk(overlay: &BufferOverlay, path: &Path) -> Option<String> {
    overlay
        .get(path)
        .cloned()
        .or_else(|| std::fs::read_to_string(path).ok())
}

fn static_output_path(root: &Path, output: &str) -> Option<PathBuf> {
    if output.contains("{{")
        || output.contains("}}")
        || output.contains("://")
        || output.trim().is_empty()
    {
        return None;
    }

    let path = Path::new(output);
    let resolved = if path.is_absolute() {
        path.to_path_buf()
    } else {
        root.join(path)
    };
    Some(normalize_path(&resolved))
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            other => normalized.push(other.as_os_str()),
        }
    }
    normalized
}

fn module_candidates(source_path: &Path, module: &str) -> [PathBuf; 2] {
    let parent = source_path.parent().unwrap_or_else(|| Path::new("."));
    let stem = source_path.file_stem().and_then(|stem| stem.to_str());
    let base = match stem {
        Some("lib" | "main" | "mod") | None => parent.to_path_buf(),
        Some(stem) => parent.join(stem),
    };

    [
        normalize_path(&base.join(format!("{module}.rs"))),
        normalize_path(&base.join(module).join("mod.rs")),
    ]
}

fn display_relative(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

fn module_declarations(source: &str) -> Vec<ModuleDeclaration> {
    let mut declarations = Vec::new();
    let mut lex = LexState::default();
    let mut path_override_pending = false;

    for (line_index, source_line) in source.lines().enumerate() {
        let code = code_only_line(source_line, &mut lex);
        let trimmed = code.trim();

        if trimmed.starts_with("#[path") {
            path_override_pending = true;
            continue;
        }
        if trimmed.is_empty() || trimmed.starts_with("#[") {
            continue;
        }

        if let Some((name, start_col, end_col)) = parse_module_declaration(&code) {
            if path_override_pending {
                path_override_pending = false;
                continue;
            }
            declarations.push(ModuleDeclaration {
                name,
                line: u32::try_from(line_index).unwrap_or(u32::MAX),
                start_col,
                end_col,
            });
        } else {
            path_override_pending = false;
        }
    }

    declarations
}

fn parse_module_declaration(line: &str) -> Option<(String, u32, u32)> {
    let semicolon = line.find(';')?;
    let head = line[..semicolon].trim();
    if head.contains('{') {
        return None;
    }

    let module = if let Some(rest) = head.strip_prefix("mod ") {
        rest
    } else {
        let after_pub = head.strip_prefix("pub")?;
        if after_pub
            .chars()
            .next()
            .is_some_and(|character| !character.is_whitespace() && character != '(')
        {
            return None;
        }
        let mut rest = after_pub.trim_start();
        if rest.starts_with('(') {
            let close = rest.find(')')?;
            rest = rest[close + 1..].trim_start();
        }
        rest.strip_prefix("mod ")?
    };

    let name = module.trim();
    if name.is_empty()
        || !name
            .chars()
            .all(|character| character == '_' || character.is_ascii_alphanumeric())
        || name
            .chars()
            .next()
            .is_some_and(|character| character.is_ascii_digit())
    {
        return None;
    }

    let start = line.find(name)?;
    let end = start.saturating_add(name.len());
    Some((
        name.to_string(),
        u32::try_from(start).unwrap_or(u32::MAX),
        u32::try_from(end).unwrap_or(u32::MAX),
    ))
}

fn code_only_line(line: &str, state: &mut LexState) -> String {
    let bytes = line.as_bytes();
    let mut out = vec![b' '; bytes.len()];
    let mut index = 0usize;

    while index < bytes.len() {
        if let Some(hashes) = state.raw_string_hashes {
            if bytes[index] == b'"'
                && bytes
                    .get(index + 1..index + 1 + hashes)
                    .is_some_and(|suffix| suffix.iter().all(|byte| *byte == b'#'))
            {
                state.raw_string_hashes = None;
                index += hashes + 1;
            } else {
                index += 1;
            }
            continue;
        }

        if state.block_comment_depth > 0 {
            if bytes.get(index..index + 2) == Some(b"/*") {
                state.block_comment_depth += 1;
                index += 2;
            } else if bytes.get(index..index + 2) == Some(b"*/") {
                state.block_comment_depth -= 1;
                index += 2;
            } else {
                index += 1;
            }
            continue;
        }

        if let Some(quote) = state.quote {
            if state.escaped {
                state.escaped = false;
            } else if bytes[index] == b'\\' {
                state.escaped = true;
            } else if bytes[index] == quote {
                state.quote = None;
            }
            index += 1;
            continue;
        }

        if bytes.get(index..index + 2) == Some(b"//") {
            break;
        }
        if bytes.get(index..index + 2) == Some(b"/*") {
            state.block_comment_depth = 1;
            index += 2;
            continue;
        }

        if let Some((prefix_len, hashes)) = raw_string_prefix(&bytes[index..]) {
            state.raw_string_hashes = Some(hashes);
            index += prefix_len;
            continue;
        }

        if bytes[index] == b'"' {
            state.quote = Some(bytes[index]);
            index += 1;
            continue;
        }

        out[index] = bytes[index];
        index += 1;
    }

    String::from_utf8(out).unwrap_or_default()
}

fn raw_string_prefix(bytes: &[u8]) -> Option<(usize, usize)> {
    let mut index = if bytes.starts_with(b"br") {
        2
    } else if bytes.starts_with(b"r") {
        1
    } else {
        return None;
    };
    let mut hashes = 0usize;
    while bytes.get(index) == Some(&b'#') {
        hashes += 1;
        index += 1;
    }
    (bytes.get(index) == Some(&b'"')).then_some((index + 1, hashes))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rule_index::RuleIndexEntry;
    use tempfile::TempDir;

    fn rule(root: &Path, id: &str, output: &str) -> RuleIndexEntry {
        RuleIndexEntry {
            rule_id: id.to_string(),
            manifest_path: root.join("ggen.toml"),
            query_inline: true,
            query_content: "SELECT ?x WHERE { ?x ?p ?o }".to_string(),
            template_path: None,
            template_content: Some(String::new()),
            output_file: output.to_string(),
            selected_vars: Default::default(),
            issues: Vec::new(),
        }
    }

    #[test]
    fn reports_module_without_generation_rule() {
        let temp = TempDir::new().expect("tempdir");
        let root = temp.path();
        std::fs::create_dir_all(root.join("src")).expect("src dir");
        std::fs::write(root.join("src/lib.rs"), "pub mod capabilities;\n").expect("lib");

        let project = ProjectIndex {
            root: root.to_path_buf(),
            rule_entries: vec![rule(root, "lib", "src/lib.rs")],
        };
        let groups = detect(&project, &BufferOverlay::new());

        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].1.len(), 1);
        assert_eq!(
            groups[0].1[0].lsp.code,
            Some(lsp_max::lsp_types_max::NumberOrString::String(
                GGEN_SRC_004.to_string()
            ))
        );
        assert!(groups[0].1[0].lsp.message.contains("src/capabilities.rs"));
    }

    #[test]
    fn accepts_either_legal_generated_module_path() {
        let temp = TempDir::new().expect("tempdir");
        let root = temp.path();
        std::fs::create_dir_all(root.join("src")).expect("src dir");
        std::fs::write(root.join("src/lib.rs"), "pub mod backend;\n").expect("lib");
        std::fs::write(root.join("src/backend.rs"), "pub struct Backend;\n").expect("backend");

        let project = ProjectIndex {
            root: root.to_path_buf(),
            rule_entries: vec![
                rule(root, "lib", "src/lib.rs"),
                rule(root, "backend", "src/backend.rs"),
            ],
        };

        assert!(detect(&project, &BufferOverlay::new()).is_empty());
    }

    #[test]
    fn ignores_inline_path_overridden_and_non_code_declarations() {
        let temp = TempDir::new().expect("tempdir");
        let root = temp.path();
        std::fs::create_dir_all(root.join("src")).expect("src dir");
        std::fs::write(
            root.join("src/lib.rs"),
            r##"
                // pub mod comment_only;
                const TEXT: &str = "pub mod string_only;";
                const RAW: &str = r#"pub mod raw_string_only;"#;
                pub mod inline { pub struct Present; }
                #[path = "elsewhere.rs"]
                pub mod overridden;
            "##,
        )
        .expect("lib");

        let project = ProjectIndex {
            root: root.to_path_buf(),
            rule_entries: vec![rule(root, "lib", "src/lib.rs")],
        };

        assert!(detect(&project, &BufferOverlay::new()).is_empty());
    }

    #[test]
    fn overlay_is_authoritative_for_unsaved_source() {
        let temp = TempDir::new().expect("tempdir");
        let root = temp.path();
        std::fs::create_dir_all(root.join("src")).expect("src dir");
        let lib = root.join("src/lib.rs");
        std::fs::write(&lib, "pub struct Clean;\n").expect("lib");

        let project = ProjectIndex {
            root: root.to_path_buf(),
            rule_entries: vec![rule(root, "lib", "src/lib.rs")],
        };
        let mut overlay = BufferOverlay::new();
        overlay.insert(lib, "pub mod unsaved;\n".to_string());

        assert_eq!(detect(&project, &overlay).len(), 1);
    }
}
