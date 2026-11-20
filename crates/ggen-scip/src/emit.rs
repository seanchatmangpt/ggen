//! Emitter: walk ggen's generated surface, extract definitions and
//! references via tree-sitter-rust, write `<root>/.ggen/scip.json`.

use crate::{
    default_conforms_to, write_canonical, DefInfo, Document, Dotted, Index, Metadata, Occurrence,
    Symbol, ROLE_DEFINITION, ROLE_REFERENCE,
};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use tree_sitter::StreamingIterator;

/// Emitter identity written into index metadata.
pub const EMITTER: &str = "ggen-scip 0.1.0";

/// Index output path relative to the project root.
pub const OUTPUT_REL: &str = ".ggen/scip.json";

/// Max banner lines inspected for generated-surface detection.
const BANNER_LINES: usize = 5;

/// True when `path` is a `.rs` file whose first 5 lines carry the ggen
/// generated banner: contains "generated" and "ggen" (case-insensitive),
/// and does not contain "placeholder".
pub fn is_generated(path: &Path) -> bool {
    let ok = std::fs::File::open(path).and_then(|f| {
        use std::io::BufRead;
        let reader = std::io::BufReader::new(f);
        let mut head = String::new();
        for line in reader.lines().take(BANNER_LINES) {
            let line = line?;
            head.push_str(&line);
            head.push('\n');
            let low = head.to_lowercase();
            if low.contains("generated") && low.contains("ggen") && !low.contains("placeholder") {
                return Ok(true);
            }
        }
        Ok(false)
    });
    ok.unwrap_or(false)
}

/// Collect candidate `.rs` files under `root` (crates/*/src and src/).
pub fn discover(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut search_dirs: Vec<PathBuf> = Vec::new();
    let crates = root.join("crates");
    if let Ok(entries) = std::fs::read_dir(&crates) {
        for e in entries.flatten() {
            let d = e.path().join("src");
            if d.is_dir() {
                search_dirs.push(d);
            }
        }
    }
    let src = root.join("src");
    if src.is_dir() {
        search_dirs.push(src);
    }
    for dir in search_dirs {
        walk(&dir, &mut out);
    }
    out.sort();
    out
}

/// Recursive walk collecting `.rs` files.
fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for e in entries.flatten() {
        let p = e.path();
        if p.is_dir() {
            walk(&p, out);
        } else if p.extension().and_then(|x| x.to_str()) == Some("rs") {
            out.push(p);
        }
    }
}

/// Package (crate) name for a file: nearest ancestor Cargo.toml
/// `[package] name`, falling back to the crate dir name.
pub fn package_name(file: &Path) -> String {
    let mut dir = file.parent();
    while let Some(d) = dir {
        let manifest = d.join("Cargo.toml");
        if manifest.is_file() {
            if let Ok(text) = std::fs::read_to_string(&manifest) {
                // Only the [package] section: `[[bin]]`/`[[test]]` names
                // must not win (witnessed: ggen-cli's manifest lists a
                // `[[bin]] name = "ggen"` before [package]).
                let mut in_pkg = false;
                for line in text.lines() {
                    let t = line.trim();
                    if t == "[package]" {
                        in_pkg = true;
                        continue;
                    }
                    if t.starts_with('[') {
                        in_pkg = false;
                    }
                    if in_pkg {
                        if let Some(rest) = t.strip_prefix("name = \"") {
                            if let Some(name) = rest.strip_suffix('"') {
                                return name.to_string();
                            }
                        }
                    }
                }
            }
            return d
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
        }
        dir = d.parent();
    }
    String::new()
}

/// One extracted definition: typed symbol + range.
#[derive(Debug, Clone)]
pub struct RawDef {
    pub symbol: Symbol,
    pub name: String,
    pub kind: String,
    pub range: [u32; 3],
}

/// Module path of `file` relative to its crate `src/` root, dotted
/// (`generated_commands`, `generated.refusals`), excluding `mod`/`lib`.
pub fn module_path(file: &Path) -> String {
    let mut dir = file.parent();
    let mut segs: Vec<String> = Vec::new();
    // file stem
    if let Some(stem) = file.file_stem() {
        let stem = stem.to_string_lossy().into_owned();
        if stem != "mod" && stem != "lib" {
            segs.push(stem);
        }
    }
    while let Some(d) = dir {
        let name = d.file_name().map(|n| n.to_string_lossy().into_owned());
        match name {
            Some(n) if n == "src" => break,
            Some(n) if n == "generated" => segs.push(n),
            Some(n) => segs.push(n),
            None => break,
        }
        dir = d.parent();
    }
    segs.reverse();
    segs.join(".")
}

/// tree-sitter Rust parser (fresh per call site; cheap wrapper around the
/// compiled grammar).
fn parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_rust::LANGUAGE.into())
        .expect("rust grammar load");
    p
}

/// Definition query: fn/struct/enum/const names.
const DEF_QUERY: &str = r#"
(function_item name: (identifier) @name)
(struct_item name: (type_identifier) @name)
(enum_item name: (type_identifier) @name)
(const_item name: (identifier) @name)
"#;

/// Extract definitions from a parsed Rust source.
pub fn extract_defs(
    tree: &tree_sitter::Tree, source: &str, package: &str, module: &str,
) -> Vec<RawDef> {
    let lang = tree_sitter_rust::LANGUAGE.into();
    let q = tree_sitter::Query::new(&lang, DEF_QUERY).expect("def query compiles");
    let mut cursor = tree_sitter::QueryCursor::new();
    let mut matches = cursor.matches(&q, tree.root_node(), source.as_bytes());
    let mut defs = Vec::new();
    while let Some(m) = matches.next() {
        let Some(name_node) = m.captures.iter().find_map(|c| {
            if q.capture_names()[c.index as usize] == "name" {
                Some(c.node)
            } else {
                None
            }
        }) else {
            continue;
        };
        {
            let name = name_node.utf8_text(source.as_bytes()).unwrap_or("");
            if name.is_empty() {
                continue;
            }
            let kind = match name_node.kind() {
                "identifier" => {
                    // fn vs const share `identifier`; resolve via parent kind.
                    match name_node.parent().map(|p| p.kind()) {
                        Some("const_item") => "const",
                        _ => "fn",
                    }
                }
                "type_identifier" => match name_node.parent().map(|p| p.kind()) {
                    Some("struct_item") => "struct",
                    Some("enum_item") => "enum",
                    _ => "struct",
                },
                _ => continue,
            };
            let identifier = if module.is_empty() {
                name.to_string()
            } else {
                format!("{}.{}", module, name)
            };
            defs.push(RawDef {
                symbol: Symbol {
                    scheme: "ggen".to_string(),
                    package: package.to_string(),
                    identifier: Dotted(identifier),
                },
                name: name.to_string(),
                kind: kind.to_string(),
                range: [
                    name_node.start_position().row as u32,
                    name_node.start_position().column as u32,
                    name_node.end_position().column as u32,
                ],
            });
        }
    }
    defs
}

/// Reference query: every identifier-like node in the file.
const REF_QUERY: &str = r#"
(identifier) @id
(type_identifier) @id
"#;

/// Extract references: identifier nodes whose text matches a known
/// definition's simple name. Ambiguous simple names resolve to the
/// lexicographically smallest symbol (deterministic).
pub fn extract_refs(
    tree: &tree_sitter::Tree, source: &str, def_names: &BTreeMap<String, String>,
) -> Vec<Occurrence> {
    let lang = tree_sitter_rust::LANGUAGE.into();
    let q = match tree_sitter::Query::new(&lang, REF_QUERY) {
        Ok(q) => q,
        Err(_) => return Vec::new(),
    };
    let mut cursor = tree_sitter::QueryCursor::new();
    let mut matches = cursor.matches(&q, tree.root_node(), source.as_bytes());
    let mut refs = Vec::new();
    let mut seen: BTreeSet<[u32; 3]> = BTreeSet::new();
    while let Some(m) = matches.next() {
        for cap in m.captures {
            let node = cap.node;
            let name = node.utf8_text(source.as_bytes()).unwrap_or("");
            let Some(sym) = def_names.get(name) else {
                continue;
            };
            let range = [
                node.start_position().row as u32,
                node.start_position().column as u32,
                node.end_position().column as u32,
            ];
            if seen.insert(range) {
                refs.push(Occurrence {
                    symbol: sym.clone(),
                    range,
                    roles: ROLE_REFERENCE,
                    def_info: None,
                });
            }
        }
    }
    refs
}

/// Emit the index for `root`: discover generated files, parse, extract,
/// canonicalize, write `<root>/.ggen/scip.json`. Returns the index.
pub fn emit(root: &Path) -> std::io::Result<Index> {
    let index = build_index(root)?;
    write_canonical(&index, &root.join(OUTPUT_REL))?;
    Ok(index)
}

/// Build the index without writing.
pub fn build_index(root: &Path) -> std::io::Result<Index> {
    let files = discover(root);
    let mut docs: Vec<(String, Vec<Occurrence>, Vec<RawDef>)> = Vec::new();

    for file in &files {
        if !is_generated(file) {
            continue;
        }
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(_) => continue,
        };
        let package = package_name(file);
        let module = module_path(file);
        let mut parser = parser();
        let Some(tree) = parser.parse(&source, None) else {
            continue;
        };
        let defs = extract_defs(&tree, &source, &package, &module);
        docs.push((rel_path(root, file), Vec::new(), defs));
    }

    // Global simple-name -> wire-symbol map (lexicographically smallest
    // wins for ambiguous names; deterministic).
    let mut def_names: BTreeMap<String, String> = BTreeMap::new();
    for (_, _, defs) in &docs {
        for d in defs {
            let wire = d.symbol.to_wire();
            def_names
                .entry(d.name.clone())
                .or_insert_with(|| wire.clone());
        }
    }

    // Attach occurrences: definitions live in their own document;
    // references resolved per document against the global def-name table.
    let mut documents = Vec::new();
    for file in &files {
        let rel = rel_path(root, file);
        let Some((_, occs, defs)) = docs.iter_mut().find(|(p, _, _)| *p == rel) else {
            continue;
        };
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(_) => continue,
        };
        let mut parser = parser();
        if let Some(tree) = parser.parse(&source, None) {
            occs.extend(extract_refs(&tree, &source, &def_names));
        }
        for d in defs.iter() {
            let wire = d.symbol.to_wire();
            occs.push(Occurrence {
                symbol: wire,
                range: d.range,
                roles: ROLE_DEFINITION,
                def_info: Some(DefInfo {
                    name: d.name.clone(),
                    kind: d.kind.clone(),
                }),
            });
        }
        occs.sort_by(|a, b| (a.range, &a.symbol).cmp(&(b.range, &b.symbol)));
        // Merge same-(range, symbol) occurrences into one, OR-ing roles and
        // keeping def_info: a definition site that is also a resolved
        // reference keeps BOTH role bits (dedup must not drop the
        // definition — witnessed: plain dedup_by kept the reference and
        // definitions lost ROLE_DEFINITION).
        let mut merged: Vec<Occurrence> = Vec::with_capacity(occs.len());
        for occ in occs.drain(..) {
            if let Some(last) = merged.last_mut() {
                if last.range == occ.range && last.symbol == occ.symbol {
                    last.roles |= occ.roles;
                    if occ.def_info.is_some() {
                        last.def_info = occ.def_info;
                    }
                    continue;
                }
            }
            merged.push(occ);
        }
        documents.push(Document {
            path: rel.clone(),
            occurrences: merged,
        });
    }
    documents.sort_by(|a, b| a.path.cmp(&b.path));

    Ok(Index {
        metadata: Metadata {
            project_root: root.to_string_lossy().into_owned(),
            emitter: EMITTER.to_string(),
            // R80-04: index self-describes its governing shapes (scip:conformsTo).
            conforms_to: default_conforms_to(),
        },
        documents,
    })
}

/// Path relative to `root`, POSIX separators.
fn rel_path(root: &Path, file: &Path) -> String {
    file.strip_prefix(root)
        .unwrap_or(file)
        .to_string_lossy()
        .into_owned()
}

/// Canonical JSON string for external callers (emit without file write).
pub fn emit_to_string(root: &Path) -> std::io::Result<String> {
    let index = build_index(root)?;
    crate::canonical_json(&index).map_err(std::io::Error::other)
}
