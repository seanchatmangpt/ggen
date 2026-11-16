//! Drift guard for the ggen.toml schema selector (`classify_ggen_toml`).
use ggen_config::{classify_ggen_toml, ConfigSchemaClassification as C};
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(rd) = std::fs::read_dir(dir) else { return };
    for e in rd.flatten() {
        let p = e.path();
        let name = e.file_name().to_string_lossy().into_owned();
        if p.is_dir() {
            if matches!(name.as_str(), "target" | ".git" | "node_modules") { continue; }
            collect(&p, out);
        } else if name == "ggen.toml" {
            out.push(p);
        }
    }
}

#[test]
fn census() {
    let root = repo_root();
    let mut files = Vec::new();
    collect(&root, &mut files);
    files.sort();
    for f in &files {
        let raw = std::fs::read_to_string(f).unwrap();
        let tag = match classify_ggen_toml(&raw) {
            C::DeclarativeRules => "D", C::Frontmatter => "F",
            C::Ambiguous{..} => "A", C::Unsupported{..} => "U", C::Malformed{..} => "M",
        };
        println!("CENSUS {tag} {}", f.strip_prefix(&root).unwrap().display());
    }
}
