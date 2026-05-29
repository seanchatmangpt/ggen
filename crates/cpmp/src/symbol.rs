use crate::models::Symbol;
use regex::Regex;
use std::path::Path;

pub fn extract_symbols(path: &Path, content: &str) -> Vec<Symbol> {
    let mut symbols = Vec::new();
    let extension = path
        .extension()
        .and_then(|ext| ext.to_str())
        .unwrap_or("")
        .to_lowercase();
    let path_str = path.to_string_lossy().to_string();

    match extension.as_str() {
        "rs" => {
            let re_fn = Regex::new(r"(?m)^(?:\s*pub\s+)?(?:async\s+)?fn\s+(\w+)").unwrap();
            find_matches(content, &re_fn, "function", &path_str, &mut symbols);
        }
        "py" => {
            let re_fn = Regex::new(r"(?m)^\s*def\s+(\w+)").unwrap();
            find_matches(content, &re_fn, "function", &path_str, &mut symbols);
        }
        "js" | "ts" => {
            let re_fn = Regex::new(r"(?m)^(?:\s*export\s+)?(?:async\s+)?function\s+(\w+)").unwrap();
            find_matches(content, &re_fn, "function", &path_str, &mut symbols);
        }
        _ => {}
    }

    symbols
}

fn find_matches(content: &str, re: &Regex, kind: &str, file_path: &str, symbols: &mut Vec<Symbol>) {
    for (line_idx, line) in content.lines().enumerate() {
        if let Some(caps) = re.captures(line) {
            if let Some(m) = caps.get(1) {
                symbols.push(Symbol {
                    file_path: file_path.to_string(),
                    name: m.as_str().to_string(),
                    kind: kind.to_string(),
                    line: line_idx + 1,
                });
            }
        }
    }
}
