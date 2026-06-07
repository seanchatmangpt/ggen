use tower_lsp_max::lsp_types_max::*;

use crate::Backend;

pub mod actions;
pub mod analysis;
pub mod rules;

pub use actions::code_actions;
pub use rules::get_diagnostics;

/// Diagnostic entry point called from `lib.rs`.
///
/// Parses the document with `syn`, locates the `impl LanguageServer` block,
/// and checks the capability-method contract.
pub async fn compute(backend: &Backend, uri: &Uri) -> Vec<Diagnostic> {
    let doc = backend.docs.get(uri);
    let doc = match doc {
        Some(d) if d.language_id == "rust" => d,
        _ => return vec![],
    };
    let source = doc.text.to_string();
    drop(doc);

    // syn::parse_file fails on incomplete files (common during editing).
    // Return empty diagnostics rather than false positives.
    let ast = match syn::parse_file(&source) {
        Ok(ast) => ast,
        Err(_) => return vec![],
    };

    let analysis = analysis::analyze_impl_block(&ast);
    rules::build_diagnostics(analysis)
}

pub fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let num_a = a_chars.len();
    let num_b = b_chars.len();

    let mut dp = vec![vec![0; num_b + 1]; num_a + 1];

    for (i, row) in dp.iter_mut().enumerate() {
        row[0] = i;
    }
    for (j, cell) in dp[0].iter_mut().enumerate() {
        *cell = j;
    }

    for i in 1..=num_a {
        for j in 1..=num_b {
            if a_chars[i - 1] == b_chars[j - 1] {
                dp[i][j] = dp[i - 1][j - 1];
            } else {
                dp[i][j] = 1 + std::cmp::min(
                    dp[i - 1][j - 1], // substitution
                    std::cmp::min(
                        dp[i - 1][j], // deletion
                        dp[i][j - 1], // insertion
                    ),
                );
            }
        }
    }

    dp[num_a][num_b]
}
