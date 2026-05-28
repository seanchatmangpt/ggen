//! `ggen lsp init` — make any project LSP-live in one command.
//!
//! Writes editor configurations that register `ggen lsp start` as a stdio language
//! server for ggen law surfaces, and emits the Agent Admissibility Pack. Helix and
//! Neovim support custom stdio servers declaratively; VS Code needs a thin
//! extension, so we drop a pointer note rather than a non-functional setting.

use std::io;
use std::path::Path;

use crate::pack::{emit, PackOptions};

/// Summary of an `init` run.
#[derive(Debug, Clone)]
pub struct InitReport {
    /// Files written (relative to root).
    pub files_written: Vec<String>,
    /// The admissibility-pack directory emitted.
    pub pack_dir: String,
}

const HELIX_LANGUAGES: &str = r#"# ggen LSP — registers `ggen lsp start` for law-surface files.
# Emitted by `ggen lsp init`. Merge into your global ~/.config/helix/languages.toml
# if you prefer global registration.
[language-server.ggen-lsp]
command = "ggen"
args = ["lsp", "start"]

[[language]]
name = "turtle"
scope = "source.turtle"
file-types = ["ttl", "nt", "nq"]
language-servers = ["ggen-lsp"]

[[language]]
name = "sparql"
scope = "source.sparql"
file-types = ["rq", "sparql"]
language-servers = ["ggen-lsp"]
"#;

const NVIM_SNIPPET: &str = r#"-- ggen LSP — emitted by `ggen lsp init`.
-- Source this from your init.lua to register the ggen stdio language server.
local lsp = vim.lsp
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "ttl", "turtle", "sparql", "ntriples", "nquads" },
  callback = function(args)
    lsp.start({
      name = "ggen-lsp",
      cmd = { "ggen", "lsp", "start" },
      root_dir = vim.fs.dirname(vim.fs.find({ "ggen.toml", ".git" }, { upward = true })[1]),
    })
  end,
})
"#;

const VSCODE_NOTE: &str = r#"{
  "//": "ggen LSP: VS Code needs a thin generic-LSP extension (e.g. 'Generic LSP Client') pointing at `ggen lsp start` over stdio for .ttl/.rq/.tera/ggen.toml. Helix (.helix/languages.toml) and Neovim (ggen-lsp.lua) are wired by `ggen lsp init` directly."
}
"#;

/// Initialize a project for the ggen LSP: write editor configs + emit the pack.
///
/// `editors` selects which configs to write (`helix`, `neovim`, `vscode`); empty
/// means all three. Existing files are not overwritten (idempotent) unless their
/// content differs and `force` is set.
///
/// # Errors
/// Returns an I/O error if a file or directory cannot be written.
pub fn init(root: &Path, editors: &[String], agents: &[String]) -> io::Result<InitReport> {
    let want = |name: &str| editors.is_empty() || editors.iter().any(|e| e == name);
    let mut written = Vec::new();

    if want("helix") {
        write_if_absent(
            &root.join(".helix").join("languages.toml"),
            HELIX_LANGUAGES,
            &mut written,
            root,
        )?;
    }
    if want("neovim") {
        write_if_absent(
            &root.join(".ggen").join("editor").join("ggen-lsp.lua"),
            NVIM_SNIPPET,
            &mut written,
            root,
        )?;
    }
    if want("vscode") {
        write_if_absent(
            &root.join(".ggen").join("editor").join("vscode-lsp-note.json"),
            VSCODE_NOTE,
            &mut written,
            root,
        )?;
    }

    // Emit the Agent Admissibility Pack alongside editor configs.
    let pack_out = root.join(".agent-admissibility");
    let agent_list = if agents.is_empty() {
        crate::pack::DEFAULT_AGENTS
            .iter()
            .map(|s| (*s).to_string())
            .collect()
    } else {
        agents.to_vec()
    };
    let pack = emit(&PackOptions {
        agents: agent_list,
        out_dir: pack_out.clone(),
        scan_hash: None,
    })?;

    Ok(InitReport {
        files_written: written,
        pack_dir: pack.out_dir,
    })
}

fn write_if_absent(
    path: &Path,
    contents: &str,
    written: &mut Vec<String>,
    root: &Path,
) -> io::Result<()> {
    if path.exists() {
        return Ok(()); // idempotent: never clobber existing editor config
    }
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, contents)?;
    let rel = path.strip_prefix(root).unwrap_or(path);
    written.push(rel.to_string_lossy().to_string());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn init_writes_editor_configs_and_pack() {
        let dir = TempDir::new().expect("tempdir");
        let report = init(dir.path(), &[], &["generic".to_string()]).expect("init");

        assert!(dir.path().join(".helix/languages.toml").is_file());
        assert!(dir.path().join(".ggen/editor/ggen-lsp.lua").is_file());
        assert!(dir
            .path()
            .join(".agent-admissibility/hooks/generic/pre-commit.sh")
            .is_file());
        assert!(!report.files_written.is_empty());
    }

    #[test]
    fn init_is_idempotent_for_editor_configs() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::create_dir_all(dir.path().join(".helix")).expect("mkdir");
        std::fs::write(dir.path().join(".helix/languages.toml"), "custom").expect("seed");

        init(dir.path(), &["helix".to_string()], &["generic".to_string()]).expect("init");
        // Existing config preserved (not clobbered).
        let content =
            std::fs::read_to_string(dir.path().join(".helix/languages.toml")).expect("read");
        assert_eq!(content, "custom");
    }
}
