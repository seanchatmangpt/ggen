//! The board: manufacture a real consumer project from a pack and play a
//! case through the **full** ggen lifecycle.
//!
//! Nothing here is simulated. A real `ggen.toml` and a real copy of the
//! pack's own `ontology.ttl` are written to a real temp directory, the real
//! engine loads the real graph, and the real write path puts real bytes on
//! disk. The only thing the harness contributes is the adversarial case and
//! the observation record.
//!
//! Root-escape detection deserves a note. The harness does not ask the
//! engine whether it stayed inside the root — a compromised or buggy write
//! path would answer "yes" either way. Instead the consumer is placed inside
//! a larger *arena* directory containing a canary tree, and the arena is
//! fingerprinted before and after. Anything that changes outside the
//! consumer subtree is observed directly, whatever the engine claims.

use std::path::{Path, PathBuf};

use crate::selfplay::case::Case;
use crate::selfplay::referee::{fingerprint, referee_verdict, Observation, Verdict};

/// A manufactured arena: `<arena>/canary/...` plus `<arena>/consumer/...`.
/// Dropping it removes the whole tree.
pub struct Board {
    arena: tempfile::TempDir,
    pack: String,
}

impl Board {
    /// Build an arena for `pack_dir` (a directory under `packs/`).
    ///
    /// # Errors
    /// Fails if the pack has no `ontology.ttl`, or if the arena cannot be
    /// created — both are harness faults, not findings, so they surface as
    /// errors rather than as violations.
    pub fn new(pack_dir: &Path) -> std::io::Result<Self> {
        let pack = pack_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "unknown".to_string());
        let ontology = pack_dir.join("ontology.ttl");
        if !ontology.is_file() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("{} has no ontology.ttl", pack_dir.display()),
            ));
        }
        let arena = tempfile::tempdir()?;

        // Canary tree: files the engine has no business touching. Their
        // hashes are part of the arena fingerprint, so a traversal write
        // that lands here is observed even if it reports success.
        let canary = arena.path().join("canary");
        std::fs::create_dir_all(canary.join("nested"))?;
        std::fs::write(canary.join("DO_NOT_TOUCH.txt"), b"canary\n")?;
        std::fs::write(canary.join("nested/DO_NOT_TOUCH.txt"), b"canary\n")?;

        let consumer = arena.path().join("consumer");
        std::fs::create_dir_all(consumer.join("templates"))?;
        std::fs::copy(&ontology, consumer.join("ontology.ttl"))?;
        std::fs::write(
            consumer.join("ggen.toml"),
            format!(
                "[project]\nname = \"selfplay-{pack}\"\n\n\
                 [ontology]\nsource = \"ontology.ttl\"\n\n\
                 [templates]\ndir = \"templates\"\n"
            ),
        )?;
        Ok(Self { arena, pack })
    }

    #[must_use]
    pub fn consumer(&self) -> PathBuf {
        self.arena.path().join("consumer")
    }

    #[must_use]
    pub fn pack(&self) -> &str {
        &self.pack
    }

    /// Play `case` through the full lifecycle and return the referee's
    /// ruling plus the raw observations behind it.
    ///
    /// The lifecycle, in order: syntax gate -> query -> independent recount
    /// -> write the template -> dry run -> apply -> receipt verify ->
    /// second apply (idempotence). Later steps are skipped when an earlier
    /// one legitimately refuses; a skipped step leaves its observation
    /// `None`, and the referee only demands an answer for steps that were
    /// actually supposed to run.
    #[must_use]
    pub fn play(&self, case: &Case) -> (Verdict, Observation) {
        let root = self.consumer();
        let root_str = root.display().to_string();
        let mut obs = Observation::default();

        let before = fingerprint(self.arena.path());

        // --- syntax gate, independent of the tool under test -------------
        obs.syntax_valid = Some(ggen_graph::sparql::check_sparql_syntax(&case.sparql).is_ok());

        // --- query -------------------------------------------------------
        let q = crate::tools::query_preview::query_preview(
            &crate::tools::query_preview::QueryPreviewParams {
                root: root_str.clone(),
                sparql: case.sparql.clone(),
                max_rows: None,
            },
        );
        match &q {
            Ok(r) => {
                obs.query_ok = Some(r.ok);
                obs.reported_rows = Some(r.row_count);
                obs.returned_rows = Some(r.returned_rows);
                obs.truncated = Some(r.truncated);
            }
            Err(_) => obs.query_ok = Some(false),
        }

        // --- independent recount ------------------------------------------
        // Only meaningful for a SELECT that executed. Wrapping the case's
        // own query as a subselect counts the SAME result set by a second
        // path, so a wrong count cannot agree with itself.
        if matches!(&q, Ok(r) if r.ok && r.boolean_result.is_none()) {
            let counted = format!(
                "SELECT (COUNT(*) AS ?selfplay_n) WHERE {{ {{ {} }} }}",
                case.sparql
            );
            if ggen_graph::sparql::check_sparql_syntax(&counted).is_ok() {
                if let Ok(c) = crate::tools::query_preview::query_preview(
                    &crate::tools::query_preview::QueryPreviewParams {
                        root: root_str.clone(),
                        sparql: counted,
                        max_rows: None,
                    },
                ) {
                    obs.independent_rows = c
                        .rows
                        .first()
                        .and_then(|row| row.get("selfplay_n"))
                        .and_then(serde_json::Value::as_u64)
                        .map(|n| n as usize);
                }
            }
        }

        // --- template on disk, then dry run and apply ---------------------
        let tpl = root.join("templates/probe.tmpl");
        if std::fs::write(&tpl, case.template_file()).is_ok() {
            let _ = crate::tools::sync_dry_run::sync_dry_run(
                &crate::tools::sync_dry_run::SyncDryRunParams {
                    root: root_str.clone(),
                },
            );

            let applied = crate::tools::write_apply::write_apply(
                &crate::tools::write_apply::WriteApplyParams {
                    root: root_str.clone(),
                    confirm: true,
                },
            );
            match &applied {
                Ok(a) => {
                    obs.applied_ok = Some(a.ok);
                    obs.written = a.written.iter().map(|w| w.path.clone()).collect();
                    // Receipt evidence: existence AND verification, checked
                    // against the path the tool itself reported.
                    if !obs.written.is_empty() {
                        let receipt = root.join(&a.receipt_path);
                        obs.receipt_verified = if receipt.is_file() {
                            Some(verify_receipt(&root))
                        } else {
                            None
                        };
                        // Idempotence: immediate re-apply of unchanged input.
                        if let Ok(second) = crate::tools::write_apply::write_apply(
                            &crate::tools::write_apply::WriteApplyParams {
                                root: root_str.clone(),
                                confirm: true,
                            },
                        ) {
                            obs.second_apply_written = Some(second.write_count);
                        }
                    }
                }
                Err(_) => obs.applied_ok = Some(false),
            }
        }

        // --- containment, judged by observation ---------------------------
        let after = fingerprint(self.arena.path());
        for (path, hash) in &after {
            if path.starts_with("consumer/") {
                continue;
            }
            if before.get(path) != Some(hash) {
                obs.changed_outside_root.push(path.clone());
            }
        }
        for path in before.keys() {
            if !path.starts_with("consumer/") && !after.contains_key(path) {
                obs.changed_outside_root.push(format!("{path} (deleted)"));
            }
        }
        obs.changed_outside_root.sort();
        obs.changed_outside_root.dedup();

        (referee_verdict(&obs), obs)
    }
}

/// Verify the project's receipt chain through the engine's own verifier
/// rather than by re-implementing chain logic here.
fn verify_receipt(root: &Path) -> bool {
    ggen_engine::verbs::handlers::handle_receipt_verify_in(root)
        .map(|v| {
            v.get("valid")
                .and_then(serde_json::Value::as_bool)
                .unwrap_or(false)
        })
        .unwrap_or(false)
}
