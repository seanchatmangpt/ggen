//! Chicago-TDD end-to-end proof for the expanded `packs/gh-terraform-pack`:
//! real filesystem (`TempDir`), real graph engine, real Tera, real `bash -n`.
//! Proves the pack generates the full Terraform GitHub-management surface
//! (16 .tf files + README), the 12-script gh CLI library, the per-resource /
//! per-data-source fan-out docs, and the Japanese docs — and that a second
//! sync is byte-identical (no drift).
//!
//! This test never runs `terraform apply` or any mutating `gh` command —
//! generation and `bash -n` syntax checks only.

use std::path::{Path, PathBuf};
use std::process::Command;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir");
    for entry in std::fs::read_dir(src).expect("read_dir") {
        let entry = entry.expect("entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy");
        }
    }
}

fn scaffold() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("gh-terraform-pack"),
        &dir.path().join("gh-terraform-pack"),
    );

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\ngh-terraform-pack = { path = \"../gh-terraform-pack\" }\n\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    (dir, project)
}

fn read(project: &Path, rel: &str) -> String {
    std::fs::read_to_string(project.join(rel)).unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

fn count_md_files(dir: &Path) -> usize {
    std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("read_dir {}: {e}", dir.display()))
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "md"))
        .count()
}

const TF_FILES: [&str; 16] = [
    "versions.tf",
    "variables.tf",
    "repository.tf",
    "branch-protection.tf",
    "rulesets.tf",
    "labels.tf",
    "milestones.tf",
    "issues.tf",
    "teams.tf",
    "collaborators.tf",
    "environments.tf",
    "secrets.tf",
    "webhooks.tf",
    "files.tf",
    "outputs.tf",
    "README.md",
];

const SCRIPTS: [&str; 18] = [
    "pr-create.sh",
    "pr-checks.sh",
    "pr-view-mergeable.sh",
    "pr-merge.sh",
    "pr-close-stale.sh",
    "run-view.sh",
    "run-rerun.sh",
    "issue-create.sh",
    "issue-close.sh",
    "release-view.sh",
    "repo-state.sh",
    "drift-report.sh",
    "accept-setup.sh",
    "accept-import.sh",
    "accept-apply.sh",
    "accept-drift-inject.sh",
    "accept-verify.sh",
    "accept-receipt.sh",
];

#[test]
fn gh_terraform_pack_generates_full_surface_and_is_idempotent() {
    let (_dir, project) = scaffold();

    let first = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    assert!(!first.written.is_empty(), "pack must write files");

    // (1) All 16 terraform-directory files exist.
    for f in TF_FILES {
        assert!(
            project.join("infra/terraform/github").join(f).is_file(),
            "missing infra/terraform/github/{f}"
        );
    }

    // (1a) versions.tf pins integrations/github 5.41.0.
    let versions = read(&project, "infra/terraform/github/versions.tf");
    assert!(
        versions.contains(r#"source  = "integrations/github""#),
        "{versions}"
    );
    assert!(versions.contains(r#"version = "5.41.0""#), "{versions}");

    // (1b) labels.tf carries the 11 desired-state labels.
    let labels = read(&project, "infra/terraform/github/labels.tf");
    for label in [
        "異常",
        "改善",
        "第五水準条件",
        "証拠待ち",
        "隔離",
        "公開停止",
        "受領済み",
        "Terraform漂流",
        "gh作動待ち",
        "標準",
        "l5-condition",
    ] {
        assert!(
            labels.contains(label),
            "labels.tf missing {label}: {labels}"
        );
    }

    // (1c) branch-protection.tf carries the real required status contexts.
    let bp = read(&project, "infra/terraform/github/branch-protection.tf");
    for ctx in ["Check", "Test", "Build", "Doctest"] {
        assert!(
            bp.contains(&format!("\"{ctx}\"")),
            "branch-protection.tf missing context {ctx}: {bp}"
        );
    }

    // (1c2) environments.tf declares the production environment.
    let envs = read(&project, "infra/terraform/github/environments.tf");
    assert!(
        envs.contains(r#""github_repository_environment" "production""#),
        "environments.tf missing production environment: {envs}"
    );

    // (1c3) milestones.tf declares the L5 milestone.
    let milestones = read(&project, "infra/terraform/github/milestones.tf");
    assert!(
        milestones.contains("v26.x TCPS L5"),
        "milestones.tf missing v26.x TCPS L5: {milestones}"
    );

    // (1c4) rulesets.tf declares the active ruleset with per-context
    // required_check blocks including Doctest.
    let rulesets = read(&project, "infra/terraform/github/rulesets.tf");
    assert!(
        rulesets.contains("required_check"),
        "rulesets.tf missing required_check: {rulesets}"
    );
    assert!(
        rulesets.contains(r#"context = "Doctest""#),
        "rulesets.tf missing Doctest context: {rulesets}"
    );

    // (1c5) intentionally-empty families carry an ontology-declared reason
    // comment (non-empty text after the marker), not just a bare header.
    let issues = read(&project, "infra/terraform/github/issues.tf");
    let reason_line = issues
        .lines()
        .find(|l| l.starts_with("# intentionally empty — "))
        .unwrap_or_else(|| panic!("issues.tf missing reason comment: {issues}"));
    assert!(
        reason_line.len() > "# intentionally empty — ".len() + 10,
        "issues.tf reason comment is empty: {reason_line}"
    );

    // (1d) secrets.tf references the variable, never a literal secret value.
    let secrets = read(&project, "infra/terraform/github/secrets.tf");
    assert!(secrets.contains("var.ggen_signing_key_value"), "{secrets}");
    // The only acceptable mention of the key is through the var reference —
    // strip those and demand no residual assignment of a literal.
    let stripped = secrets.replace("var.ggen_signing_key_value", "");
    assert!(
        !stripped.contains("plaintext_value = \"") && !stripped.contains("encrypted_value = \""),
        "secrets.tf must not embed a literal secret value: {secrets}"
    );

    // (2) Fan-out doc counts.
    let resources = count_md_files(&project.join("docs/gh-terraform/resources"));
    assert!(resources >= 87, "resources fan-out: {resources} < 87");
    let data_sources = count_md_files(&project.join("docs/gh-terraform/data-sources"));
    assert!(
        data_sources >= 75,
        "data-sources fan-out: {data_sources} < 75"
    );

    // (3) Deprecation honesty in the fan-out docs.
    let custom_role = read(
        &project,
        "docs/gh-terraform/resources/github_organization_custom_role.md",
    );
    assert!(
        custom_role.contains("github_organization_repository_role"),
        "custom_role doc must name its successor: {custom_role}"
    );
    let project_card = read(
        &project,
        "docs/gh-terraform/resources/github_project_card.md",
    );
    assert!(
        project_card.contains("dead")
            || project_card.contains("廃止")
            || project_card.contains("Dead"),
        "github_project_card.md must mark the dead API: {project_card}"
    );

    // (4) All 12 scripts exist, have the bash shebang, and pass `bash -n`.
    for script in SCRIPTS {
        let path = project.join("scripts/gh").join(script);
        assert!(path.is_file(), "missing scripts/gh/{script}");
        let body = read(&project, &format!("scripts/gh/{script}"));
        assert!(
            body.starts_with("#!/usr/bin/env bash"),
            "{script} missing shebang: {}",
            &body[..body.len().min(80)]
        );
        let out = Command::new("bash")
            .arg("-n")
            .arg(&path)
            .output()
            .expect("spawn bash -n");
        assert!(
            out.status.success(),
            "bash -n {script} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }

    // (5) Safety mechanisms in the mutating / diffing scripts.
    let close_stale = read(&project, "scripts/gh/pr-close-stale.sh");
    assert!(
        close_stale.contains("--confirm"),
        "pr-close-stale.sh must gate on --confirm: {close_stale}"
    );
    let drift = read(&project, "scripts/gh/drift-report.sh");
    assert!(
        drift.contains("detailed-exitcode"),
        "drift-report.sh must use -detailed-exitcode: {drift}"
    );

    // (5b) accept-import.sh projects ALL 11 label names from the ontology
    // (ontology projection, not a hand-maintained list), and runs its own
    // terraform init.
    let accept_import = read(&project, "scripts/gh/accept-import.sh");
    for label in [
        "異常",
        "改善",
        "第五水準条件",
        "証拠待ち",
        "隔離",
        "公開停止",
        "受領済み",
        "Terraform漂流",
        "gh作動待ち",
        "標準",
        "l5-condition",
    ] {
        assert!(
            accept_import.contains(label),
            "accept-import.sh missing projected label {label}: {accept_import}"
        );
    }
    assert!(
        accept_import.contains("terraform init"),
        "accept-import.sh must self-init: {accept_import}"
    );

    // (5c) accept-apply.sh mechanized deletion approval (第二十六章第四段階):
    // static markers, then a real subprocess sabotage proof with a stub
    // `terraform` on PATH that reports a destructive plan.
    let accept_apply = read(&project, "scripts/gh/accept-apply.sh");
    assert!(
        accept_apply.contains("--approve-deletions"),
        "accept-apply.sh must support --approve-deletions: {accept_apply}"
    );
    assert!(
        accept_apply.contains("exit 3"),
        "accept-apply.sh must refuse destructive plans with exit 3: {accept_apply}"
    );

    let stub_dir = project.parent().expect("project parent").join("tf-stub");
    std::fs::create_dir_all(&stub_dir).expect("mkdir tf-stub");
    let stub = stub_dir.join("terraform");
    std::fs::write(
        &stub,
        "#!/usr/bin/env bash\n\
         if [ \"$1\" = \"plan\" ]; then\n\
           echo \"  # github_issue_label.ijou will be destroyed\"\n\
           exit 2\n\
         fi\n\
         echo \"APPLY-RAN\"\n\
         exit 0\n",
    )
    .expect("write terraform stub");
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&stub, std::fs::Permissions::from_mode(0o755))
            .expect("chmod terraform stub");
    }
    let path_env = format!(
        "{}:{}",
        stub_dir.display(),
        std::env::var("PATH").unwrap_or_default()
    );
    let out = Command::new("bash")
        .arg(project.join("scripts/gh/accept-apply.sh"))
        .args(["--dir", &project.display().to_string()])
        .args(["--owner", "o", "--repo", "r"])
        .env("PATH", &path_env)
        .output()
        .expect("spawn accept-apply.sh sabotage run");
    let stderr = String::from_utf8_lossy(&out.stderr);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(
        out.status.code(),
        Some(3),
        "accept-apply.sh must exit 3 on destructive plan without --approve-deletions; \
         stdout: {stdout} stderr: {stderr}"
    );
    assert!(
        stderr.contains("--approve-deletions") && stderr.contains("will be destroyed"),
        "refusal must name the flag and the destroyed resources: {stderr}"
    );
    assert!(
        stderr.contains("github_issue_label.ijou"),
        "refusal must name the destroyed resource lines: {stderr}"
    );
    assert!(
        !stdout.contains("APPLY-RAN"),
        "apply must not run after refusal: {stdout}"
    );

    // (6a) ggen.lock records the pack.
    let lock = read(&project, "ggen.lock");
    assert!(lock.contains("[packs.gh-terraform-pack]"), "lock: {lock}");
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (7) L5 standing doc stays honest: C15 (provider-version procedure,
    // execution deferred by user decision) remains PARTIAL, and the ALIVE
    // rows cite the concrete live receipts — the original sandbox acceptance
    // run (blake3:80aa8e6d, C10-C12), the second-sandbox multi-repo run
    // (blake3:c394e85b, C16), and the 標準-label improvement cycle
    // (blake3:ae62dda5, C18) chained in .tcps/acceptance-receipts.jsonl.
    let l5 = read(&project, "docs/gh-terraform/L5-STATUS.md");
    assert!(
        l5.contains("PARTIAL"),
        "L5-STATUS.md must keep C15 honestly PARTIAL: {l5}"
    );
    assert!(
        l5.contains("提供者版差.md"),
        "the PARTIAL row must cite the documented-but-deferred procedure: {l5}"
    );
    assert!(
        !l5.contains("未実施"),
        "no 未実施 rows should remain after C16/C18 closure: {l5}"
    );
    for receipt in [
        "blake3:80aa8e6d",
        "blake3:c394e85b",
        "blake3:ae62dda5",
    ] {
        assert!(
            l5.contains(receipt),
            "L5-STATUS.md ALIVE rows must cite live receipt {receipt}: {l5}"
        );
    }
    assert!(
        l5.contains(".tcps/acceptance-receipts.jsonl"),
        "L5-STATUS.md must name the durable receipt chain file: {l5}"
    );

    // (6) Second sync is a no-op: nothing written, lock byte-identical.
    let second = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(
        second.written.is_empty(),
        "second sync wrote: {:?}",
        second.written
    );
    let lock2 = read(&project, "ggen.lock");
    assert_eq!(
        lock2, lock,
        "ggen.lock must be byte-identical across identical runs"
    );
}

/// Hermetic chain proof for the generated accept-receipt.sh: two appends to a
/// TempDir jsonl with real bash + jq; the second line's prev_hash must equal
/// sha256 of the first line. Loud skip if jq is absent.
#[test]
fn gh_terraform_accept_receipt_chains_hermetically() {
    if Command::new("jq")
        .arg("--version")
        .output()
        .map(|o| !o.status.success())
        .unwrap_or(true)
    {
        eprintln!(
            "=== SKIP: jq not on PATH — accept-receipt.sh chain proof not run \
             (install jq to exercise this test) ==="
        );
        return;
    }

    let (_dir, project) = scaffold();
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
    let script = project.join("scripts/gh/accept-receipt.sh");
    assert!(script.is_file(), "generated accept-receipt.sh missing");

    let ledger_dir = TempDir::new().expect("ledger tempdir");
    let ledger = ledger_dir.path().join("receipts.jsonl");
    let ledger_s = ledger.display().to_string();

    let run_receipt = |json: &str| -> (i32, String, String) {
        let out = Command::new("bash")
            .arg(&script)
            .args(["--file", &ledger_s, "--json", json])
            .output()
            .expect("spawn accept-receipt.sh");
        (
            out.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&out.stdout).into_owned(),
            String::from_utf8_lossy(&out.stderr).into_owned(),
        )
    };

    // Missing args must refuse with exit 2 before doing anything.
    let usage = Command::new("bash")
        .arg(&script)
        .output()
        .expect("spawn accept-receipt.sh no-args");
    assert_eq!(usage.status.code(), Some(2), "no-args must exit 2");

    let (c1, out1, err1) = run_receipt(r#"{"receipt":"first","n":1}"#);
    assert_eq!(c1, 0, "first append failed: {out1} {err1}");
    assert!(
        out1.contains(r#""prev_hash":"genesis""#),
        "first line must chain to genesis: {out1}"
    );

    let (c2, out2, err2) = run_receipt(r#"{"receipt":"second","n":2}"#);
    assert_eq!(c2, 0, "second append failed: {out2} {err2}");

    let content = std::fs::read_to_string(&ledger).expect("read ledger");
    let lines: Vec<&str> = content.lines().collect();
    assert_eq!(lines.len(), 2, "ledger must have exactly 2 lines: {content}");

    // Expected prev_hash = sha256 of the first line exactly as the script
    // hashes it (tail -n 1 output, i.e. line + trailing newline).
    let hash_out = Command::new("bash")
        .arg("-c")
        .arg(format!(
            "head -n 1 '{ledger_s}' | shasum -a 256 | cut -d ' ' -f 1"
        ))
        .output()
        .expect("spawn shasum");
    assert!(hash_out.status.success(), "shasum failed");
    let expected = String::from_utf8_lossy(&hash_out.stdout).trim().to_owned();
    assert!(!expected.is_empty(), "empty sha256");

    assert!(
        lines[1].contains(&format!(r#""prev_hash":"{expected}""#)),
        "second line prev_hash must equal sha256 of first line ({expected}): {}",
        lines[1]
    );
    assert_eq!(out2.trim(), lines[1], "script must print the appended line");
}
