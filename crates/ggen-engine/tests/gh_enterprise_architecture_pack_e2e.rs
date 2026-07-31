//! Clean-consumer proof for the enterprise GitHub repository architecture.
//! Uses the real filesystem, graph engine, SPARQL gates, Tera rendering, and bash parser.

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
    for pack in ["gh-terraform-pack", "gh-enterprise-architecture-pack"] {
        copy_tree(&packs_dir().join(pack), &dir.path().join(pack));
    }

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"enterprise-consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\ngh-terraform-pack = { path = \"../gh-terraform-pack\" }\n\
         gh-enterprise-architecture-pack = { path = \"../gh-enterprise-architecture-pack\" }\n\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    (dir, project)
}

fn read(project: &Path, relative: &str) -> String {
    std::fs::read_to_string(project.join(relative))
        .unwrap_or_else(|error| panic!("read {relative}: {error}"))
}

#[test]
fn enterprise_repository_factory_is_generated_and_idempotent() {
    let (_dir, project) = scaffold();
    let first = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    assert!(!first.written.is_empty(), "first sync must manufacture artifacts");

    for relative in [
        "infra/terraform/github/repository.tf",
        "infra/terraform/github-enterprise/versions.tf",
        "infra/terraform/github-enterprise/variables.tf",
        "infra/terraform/github-enterprise/repositories.tf",
        "infra/terraform/github-enterprise/outputs.tf",
        "scripts/gh/terraform-corpus-census.sh",
        "docs/gh-enterprise/TERRAFORM-CORPUS.md",
        "docs/gh-enterprise/ARCHITECTURE.md",
    ] {
        assert!(project.join(relative).is_file(), "missing {relative}");
    }

    let repositories = read(
        &project,
        "infra/terraform/github-enterprise/repositories.tf",
    );
    for required in [
        "resource \"github_repository\" \"managed\"",
        "for_each = var.repositories",
        "resource \"github_branch_protection\" \"main\"",
        "resource \"github_issue_label\" \"managed\"",
        "resource \"github_repository_milestone\" \"managed\"",
        "gitignore_template",
        "delete_branch_on_merge",
    ] {
        assert!(repositories.contains(required), "missing {required}: {repositories}");
    }
    assert!(
        !repositories.contains("resource \"github_issue\""),
        "issues must remain line events, not enterprise Terraform desired state"
    );
    assert!(
        !repositories.contains("resource \"github_repository_file\""),
        "repository files must remain under ggen generation authority"
    );

    let corpus = read(&project, "docs/gh-enterprise/TERRAFORM-CORPUS.md");
    for required in [
        "seanchatmangpt/kanban",
        "e8ce923e8e16996204fb9ff0e6052c71494e26a8",
        "2db347a0ff75422fc3a8172fc09e91993a772936",
        "seanchatmangpt/my-terraform-project",
        "EXCLUDED",
        "UNKNOWN",
    ] {
        assert!(corpus.contains(required), "corpus missing {required}: {corpus}");
    }

    let scanner_path = project.join("scripts/gh/terraform-corpus-census.sh");
    let scanner = read(&project, "scripts/gh/terraform-corpus-census.sh");
    for mutating in ["-X POST", "-X PUT", "-X PATCH", "-X DELETE", "--method POST"] {
        assert!(
            !scanner.contains(mutating),
            "read-only scanner contains mutating method {mutating}"
        );
    }
    let parsed = Command::new("bash")
        .arg("-n")
        .arg(&scanner_path)
        .output()
        .expect("bash -n scanner");
    assert!(
        parsed.status.success(),
        "scanner syntax failed: {}",
        String::from_utf8_lossy(&parsed.stderr)
    );

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
        "second sync must be byte-identical: {:?}",
        second.written
    );
}

#[test]
fn admitted_corpus_without_blob_is_refused() {
    let (_dir, project) = scaffold();
    let ontology = project
        .parent()
        .expect("consumer parent")
        .join("gh-enterprise-architecture-pack/ontology.ttl");
    let original = std::fs::read_to_string(&ontology).expect("read pack ontology");
    let damaged = original.replace(
        "    ghea:sourceBlob \"2db347a0ff75422fc3a8172fc09e91993a772936\" ;\n",
        "",
    );
    assert_ne!(damaged, original, "negative control must remove the source blob");
    std::fs::write(&ontology, damaged).expect("write damaged ontology");

    let error = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("admitted source without immutable blob provenance must be refused");
    let message = error.to_string();
    assert!(
        message.contains("corpus observations must be complete")
            || message.contains("sourceBlob")
            || message.contains("gate"),
        "refusal must identify the admission gate: {message}"
    );
}
