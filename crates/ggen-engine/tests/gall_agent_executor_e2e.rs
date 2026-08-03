//! Real isolated-worktree tests for the generated Gall coding-agent executor.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir destination");
    for entry in std::fs::read_dir(src).expect("read source directory") {
        let entry = entry.expect("directory entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy file");
        }
    }
}

fn git(project: &Path, args: &[&str]) {
    let output = Command::new("git")
        .current_dir(project)
        .args(args)
        .output()
        .expect("spawn git");
    assert!(
        output.status.success(),
        "git {:?} failed\nstdout={}\nstderr={}",
        args,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn run_agent(project: &Path, agent_command: &str) -> Output {
    Command::new("bash")
        .current_dir(project)
        .args([
            "scripts/gall/gall",
            "work",
            "dispatch",
            "AGENT-WORK-001",
            "--apply",
        ])
        .env("GALL_AGENT_COMMAND", agent_command)
        .output()
        .expect("run generated agent executor")
}

fn ontology(verification: &str, evidence: &str) -> String {
    format!(
        r#"@prefix gall: <http://seanchatmangpt.github.io/packs/gall-core#> .
@prefix ex:   <https://example.org/gall-agent/> .

ex:program a gall:GallProgram ;
    gall:programId "AGENT-EXECUTOR" ;
    gall:releaseIdentity "v26.7.30-test" ;
    gall:jiraProjectKey "AGENT" ;
    gall:hasCheckpoint ex:checkpoint ;
    gall:hasWorkItem ex:work-item ;
    gall:hasAutomationProfile ex:profile .

ex:profile a gall:AutomationProfile ;
    gall:automationProfileId "AGENT-EXECUTOR-PROFILE" ;
    gall:trackerProvider gall:FileTracker ;
    gall:executionMode gall:ApplyAllowed ;
    gall:agentMode gall:CommandAgent ;
    gall:maxParallelism 1 ;
    gall:branchPattern "agent/{{workItemId}}" ;
    gall:runtimeDirectory ".gall" ;
    gall:receiptDirectory "receipts/gall" .

ex:capability a gall:Capability ;
    gall:capabilityId "isolated-agent" ;
    gall:title "Isolated coding-agent execution" .

ex:checkpoint a gall:Checkpoint, gall:RequiredCheckpoint ;
    gall:checkpointId "AGENT-CHECKPOINT" ;
    gall:title "Agent executor floor" ;
    gall:producesCapability ex:capability ;
    gall:runnerCommand "true" ;
    gall:positiveWitness ex:witness ;
    gall:negativeFalsifier ex:falsifier ;
    gall:receiptObligation ex:receipt ;
    gall:replayObligation ex:replay ;
    gall:hasWorkItem ex:work-item .

ex:witness a gall:PositiveWitness ; gall:name "positive" ; gall:command "true" .
ex:falsifier a gall:NegativeFalsifier ; gall:name "negative" ; gall:command "true" .
ex:receipt a gall:ReceiptObligation ; gall:name "receipt" ; gall:command "true" .
ex:replay a gall:ReplayObligation ; gall:name "replay" ; gall:command "true" .

ex:work-item a gall:WorkItem ;
    gall:workItemId "AGENT-WORK-001" ;
    gall:issueType gall:Task ;
    gall:summary "Execute an isolated coding agent" ;
    gall:objective "Prove that command-agent changes are confined to admitted paths" ;
    gall:rationale "Instructions alone cannot enforce write authority" ;
    gall:belongsToProgram ex:program ;
    gall:belongsToCheckpoint ex:checkpoint ;
    gall:implementationOrder 10 ;
    gall:priority gall:Highest ;
    gall:component "agent-executor" ;
    gall:label "gall" ;
    gall:assigneeRole "Coding agent" ;
    gall:reviewerRole "Boundary reviewer" ;
    gall:approvalGate "Changed paths and verification are green" ;
    gall:protocolState gall:Draft ;
    gall:requiredContext "ontology.ttl" ;
    gall:allowedPath "out/" ;
    gall:forbiddenPath "vendor/" ;
    gall:mustDo "Execute inside an isolated Git worktree" ;
    gall:mustNotDo "Do not change files outside out" ;
    gall:outOfScope "Merging or pushing the agent branch" ;
    gall:acceptanceCriterion "Only admitted paths change" ;
    gall:definitionOfDone "Agent result receipt records green path and verifier evidence" ;
    gall:verificationCommand "{verification}" ;
    gall:evidenceArtifact "{evidence}" ;
    gall:adversarialQuestion "Would a forbidden write be detected even when the agent exits zero" .
"#
    )
}

fn scaffold(agent_script: &str, verification: &str, evidence: &str) -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("gall-core-pack"),
        &dir.path().join("packs/gall-core-pack"),
    );
    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(
        project.join("ontology.ttl"),
        ontology(verification, evidence),
    )
    .expect("write ontology");
    std::fs::write(project.join("fake-agent.sh"), agent_script).expect("write fake agent");
    std::fs::write(project.join(".gitignore"), ".gall/\nreceipts/gall/\n")
        .expect("write gitignore");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"gall-agent-executor\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\ngall-core-pack = { path = \"../packs/gall-core-pack\" }\n\n\
         [templates]\ndir = \"templates\"\n\n\
         [law]\nreflexive = true\n",
    )
    .expect("write manifest");

    git(&project, &["init"]);
    git(&project, &["config", "user.name", "Gall Agent E2E"]);
    git(
        &project,
        &["config", "user.email", "gall-agent-e2e@example.invalid"],
    );
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("generate Gall agent executor");
    git(&project, &["add", "."]);
    git(&project, &["commit", "-m", "seal generated agent executor"]);
    (dir, project)
}

#[test]
fn authorized_agent_change_passes_in_isolated_worktree() {
    let (_dir, project) = scaffold(
        "#!/usr/bin/env bash\nset -euo pipefail\nmkdir -p out\nprintf agent > out/agent.txt\n",
        "test -f out/agent.txt && grep -qx agent out/agent.txt",
        "out/agent.txt",
    );
    let output = run_agent(&project, "bash fake-agent.sh");
    assert!(
        output.status.success(),
        "stdout={}\nstderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let worktree = project.join(".gall/worktrees/AGENT-WORK-001");
    assert_eq!(
        std::fs::read_to_string(worktree.join("out/agent.txt")).expect("agent output"),
        "agent"
    );
    let receipt = std::fs::read_to_string(
        project.join("receipts/gall/work-items/AGENT-WORK-001/agent-execution-result.json"),
    )
    .expect("agent result receipt");
    assert!(receipt.contains("\"passed\": true"), "{receipt}");
    assert!(receipt.contains("out/agent.txt"), "{receipt}");
    assert!(!project.join("out/agent.txt").exists());
}

#[test]
fn forbidden_agent_change_is_refused_even_when_agent_exits_zero() {
    let (_dir, project) = scaffold(
        "#!/usr/bin/env bash\nset -euo pipefail\nmkdir -p vendor\nprintf forbidden > vendor/bad.txt\n",
        "true",
        "ontology.ttl",
    );
    let output = run_agent(&project, "bash fake-agent.sh");
    assert!(
        !output.status.success(),
        "forbidden agent change must fail\nstdout={}\nstderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let receipt = std::fs::read_to_string(
        project.join("receipts/gall/work-items/AGENT-WORK-001/agent-execution-result.json"),
    )
    .expect("agent result receipt");
    assert!(receipt.contains("forbidden:vendor/bad.txt"), "{receipt}");
    assert!(receipt.contains("\"passed\": false"), "{receipt}");
}
