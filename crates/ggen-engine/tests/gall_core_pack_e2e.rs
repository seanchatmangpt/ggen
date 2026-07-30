//! Chicago-TDD end-to-end proof for `gall-core-pack`.
//!
//! Real boundaries throughout: real filesystem, real pack loading, real
//! Oxigraph/SPARQL gates, real Tera generation, real git commits/worktrees,
//! real bash and Python commands, real ggen receipt verification, admitted
//! evidence, APS-grade Jira and coding-agent projections, generated automation,
//! tracker actuation, chained receipts, and named sabotage refusals.

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

fn run(project: &Path, program: &str, args: &[&str]) -> Output {
    Command::new(program)
        .current_dir(project)
        .args(args)
        .output()
        .unwrap_or_else(|error| panic!("spawn {program} {args:?}: {error}"))
}

fn run_ok(project: &Path, program: &str, args: &[&str]) -> Output {
    let output = run(project, program, args);
    assert!(
        output.status.success(),
        "{program} {args:?} failed\nstdout={}\nstderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

fn commit_all(project: &Path, message: &str) {
    git(project, &["add", "."]);
    git(project, &["commit", "-m", message]);
}

fn sync_project(project: &Path) -> Result<ggen_engine::sync::SyncReport, String> {
    sync(
        project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())
}

fn write_manifest(project: &Path, include_evidence: bool) {
    let evidence = if include_evidence {
        "gall-evidence = { path = \"evidence/gall\", lock = false }\n"
    } else {
        ""
    };
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"gall-core-e2e\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n\
             gall-core-pack = {{ path = \"../packs/gall-core-pack\" }}\n\
             {evidence}\n\
             [templates]\ndir = \"templates\"\n\n\
             [law]\nreflexive = true\n"
        ),
    )
    .expect("write ggen.toml");
}

fn planning_ontology() -> String {
    r#"@prefix gall: <http://seanchatmangpt.github.io/packs/gall-core#> .
@prefix ex:   <https://example.org/gall/> .

ex:program a gall:GallProgram ;
    gall:programId "GALL-CORE-E2E" ;
    gall:releaseIdentity "v26.7.30-test" ;
    gall:jiraProjectKey "GALL" ;
    gall:hasCheckpoint ex:checkpoint-000 ;
    gall:hasWorkItem ex:work-item-001, ex:work-item-002 ;
    gall:hasAutomationProfile ex:automation-profile .

ex:automation-profile a gall:AutomationProfile ;
    gall:automationProfileId "GALL-AUTOMATION-E2E" ;
    gall:trackerProvider gall:FileTracker ;
    gall:executionMode gall:ApplyAllowed ;
    gall:agentMode gall:HandoffOnly ;
    gall:maxParallelism 2 ;
    gall:branchPattern "agent/{workItemId}" ;
    gall:runtimeDirectory ".gall" ;
    gall:receiptDirectory "receipts/gall" .

ex:capability a gall:Capability ;
    gall:capabilityId "real-useful-file" ;
    gall:title "Real useful file crosses the filesystem boundary" .

ex:checkpoint-000 a gall:Checkpoint, gall:RequiredCheckpoint ;
    gall:checkpointId "GALL-CORE-000" ;
    gall:title "Executable Gall floor" ;
    gall:producesCapability ex:capability ;
    gall:runnerCommand "mkdir -p out && printf alive > out/useful.txt" ;
    gall:positiveWitness ex:witness ;
    gall:negativeFalsifier ex:falsifier ;
    gall:receiptObligation ex:receipt ;
    gall:replayObligation ex:replay ;
    gall:ownsArtifact ex:useful-artifact ;
    gall:hasWorkItem ex:work-item-001, ex:work-item-002 .

ex:useful-artifact gall:artifactPath "out/useful.txt" .

ex:witness a gall:PositiveWitness ;
    gall:name "useful-file-contains-alive" ;
    gall:command "grep -qx alive out/useful.txt" .

ex:falsifier a gall:NegativeFalsifier ;
    gall:name "forbidden-file-remains-absent" ;
    gall:command "test ! -e out/forbidden.txt" .

ex:receipt a gall:ReceiptObligation ;
    gall:name "ggen-receipt-verifies" ;
    gall:command "ggen receipt verify" .

ex:replay a gall:ReplayObligation ;
    gall:name "roadmap-replays-from-sealed-revision" ;
    gall:command "test -f docs/GALL_CHECKPOINT_ROADMAP.md" .

ex:work-item-001 a gall:WorkItem ;
    gall:workItemId "GALL-CORE-001" ;
    gall:issueType gall:Task ;
    gall:summary "Manufacture the useful checkpoint artifact" ;
    gall:objective "Create the real useful file through the checkpoint runner" ;
    gall:rationale "The executable floor must cross a real filesystem boundary before later proof surfaces can rely on it" ;
    gall:belongsToProgram ex:program ;
    gall:belongsToCheckpoint ex:checkpoint-000 ;
    gall:implementationOrder 10 ;
    gall:priority gall:Highest ;
    gall:component "gall-runtime" ;
    gall:label "gall" ;
    gall:label "v26.7.30" ;
    gall:assigneeRole "Rust implementation agent" ;
    gall:reviewerRole "Gall evidence reviewer" ;
    gall:approvalGate "Runner witness and receipt are green" ;
    gall:protocolState gall:Draft ;
    gall:requiredContext "docs/GALL_CHECKPOINT_ROADMAP.md" ;
    gall:allowedPath "out/" ;
    gall:forbiddenPath "crates/" ;
    gall:mustDo "Execute the ontology-declared runner command without replacing the real filesystem boundary" ;
    gall:mustNotDo "Do not hardcode a successful receipt or bypass the checkpoint runner" ;
    gall:outOfScope "Changing unrelated workspace crates" ;
    gall:acceptanceCriterion "The runner creates out/useful.txt containing exactly alive" ;
    gall:definitionOfDone "The positive witness negative falsifier receipt verifier and clean replay all pass" ;
    gall:verificationCommand "grep -qx alive out/useful.txt" ;
    gall:evidenceArtifact "out/useful.txt" ;
    gall:adversarialQuestion "Would this ticket still pass if the runner did not create the file" .

ex:work-item-002 a gall:WorkItem ;
    gall:workItemId "GALL-CORE-002" ;
    gall:issueType gall:Task ;
    gall:summary "Verify the generated Gall work package" ;
    gall:objective "Prove that Jira tickets agent work orders dependency graphs and automation are generated from the admitted work graph" ;
    gall:rationale "A coding agent requires one consistent machine-derived instruction and execution surface rather than manually synchronized ticket prose" ;
    gall:belongsToProgram ex:program ;
    gall:belongsToCheckpoint ex:checkpoint-000 ;
    gall:dependsOnWorkItem ex:work-item-001 ;
    gall:implementationOrder 20 ;
    gall:priority gall:High ;
    gall:component "gall-planning" ;
    gall:label "jira" ;
    gall:label "coding-agent" ;
    gall:assigneeRole "Verification agent" ;
    gall:reviewerRole "Adversarial agile reviewer" ;
    gall:approvalGate "Generated ticket and automation surfaces are complete and mutually consistent" ;
    gall:protocolState gall:Draft ;
    gall:requiredContext "docs/GALL_AGENT_WORK_ORDERS.md" ;
    gall:allowedPath "docs/" ;
    gall:allowedPath "jira/" ;
    gall:allowedPath "automation/" ;
    gall:forbiddenPath "src/" ;
    gall:mustDo "Inspect every generated ticket and automation surface and preserve the declared proof order" ;
    gall:mustNotDo "Do not rewrite generated work artifacts by hand or omit prohibited scope" ;
    gall:outOfScope "Using live network side effects in the local lifecycle test" ;
    gall:acceptanceCriterion "The Jira CSV automation manifest and agent work orders contain both work item identities" ;
    gall:definitionOfDone "The work-item dependency graph and automation receipts prove the order from GALL-CORE-001 to GALL-CORE-002" ;
    gall:verificationCommand "test -s automation/GALL_AUTOMATION_WORK_ITEMS.csv" ;
    gall:evidenceArtifact "docs/GALL_AGENT_WORK_ORDERS.md" ;
    gall:adversarialQuestion "Can an agent complete this ticket while violating a declared MUST NOT rule" .
"#
    .to_string()
}

fn activate_crown(project: &Path) {
    let mut ontology =
        std::fs::read_to_string(project.join("ontology.ttl")).expect("read planning ontology");
    ontology.push_str(
        r#"
ex:program gall:hasCrown ex:crown .

ex:crown a gall:Crown ;
    gall:crownId "GALL-CORE-CROWN" ;
    gall:includesCheckpoint ex:checkpoint-000 .
"#,
    );
    std::fs::write(project.join("ontology.ttl"), ontology).expect("activate crown");
}

fn run_generated_evidence(project: &Path) {
    let ggen_bin = assert_cmd::cargo::cargo_bin("ggen");
    let mut paths = vec![ggen_bin
        .parent()
        .expect("ggen binary parent")
        .to_path_buf()];
    if let Some(existing) = std::env::var_os("PATH") {
        paths.extend(std::env::split_paths(&existing));
    }
    let joined_path = std::env::join_paths(paths).expect("join PATH");

    let output = Command::new("bash")
        .current_dir(project)
        .arg("scripts/gall/run-checkpoints.sh")
        .env("PATH", joined_path)
        .output()
        .expect("run generated Gall evidence emitter");
    assert!(
        output.status.success(),
        "generated runner failed\nstdout={}\nstderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let evidence = std::fs::read_to_string(project.join("evidence/gall/ontology.ttl"))
        .expect("generated Gall evidence");
    assert!(evidence.contains("gall:runnerExitCode 0"), "{evidence}");
    assert!(evidence.contains("gall:witnessExitCode 0"), "{evidence}");
    assert!(evidence.contains("gall:falsifierExitCode 0"), "{evidence}");
    assert!(evidence.contains("gall:receiptExitCode 0"), "{evidence}");
    assert!(evidence.contains("gall:replayExitCode 0"), "{evidence}");
    assert!(
        evidence.contains("gall:independentReplay true"),
        "{evidence}"
    );
    assert!(!evidence.contains("UNVERSIONED"), "{evidence}");
}

fn scaffold() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("gall-core-pack"),
        &dir.path().join("packs/gall-core-pack"),
    );

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), planning_ontology()).expect("write ontology");
    std::fs::write(project.join(".gitignore"), ".gall/\nreceipts/gall/\n")
        .expect("write gitignore");
    write_manifest(&project, false);

    git(&project, &["init"]);
    git(&project, &["config", "user.name", "Gall Core E2E"]);
    git(
        &project,
        &["config", "user.email", "gall-core-e2e@example.invalid"],
    );
    commit_all(&project, "initial Gall program");

    (dir, project)
}

#[test]
fn gall_core_planning_automation_evidence_crown_and_sabotage_are_real() {
    let (_dir, project) = scaffold();

    sync_project(&project).expect("planning sync");
    for path in [
        "docs/GALL_CHECKPOINT_ROADMAP.md",
        "docs/GALL_CHECKPOINT_DAG.dot",
        "docs/GALL_STATUS_LEDGER.md",
        "docs/GALL_CROWN_REPORT.md",
        "docs/GALL_AGENT_WORK_ORDERS.md",
        "docs/GALL_JIRA_TICKET_CATALOG.md",
        "docs/GALL_WORK_ITEM_DAG.dot",
        "jira/GALL_JIRA_WORK_ITEMS.csv",
        "automation/GALL_AUTOMATION_WORK_ITEMS.csv",
        "automation/schemas/gall-automation-receipt.schema.json",
        "scripts/gall/control_plane.py",
        "scripts/gall/tracker_sync.py",
        "scripts/gall/verify_automation_receipts.py",
        "scripts/gall/gall",
        "scripts/gall/run-checkpoints.sh",
        ".github/workflows/gall-control-plane.yml",
    ] {
        assert!(project.join(path).is_file(), "missing generated {path}");
    }

    let planning_ledger =
        std::fs::read_to_string(project.join("docs/GALL_STATUS_LEDGER.md"))
            .expect("planning ledger");
    assert!(planning_ledger.contains("UNKNOWN"), "{planning_ledger}");

    let work_orders = std::fs::read_to_string(project.join("docs/GALL_AGENT_WORK_ORDERS.md"))
        .expect("agent work orders");
    assert!(work_orders.contains("GALL-CORE-001"), "{work_orders}");
    assert!(work_orders.contains("GALL-CORE-002"), "{work_orders}");
    assert!(work_orders.contains("### MUST NOT"), "{work_orders}");
    assert!(
        work_orders.contains("### Agent stop conditions"),
        "{work_orders}"
    );

    let jira = std::fs::read_to_string(project.join("jira/GALL_JIRA_WORK_ITEMS.csv"))
        .expect("Jira CSV");
    assert!(jira.contains("Project Key,Issue Type,Summary"), "{jira}");
    assert!(jira.contains("GALL-CORE-001"), "{jira}");
    assert!(jira.contains("GALL-CORE-002"), "{jira}");

    let automation =
        std::fs::read_to_string(project.join("automation/GALL_AUTOMATION_WORK_ITEMS.csv"))
            .expect("automation CSV");
    assert!(automation.contains("Automation Profile"), "{automation}");
    assert!(automation.contains("file-only"), "{automation}");
    assert!(automation.contains("GALL-CORE-002"), "{automation}");

    let work_item_dag = std::fs::read_to_string(project.join("docs/GALL_WORK_ITEM_DAG.dot"))
        .expect("work item DAG");
    assert!(
        work_item_dag.contains("\"GALL-CORE-001\" -> \"GALL-CORE-002\""),
        "{work_item_dag}"
    );

    commit_all(&project, "seal generated Gall planning artifacts");

    run_ok(
        &project,
        "bash",
        &["scripts/gall/gall", "automation", "validate"],
    );
    let next = run_ok(
        &project,
        "bash",
        &["scripts/gall/gall", "work", "next"],
    );
    assert_eq!(
        String::from_utf8_lossy(&next.stdout).trim(),
        "GALL-CORE-001"
    );
    run_ok(
        &project,
        "bash",
        &[
            "scripts/gall/gall",
            "work",
            "dispatch",
            "GALL-CORE-001",
        ],
    );
    assert!(
        project
            .join(".gall/dispatch/GALL-CORE-001/WORK_ORDER.md")
            .is_file()
    );
    run_ok(
        &project,
        "bash",
        &["scripts/gall/gall", "tracker", "apply"],
    );
    assert!(
        project
            .join(".gall/file-tracker/GALL-CORE-001.md")
            .is_file()
    );
    run_ok(
        &project,
        "bash",
        &["scripts/gall/gall", "receipt", "verify"],
    );

    run_generated_evidence(&project);
    run_ok(
        &project,
        "bash",
        &[
            "scripts/gall/gall",
            "work",
            "verify",
            "GALL-CORE-001",
        ],
    );
    run_ok(
        &project,
        "bash",
        &[
            "scripts/gall/gall",
            "work",
            "complete",
            "GALL-CORE-001",
        ],
    );
    let next = run_ok(
        &project,
        "bash",
        &["scripts/gall/gall", "work", "next"],
    );
    assert_eq!(
        String::from_utf8_lossy(&next.stdout).trim(),
        "GALL-CORE-002"
    );
    run_ok(
        &project,
        "bash",
        &[
            "scripts/gall/gall",
            "work",
            "verify",
            "GALL-CORE-002",
        ],
    );
    run_ok(
        &project,
        "bash",
        &[
            "scripts/gall/gall",
            "work",
            "complete",
            "GALL-CORE-002",
        ],
    );

    write_manifest(&project, true);
    activate_crown(&project);
    commit_all(&project, "activate Gall crown");
    sync_project(&project).expect("first crown sync");

    commit_all(&project, "seal Gall crown outputs");
    run_generated_evidence(&project);
    sync_project(&project).expect("final crown sync");

    let crown_ledger = std::fs::read_to_string(project.join("docs/GALL_STATUS_LEDGER.md"))
        .expect("crown ledger");
    assert!(crown_ledger.contains("**ALIVE**"), "{crown_ledger}");
    let crown_report = std::fs::read_to_string(project.join("docs/GALL_CROWN_REPORT.md"))
        .expect("crown report");
    assert!(crown_report.contains("GALL-CORE-CROWN"), "{crown_report}");
    assert!(crown_report.contains("**ALIVE**"), "{crown_report}");

    run_ok(
        &project,
        "bash",
        &["scripts/gall/gall", "crown"],
    );

    let evidence_path = project.join("evidence/gall/ontology.ttl");
    let evidence = std::fs::read_to_string(&evidence_path).expect("read evidence");
    let sabotaged = evidence.replacen("gall:falsifierExitCode 0", "gall:falsifierExitCode 1", 1);
    assert_ne!(sabotaged, evidence, "sabotage must alter real evidence");
    std::fs::write(&evidence_path, sabotaged).expect("write sabotaged evidence");

    let error = sync_project(&project).expect_err("red falsifier evidence must refuse");
    assert!(
        error.contains("100_crown_evidence_green"),
        "refusal must name the evidence gate: {error}"
    );
}

#[test]
fn missing_agent_prohibition_is_refused() {
    let (_dir, project) = scaffold();
    let ontology_path = project.join("ontology.ttl");
    let ontology = std::fs::read_to_string(&ontology_path).expect("read ontology");
    let malformed = ontology.replace(
        "    gall:mustNotDo \"Do not hardcode a successful receipt or bypass the checkpoint runner\" ;\n",
        "",
    );
    assert_ne!(malformed, ontology, "malformation must remove a real rule");
    std::fs::write(&ontology_path, malformed).expect("write malformed ontology");

    let error = sync_project(&project).expect_err("missing MUST NOT rule must refuse");
    assert!(
        error.contains("150_work_item_contract_complete"),
        "refusal must name the work-item contract gate: {error}"
    );
    assert!(error.contains("mustNotDo"), "{error}");
}

#[test]
fn cyclic_ticket_execution_order_is_refused() {
    let (_dir, project) = scaffold();
    let ontology_path = project.join("ontology.ttl");
    let ontology = std::fs::read_to_string(&ontology_path).expect("read ontology");
    let cyclic = ontology.replace(
        "ex:work-item-001 a gall:WorkItem ;\n",
        "ex:work-item-001 a gall:WorkItem ;\n    gall:dependsOnWorkItem ex:work-item-002 ;\n",
    );
    assert_ne!(cyclic, ontology, "cycle injection must alter ontology");
    std::fs::write(&ontology_path, cyclic).expect("write cyclic ontology");

    let error = sync_project(&project).expect_err("cyclic work items must refuse");
    assert!(
        error.contains("175_work_item_dependency_cycle"),
        "refusal must name the cycle gate: {error}"
    );
}

#[test]
fn missing_automation_profile_is_refused() {
    let (_dir, project) = scaffold();
    let ontology_path = project.join("ontology.ttl");
    let ontology = std::fs::read_to_string(&ontology_path).expect("read ontology");
    let malformed = ontology.replace(
        "    gall:hasWorkItem ex:work-item-001, ex:work-item-002 ;\n    gall:hasAutomationProfile ex:automation-profile .\n",
        "    gall:hasWorkItem ex:work-item-001, ex:work-item-002 .\n",
    );
    assert_ne!(malformed, ontology, "profile removal must alter ontology");
    std::fs::write(&ontology_path, malformed).expect("write malformed ontology");

    let error = sync_project(&project).expect_err("missing automation profile must refuse");
    assert!(
        error.contains("190_automation_profile_complete"),
        "refusal must name automation profile gate: {error}"
    );
}

#[test]
fn automation_receipt_tampering_is_refused() {
    let (_dir, project) = scaffold();
    sync_project(&project).expect("planning sync");
    commit_all(&project, "seal automation planning artifacts");
    run_ok(
        &project,
        "bash",
        &["scripts/gall/gall", "automation", "validate"],
    );

    let chain_path = project.join("receipts/gall/automation-receipt-chain.jsonl");
    let chain = std::fs::read_to_string(&chain_path).expect("read receipt chain");
    let sabotaged = chain.replacen("\"digest\": \"", "\"digest\": \"0", 1);
    assert_ne!(sabotaged, chain, "receipt sabotage must alter bytes");
    std::fs::write(&chain_path, sabotaged).expect("write sabotaged receipt chain");

    let output = run(
        &project,
        "python3",
        &["scripts/gall/verify_automation_receipts.py"],
    );
    assert!(!output.status.success(), "tampered chain must be refused");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("GALL_RECEIPT_ERROR"),
        "stderr={}",
        String::from_utf8_lossy(&output.stderr)
    );
}
