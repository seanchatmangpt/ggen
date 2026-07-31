//! Adversarial Gall automation-profile gate coverage.
//!
//! Every test mutates one admitted automation law and requires the exact named
//! SPARQL gate to refuse the graph before generation or actuation.

use std::path::{Path, PathBuf};

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

fn valid_ontology() -> String {
    r#"@prefix gall: <http://seanchatmangpt.github.io/packs/gall-core#> .
@prefix ex:   <https://example.org/gall-automation/> .

ex:program a gall:GallProgram ;
    gall:programId "AUTOMATION-GATES" ;
    gall:releaseIdentity "v26.7.30-test" ;
    gall:jiraProjectKey "AUTO" ;
    gall:hasCheckpoint ex:checkpoint ;
    gall:hasWorkItem ex:work-item ;
    gall:hasAutomationProfile ex:automation-profile .

ex:automation-profile a gall:AutomationProfile ;
    gall:automationProfileId "AUTO-PROFILE" ;
    gall:trackerProvider gall:FileTracker ;
    gall:executionMode gall:PlanOnly ;
    gall:agentMode gall:HandoffOnly ;
    gall:maxParallelism 4 ;
    gall:branchPattern "agent/{workItemId}" ;
    gall:runtimeDirectory ".gall" ;
    gall:receiptDirectory "receipts/gall" .

ex:capability a gall:Capability ;
    gall:capabilityId "automation-floor" ;
    gall:title "Automation floor" .

ex:checkpoint a gall:Checkpoint, gall:RequiredCheckpoint ;
    gall:checkpointId "AUTO-CHECKPOINT" ;
    gall:title "Automation checkpoint" ;
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
    gall:workItemId "AUTO-WORK-001" ;
    gall:issueType gall:Task ;
    gall:summary "Exercise the automation floor" ;
    gall:objective "Generate a complete automation control plane" ;
    gall:rationale "Automation law requires a real consumer" ;
    gall:belongsToProgram ex:program ;
    gall:belongsToCheckpoint ex:checkpoint ;
    gall:implementationOrder 10 ;
    gall:priority gall:High ;
    gall:component "automation" ;
    gall:label "gall" ;
    gall:assigneeRole "Automation agent" ;
    gall:reviewerRole "Automation reviewer" ;
    gall:approvalGate "Automation gates pass" ;
    gall:protocolState gall:Draft ;
    gall:requiredContext "ontology.ttl" ;
    gall:allowedPath "out/" ;
    gall:forbiddenPath "vendor/" ;
    gall:mustDo "Generate the automation control plane" ;
    gall:mustNotDo "Do not bypass automation policy" ;
    gall:outOfScope "External network mutation" ;
    gall:acceptanceCriterion "The generated automation manifest is nonempty" ;
    gall:definitionOfDone "All automation gates and projections pass" ;
    gall:verificationCommand "test -f ontology.ttl" ;
    gall:evidenceArtifact "ontology.ttl" ;
    gall:adversarialQuestion "Would invalid automation policy be refused" .
"#
    .to_string()
}

fn scaffold(ontology: &str) -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("gall-core-pack"),
        &dir.path().join("packs/gall-core-pack"),
    );
    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), ontology).expect("write ontology");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"gall-automation-gates\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\ngall-core-pack = { path = \"../packs/gall-core-pack\" }\n\n\
         [templates]\ndir = \"templates\"\n\n\
         [law]\nreflexive = true\n",
    )
    .expect("write manifest");
    (dir, project)
}

fn assert_refused(ontology: String, gate: &str) {
    let (_dir, project) = scaffold(&ontology);
    let error = sync_project(&project).expect_err("mutated automation law must refuse");
    assert!(error.contains(gate), "expected gate {gate}, got: {error}");
}

#[test]
fn valid_automation_profile_generates_control_plane() {
    let (_dir, project) = scaffold(&valid_ontology());
    sync_project(&project).expect("valid automation profile");
    for path in [
        "automation/GALL_AUTOMATION_WORK_ITEMS.csv",
        "docs/GALL_AUTOMATION_RUNBOOK.md",
        "scripts/gall/control_plane.py",
        "scripts/gall/tracker_sync.py",
        "scripts/gall/snapshot_work_evidence.py",
        "scripts/gall/verify_automation_receipts.py",
        "scripts/gall/gall",
        ".github/workflows/gall-control-plane.yml",
    ] {
        assert!(project.join(path).is_file(), "missing generated {path}");
    }
}

#[test]
fn exact_automation_profile_type_is_required() {
    let malformed = valid_ontology().replace(
        "ex:automation-profile a gall:AutomationProfile ;",
        "ex:automation-profile a gall:TrackerProvider ;",
    );
    assert_refused(malformed, "190_automation_profile_complete");
}

#[test]
fn multiple_program_profiles_are_refused() {
    let addition = r#"
ex:automation-profile-two a gall:AutomationProfile ;
    gall:automationProfileId "AUTO-PROFILE-TWO" ;
    gall:trackerProvider gall:FileTracker ;
    gall:executionMode gall:PlanOnly ;
    gall:agentMode gall:HandoffOnly ;
    gall:maxParallelism 2 ;
    gall:branchPattern "agent/{workItemId}" ;
    gall:runtimeDirectory ".gall-two" ;
    gall:receiptDirectory "receipts/gall-two" .
"#;
    let malformed = valid_ontology().replace(
        "    gall:hasAutomationProfile ex:automation-profile .",
        "    gall:hasAutomationProfile ex:automation-profile, ex:automation-profile-two .",
    ) + addition;
    assert_refused(malformed, "192_automation_scalar_cardinality_one");
}

#[test]
fn malformed_parallelism_is_refused() {
    let malformed = valid_ontology().replace(
        "    gall:maxParallelism 4 ;",
        "    gall:maxParallelism \"many\" ;",
    );
    assert_refused(malformed, "194_automation_controlled_values");
}

#[test]
fn unsafe_branch_pattern_is_refused() {
    let malformed = valid_ontology().replace(
        "    gall:branchPattern \"agent/{workItemId}\" ;",
        "    gall:branchPattern \"../{workItemId}\" ;",
    );
    assert_refused(malformed, "194_automation_controlled_values");
}

#[test]
fn escaping_runtime_path_is_refused() {
    let malformed = valid_ontology().replace(
        "    gall:runtimeDirectory \".gall\" ;",
        "    gall:runtimeDirectory \"../escape\" ;",
    );
    assert_refused(malformed, "196_automation_paths_safe");
}

#[test]
fn duplicate_automation_identity_is_refused() {
    let malformed = valid_ontology()
        + r#"
ex:duplicate-profile a gall:AutomationProfile ;
    gall:automationProfileId "AUTO-PROFILE" ;
    gall:trackerProvider gall:FileTracker ;
    gall:executionMode gall:PlanOnly ;
    gall:agentMode gall:HandoffOnly ;
    gall:maxParallelism 1 ;
    gall:branchPattern "duplicate/{workItemId}" ;
    gall:runtimeDirectory ".gall-duplicate" ;
    gall:receiptDirectory "receipts/gall-duplicate" .
"#;
    assert_refused(malformed, "198_automation_identity_unique");
}

#[test]
fn shared_automation_profile_is_refused() {
    let malformed = valid_ontology()
        + r#"
ex:program-two a gall:GallProgram ;
    gall:programId "AUTOMATION-GATES-TWO" ;
    gall:releaseIdentity "v26.7.30-test" ;
    gall:jiraProjectKey "AUTOTWO" ;
    gall:hasCheckpoint ex:checkpoint-two ;
    gall:hasWorkItem ex:work-item-two ;
    gall:hasAutomationProfile ex:automation-profile .

ex:capability-two a gall:Capability ;
    gall:capabilityId "automation-floor-two" ;
    gall:title "Automation floor two" .

ex:checkpoint-two a gall:Checkpoint, gall:RequiredCheckpoint ;
    gall:checkpointId "AUTO-CHECKPOINT-TWO" ;
    gall:title "Automation checkpoint two" ;
    gall:producesCapability ex:capability-two ;
    gall:runnerCommand "true" ;
    gall:positiveWitness ex:witness-two ;
    gall:negativeFalsifier ex:falsifier-two ;
    gall:receiptObligation ex:receipt-two ;
    gall:replayObligation ex:replay-two ;
    gall:hasWorkItem ex:work-item-two .

ex:witness-two a gall:PositiveWitness ; gall:name "positive-two" ; gall:command "true" .
ex:falsifier-two a gall:NegativeFalsifier ; gall:name "negative-two" ; gall:command "true" .
ex:receipt-two a gall:ReceiptObligation ; gall:name "receipt-two" ; gall:command "true" .
ex:replay-two a gall:ReplayObligation ; gall:name "replay-two" ; gall:command "true" .

ex:work-item-two a gall:WorkItem ;
    gall:workItemId "AUTO-WORK-002" ;
    gall:issueType gall:Task ;
    gall:summary "Exercise the second automation floor" ;
    gall:objective "Prove automation ownership" ;
    gall:rationale "One profile cannot own two programs" ;
    gall:belongsToProgram ex:program-two ;
    gall:belongsToCheckpoint ex:checkpoint-two ;
    gall:implementationOrder 20 ;
    gall:priority gall:High ;
    gall:component "automation" ;
    gall:label "gall" ;
    gall:assigneeRole "Automation agent" ;
    gall:reviewerRole "Automation reviewer" ;
    gall:approvalGate "Ownership gate passes" ;
    gall:protocolState gall:Draft ;
    gall:requiredContext "ontology.ttl" ;
    gall:allowedPath "out-two/" ;
    gall:forbiddenPath "vendor/" ;
    gall:mustDo "Generate the second automation control plane" ;
    gall:mustNotDo "Do not share automation authority" ;
    gall:outOfScope "External mutation" ;
    gall:acceptanceCriterion "Ownership is unique" ;
    gall:definitionOfDone "The ownership gate executes" ;
    gall:verificationCommand "test -f ontology.ttl" ;
    gall:evidenceArtifact "ontology.ttl" ;
    gall:adversarialQuestion "Can a profile govern two programs" .
"#;
    assert_refused(malformed, "199_automation_owner_unique");
}
