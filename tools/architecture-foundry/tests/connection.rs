use serde_json::{json, Value};
use sha2::{Digest, Sha256};
use std::fs;
use std::process::Command;
use tempfile::tempdir;

fn sha256(data: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(data))
}

fn write_inputs(root: &std::path::Path) -> (std::path::PathBuf, std::path::PathBuf, Vec<u8>) {
    let input = root.join("catalog.json");
    let program = root.join("program.json");
    let catalog = json!({
        "architecture":{"capabilities":["global-cloud"],"constraints":["ZERO_UNRECEIPTED_ACTUATION"],"graph":null},
        "artifacts":[],
        "authority":{"ceiling":"CONSTRUCT_ONLY","do_authority":false},
        "connection_id":"urn:test:connection",
        "evidence":[],
        "labels":{},
        "next":[{"consumer":"seanchatmangpt/ggen","operation":"manufacture"}],
        "packs":[{"admission":"CANDIDATE","digest":"sha256:".to_owned()+&"1".repeat(64),"name":"candidate","version":null}],
        "parent":{"digest":"sha256:".to_owned()+&"2".repeat(64),"producer":"seanchatmangpt/ggen-create@".to_owned()+&"b".repeat(40)},
        "producer":{"component":"enterprise-architecture-connection-pack","repository":"seanchatmangpt/ggen-marketplace","revision":"a".repeat(40)},
        "schema":"urn:ggen:enterprise-connection:v1",
        "stage":"CATALOG",
        "standing":{"claim":"contract admitted","state":"PARTIAL_ALIVE"},
        "subject":{"id":"subject","kind":"generalized-enterprise-architecture-factory","revision":"sha256:".to_owned()+&"0".repeat(64)}
    });
    let input_bytes = serde_json::to_vec(&catalog).unwrap();
    fs::write(&input, &input_bytes).unwrap();
    let p = json!({
        "schema_version":"ggen.enterprise-architecture-foundry.work-program/1",
        "manufacturing_kernel":"kernel-test",
        "initial_solution_packs":["global-cloud-foundation","identity-policy"]
    });
    fs::write(&program, serde_json::to_vec(&p).unwrap()).unwrap();
    (input, program, input_bytes)
}

#[test]
fn catalog_to_manufacture_plan_is_deterministic_and_powerless() {
    let tmp = tempdir().unwrap();
    let (input, program, input_bytes) = write_inputs(tmp.path());
    let out_a = tmp.path().join("a.json");
    let out_b = tmp.path().join("b.json");
    let revision = "c".repeat(40);
    for out in [&out_a, &out_b] {
        let status = Command::new(env!("CARGO_BIN_EXE_ggen-foundry-connection"))
            .args([
                "--input",
                input.to_str().unwrap(),
                "--program",
                program.to_str().unwrap(),
                "--revision",
                revision.as_str(),
                "--out",
                out.to_str().unwrap(),
            ])
            .status()
            .unwrap();
        assert!(status.success());
    }
    assert_eq!(fs::read(&out_a).unwrap(), fs::read(&out_b).unwrap());
    let output: Value = serde_json::from_slice(&fs::read(&out_a).unwrap()).unwrap();
    assert_eq!(output["stage"], "MANUFACTURE");
    assert_eq!(output["authority"]["do_authority"], false);
    assert_eq!(output["parent"]["digest"], sha256(&input_bytes));
    assert_eq!(output["standing"]["state"], "PARTIAL_ALIVE");
    assert_eq!(output["labels"]["native_ggen_sync"], "NOT_EXECUTED");
    assert!(output["packs"]
        .as_array()
        .unwrap()
        .iter()
        .any(|p| p["name"] == "global-cloud-foundation"));
}

#[cfg(unix)]
#[test]
fn native_manufacture_requires_sync_receipt_verify_and_concrete_output() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = tempdir().unwrap();
    let (input, program, input_bytes) = write_inputs(tmp.path());
    let project = tmp.path().join("fortune5-global-cloud-factory");
    fs::create_dir(&project).unwrap();
    fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"fortune5-global-cloud-factory\"\n",
    )
    .unwrap();

    let fake = tmp.path().join("ggen");
    fs::write(
        &fake,
        r#"#!/bin/sh
set -eu
if [ "$1 $2" = "sync run" ]; then
  mkdir -p .ggen-v2
  printf '%s' '{"schema":"fake-native-ggen-receipt","valid":true}' > .ggen-v2/receipt.json
  printf 'tenant=Global\nregion=West\n' > architecture.txt
  exit 0
fi
if [ "$1 $2" = "receipt verify" ]; then
  test -f .ggen-v2/receipt.json
  test -f architecture.txt
  printf '%s\n' '{"valid":true}'
  exit 0
fi
exit 9
"#,
    )
    .unwrap();
    let mut permissions = fs::metadata(&fake).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&fake, permissions).unwrap();

    let out = tmp.path().join("manufacture.json");
    let revision = "d".repeat(40);
    let status = Command::new(env!("CARGO_BIN_EXE_ggen-foundry-connection"))
        .args([
            "--input",
            input.to_str().unwrap(),
            "--program",
            program.to_str().unwrap(),
            "--revision",
            revision.as_str(),
            "--out",
            out.to_str().unwrap(),
            "--ggen-bin",
            fake.to_str().unwrap(),
            "--project-root",
            project.to_str().unwrap(),
            "--required-output",
            "architecture.txt",
        ])
        .status()
        .unwrap();
    assert!(status.success());

    let output: Value = serde_json::from_slice(&fs::read(&out).unwrap()).unwrap();
    assert_eq!(output["stage"], "MANUFACTURE");
    assert_eq!(output["authority"]["do_authority"], false);
    assert_eq!(output["parent"]["digest"], sha256(&input_bytes));
    assert_eq!(output["standing"]["state"], "ALIVE");
    assert_eq!(output["labels"]["native_ggen_sync"], "EXECUTED");
    assert_eq!(
        output["subject"]["kind"],
        "enterprise-architecture-manufactured-artifact-set"
    );
    let roles = output["artifacts"]
        .as_array()
        .unwrap()
        .iter()
        .map(|item| item["role"].as_str().unwrap())
        .collect::<Vec<_>>();
    assert!(roles.contains(&"ggen:native-sync-receipt"));
    assert!(roles.contains(&"ggen:manufactured-output"));
    assert!(output["evidence"]
        .as_array()
        .unwrap()
        .iter()
        .any(|item| item["kind"] == "ggen-native-sync-receipt"));
}
