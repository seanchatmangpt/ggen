use serde_json::{json,Value};
use sha2::{Digest,Sha256};
use std::fs;
use std::process::Command;
use tempfile::tempdir;

fn sha256(data:&[u8])->String { format!("sha256:{:x}",Sha256::digest(data)) }

#[test]
fn catalog_to_manufacture_is_deterministic_and_powerless(){
    let tmp=tempdir().unwrap(); let input=tmp.path().join("catalog.json"); let program=tmp.path().join("program.json"); let out_a=tmp.path().join("a.json"); let out_b=tmp.path().join("b.json");
    let catalog=json!({"schema":"urn:ggen:enterprise-connection:v1","connection_id":"urn:test:connection","stage":"CATALOG","producer":{"repository":"seanchatmangpt/ggen-marketplace","revision":"a".repeat(40),"component":"enterprise-architecture-connection-pack"},"subject":{"id":"subject","kind":"generalized-enterprise-architecture-factory","revision":"sha256:".to_owned()+&"0".repeat(64)},"architecture":{"graph":null,"capabilities":["global-cloud"],"constraints":["ZERO_UNRECEIPTED_ACTUATION"]},"packs":[{"name":"candidate","version":null,"digest":"sha256:".to_owned()+&"1".repeat(64),"admission":"CANDIDATE"}],"artifacts":[],"authority":{"ceiling":"CONSTRUCT_ONLY","do_authority":false},"standing":{"state":"PARTIAL_ALIVE","claim":"contract admitted"},"parent":{"digest":"sha256:".to_owned()+&"2".repeat(64),"producer":"seanchatmangpt/ggen-create@".to_owned()+&"b".repeat(40)},"evidence":[],"next":[{"consumer":"seanchatmangpt/ggen","operation":"manufacture"}],"labels":{}});
    let input_bytes=serde_json::to_vec(&catalog).unwrap(); fs::write(&input,&input_bytes).unwrap();
    let p=json!({"schema_version":"ggen.enterprise-architecture-foundry.work-program/1","manufacturing_kernel":"kernel-test","initial_solution_packs":["global-cloud-foundation","identity-policy"]}); fs::write(&program,serde_json::to_vec(&p).unwrap()).unwrap();
    let revision="c".repeat(40);
    for out in [&out_a,&out_b] { let status=Command::new(env!("CARGO_BIN_EXE_ggen-foundry-connection")).args(["--input",input.to_str().unwrap(),"--program",program.to_str().unwrap(),"--revision",revision.as_str(),"--out",out.to_str().unwrap()]).status().unwrap(); assert!(status.success()); }
    assert_eq!(fs::read(&out_a).unwrap(),fs::read(&out_b).unwrap()); let output:Value=serde_json::from_slice(&fs::read(&out_a).unwrap()).unwrap();
    assert_eq!(output["stage"],"MANUFACTURE"); assert_eq!(output["authority"]["do_authority"],false); assert_eq!(output["parent"]["digest"],sha256(&input_bytes)); assert_eq!(output["standing"]["state"],"PARTIAL_ALIVE"); assert!(output["packs"].as_array().unwrap().iter().any(|p|p["name"]=="global-cloud-foundation"));
}
