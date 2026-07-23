use tcps_lifecycle::{ProductionLine, ReferenceCargoCicdExecutor, ReferencePraxisGate};

fn main() {
    let gate = ReferencePraxisGate::new("standard-v1");
    let mut executor = ReferenceCargoCicdExecutor::green();
    let evidence = executor.verify(
        "source-digest-42",
        vec!["tcps-lifecycle".to_owned()],
    );
    let observed = ProductionLine::observe("change-42", 1, "standard-v1");
    let admitted = observed.admit(&gate).expect("Praxis admission must succeed");
    let planned = admitted
        .plan(evidence)
        .expect("cargo-cicd evidence must be green");
    let authorization = gate.authorize(&planned, "release-manager");
    let authorized = planned
        .authorize(authorization)
        .expect("authorization must bind the plan");

    match authorized.execute(&mut executor) {
        Ok(receipted) => println!("{}", receipted.receipt().receipt_digest()),
        Err(stopped) => {
            eprintln!("ANDON: {}", stopped.andon().reason());
            std::process::exit(1);
        }
    }
}
