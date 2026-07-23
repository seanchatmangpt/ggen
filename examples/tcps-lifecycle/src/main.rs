use tcps_lifecycle::{
    Authorization, CargoCicdEvidence, ProductionLine, ReferenceCargoCicdExecutor,
    ReferencePraxisGate,
};

fn main() {
    let gate = ReferencePraxisGate::new("standard-v1");
    let observed = ProductionLine::observe("change-42", 1, "standard-v1");
    let admitted = observed.admit(&gate).expect("Praxis admission must succeed");
    let planned = admitted
        .plan(CargoCicdEvidence::green("source-digest-42"))
        .expect("cargo-cicd evidence must be green");
    let authorization = Authorization::for_plan(planned.plan_digest(), "release-manager");
    let authorized = planned
        .authorize(authorization)
        .expect("authorization must bind the plan");
    let mut executor = ReferenceCargoCicdExecutor::green();

    match authorized.execute(&mut executor) {
        Ok(receipted) => println!("{}", receipted.receipt().receipt_digest),
        Err(stopped) => {
            eprintln!("ANDON: {}", stopped.andon().reason);
            std::process::exit(1);
        }
    }
}
