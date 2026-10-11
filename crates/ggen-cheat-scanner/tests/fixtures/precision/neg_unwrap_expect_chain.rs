// NEGATIVE: .unwrap()/.expect() are failure-capable, so this is not a
// no-assertion test. Expected verdict: [] (clean).
#[test]
fn failure_capable_via_unwrap_and_expect() {
    let parsed: Result<u8, std::num::ParseIntError> = "7".parse();
    assert_eq!(parsed.as_ref().unwrap(), &7);
    let opt: Option<u8> = Some(9);
    let got = opt.expect("must be present");
    assert!(got == 9);
}
