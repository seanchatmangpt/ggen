// POSITIVE: nothing in the body can ever fail. Expected verdict: [CHEAT-T03].
#[test]
fn computes_but_never_checks() {
    let a = vec![1, 2, 3];
    let b: Vec<_> = a.iter().map(|x| x * 2).collect();
    let _len = b.len();
}
