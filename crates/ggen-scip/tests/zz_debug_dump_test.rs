#[test]
fn dump() {
    let root = std::path::Path::new("/tmp/scipdbg");
    let idx = ggen_scip::emit::build_index(root).unwrap();
    eprintln!("{}", ggen_scip::canonical_json(&idx).unwrap());
}
