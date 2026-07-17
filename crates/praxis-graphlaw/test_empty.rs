fn main() {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_bad_sparql a kh:Hook ;
            kh:name "bad_sparql" ;
            kh:kind "sparql" ;
            kh:query "SELECT * WHERE { ?s ?p }" ; # missing brackets
            kh:effect "emit-delta" .
    "#;
    let mut store = praxis_graphlaw::TripleStore::new();
    let res = store.load_hook_pack(hook_pack);
    println!("RES: {:?}", res);
    println!("STORE HOOKS: {:?}", store.hooks.len());
}
