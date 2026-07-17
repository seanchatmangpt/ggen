//! Blue River Dam control benchmark (divan) — ggen render surface.
//!
//! Measures one small Tera render through the sync render engine
//! (`ggen_engine::template::build_tera`, the same engine `ggen_engine::sync` renders every
//! template body with). Runs alongside the existing criterion bench
//! (`bench_main.rs`) — it does not replace it.

#![allow(missing_docs)]
#![allow(clippy::pedantic, clippy::style, clippy::complexity, clippy::perf)]

use std::hint::black_box;
use std::sync::Arc;

use ggen_engine::graph::{GraphEngine, GraphLawStore};
use ggen_engine::template::build_tera;

fn main() {
    divan::main();
}

const TTL: &str = r#"
@prefix ex: <http://example.org/> .
ex:run1 ex:kind "plan-run" ; ex:planLen 5 .
ex:run2 ex:kind "plan-run" ; ex:planLen 3 .
"#;

/// A small report body of the shape sync templates render: a heading, a
/// loop, and interpolations.
const TEMPLATE: &str = "\
# Receipt report\n\
{% for r in runs %}- {{ r.id }}: {{ r.plan_len }} steps ({{ r.chain }})\n{% endfor %}\
total: {{ runs | length }} runs\n";

/// One small Tera render through the sync render engine (`build_tera` over a
/// preloaded `GraphLawStore`, exactly the engine + registered functions
/// `ggen_engine::sync`'s render stage uses; the template body is bench-owned).
#[divan::bench]
fn ggen_render_report_small(bencher: divan::Bencher) {
    let store = GraphLawStore::new().expect("in-memory store");
    store.insert_turtle(TTL).expect("ttl loads");
    let graph: Arc<dyn GraphEngine> = Arc::new(store);
    let mut tera = build_tera(graph).expect("build tera");

    let mut ctx = tera::Context::new();
    let runs: Vec<serde_json::Value> = (0..4)
        .map(|i| {
            serde_json::json!({
                "id": format!("run{i}"),
                "plan_len": 5,
                "chain": format!("blake3:{i:064x}"),
            })
        })
        .collect();
    ctx.insert("runs", &runs);

    bencher.bench_local(|| {
        tera.render_str(black_box(TEMPLATE), &ctx)
            .expect("template renders")
    });
}
