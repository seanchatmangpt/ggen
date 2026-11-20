//! Cross-port admission fixture: one golden JSON that BOTH
//! `ggen_sjira::admit_work_order` and Elixir
//! `GgenIgniter.SemanticJira.admit_work_order/1` must admit.
//!
//! Elixir probe (coordinator / verifier; run from ggen_igniter checkout):
//!
//! ```elixir
//! # mix run -e '
//! golden = Jason.decode!(File.read!("../ggen/crates/ggen-sjira/tests/fixtures/golden_admit.json"))
//! {:ok, admitted} = GgenIgniter.SemanticJira.admit_work_order(golden)
//! IO.puts("ELIXIR_ADMIT ok identity=" <> admitted["identity"] <> " authority=" <> admitted["authority"])
//! '
//! ```
//!
//! Or from iex -S mix:
//!
//! ```elixir
//! path = Path.expand("../ggen/crates/ggen-sjira/tests/fixtures/golden_admit.json", __DIR__)
//! # adjust path relative to ggen_igniter root:
//! path = "/Users/sac/ggen/crates/ggen-sjira/tests/fixtures/golden_admit.json"
//! {:ok, _} = GgenIgniter.SemanticJira.admit_work_order(Jason.decode!(File.read!(path)))
//! ```

use ggen_sjira::{admit_work_order, Standing};
use serde_json::Value;

const GOLDEN: &str = include_str!("fixtures/golden_admit.json");

#[test]
fn golden_json_admits_on_rust() {
    let raw: Value = serde_json::from_str(GOLDEN).expect("golden_admit.json parses");
    let admitted = admit_work_order(&raw).expect(
        "golden fixture must admit on Rust (WP9 cross-port SoT; if this fails, \
         Rust drifted from Elixir SemanticJira.admit_work_order/1)",
    );
    assert_eq!(admitted.authority, "NONE");
    assert_eq!(admitted.work_order.identity, "SJ-ALIGN-001");
    assert_eq!(admitted.work_order.standing, Standing::Unknown);
    assert_eq!(
        admitted.work_order.path_scope,
        vec!["lib/ggen_igniter".to_string()]
    );
    assert_eq!(admitted.work_order.replay_identity, "semantic-jira:align:1");
    assert_eq!(admitted.work_order.authority_requirement, "NONE");
    assert!(admitted.definition_digest.starts_with("sha256:"));
}
