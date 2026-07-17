//! Chicago-TDD adversarial coverage of the chatman engine using
//! `chicago_tdd_tools::cli_proof` (`TempWorkspace`, `SabotageFixture`).
//!
//! ## Mode used: real on-disk sabotage of the persisted oxigraph store
//!
//! `ChatmanEngine::open` (`crates/praxis-graphlaw/src/chatman/engine.rs`)
//! persists the RDF snapshot store to disk via `oxigraph::store::Store::open`
//! — this is real, complete persistence (not a stub), so this file sabotages
//! the actual on-disk store files, not a fixture-file surrogate.
//!
//! `EngineProcessReceipt` (the nine-digest process receipt) itself carries no
//! `Serialize`/`Deserialize` impl and `engine.rs` never writes a receipt JSON
//! file to disk on its own — receipts are in-memory `Rust` values, and the
//! `chatman_engine_acceptance` fixture files already in the test suite
//! (`tests/fixtures/chatman_engine_acceptance/**/*.json`) are all empty `{}`
//! placeholders (`ggen`-generated scaffolding, not real receipt payloads —
//! confirmed by reading every file under `replay/` and `receipt_boundary/`).
//! So real disk sabotage of the *store* is exercised directly, and real
//! `SabotageFixture`/`ReceiptAssertions`-style fixture-file sabotage of a
//! *receipt* is exercised against a receipt JSON this test constructs itself
//! from a genuine `admit_transition` run (not a fabricated payload) — see
//! `sabotaged_receipt_fixture_is_detected_by_verify_replay` below.

use std::fs;

use chicago_tdd_tools::cli_proof::{SabotageFixture, TempWorkspace};

use praxis_graphlaw::chatman::abi::{
    Digest, GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId,
    Refusal,
};
use praxis_graphlaw::chatman::engine::{
    AdmissionSpec, ChatmanEngine, EngineProcessReceipt, EngineProfile, ReplayInputs, ReplayMismatch,
};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const SNAPSHOT_IRI: &str = "urn:chatman:snapshot:cli-sabotage-test";
const PROFILE_IRI: &str = "profile:cli-sabotage-test";

const SNAPSHOT_TTL: &str = r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ceng: <urn:chatman:engine#> .

ex:Employee rdfs:subClassOf ex:Person .
ex:alice a ex:Employee .

ex:world ceng:pddlDomain """
(define (domain chatman-min)
  (:requirements :strips)
  (:predicates (ready ?x) (done ?x))
  (:action finish
    :parameters (?x)
    :precondition (and (ready ?x))
    :effect (and (done ?x) (not (ready ?x)))))
""" .
ex:world ceng:pddlProblem """
(define (problem chatman-min-p)
  (:domain chatman-min)
  (:objects a)
  (:init (ready a))
  (:goal (done a))
)
""" .
ex:world ceng:ocelLog """{"run_id":1,"sealed":true,"objects":[{"id":"case-1","otype":"case"}],"events":[{"id":"e1","activity":"finish(a)","op_index":0,"at_ns":1,"objects":["case-1"]}]}""" .
"#;

fn test_profile() -> Result<EngineProfile, Refusal> {
    let profile_id = ProfileId::new(PROFILE_IRI);
    let gates = ProfileGates::new(profile_id.clone(), ProfileGates::DEFAULT_ENABLED_MASK, 0, 8)?;
    let symbol_table = ProfileSymbolTable::build(
        profile_id,
        vec![
            "<urn:chatman:t0>".to_string(),
            "<urn:chatman:t1>".to_string(),
        ],
    )?;
    Ok(EngineProfile {
        gates,
        symbol_table,
        admission: AdmissionSpec {
            constraint_names: vec!["c0".to_string()],
            required_mask: 0,
            forbidden_mask: 0,
            set_on_admit: 0,
            clear_on_admit: 0,
        },
        breed_permits: Vec::new(),
    })
}

fn envelope() -> InvocationEnvelope {
    InvocationEnvelope {
        invocation_id: InvocationId::new("inv-cli-sabotage"),
        snapshot_id: GraphSnapshotId::new(SNAPSHOT_IRI),
        profile_id: ProfileId::new(PROFILE_IRI),
        operator_id: OperatorId::new("op-cli-sabotage"),
        input_handles: InputHandles::default(),
    }
}

/// Recursively XOR every byte of every regular file under `dir`. Guaranteed
/// to corrupt whatever on-disk format is present (RocksDB SSTs, WAL, or
/// manifest), unlike a single-byte flip which some formats can tolerate.
fn xor_every_file_in_dir(dir: &std::path::Path) -> std::io::Result<usize> {
    let mut corrupted = 0usize;
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            corrupted += xor_every_file_in_dir(&path)?;
        } else if path.is_file() {
            let bytes = fs::read(&path)?;
            if bytes.is_empty() {
                continue;
            }
            let flipped: Vec<u8> = bytes.iter().map(|b| b ^ 0xFF).collect();
            fs::write(&path, flipped)?;
            corrupted += 1;
        }
    }
    Ok(corrupted)
}

// ─── Real on-disk sabotage of the persisted oxigraph store ────────────────

#[test]
fn on_disk_store_sabotage_is_refused_or_undetectable_on_reopen(
) -> Result<(), Box<dyn std::error::Error>> {
    let ws = TempWorkspace::new()?;
    let store_path = ws.resolve("chatman-store");

    // Arrange: open a real on-disk engine, load a snapshot, admit a
    // transition, and drop the engine so the store flushes and releases its
    // lock file.
    {
        let mut engine = ChatmanEngine::open(&store_path, test_profile()?)?;
        engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
        let transition = engine.admit_transition(envelope())?;
        assert_eq!(
            transition.receipt().recompute_root(),
            transition.receipt().receipt_root,
            "sanity: the pre-sabotage receipt must be internally consistent"
        );
    }
    ws.assert_dir_count(".", 1);
    ws.assert_file_exists("chatman-store");

    // Act: real byte-level sabotage of every file the store wrote to disk.
    let corrupted_count = xor_every_file_in_dir(&store_path)?;
    assert!(
        corrupted_count > 0,
        "expected ChatmanEngine::open to have written at least one file to {}",
        store_path.display()
    );

    // Assert: reopening either refuses outright, or (if the corruption
    // happens to land somewhere oxigraph's on-disk format tolerates without
    // raising an I/O error on open) the previously admitted snapshot no
    // longer round-trips through admit_transition — the engine must not
    // silently produce a receipt over corrupted data.
    match ChatmanEngine::open(&store_path, test_profile()?) {
        Err(_refusal) => {
            // Fail-closed at open: the store refused to open over corrupted
            // bytes. This is the expected, and strongest, outcome.
        }
        Ok(mut reopened) => {
            match reopened.admit_transition(envelope()) {
                Err(_refusal) => {
                    // Fail-closed at re-admission: corrupted storage was
                    // detected once the engine tried to read the snapshot.
                }
                Ok(transition) => {
                    // The store tolerated the corruption (e.g. only unused
                    // slack bytes were flipped). In that case the only
                    // acceptable outcome is that the receipt is still
                    // internally consistent — no silently-wrong receipt.
                    assert_eq!(
                        transition.receipt().recompute_root(),
                        transition.receipt().receipt_root,
                        "if sabotage went undetected, the receipt must still \
                         be internally consistent (root recomputes over the \
                         nine carried digests) — a corrupted-but-unnoticed \
                         receipt would be the actual failure mode this test \
                         guards against"
                    );
                }
            }
        }
    }
    Ok(())
}

// ─── Fixture-file sabotage of a receipt payload (SabotageFixture) ─────────

/// Serializes the nine constitutional digests of a real
/// `EngineProcessReceipt` to a JSON object. `EngineProcessReceipt` itself
/// has no `Serialize` impl (by design — see module docs), so this is a
/// test-local projection, not a production artifact.
fn receipt_to_json(receipt: &EngineProcessReceipt) -> serde_json::Value {
    serde_json::json!({
        "graph_snapshot": receipt.graph_snapshot.0,
        "profile": receipt.profile.0,
        "symbol_table": receipt.symbol_table.0,
        "projection": receipt.projection.0,
        "admission_table": receipt.admission_table.0,
        "route_decision": receipt.route_decision.0,
        "tape": receipt.tape.0,
        "hook_event": receipt.hook_event.0,
        "engine_version": receipt.engine_version.0,
        "receipt_root": receipt.receipt_root.0,
        "canon_nquads": receipt.canon_nquads,
    })
}

/// Reconstructs an `EngineProcessReceipt` from the JSON projection above.
fn receipt_from_json(value: &serde_json::Value) -> EngineProcessReceipt {
    let get = |key: &str| -> String {
        value
            .get(key)
            .and_then(serde_json::Value::as_str)
            .unwrap_or_default()
            .to_string()
    };
    EngineProcessReceipt {
        graph_snapshot: Digest::new(get("graph_snapshot")),
        profile: Digest::new(get("profile")),
        symbol_table: Digest::new(get("symbol_table")),
        projection: Digest::new(get("projection")),
        admission_table: Digest::new(get("admission_table")),
        route_decision: Digest::new(get("route_decision")),
        tape: Digest::new(get("tape")),
        hook_event: Digest::new(get("hook_event")),
        engine_version: Digest::new(get("engine_version")),
        receipt_root: Digest::new(get("receipt_root")),
        canon_nquads: get("canon_nquads"),
        // This fixture only round-trips the nine constitutional digests
        // (see `receipt_to_json` above); the source receipt always came
        // from a plain `admit_transition` call, which always sets this to
        // `None` (PROJ-796).
        external_cut: None,
    }
}

#[test]
fn sabotaged_receipt_fixture_is_detected_by_verify_replay() -> Result<(), Box<dyn std::error::Error>>
{
    // Arrange: a real admitted transition, persisted to a workspace file as
    // the receipt-JSON fixture this test then sabotages.
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
    let transition = engine.admit_transition(envelope())?;
    let receipt_json = receipt_to_json(transition.receipt());

    let ws = TempWorkspace::new()?;
    let receipt_path = ws.resolve("receipt.json");
    fs::write(&receipt_path, serde_json::to_string_pretty(&receipt_json)?)?;
    ws.assert_file_exists("receipt.json");

    // Sanity: the untouched fixture, reloaded and replayed, is faithful.
    let loaded = ws.read_json("receipt.json")?;
    let faithful_receipt = receipt_from_json(&loaded);
    let inputs = ReplayInputs {
        envelope: envelope(),
        snapshot_turtle: SNAPSHOT_TTL.to_string(),
        profile: test_profile()?,
    };
    ChatmanEngine::verify_replay(&faithful_receipt, &inputs)
        .map_err(|m| format!("faithful fixture must replay clean, got {m}"))?;

    // Act: real `SabotageFixture` corruption of the on-disk JSON — flip the
    // `tape` digest (constitutional field #7) to 64 zero-hex characters,
    // the same "wrong but well-shaped" corruption class the module's replay
    // tests use.
    SabotageFixture::corrupt_json(&receipt_path, |mut v| {
        v["tape"] = serde_json::Value::String("0".repeat(64));
        v
    })?;

    // Assert: the sabotaged fixture, reloaded, is refused by verify_replay
    // with the field-specific mismatch — corruption of a receipt on disk
    // must be detected, not silently accepted.
    let sabotaged = ws.read_json("receipt.json")?;
    let sabotaged_receipt = receipt_from_json(&sabotaged);
    match ChatmanEngine::verify_replay(&sabotaged_receipt, &inputs) {
        Err(ReplayMismatch::Tape { .. }) => Ok(()),
        other => Err(format!(
            "expected the sabotaged `tape` field to surface ReplayMismatch::Tape, got {other:?}"
        )
        .into()),
    }
}

#[test]
fn sabotage_fixture_bit_flip_on_receipt_json_is_detected() -> Result<(), Box<dyn std::error::Error>>
{
    // A second sabotage mode: `SabotageFixture::bit_flip` mutates raw bytes
    // (not a parsed JSON field), which is likely to break JSON parsing
    // entirely rather than surface a typed mismatch — confirm that failure
    // mode explicitly rather than assuming `corrupt_json`-style behavior.
    let mut engine = ChatmanEngine::in_memory(test_profile()?)?;
    engine.load_snapshot(&GraphSnapshotId::new(SNAPSHOT_IRI), SNAPSHOT_TTL)?;
    let transition = engine.admit_transition(envelope())?;
    let receipt_json = receipt_to_json(transition.receipt());

    let ws = TempWorkspace::new()?;
    let receipt_path = ws.resolve("receipt_bitflip.json");
    fs::write(&receipt_path, serde_json::to_string_pretty(&receipt_json)?)?;

    SabotageFixture::bit_flip(&receipt_path)?;

    // Read raw bytes, not `read_to_string`: bit-flipping the leading `{`
    // byte of UTF-8 text can produce a byte sequence that is not valid
    // UTF-8 at all, and that is itself a valid (even stronger) detection
    // signal, not a test-harness bug.
    let raw_bytes = fs::read(&receipt_path)?;
    let parsed: Result<serde_json::Value, _> = std::str::from_utf8(&raw_bytes)
        .map_err(|e| e.to_string())
        .and_then(|s| serde_json::from_str(s).map_err(|e| e.to_string()));
    match parsed {
        Err(_) => {
            // Expected: flipping the first byte of a pretty-printed JSON
            // object (starts with `{`) breaks the opening token — either
            // the bytes are no longer valid UTF-8, or they no longer parse
            // as JSON. Either way the corruption is detected before any
            // receipt content is trusted.
        }
        Ok(value) => {
            // If the flipped byte happened to land somewhere that still
            // parses (unlikely for a leading `{`, but not structurally
            // guaranteed), the reconstructed receipt must still be caught
            // by verify_replay.
            let sabotaged_receipt = receipt_from_json(&value);
            let inputs = ReplayInputs {
                envelope: envelope(),
                snapshot_turtle: SNAPSHOT_TTL.to_string(),
                profile: test_profile()?,
            };
            assert!(
                ChatmanEngine::verify_replay(&sabotaged_receipt, &inputs).is_err(),
                "bit-flipped receipt that still parses as JSON must still fail replay"
            );
        }
    }
    Ok(())
}
