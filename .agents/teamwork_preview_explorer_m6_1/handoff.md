# Handoff Report — External Observer Script Ring Formulation

## 1. Observation

*   **Existing Compliance Scripts**: Located at `/Users/sac/ggen/scripts/gall/`:
    *   `anti_fake_implementation.sh`
    *   `emit_ocel_self_audit.sh`
    *   `forbidden_surface.sh`
    *   `verify_ocel_self_audit.sh`
*   **Contradiction in Self-Audit Log Generator**: In `/Users/sac/ggen/crates/ggen-graph/src/ocel/self_audit.rs`, both promotion and refusal events are statically appended:
    *   Line 562:
        ```rust
        // Event 15: CheckpointPromoted
        log.events.push(OcelEvent {
            id: "ev_checkpoint_promoted".to_string(),
            activity: "CheckpointPromoted".to_string(),
            ...
        ```
    *   Line 582:
        ```rust
        // Event 16: CheckpointRefused
        log.events.push(OcelEvent {
            id: "ev_checkpoint_refused".to_string(),
            activity: "CheckpointRefused".to_string(),
            ...
        ```
*   **Available Hashing/Parsing Tools on Mac OS**:
    *   `/sbin/sha256sum` (version `sha256sum (Darwin) 1.0`)
    *   `/usr/bin/shasum` (supports `shasum -a 256`)
    *   `/usr/bin/openssl` (supports `openssl dgst -sha256`)
    *   `/usr/bin/jq` (version `jq-1.7.1-apple`)
    *   `/usr/bin/python3` (version `Python 3.9.6`)
*   **Rust Test Status**: Running `cargo test -p ggen-graph` runs successfully with `2 tests passed`.

---

## 2. Logic Chain

1.  The `ORIGINAL_REQUEST.md` specifies that the External Observer Script Ring must consist of scripts `00` to `13` and must perform tasks including capturing baseline, extracting requirements, verifying package constraints, checking feature flags, running tests, scanning forbidden surfaces, verifying OCEL self-audits, detecting contradictions, and performing final promotion adjudication.
2.  Because macOS/Darwin is the target OS and has `/sbin/sha256sum`, `/usr/bin/shasum`, and `/usr/bin/openssl`, any of these tools can be used in a wrapper shell function `get_sha256()` to calculate script digests and project file digests cross-platform.
3.  Each verification script can obtain its path using `BASH_SOURCE[0]` and calculate its current digest, allowing the master script (`13_adjudicate_gall_promotion.sh`) to detect if any script in the ring was bypassed or mutated by comparing actual hashes against an expected registry or writing them directly to the final `vision2030.external_adjudication.json`.
4.  In `/Users/sac/ggen/crates/ggen-graph/src/ocel/self_audit.rs`, both `CheckpointPromoted` and `CheckpointRefused` are statically appended to the same log. In a logically sound process model, a single checkpoint verification run cannot result in both outcomes.
5.  A contradiction detector like `12_detect_contradictions.sh` using `python3` can parse this JSON and check for status singularity and causal evaluation precedence, immediately flagging this double-state as a violation. Therefore, the implementation team must parameterize `self_audit.rs` to conditionally emit only one event type.

---

## 3. Caveats

*   **Implementation Restriction**: I have not modified any source code or created any scripts under `scripts/gall/external/` because my role constraint strictly enforces read-only investigation.
*   **Subagent Scope**: The actual implementation of `12_detect_contradictions.sh` and the parameterization of `self_audit.rs` is left to the implementer/worker agent, who has write permissions on the crate source.

---

## 4. Conclusion

The design for the 14 scripts (`00_` to `13_`) under `scripts/gall/external/` is complete and documented in `/Users/sac/ggen/.agents/teamwork_preview_explorer_m6_1/analysis_lifecycle.md`. 
*   **Actionable Next Step**: The implementer must generate these 14 scripts in `scripts/gall/external/`, integrate the python-based contradiction detection logic, and update `/Users/sac/ggen/crates/ggen-graph/src/ocel/self_audit.rs` to conditionally output either `CheckpointPromoted` or `CheckpointRefused` (based on execution outcome) instead of both.

---

## 5. Verification Method

*   To verify the availability of hashing and JSON utilities, run:
    ```bash
    which sha256sum shasum openssl jq python3
    ```
*   To verify the contradiction detector logic draft, execute a python test script containing the logic against `crates/ggen-graph/audit/vision2030.self_audit.ocel.json` and ensure it raises the expected double-event violation error.
*   To verify the project compiles and tests pass, run:
    ```bash
    cargo test -p ggen-graph
    ```
