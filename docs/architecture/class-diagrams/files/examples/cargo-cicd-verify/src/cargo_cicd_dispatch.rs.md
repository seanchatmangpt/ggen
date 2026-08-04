# `examples/cargo-cicd-verify/src/cargo_cicd_dispatch.rs`

Source SHA-256: `a374f7e7ee42aa9a51a7412f68e645cb8f07bedbf8478ad6d590c3760a196df4`

```mermaid
classDiagram
    class fn_build_command {
      <<fn>>
    }
    class fn_dispatch {
      <<fn>>
    }
    class fn_certification_show {
      <<fn>>
    }
    class fn_claude_context_show {
      <<fn>>
    }
    class fn_doctor_diff {
      <<fn>>
    }
    class fn_doctor_evidence {
      <<fn>>
    }
    class fn_doctor_repo {
      <<fn>>
    }
    class fn_evidence_audit {
      <<fn>>
    }
    class fn_evidence_doctor {
      <<fn>>
    }
    class fn_evidence_list {
      <<fn>>
    }
    class fn_evidence_reset {
      <<fn>>
    }
    class fn_evidence_show {
      <<fn>>
    }
    class fn_gate_repo {
      <<fn>>
    }
    class fn_git_close {
      <<fn>>
    }
    class fn_git_commit {
      <<fn>>
    }
    class fn_git_diff {
      <<fn>>
    }
    class fn_git_fetch {
      <<fn>>
    }
    class fn_git_stage {
      <<fn>>
    }
    class fn_git_status {
      <<fn>>
    }
    class fn_hooks_install {
      <<fn>>
    }
    class fn_hooks_pre_tool_use {
      <<fn>>
    }
    class fn_hooks_uninstall {
      <<fn>>
    }
    class fn_ocel_replay {
      <<fn>>
    }
    class fn_pipeline_run {
      <<fn>>
    }
    class fn_pipeline_status {
      <<fn>>
    }
    class fn_pipeline_validate {
      <<fn>>
    }
    class fn_publish_check {
      <<fn>>
    }
    class fn_publish_run {
      <<fn>>
    }
    class fn_publish_validate {
      <<fn>>
    }
    class fn_receipt_audit {
      <<fn>>
    }
    class fn_receipt_verify {
      <<fn>>
    }
    class fn_release_gate_check {
      <<fn>>
    }
    class fn_sbom_generate {
      <<fn>>
    }
    class fn_sbom_show {
      <<fn>>
    }
    class fn_standing_refresh {
      <<fn>>
    }
    class fn_standing_report {
      <<fn>>
    }
    class fn_standing_verify {
      <<fn>>
    }
    class fn_status_audit {
      <<fn>>
    }
    class fn_status_show {
      <<fn>>
    }
    class fn_target_prune {
      <<fn>>
    }
    class fn_target_show {
      <<fn>>
    }
    class fn_test_bench {
      <<fn>>
    }
    class fn_test_changed {
      <<fn>>
    }
    class fn_test_run {
      <<fn>>
    }
    class fn_trace_profile {
      <<fn>>
    }
    class fn_trybuild_changed {
      <<fn>>
    }
    class fn_trybuild_review {
      <<fn>>
    }
    class fn_trybuild_update {
      <<fn>>
    }
    class fn_verify_repo {
      <<fn>>
    }
    class fn_workspace_doctor {
      <<fn>>
    }
    class fn_workspace_list {
      <<fn>>
    }
    class fn_workspace_sync {
      <<fn>>
    }
    class fn_workspace_validate {
      <<fn>>
    }
```

## Dependencies

- `std::process::Command`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
