import re

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "r") as file:
    content = file.read()

def replace_test(content, name, old_str, new_str):
    pattern = rf'fn {name}\(\) \{{.*?did_open\("file:///virtual/project/events.jsonl", "json", "{old_str}"\).*?\}}'
    match = re.search(pattern, content, re.DOTALL)
    if match:
        old_block = match.group(0)
        new_block = old_block.replace(f'"{old_str}"', f'"{new_str}"')
        content = content.replace(old_block, new_block)
    return content

content = replace_test(content, "test_f4_t2_wasm4pm_digest_chain_broken", "broken_digest_chain", "{\\\"event_type\\\": \\\"BoundaryDeclared\\\", \\\"digest_mismatch\\\": true}")
content = replace_test(content, "test_f4_t2_wasm4pm_conformance_blocked", "conformance_blocked", "{\\\"event_type\\\": \\\"ArtifactWritten\\\"}")
content = replace_test(content, "test_f4_t2_wasm4pm_replay_deviation", "replay_deviation", "{\\\"event_type\\\": \\\"BoundaryDeclared\\\"}\\n{\\\"event_type\\\": \\\"MutationGateAdmitted\\\"}\\n{\\\"event_type\\\": \\\"ArtifactWritten\\\"}")
content = replace_test(content, "test_f4_t2_wasm4pm_verdict_fit", "verdict_fit", "{\\\"event_type\\\": \\\"BoundaryDeclared\\\"}\\n{\\\"event_type\\\": \\\"StagingPrepared\\\"}\\n{\\\"event_type\\\": \\\"MutationGateAdmitted\\\"}\\n{\\\"event_type\\\": \\\"ArtifactWritten\\\"}")

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "w") as file:
    file.write(content)

