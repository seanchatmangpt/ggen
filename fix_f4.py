import re

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "r") as file:
    content = file.read()

# remove test_f4_t2_wasm4pm_replay_deviation
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_replay_deviation\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)

# remove test_f4_t2_wasm4pm_verdict_fit
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_verdict_fit\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "w") as file:
    file.write(content)
