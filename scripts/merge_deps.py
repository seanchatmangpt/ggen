import toml
import sys
from pathlib import Path

MERGE_PLAN = {
    "ggen-utils": ("ggen-core", "utils"),
    "ggen-domain": ("ggen-core", "domain"),
    "ggen-config": ("ggen-core", "config_lib"),
    "ggen-ontology-core": ("ggen-core", "ontology_core"),
    "ggen-codegen": ("ggen-core", "codegen_lib"),
    "ggen-canonical": ("ggen-core", "canonical"),
    "ggen-semantic-bit": ("ggen-core", "semantic_bit"),
    "ggen-prompt-mfg": ("ggen-core", "prompt_mfg"),
    "ggen-receipt": ("ggen-core", "receipt"),
    "ggen-transport": ("ggen-core", "transport"),
    "a2a-generated": ("ggen-core", "a2a_generated"),
    "ggen-a2a-registry": ("ggen-core", "a2a_registry"),
    "ggen-a2a": ("ggen-core", "a2a"),
    "ggen-cli-validation": ("ggen-cli", "validation_lib"),
    "ggen-config-clap": ("ggen-cli", "config_clap"),
}

def merge_deps(target_crate, source_crates):
    target_toml = Path(f"crates/{target_crate}/Cargo.toml")
    try:
        target_data = toml.load(target_toml)
    except Exception as e:
        print(f"Error loading {target_toml}: {e}")
        return

    if "dependencies" not in target_data:
        target_data["dependencies"] = {}

    for src_crate in source_crates:
        src_toml = Path(f"crates/{src_crate}/Cargo.toml")
        if not src_toml.exists():
            continue
        try:
            src_data = toml.load(src_toml)
            if "dependencies" in src_data:
                for dep, val in src_data["dependencies"].items():
                    # Skip internal dependencies that are now merged
                    if dep in MERGE_PLAN or dep == target_crate:
                        continue
                    if dep not in target_data["dependencies"]:
                        target_data["dependencies"][dep] = val
        except Exception as e:
            print(f"Error parsing {src_toml}: {e}")

    with target_toml.open("w") as f:
        toml.dump(target_data, f)
        print(f"Updated {target_toml}")

core_sources = [k for k, v in MERGE_PLAN.items() if v[0] == "ggen-core"]
cli_sources = [k for k, v in MERGE_PLAN.items() if v[0] == "ggen-cli"]

merge_deps("ggen-core", core_sources)
merge_deps("ggen-cli", cli_sources)
