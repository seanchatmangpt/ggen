import toml
from pathlib import Path

DROP_CRATES = ["ggen-utils", "ggen-domain", "ggen-config", "ggen-ontology-core", "ggen-codegen", "ggen-canonical", "ggen-semantic-bit", "ggen-prompt-mfg", "ggen-receipt", "ggen-transport", "a2a-generated", "ggen-a2a-registry", "ggen-a2a", "ggen-cli-validation", "ggen-config-clap", "ggen-test-audit", "ggen-test-opt", "ggen-e2e", "pictl-types", "pictl-algos", "ggen-node", "ggen-marketplace"]

for toml_path in ["crates/ggen-core/Cargo.toml", "crates/ggen-cli/Cargo.toml"]:
    p = Path(toml_path)
    if not p.exists(): continue
    try:
        data = toml.load(p)
        changed = False
        
        for section in ["dependencies", "dev-dependencies", "build-dependencies"]:
            if section in data:
                for crate in DROP_CRATES:
                    if crate in data[section]:
                        del data[section][crate]
                        changed = True
        
        if changed:
            with open(p, "w") as f:
                toml.dump(data, f)
            print(f"Cleaned {toml_path}")
    except Exception as e:
        print(f"Error parsing {toml_path}: {e}")
