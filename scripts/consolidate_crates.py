import os
import shutil
import re
from pathlib import Path

# Mapping of source crate -> (target crate, module name)
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

DROP_CRATES = [
    "ggen-test-audit",
    "ggen-test-opt",
    "ggen-e2e",
    "pictl-types",
    "pictl-algos",
    "ggen-node",
]

CRATES_DIR = Path("crates")

def move_files(source_crate, target_crate, module_name):
    src_dir = CRATES_DIR / source_crate / "src"
    if not src_dir.exists():
        print(f"Warning: {src_dir} does not exist.")
        return
        
    target_dir = CRATES_DIR / target_crate / "src" / module_name
    
    if target_dir.exists():
        shutil.rmtree(target_dir)
    
    # Move src contents to the new module directory
    shutil.copytree(src_dir, target_dir)
    
    # If the source crate had a lib.rs, rename it to mod.rs in the new location
    lib_rs = target_dir / "lib.rs"
    if lib_rs.exists():
        lib_rs.rename(target_dir / "mod.rs")
        
    # Same for main.rs just in case, though they might need manual fixes
    main_rs = target_dir / "main.rs"
    if main_rs.exists():
        main_rs.unlink() # Usually don't want to merge binaries directly this way
        
def register_module(target_crate, module_name):
    lib_rs = CRATES_DIR / target_crate / "src" / "lib.rs"
    if not lib_rs.exists():
        print(f"Warning: {lib_rs} does not exist.")
        return
        
    content = lib_rs.read_text()
    mod_decl = f"pub mod {module_name};\n"
    if mod_decl not in content:
        # Prepend to lib.rs
        lib_rs.write_text(mod_decl + content)

def refactor_imports(source_crate, target_crate, module_name):
    # Convert crate name to rust module name (e.g. ggen-utils -> ggen_utils)
    old_mod = source_crate.replace("-", "_")
    target_base = target_crate.replace("-", "_") # e.g. ggen_core
    
    # We want to replace `use ggen_utils::` with `use ggen_core::utils::`
    # However, if we are inside `ggen_core` itself, it might just be `use crate::utils::`
    # Let's do a global replace across all `.rs` files.
    
    for path in Path(".").rglob("*.rs"):
        if "target/" in str(path) or ".cargo" in str(path):
            continue
            
        try:
            content = path.read_text()
            
            # If the file is inside the target_crate, use `crate::module_name`
            if f"crates/{target_crate}/" in str(path):
                new_import = f"crate::{module_name}"
            else:
                new_import = f"{target_base}::{module_name}"
                
            # Replace `use old_mod::`
            new_content = re.sub(rf"\b{old_mod}::", f"{new_import}::", content)
            
            # Replace extern crate old_mod
            new_content = re.sub(rf"extern crate {old_mod};", "", new_content)
            
            if new_content != content:
                path.write_text(new_content)
        except Exception as e:
            print(f"Could not process {path}: {e}")

def update_root_cargo_toml():
    root_toml = Path("Cargo.toml")
    content = root_toml.read_text()
    
    for crate in MERGE_PLAN.keys():
        content = re.sub(rf'"{crate}",\n?', '', content)
        content = re.sub(rf'"crates/{crate}",\n?', '', content)
        
    for crate in DROP_CRATES:
        content = re.sub(rf'"{crate}",\n?', '', content)
        content = re.sub(rf'"crates/{crate}",\n?', '', content)
        
    root_toml.write_text(content)

def drop_crates():
    for crate in DROP_CRATES + list(MERGE_PLAN.keys()):
        crate_dir = CRATES_DIR / crate
        if crate_dir.exists():
            shutil.rmtree(crate_dir)

def main():
    print("Starting aggressive consolidation...")
    
    for source_crate, (target_crate, module_name) in MERGE_PLAN.items():
        print(f"Merging {source_crate} -> {target_crate}::{module_name}")
        move_files(source_crate, target_crate, module_name)
        register_module(target_crate, module_name)
        refactor_imports(source_crate, target_crate, module_name)
        
    print("Updating workspace Cargo.toml...")
    update_root_cargo_toml()
    
    print("Dropping old crates...")
    drop_crates()
    
    print("Consolidation script complete. Note: You will need to manually merge dependencies in Cargo.toml files.")

if __name__ == "__main__":
    main()
