use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    // Put the linker script in the output directory and ensure it's on the linker search path
    let out = &PathBuf::from(env::var_os("OUT_DIR").unwrap());

    // Copy memory.x to OUT_DIR so the linker can find it
    File::create(out.join("memory.x"))
        .unwrap()
        .write_all(include_bytes!("memory.x"))
        .unwrap();

    println!("cargo:rustc-link-search={}", out.display());

    // Also search in the current directory
    println!("cargo:rerun-if-changed=memory.x");
}
