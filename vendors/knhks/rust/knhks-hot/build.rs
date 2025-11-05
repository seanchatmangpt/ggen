fn main() {
    println!("cargo:rustc-link-search=native=../..");
    println!("cargo:rustc-link-lib=static=knhks");
    println!("cargo:rustc-link-lib=c");
}

