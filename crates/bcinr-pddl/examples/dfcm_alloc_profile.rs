//! Runs the DfCM crown suite once under the `dhat-heap`-feature counting
//! allocator (see `src/alloc_counter.rs`) and prints per-stage allocation
//! counts/bytes — surface 7 of `docs/DFCM_CONTRACT.md`'s benchmark
//! attribution plan.
//!
//! Run with: `cargo run -p bcinr-pddl --example dfcm_alloc_profile --features dhat-heap`

#[global_allocator]
static ALLOC: bcinr_pddl::alloc_counter::counting_alloc::CountingAlloc =
    bcinr_pddl::alloc_counter::counting_alloc::CountingAlloc;

fn main() {
    let receipt = bcinr_pddl::run_dfcm_crown_suite().expect("dfcm crown suite");
    println!("wall_clock_ms = {}", receipt.wall_clock_ms);
    println!("alloc_count_by_stage = {:#?}", receipt.alloc_count_by_stage);
    println!(
        "bytes_allocated_by_stage = {:#?}",
        receipt.bytes_allocated_by_stage
    );
}
