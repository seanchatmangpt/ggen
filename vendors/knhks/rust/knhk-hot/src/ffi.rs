// knhk-hot v1.0 — Rust hot API (FFI-safe, ≤8 ticks)
// Law: A = μ(O). Contracts: O ⊨ Σ, run.len ≤ 8, SoA aligned 64B.

#![allow(non_camel_case_types)]

pub const TICK_BUDGET: u32 = 8;
pub const NROWS: usize = 8;
pub const ALIGN: usize = 64;

// Alignment helpers (SoA must be 64B-aligned)
#[repr(align(64))]
pub struct Aligned<T>(pub T);

// Core types (KGC: Σ, Λ, τ are enforced by wrapper; μ runs in C)
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Run {
    pub pred: u64,   // P id
    pub off: u64,    // SoA offset
    pub len: u64,    // must be ≤ 8 (guarded)
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Ctx {
    pub S: *const u64,
    pub P: *const u64,
    pub O: *const u64,
    pub run: Run,
}

#[repr(u32)]
#[derive(Clone, Copy, Debug)]
pub enum Op {
    AskSp           = 1,
    CountSpGe       = 2,
    AskSpo          = 3,
    CountSpLe       = 5,
    CountSpEq       = 6,
    AskOp           = 7,
    UniqueSp        = 8,
    CountOpGe       = 9,
    CountOpLe       = 10,
    CountOpEq       = 11,
    CompareOEQ      = 12,
    CompareOGT      = 13,
    CompareOLT      = 14,
    CompareOGE      = 15,
    CompareOLE      = 16,
    Construct8      = 32, // fixed-template emit, run.len ≤ 8
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Ir {
    pub op: Op,
    pub s: u64,
    pub p: u64,
    pub o: u64,
    pub k: u64,
    // For Construct8 only (preallocated; length = NROWS)
    pub out_S: *mut u64,
    pub out_P: *mut u64,
    pub out_O: *mut u64,
    pub out_mask: u64, // lanes written bitmask (μ sets)
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Receipt {
    pub ticks: u32,    // ≤ 8
    pub lanes: u32,    // SIMD lanes used
    pub span_id: u64,  // OTEL-compatible id
    pub a_hash: u64,   // fragment toward hash(A) = hash(μ(O))
}

// FFI (μ-hot in C)
#[link(name = "knhk")]
extern "C" {
    pub fn knhk_init_ctx(ctx: *mut Ctx, S: *const u64, P: *const u64, O: *const u64);
    pub fn knhk_pin_run(ctx: *mut Ctx, run: Run);
    pub fn knhk_eval_bool(ctx: *const Ctx, ir: *mut Ir, rcpt: *mut Receipt) -> i32;
    pub fn knhk_eval_construct8(ctx: *const Ctx, ir: *mut Ir, rcpt: *mut Receipt) -> i32;
    pub fn knhk_eval_batch8(ctx: *const Ctx, irs: *mut Ir, n: usize, rcpts: *mut Receipt) -> i32;
}

// Safe wrapper (Σ, Λ, τ, H guards)
pub struct Engine {
    ctx: Ctx,
}

impl Engine {
    /// Σ: arrays must be 64B-aligned, len = NROWS.
    pub fn new(s: *const u64, p: *const u64, o: *const u64) -> Self {
        let mut ctx = Ctx {
            S: std::ptr::null(),
            P: std::ptr::null(),
            O: std::ptr::null(),
            run: Run { pred: 0, off: 0, len: 0 },
        };
        unsafe { knhk_init_ctx(&mut ctx, s, p, o) };
        Self { ctx }
    }

    /// Guard H: run.len ≤ 8. Violations are rejected.
    pub fn pin_run(&mut self, run: Run) -> Result<(), &'static str> {
        if run.len > NROWS as u64 {
            return Err("H: run.len > 8 blocked");
        }
        unsafe { knhk_pin_run(&mut self.ctx, run) };
        Ok(())
    }

    /// μ: boolean reflex, τ ≤ 8. Returns bool and fills rcpt.
    pub fn eval_bool(&self, ir: &mut Ir, rcpt: &mut Receipt) -> bool {
        let r = unsafe { knhk_eval_bool(&self.ctx as *const Ctx, ir as *mut Ir, rcpt as *mut Receipt) };
        r != 0
    }

    /// μ: fixed-template emit, τ ≤ 8. Returns lanes written and fills rcpt.
    pub fn eval_construct8(&self, ir: &mut Ir, rcpt: &mut Receipt) -> usize {
        let n = unsafe { knhk_eval_construct8(&self.ctx as *const Ctx, ir as *mut Ir, rcpt as *mut Receipt) };
        n as usize
    }

    /// Λ: batch of ≤ 8 hooks in ≺-total order. τ holds per hook. rcpts len ≥ n.
    pub fn eval_batch8(&self, irs: &mut [Ir; NROWS], n: usize, rcpts: &mut [Receipt; NROWS]) -> usize {
        if n > NROWS {
            return 0;
        }
        let got = unsafe {
            knhk_eval_batch8(&self.ctx as *const Ctx, irs.as_mut_ptr(), n, rcpts.as_mut_ptr())
        };
        got as usize
    }

    pub fn ctx(&self) -> &Ctx {
        &self.ctx
    }
}

// Receipt merge (Π ⊕)
impl Receipt {
    pub fn merge(a: Receipt, b: Receipt) -> Receipt {
        Receipt {
            ticks: a.ticks.max(b.ticks), // max ticks
            lanes: a.lanes + b.lanes,     // sum lanes
            span_id: a.span_id ^ b.span_id, // XOR merge
            a_hash: a.a_hash ^ b.a_hash,  // ⊕ merge (XOR)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_merge() {
        let a = Receipt { ticks: 4, lanes: 8, span_id: 0x1234, a_hash: 0x5678, ..Default::default() };
        let b = Receipt { ticks: 6, lanes: 8, span_id: 0xabcd, a_hash: 0xef00, ..Default::default() };
        let merged = Receipt::merge(a, b);
        assert_eq!(merged.ticks, 6);
        assert_eq!(merged.lanes, 16);
        assert_eq!(merged.span_id, 0x1234 ^ 0xabcd);
        assert_eq!(merged.a_hash, 0x5678 ^ 0xef00);
    }
}
