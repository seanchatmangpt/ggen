//! C-ABI surface over the safe ML-DSA-65 functions (FNOX-SJ-SIGN-4).
//!
//! Return codes: `GGEN_DSA_OK` (0) success; `GGEN_DSA_ERR_ARG` (-1) null pointer or
//! wrong buffer length. `ggen_dsa_verify` returns 1 for a valid signature, 0 for an
//! invalid one, and -1 on argument errors. Nothing here reimplements crypto: every
//! entry point delegates to `crate::{signing_key_from_seed, sign, verify,
//! verifying_key_bytes}`.

use crate::{
    sign, signing_key_from_seed, verify, verifying_key_bytes, ML_DSA_65_SIGNATURE_LEN,
    ML_DSA_65_SIGNING_SEED_LEN, ML_DSA_65_VERIFYING_KEY_LEN,
};
use ml_dsa::{EncodedVerifyingKey, MlDsa65, VerifyingKey};

pub const GGEN_DSA_OK: i32 = 0;
pub const GGEN_DSA_ERR_ARG: i32 = -1;

unsafe fn slice<'a>(p: *const u8, len: usize) -> Option<&'a [u8]> {
    if len == 0 {
        return Some(&[]);
    }
    if p.is_null() {
        return None;
    }
    Some(std::slice::from_raw_parts(p, len))
}

/// Derive the 1952-byte verifying key from a 32-byte seed. `pk_out` must hold 1952 bytes.
///
/// # Safety
/// `seed` must point to 32 readable bytes; `pk_out` to 1952 writable bytes.
#[no_mangle]
pub unsafe extern "C" fn ggen_dsa_keygen_from_seed(
    seed: *const u8, seed_len: usize, pk_out: *mut u8, pk_out_len: usize,
) -> i32 {
    if seed.is_null()
        || pk_out.is_null()
        || seed_len != ML_DSA_65_SIGNING_SEED_LEN
        || pk_out_len != ML_DSA_65_VERIFYING_KEY_LEN
    {
        return GGEN_DSA_ERR_ARG;
    }
    let mut s = [0u8; 32];
    s.copy_from_slice(std::slice::from_raw_parts(seed, 32));
    let pk = verifying_key_bytes(&signing_key_from_seed(&s));
    std::ptr::copy_nonoverlapping(pk.as_ptr(), pk_out, ML_DSA_65_VERIFYING_KEY_LEN);
    GGEN_DSA_OK
}

/// Deterministic sign. `sig_out` must hold 3309 bytes.
///
/// # Safety
/// Pointers must be valid for their stated lengths.
#[no_mangle]
pub unsafe extern "C" fn ggen_dsa_sign(
    seed: *const u8, seed_len: usize, msg: *const u8, msg_len: usize, ctx: *const u8,
    ctx_len: usize, sig_out: *mut u8, sig_out_len: usize,
) -> i32 {
    if seed.is_null() || sig_out.is_null() || seed_len != ML_DSA_65_SIGNING_SEED_LEN {
        return GGEN_DSA_ERR_ARG;
    }
    if sig_out_len != ML_DSA_65_SIGNATURE_LEN || ctx_len > 255 {
        return GGEN_DSA_ERR_ARG;
    }
    let (Some(m), Some(c)) = (slice(msg, msg_len), slice(ctx, ctx_len)) else {
        return GGEN_DSA_ERR_ARG;
    };
    let mut s = [0u8; 32];
    s.copy_from_slice(std::slice::from_raw_parts(seed, 32));
    let sig = sign(&signing_key_from_seed(&s), m, c);
    if sig.len() != ML_DSA_65_SIGNATURE_LEN {
        return GGEN_DSA_ERR_ARG;
    }
    std::ptr::copy_nonoverlapping(sig.as_ptr(), sig_out, sig.len());
    GGEN_DSA_OK
}

/// Verify. Returns 1 valid, 0 invalid, -1 argument error.
///
/// # Safety
/// Pointers must be valid for their stated lengths.
#[no_mangle]
pub unsafe extern "C" fn ggen_dsa_verify(
    pk: *const u8, pk_len: usize, msg: *const u8, msg_len: usize, ctx: *const u8, ctx_len: usize,
    sig: *const u8, sig_len: usize,
) -> i32 {
    if pk.is_null() || sig.is_null() || pk_len != ML_DSA_65_VERIFYING_KEY_LEN {
        return GGEN_DSA_ERR_ARG;
    }
    let (Some(m), Some(c)) = (slice(msg, msg_len), slice(ctx, ctx_len)) else {
        return GGEN_DSA_ERR_ARG;
    };
    let pk_bytes = std::slice::from_raw_parts(pk, pk_len);
    let sig_bytes = std::slice::from_raw_parts(sig, sig_len);
    let Ok(enc) = EncodedVerifyingKey::<MlDsa65>::try_from(pk_bytes) else {
        return GGEN_DSA_ERR_ARG;
    };
    let vk = VerifyingKey::<MlDsa65>::decode(&enc);
    if verify(&vk, m, c, sig_bytes) {
        1
    } else {
        0
    }
}
