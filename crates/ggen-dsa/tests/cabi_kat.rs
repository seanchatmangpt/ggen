//! FNOX-SJ-SIGN-4: KAT vectors through the exported extern "C" surface.

use ggen_dsa::cabi::{ggen_dsa_keygen_from_seed, ggen_dsa_sign, ggen_dsa_verify};
use ggen_dsa::{sign, signing_key_from_seed, verifying_key_bytes};

fn kat_seed() -> [u8; 32] {
    core::array::from_fn(|i| i as u8)
}

fn kat_pk() -> Vec<u8> {
    let h = include_str!("fixtures_ml_dsa_65_pk.hex").trim();
    (0..h.len() / 2)
        .map(|i| u8::from_str_radix(&h[2 * i..2 * i + 2], 16).unwrap())
        .collect()
}

fn c_sign(seed: &[u8; 32], msg: &[u8], ctx: &[u8]) -> Vec<u8> {
    let mut sig = vec![0u8; 3309];
    let rc = unsafe {
        ggen_dsa_sign(
            seed.as_ptr(),
            32,
            msg.as_ptr(),
            msg.len(),
            ctx.as_ptr(),
            ctx.len(),
            sig.as_mut_ptr(),
            sig.len(),
        )
    };
    assert_eq!(rc, 0);
    sig
}

fn c_verify(pk: &[u8], msg: &[u8], ctx: &[u8], sig: &[u8]) -> i32 {
    unsafe {
        ggen_dsa_verify(
            pk.as_ptr(),
            pk.len(),
            msg.as_ptr(),
            msg.len(),
            ctx.as_ptr(),
            ctx.len(),
            sig.as_ptr(),
            sig.len(),
        )
    }
}

#[test]
fn keygen_matches_published_kat_pk_and_rust_fn() {
    let seed = kat_seed();
    let mut pk = vec![0u8; 1952];
    let rc = unsafe { ggen_dsa_keygen_from_seed(seed.as_ptr(), 32, pk.as_mut_ptr(), pk.len()) };
    assert_eq!(rc, 0);
    assert_eq!(pk, kat_pk());
    assert_eq!(pk, verifying_key_bytes(&signing_key_from_seed(&seed)));
}

#[test]
fn c_sign_matches_rust_sign_and_verifies() {
    let seed = kat_seed();
    let sig = c_sign(&seed, b"ggen receipt body", b"OI-3");
    assert_eq!(
        sig,
        sign(&signing_key_from_seed(&seed), b"ggen receipt body", b"OI-3")
    );
    assert_eq!(c_verify(&kat_pk(), b"ggen receipt body", b"OI-3", &sig), 1);
}

#[test]
fn flipped_byte_and_wrong_context_refused() {
    let seed = kat_seed();
    let sig = c_sign(&seed, b"msg", b"A");
    let pk = kat_pk();
    assert_eq!(c_verify(&pk, b"msg", b"A", &sig), 1);
    assert_eq!(c_verify(&pk, b"msg", b"B", &sig), 0);
    let mut bad = sig.clone();
    bad[100] ^= 1;
    assert_eq!(c_verify(&pk, b"msg", b"A", &bad), 0);
}

#[test]
fn bad_arguments_return_error_code() {
    let sig = [0u8; 3309];
    assert_eq!(c_verify(&[0u8; 10], b"m", b"", &sig), -1);
    let mut out = [0u8; 10];
    let seed = kat_seed();
    let rc = unsafe {
        ggen_dsa_sign(
            seed.as_ptr(),
            32,
            b"m".as_ptr(),
            1,
            b"".as_ptr(),
            0,
            out.as_mut_ptr(),
            10,
        )
    };
    assert_eq!(rc, -1);
}
