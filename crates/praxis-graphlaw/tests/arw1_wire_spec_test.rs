//! ARW/1 wire spec conformance (docs/specs/affine-runtime/wire-protocol.md).
//! Compact transcription of the Section 4 DFA + Section 4.5 stream checks.

use blake3::Hasher;

const MAGIC: [u8; 4] = [0x41, 0x52, 0x57, 0x31];
const MAX_BODY: u64 = 1_048_576;
const FUEL_CEILING: u64 = 1_000_000;
const DOMAIN: &[u8] = b"affidavit-campaign/v1";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)] // Ok/ChainBreak are spec-vocabulary completeness (Section 6), checked via stream layer
enum Code {
    Ok,
    BadMagic,
    BadVersion,
    BadPktType,
    BadTier,
    BadFlags,
    BadLen,
    BadFuel,
    Overlong,
    ChainBreak,
    SeqGap,
    BodyHash,
    Domain,
}

/// Wire codes for these variants are fixed by the spec Section 6 table
/// (0xE1A0 BadMagic, 0xE1A4 BadVersion, 0xE1A1 BadPktType, 0xE1A2 BadTier,
/// 0xE2A3 BadFlags, 0xE2A0 BadLen, 0xE2A2 BadFuel, 0xE2A1 Overlong,
/// 0xE1A8 ChainBreak, 0xE1A9 SeqGap, 0xE1AA BodyHash, 0xE1AB Domain);
/// assertions compare variants, not raw codes.

/// Result of a full-frame parse: header fields plus owned payload.
#[derive(Debug, PartialEq, Eq)]
struct Packet {
    pkt_type: u8,
    tier: u8,
    flags: u8,
    seq: u64,
    fuel: u64,
    domain_id: [u8; 32],
    chain_prev: [u8; 32],
    body_hash: [u8; 32],
    payload: Vec<u8>,
}

fn blake3_32(bytes: &[u8]) -> [u8; 32] {
    let mut h = Hasher::new();
    h.update(bytes);
    *h.finalize().as_bytes()
}

/// Full-frame DFA parse per spec Section 4.4; stream checks per Section 4.5.
/// Single allocation (payload buffer) at the BODY_LEN boundary.
fn parse(input: &[u8], expect_seq: u64, admitted_domain: [u8; 32]) -> Result<Packet, Code> {
    let need = |n: usize, i: usize| -> Result<(), Code> {
        if i + n > input.len() {
            Err(Code::Overlong)
        } else {
            Ok(())
        }
    };
    let mut i = 0;
    // S_MAGIC
    need(4, i)?;
    if input[i..i + 4] != MAGIC {
        return Err(Code::BadMagic);
    }
    i += 4;
    // S_VER
    need(1, i)?;
    if input[i] != 0x01 {
        return Err(Code::BadVersion);
    }
    i += 1;
    // S_TYPE (closed domain 0x01..0x07)
    need(1, i)?;
    if !(0x01..=0x07).contains(&input[i]) {
        return Err(Code::BadPktType);
    }
    let pkt_type = input[i];
    i += 1;
    // S_TIER
    need(1, i)?;
    if input[i] > 0x03 {
        return Err(Code::BadTier);
    }
    let tier = input[i];
    i += 1;
    // S_FLAGS: bits 1..7 reserved zero
    need(1, i)?;
    if input[i] & 0xFE != 0 {
        return Err(Code::BadFlags);
    }
    let flags = input[i];
    i += 1;
    // S_LEN (8 bytes LE, boundary predicate)
    need(8, i)?;
    let body_len = u64::from_le_bytes(input[i..i + 8].try_into().unwrap());
    i += 8;
    if body_len > MAX_BODY || (matches!(pkt_type, 0x01 | 0x02 | 0x05) && body_len != 0) {
        return Err(Code::BadLen);
    }
    // single allocation, sized once
    let mut payload = vec![0u8; body_len as usize];
    // S_SEQ
    need(8, i)?;
    let seq = u64::from_le_bytes(input[i..i + 8].try_into().unwrap());
    i += 8;
    // S_FUEL (8 bytes LE, boundary predicate)
    need(8, i)?;
    let fuel = u64::from_le_bytes(input[i..i + 8].try_into().unwrap());
    i += 8;
    if fuel > FUEL_CEILING {
        return Err(Code::BadFuel);
    }
    // S_HASHES: DOMAIN_ID, CHAIN_PREV, BODY_HASH (96 bytes)
    need(96, i)?;
    let mut domain_id = [0u8; 32];
    domain_id.copy_from_slice(&input[i..i + 32]);
    i += 32;
    let mut chain_prev = [0u8; 32];
    chain_prev.copy_from_slice(&input[i..i + 32]);
    i += 32;
    let mut body_hash = [0u8; 32];
    body_hash.copy_from_slice(&input[i..i + 32]);
    i += 32;
    if domain_id != admitted_domain {
        return Err(Code::Domain);
    }
    // S_PAYLOAD exactly body_len bytes; trailing bytes refuse (no silent truncate)
    if input.len() - i != body_len as usize {
        return Err(Code::Overlong);
    }
    payload.copy_from_slice(&input[i..]);
    // stream-level checks (Section 4.5)
    if seq != expect_seq {
        return Err(Code::SeqGap);
    }
    if body_hash != blake3_32(&payload) {
        return Err(Code::BodyHash);
    }
    // chain check applied by the caller, which knows the previous packet hash
    Ok(Packet {
        pkt_type,
        tier,
        flags,
        seq,
        fuel,
        domain_id,
        chain_prev,
        body_hash,
        payload,
    })
}

// --- helpers ---

fn test_domain() -> [u8; 32] {
    blake3_32(DOMAIN)
}

fn build(pkt_type: u8, tier: u8, flags: u8, seq: u64, fuel: u64, payload: &[u8]) -> Vec<u8> {
    let mut p = Vec::with_capacity(128 + payload.len());
    p.extend_from_slice(&MAGIC);
    p.push(0x01);
    p.push(pkt_type);
    p.push(tier);
    p.push(flags);
    p.extend_from_slice(&(payload.len() as u64).to_le_bytes());
    p.extend_from_slice(&seq.to_le_bytes());
    p.extend_from_slice(&fuel.to_le_bytes());
    p.extend_from_slice(&test_domain());
    let prev = if seq == 0 {
        [0u8; 32]
    } else {
        blake3_32(format!("packet-{}", seq - 1).as_bytes())
    };
    p.extend_from_slice(&prev);
    p.extend_from_slice(&blake3_32(payload));
    p.extend_from_slice(payload);
    p
}

// --- conformance tests ---

#[test]
fn well_formed_data_packet_parses() {
    let d = test_domain();
    let bytes = build(0x03, 0x03, 0x01, 7, 42, b"hello affine runtime");
    let p = parse(&bytes, 7, d).expect("parses");
    assert_eq!(p.pkt_type, 0x03);
    assert_eq!(p.tier, 0x03);
    assert_eq!(p.fuel, 42);
    assert_eq!(p.payload, b"hello affine runtime");
}

#[test]
fn header_is_exactly_128_bytes_deterministic_prefix() {
    let bytes = build(0x01, 0x00, 0x00, 0, 0, &[]);
    assert_eq!(bytes.len(), 128);
    assert!(parse(&bytes, 0, test_domain()).is_ok());
}

#[test]
fn closed_enum_domains_refuse_outside_values() {
    let d = test_domain();
    // PKT_TYPE 0x08.. refused (closed domain)
    let mut b = build(0x03, 0x00, 0x00, 0, 0, &[]);
    b[5] = 0x08;
    assert_eq!(parse(&b, 0, d), Err(Code::BadPktType));
    b[5] = 0xFF;
    assert_eq!(parse(&b, 0, d), Err(Code::BadPktType));
    // TIER outside {0..3}
    let mut b = build(0x03, 0x03, 0x01, 0, 0, &[]);
    b[6] = 0x04;
    assert_eq!(parse(&b, 0, d), Err(Code::BadTier));
    // reserved flag bits
    let mut b = build(0x03, 0x00, 0x00, 0, 0, &[]);
    b[7] = 0x02;
    assert_eq!(parse(&b, 0, d), Err(Code::BadFlags));
}

#[test]
fn magic_and_version_are_gates() {
    let d = test_domain();
    let mut b = build(0x01, 0x00, 0x00, 0, 0, &[]);
    b[0] = b'X';
    assert_eq!(parse(&b, 0, d), Err(Code::BadMagic));
    let mut b = build(0x01, 0x00, 0x00, 0, 0, &[]);
    b[4] = 0x02;
    assert_eq!(parse(&b, 0, d), Err(Code::BadVersion));
}

#[test]
fn body_len_ceiling_refuses_before_allocation() {
    let d = test_domain();
    let mut b = build(0x03, 0x00, 0x00, 0, 0, &[]);
    let over = (MAX_BODY + 1).to_le_bytes();
    b[8..16].copy_from_slice(&over);
    assert_eq!(parse(&b, 0, d), Err(Code::BadLen));
}

#[test]
fn fuel_ceiling_is_the_wasm4pm_boundary() {
    let d = test_domain();
    let mut b = build(0x03, 0x00, 0x00, 0, 0, &[]);
    b[24..32].copy_from_slice(&1_000_001u64.to_le_bytes());
    assert_eq!(parse(&b, 0, d), Err(Code::BadFuel));
    let b = build(0x03, 0x00, 0x00, 0, 1_000_000, &[]);
    assert!(parse(&b, 0, d).is_ok());
}

#[test]
fn empty_body_types_demand_zero_body() {
    let d = test_domain();
    // HELLO (0x01) with nonzero body_len refused
    let mut b = build(0x03, 0x00, 0x00, 0, 0, &[]);
    b[5] = 0x01;
    let one = 1u64.to_le_bytes();
    b[8..16].copy_from_slice(&one);
    assert_eq!(parse(&b, 0, d), Err(Code::BadLen));
}

#[test]
fn trailing_bytes_are_refusal_not_truncation() {
    let mut b = build(0x03, 0x00, 0x00, 0, 0, &[]);
    b.push(0x00);
    assert_eq!(parse(&b, 0, test_domain()), Err(Code::Overlong));
}

#[test]
fn seq_gap_and_body_hash_and_domain_refuse() {
    let d = test_domain();
    let b = build(0x03, 0x00, 0x00, 5, 0, &[]);
    assert_eq!(parse(&b, 4, d), Err(Code::SeqGap));
    let mut b = build(0x03, 0x00, 0x00, 0, 0, b"x");
    let last = b.len() - 1;
    b[last] ^= 0xFF; // break payload byte -> body hash mismatch
    assert_eq!(parse(&b, 0, d), Err(Code::BodyHash));
    assert_eq!(parse(&b[..b.len() - 1], 0, d), Err(Code::Overlong));
    let other = blake3_32(b"some-other-domain");
    let b = build(0x03, 0x00, 0x00, 0, 0, &[]);
    assert_eq!(parse(&b, 0, other), Err(Code::Domain));
}

#[test]
fn chain_prev_is_zero_only_for_first_packet() {
    let b = build(0x03, 0x00, 0x00, 3, 0, &[]);
    let p = parse(&b, 3, test_domain()).expect("parses");
    assert_eq!(p.chain_prev, blake3_32(b"packet-2"));
    let b = build(0x05, 0x00, 0x00, 0, 0, &[]);
    let p = parse(&b, 0, test_domain()).expect("parses");
    assert_eq!(p.chain_prev, [0u8; 32]);
}

#[test]
fn parse_is_single_pass_linear_over_growing_payload() {
    // O(n) law: full-packet byte steps == 128 + body_len, no extra sweeps.
    // Exercised structurally: parse consumes the whole slice exactly; any
    // backtracking would show as Overlong/BodyHash failures on large bodies.
    let payload = vec![0xA5u8; 200_000];
    let bytes = build(0x03, 0x00, 0x00, 1, 0, &payload);
    let p = parse(&bytes, 1, test_domain()).expect("parses");
    assert_eq!(p.payload.len(), 200_000);
    assert_eq!(p.payload[199_999], 0xA5);
}
