//! BRCE + WebAuthn AS-01 seat presence gate (structural only).
//!
//! ECOSYSTEM-SPEC-27.1.1 names **BRCE + WebAuthn AS-01** (`:prime256v1` / ES256)
//! as the sole lawful CONSTRUCT → DO gate. This module refuses **ungated DO**
//! when no seat claim is presented, and shape-checks the *declared* algorithm
//! and curve strings.
//!
//! It deliberately does **not** verify WebAuthn assertions, COSE signatures,
//! or P-256 public keys. Crypto verify remains `UNSUPPORTED` until a BRCE
//! adapter and `p256`/WebAuthn court land. Presence of a well-shaped claim is
//! not ambient DO authority and must not be promoted to `ALIVE` seat verify.
//!
//! Distinct from `ggen-sjira::lease` (Ed25519 / `EdDSA` sovereign lease) and
//! from this crate's manufacture ceiling (CONSTRUCT — [`crate::Authority`]).

use thiserror::Error;

/// Sole lawful DO broker name (matches `ggen-architecture::REQUIRED_BROKER`).
pub const REQUIRED_BROKER: &str = "BRCE";

/// COSE / WebAuthn algorithm string for ECDSA P-256 (AS-01).
pub const AS01_ALG: &str = "ES256";

/// OpenSSL / WebCrypto curve name for P-256 (AS-01 seat).
pub const AS01_CURVE: &str = "prime256v1";

/// Declared AS-01 seat claim — identity + algorithm shape only.
///
/// Callers that obtain a claim from a verified WebAuthn ceremony may present
/// it here later; this type carries no signature bytes and performs no crypto.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct As01SeatClaim {
    pub seat_id: String,
    pub alg: String,
    pub curve: String,
}

impl As01SeatClaim {
    /// Construct a claim that declares the admitted AS-01 alg/curve pair.
    pub fn declared(seat_id: impl Into<String>) -> Self {
        Self {
            seat_id: seat_id.into(),
            alg: AS01_ALG.to_string(),
            curve: AS01_CURVE.to_string(),
        }
    }
}

/// Typed refusal of an ungated or mis-shaped DO attempt at the AS-01 seat fence.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum As01SeatRefusal {
    /// No seat claim was presented — ambient / ungated DO.
    #[error(
        "REFUSED_NO_AS01_SEAT: ungated DO without WebAuthn AS-01 seat (broker={broker})"
    )]
    NoSeat { broker: String },
    /// Broker is not the admitted BRCE broker.
    #[error("REFUSED_WRONG_BROKER: DO requires broker BRCE, got {got}")]
    WrongBroker { got: String },
    /// Seat id empty or alg/curve not the declared AS-01 pair.
    #[error("REFUSED_AS01_SEAT_SHAPE: {reason}")]
    SeatShape { reason: String },
}

/// Structural DO gate: require BRCE broker + well-shaped AS-01 seat claim.
///
/// Returns `Ok(())` only when `broker == "BRCE"` and `seat` declares non-empty
/// `seat_id` with `alg == "ES256"` and `curve == "prime256v1"`. Does not
/// verify cryptography. Does not grant manufacture DO (see crate ceiling).
pub fn admit_do_with_as01_seat(
    broker: &str, seat: Option<&As01SeatClaim>,
) -> Result<(), As01SeatRefusal> {
    if broker != REQUIRED_BROKER {
        return Err(As01SeatRefusal::WrongBroker {
            got: broker.to_string(),
        });
    }
    let Some(seat) = seat else {
        return Err(As01SeatRefusal::NoSeat {
            broker: broker.to_string(),
        });
    };
    if seat.seat_id.trim().is_empty() {
        return Err(As01SeatRefusal::SeatShape {
            reason: "seat_id must be non-empty".to_string(),
        });
    }
    if seat.alg != AS01_ALG {
        return Err(As01SeatRefusal::SeatShape {
            reason: format!("alg must be {AS01_ALG}, got {}", seat.alg),
        });
    }
    if seat.curve != AS01_CURVE {
        return Err(As01SeatRefusal::SeatShape {
            reason: format!("curve must be {AS01_CURVE}, got {}", seat.curve),
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declared_constants_name_brce_es256_prime256v1() {
        assert_eq!(REQUIRED_BROKER, "BRCE");
        assert_eq!(AS01_ALG, "ES256");
        assert_eq!(AS01_CURVE, "prime256v1");
    }

    #[test]
    fn ungated_do_without_seat_is_refused() {
        let err = admit_do_with_as01_seat(REQUIRED_BROKER, None)
            .expect_err("ungated DO must refuse");
        assert_eq!(
            err,
            As01SeatRefusal::NoSeat {
                broker: "BRCE".to_string()
            }
        );
        assert!(err.to_string().contains("REFUSED_NO_AS01_SEAT"));
    }

    #[test]
    fn well_shaped_as01_seat_passes_presence_gate_only() {
        let seat = As01SeatClaim::declared("seat:as01:fixture-001");
        assert!(admit_do_with_as01_seat(REQUIRED_BROKER, Some(&seat)).is_ok());
    }

    #[test]
    fn wrong_broker_is_refused_even_with_seat() {
        let seat = As01SeatClaim::declared("seat:as01:fixture-001");
        let err = admit_do_with_as01_seat("NOT-BRCE", Some(&seat))
            .expect_err("non-BRCE broker must refuse");
        assert_eq!(
            err,
            As01SeatRefusal::WrongBroker {
                got: "NOT-BRCE".to_string()
            }
        );
    }

    #[test]
    fn empty_seat_id_is_refused() {
        let seat = As01SeatClaim {
            seat_id: "   ".to_string(),
            alg: AS01_ALG.to_string(),
            curve: AS01_CURVE.to_string(),
        };
        let err = admit_do_with_as01_seat(REQUIRED_BROKER, Some(&seat))
            .expect_err("empty seat_id must refuse");
        assert!(matches!(err, As01SeatRefusal::SeatShape { .. }));
    }

    #[test]
    fn ed25519_lease_alg_is_not_as01_seat() {
        let seat = As01SeatClaim {
            seat_id: "seat:confused-ed25519".to_string(),
            alg: "EdDSA".to_string(),
            curve: "ed25519".to_string(),
        };
        let err = admit_do_with_as01_seat(REQUIRED_BROKER, Some(&seat))
            .expect_err("EdDSA must not satisfy AS-01");
        assert!(matches!(err, As01SeatRefusal::SeatShape { .. }));
    }
}
