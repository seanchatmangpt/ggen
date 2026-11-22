//! Chicago court: ungated DO without WebAuthn AS-01 seat is refused.
//!
//! ECOSYSTEM-SPEC-27.1.1 — BRCE + AS-01 (`ES256` / `prime256v1`) presence fence.
//! AAA, no mocks. Does **not** claim crypto verify ALIVE.
//! Falsifier: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court`

use ggen_abb_sbb::as01_seat::{
    admit_do_with_as01_seat, As01SeatClaim, As01SeatRefusal, AS01_ALG, AS01_CURVE, REQUIRED_BROKER,
};

#[test]
fn declared_constants_name_brce_es256_prime256v1() {
    // Arrange / Act / Assert
    assert_eq!(REQUIRED_BROKER, "BRCE");
    assert_eq!(AS01_ALG, "ES256");
    assert_eq!(AS01_CURVE, "prime256v1");
}

#[test]
fn ungated_do_without_seat_is_refused() {
    // Arrange
    let broker = REQUIRED_BROKER;

    // Act
    let result = admit_do_with_as01_seat(broker, None);

    // Assert
    assert!(result.is_err(), "ungated DO must refuse");
    if let Err(err) = result {
        assert_eq!(
            err,
            As01SeatRefusal::NoSeat {
                broker: "BRCE".to_string()
            }
        );
        let msg = err.to_string();
        assert!(
            msg.contains("REFUSED_NO_AS01_SEAT"),
            "refusal must carry REFUSED_NO_AS01_SEAT marker, got {msg}"
        );
    }
}

#[test]
fn well_shaped_as01_seat_passes_presence_gate_only() {
    // Arrange — declared shape only; no signature bytes, no crypto
    let seat = As01SeatClaim::declared("seat:as01:fixture-001");

    // Act
    let admitted = admit_do_with_as01_seat(REQUIRED_BROKER, Some(&seat));

    // Assert
    assert!(
        admitted.is_ok(),
        "presence gate must admit well-shaped claim; crypto verify remains out of scope"
    );
}

#[test]
fn wrong_broker_is_refused_even_with_seat() {
    // Arrange
    let seat = As01SeatClaim::declared("seat:as01:fixture-001");

    // Act
    let result = admit_do_with_as01_seat("NOT-BRCE", Some(&seat));

    // Assert
    assert!(result.is_err(), "non-BRCE broker must refuse");
    if let Err(err) = result {
        assert_eq!(
            err,
            As01SeatRefusal::WrongBroker {
                got: "NOT-BRCE".to_string()
            }
        );
    }
}

#[test]
fn empty_seat_id_is_refused() {
    // Arrange
    let seat = As01SeatClaim {
        seat_id: "   ".to_string(),
        alg: AS01_ALG.to_string(),
        curve: AS01_CURVE.to_string(),
    };

    // Act
    let result = admit_do_with_as01_seat(REQUIRED_BROKER, Some(&seat));

    // Assert
    assert!(result.is_err(), "empty seat_id must refuse");
    if let Err(err) = result {
        assert!(matches!(err, As01SeatRefusal::SeatShape { .. }));
        assert!(err.to_string().contains("REFUSED_AS01_SEAT_SHAPE"));
    }
}

#[test]
fn ed25519_lease_alg_is_not_as01_seat() {
    // Arrange — SovereignLease uses EdDSA/ed25519; must not pass AS-01 shape
    let seat = As01SeatClaim {
        seat_id: "seat:confused-ed25519".to_string(),
        alg: "EdDSA".to_string(),
        curve: "ed25519".to_string(),
    };

    // Act
    let result = admit_do_with_as01_seat(REQUIRED_BROKER, Some(&seat));

    // Assert
    assert!(
        result.is_err(),
        "EdDSA must not satisfy AS-01 ES256/prime256v1"
    );
    if let Err(err) = result {
        assert!(matches!(err, As01SeatRefusal::SeatShape { .. }));
        let msg = err.to_string();
        assert!(
            msg.contains("ES256") || msg.contains("prime256v1"),
            "shape refusal should name admitted AS-01 alg/curve, got {msg}"
        );
    }
}
