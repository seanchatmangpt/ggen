#[test]
fn test_size_string_roundtrip_convention_resolved() {
    // RESOLVED SEAM (was: type-identical inverses with divergent bases). The
    // canonical size base for this crate is now SI/1000 everywhere:
    // `parse_size_in_bytes` (parser) and both `human_bytes` definitions (display)
    // are inverses on the SAME base, so the round-trip is lossless at unit
    // boundaries. This test now witnesses CONVERGENCE; if either side ever drifts
    // back to binary/1024, the round-trip breaks here and forces a conscious fix.
    use osx_clnr::{
        domain::{time::parse_size_in_bytes, tool_roots::human_bytes as hb_domain},
        integration::progress::human_bytes as hb_integration,
    };

    assert_eq!(parse_size_in_bytes("1GB"), Ok(1_000_000_000));

    // Clean round-trip: "1GB" -> bytes -> "1.00 GB".
    let bytes = parse_size_in_bytes("1GB").unwrap();
    assert_eq!(hb_integration(bytes), "1.00 GB");

    // The two byte-identical definitions agree (same canonical base).
    assert_eq!(hb_integration(bytes), hb_domain(bytes));
    assert_eq!(hb_domain(2_500_000), "2.50 MB");
}
