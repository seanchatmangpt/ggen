use ggen_transport::origin::{Origin, OriginValidator};

#[test]
fn test_origin_from_url_with_port() {
    let origin = Origin::from_url("https://example.com:8080").unwrap();
    assert_eq!(origin.scheme, "https");
    assert_eq!(origin.host, "example.com");
    assert_eq!(origin.port, Some(8080));
}

#[test]
fn test_origin_from_url_without_port() {
    let origin = Origin::from_url("https://example.com").unwrap();
    assert_eq!(origin.scheme, "https");
    assert_eq!(origin.host, "example.com");
    assert_eq!(origin.port, None);
}

#[test]
fn test_origin_from_url_http() {
    let origin = Origin::from_url("http://localhost:3000").unwrap();
    assert_eq!(origin.scheme, "http");
    assert_eq!(origin.host, "localhost");
    assert_eq!(origin.port, Some(3000));
}

#[test]
fn test_origin_from_url_invalid() {
    let result = Origin::from_url("not-a-valid-url");
    assert!(result.is_err());
}

#[test]
fn test_origin_to_string_with_port() {
    let origin = Origin::from_url("https://example.com:8080").unwrap();
    assert_eq!(origin.to_string(), "https://example.com:8080");
}

#[test]
fn test_origin_to_string_without_port() {
    let origin = Origin::from_url("https://example.com").unwrap();
    assert_eq!(origin.to_string(), "https://example.com");
}

#[test]
fn test_origin_matches_same() {
    let origin1 = Origin::from_url("https://example.com:8080").unwrap();
    let origin2 = Origin::from_url("https://example.com:8080").unwrap();
    assert!(origin1.matches(&origin2));
}

#[test]
fn test_origin_matches_different_port() {
    let origin1 = Origin::from_url("https://example.com:8080").unwrap();
    let origin2 = Origin::from_url("https://example.com:9090").unwrap();
    assert!(!origin1.matches(&origin2));
}

#[test]
fn test_origin_matches_different_scheme() {
    let origin1 = Origin::from_url("https://example.com").unwrap();
    let origin2 = Origin::from_url("http://example.com").unwrap();
    assert!(!origin1.matches(&origin2));
}

#[test]
fn test_origin_matches_different_host() {
    let origin1 = Origin::from_url("https://example.com").unwrap();
    let origin2 = Origin::from_url("https://other.com").unwrap();
    assert!(!origin1.matches(&origin2));
}

#[test]
fn test_origin_validator_allow_all() {
    let validator = OriginValidator::allow_all();
    let origin = Origin::from_url("https://any-domain.com").unwrap();
    assert!(validator.validate(&origin).is_ok());
}

#[test]
fn test_origin_validator_allowed_list() {
    let validator = OriginValidator::new(vec![
        "https://trusted1.com".to_string(),
        "https://trusted2.com:8080".to_string(),
    ]);

    let trusted1 = Origin::from_url("https://trusted1.com").unwrap();
    let trusted2 = Origin::from_url("https://trusted2.com:8080").unwrap();
    let untrusted = Origin::from_url("https://untrusted.com").unwrap();

    assert!(validator.validate(&trusted1).is_ok());
    assert!(validator.validate(&trusted2).is_ok());
    assert!(validator.validate(&untrusted).is_err());
}

#[test]
fn test_origin_validator_add_origin() {
    let mut validator = OriginValidator::new(vec![]);
    let origin = Origin::from_url("https://new.com").unwrap();

    assert!(validator.validate(&origin).is_err());

    validator.add_origin("https://new.com".to_string());
    assert!(validator.validate(&origin).is_ok());
}

#[test]
fn test_origin_validator_remove_origin() {
    let mut validator = OriginValidator::new(vec!["https://example.com".to_string()]);
    let origin = Origin::from_url("https://example.com").unwrap();

    assert!(validator.validate(&origin).is_ok());

    validator.remove_origin("https://example.com");
    assert!(validator.validate(&origin).is_err());
}

#[test]
fn test_origin_validator_is_allowed() {
    let validator = OriginValidator::new(vec!["https://allowed.com".to_string()]);
    let allowed = Origin::from_url("https://allowed.com").unwrap();
    let blocked = Origin::from_url("https://blocked.com").unwrap();

    assert!(validator.is_allowed(&allowed));
    assert!(!validator.is_allowed(&blocked));
}

#[test]
fn test_origin_validator_multiple_origins() {
    let validator = OriginValidator::new(vec![
        "https://app1.example.com".to_string(),
        "https://app2.example.com".to_string(),
        "http://localhost:3000".to_string(),
    ]);

    let app1 = Origin::from_url("https://app1.example.com").unwrap();
    let app2 = Origin::from_url("https://app2.example.com").unwrap();
    let localhost = Origin::from_url("http://localhost:3000").unwrap();
    let unknown = Origin::from_url("https://unknown.com").unwrap();

    assert!(validator.validate(&app1).is_ok());
    assert!(validator.validate(&app2).is_ok());
    assert!(validator.validate(&localhost).is_ok());
    assert!(validator.validate(&unknown).is_err());
}

#[test]
fn test_origin_serialization() {
    let origin = Origin::from_url("https://example.com:8080").unwrap();
    let serialized = serde_json::to_string(&origin).unwrap();
    let deserialized: Origin = serde_json::from_str(&serialized).unwrap();

    assert_eq!(deserialized.scheme, "https");
    assert_eq!(deserialized.host, "example.com");
    assert_eq!(deserialized.port, Some(8080));
}

#[test]
fn test_origin_localhost_variants() {
    let localhost1 = Origin::from_url("http://localhost").unwrap();
    let localhost2 = Origin::from_url("http://127.0.0.1").unwrap();

    assert_eq!(localhost1.host, "localhost");
    assert_eq!(localhost2.host, "127.0.0.1");
    assert!(!localhost1.matches(&localhost2));
}

#[test]
fn test_origin_default_ports() {
    let https = Origin::from_url("https://example.com").unwrap();
    let http = Origin::from_url("http://example.com").unwrap();

    assert_eq!(https.port, None);
    assert_eq!(http.port, None);
}

#[test]
fn test_origin_case_sensitivity() {
    let origin1 = Origin::from_url("https://Example.com").unwrap();
    let origin2 = Origin::from_url("https://example.com").unwrap();

    assert_ne!(origin1.host, origin2.host);
}
