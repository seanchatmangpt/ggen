use reqwest::{Client, StatusCode};
use serde_json::json;

const API_BASE: &str = "http://127.0.0.1:8080/api/v1";

#[tokio::test]
async fn test_health_check() {
    let client = Client::new();
    let response = client
        .get(&format!("{}/health", API_BASE))
        .send()
        .await
        .expect("Failed to send request");

    assert_eq!(response.status(), StatusCode::OK);
    let body: serde_json::Value = response.json().await.expect("Failed to parse JSON");
    assert_eq!(body["status"], "healthy");
}

#[tokio::test]
async fn test_user_registration_and_login() {
    let client = Client::new();

    // Register new user
    let register_body = json!({
        "email": "test@example.com",
        "username": "testuser",
        "password": "securepassword123"
    });

    let register_response = client
        .post(&format!("{}/auth/register", API_BASE))
        .json(&register_body)
        .send()
        .await
        .expect("Failed to register");

    assert!(
        register_response.status() == StatusCode::CREATED
            || register_response.status() == StatusCode::CONFLICT
    );

    // Login
    let login_body = json!({
        "email": "test@example.com",
        "password": "securepassword123"
    });

    let login_response = client
        .post(&format!("{}/auth/login", API_BASE))
        .json(&login_body)
        .send()
        .await
        .expect("Failed to login");

    assert_eq!(login_response.status(), StatusCode::OK);
    let login_data: serde_json::Value = login_response.json().await.expect("Failed to parse");
    assert!(login_data["token"].is_string());
    assert_eq!(login_data["user"]["email"], "test@example.com");
}

#[tokio::test]
async fn test_protected_endpoint_without_auth() {
    let client = Client::new();
    let response = client
        .get(&format!("{}/users", API_BASE))
        .send()
        .await
        .expect("Failed to send request");

    assert_eq!(response.status(), StatusCode::UNAUTHORIZED);
}

#[tokio::test]
async fn test_user_crud_operations() {
    let client = Client::new();

    // First, login to get token
    let login_body = json!({
        "email": "test@example.com",
        "password": "securepassword123"
    });

    let login_response = client
        .post(&format!("{}/auth/login", API_BASE))
        .json(&login_body)
        .send()
        .await
        .expect("Failed to login");

    let login_data: serde_json::Value = login_response.json().await.expect("Failed to parse");
    let token = login_data["token"].as_str().unwrap();
    let user_id = login_data["user"]["id"].as_str().unwrap();

    // Get user details
    let get_response = client
        .get(&format!("{}/users/{}", API_BASE, user_id))
        .bearer_auth(token)
        .send()
        .await
        .expect("Failed to get user");

    assert_eq!(get_response.status(), StatusCode::OK);

    // Update user
    let update_body = json!({
        "username": "updateduser"
    });

    let update_response = client
        .put(&format!("{}/users/{}", API_BASE, user_id))
        .bearer_auth(token)
        .json(&update_body)
        .send()
        .await
        .expect("Failed to update user");

    assert_eq!(update_response.status(), StatusCode::OK);
}
