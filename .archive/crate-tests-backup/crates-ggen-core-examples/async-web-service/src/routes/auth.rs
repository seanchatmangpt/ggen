use actix_web::{post, web, HttpResponse, Responder};
use jsonwebtoken::{encode, EncodingKey, Header};
use validator::Validate;

use crate::db::users as db_users;
use crate::models::{AuthResponse, Claims, ErrorResponse, LoginRequest, RegisterRequest};
use crate::AppState;

pub fn configure(cfg: &mut web::ServiceConfig) {
    cfg.service(login).service(register);
}

/// Login endpoint
#[utoipa::path(
    post,
    path = "/api/v1/auth/login",
    tag = "auth",
    request_body = LoginRequest,
    responses(
        (status = 200, description = "Login successful", body = AuthResponse),
        (status = 401, description = "Invalid credentials", body = ErrorResponse)
    )
)]
#[post("/auth/login")]
async fn login(state: web::Data<AppState>, req: web::Json<LoginRequest>) -> impl Responder {
    if let Err(e) = req.validate() {
        return HttpResponse::BadRequest().json(ErrorResponse {
            error: format!("Validation error: {}", e),
        });
    }

    let user = match db_users::get_user_by_email(&state.db, &req.email).await {
        Ok(Some(user)) => user,
        Ok(None) => {
            return HttpResponse::Unauthorized().json(ErrorResponse {
                error: "Invalid credentials".to_string(),
            })
        }
        Err(e) => {
            return HttpResponse::InternalServerError().json(ErrorResponse {
                error: format!("Database error: {}", e),
            })
        }
    };

    let valid = bcrypt::verify(&req.password, &user.password_hash).unwrap_or(false);
    if !valid {
        return HttpResponse::Unauthorized().json(ErrorResponse {
            error: "Invalid credentials".to_string(),
        });
    }

    let claims = Claims::new(user.id, user.email.clone(), 24);
    let token = match encode(
        &Header::default(),
        &claims,
        &EncodingKey::from_secret(state.jwt_secret.as_bytes()),
    ) {
        Ok(t) => t,
        Err(e) => {
            return HttpResponse::InternalServerError().json(ErrorResponse {
                error: format!("Failed to generate token: {}", e),
            })
        }
    };

    HttpResponse::Ok().json(AuthResponse { token, user })
}

/// Register new user
#[utoipa::path(
    post,
    path = "/api/v1/auth/register",
    tag = "auth",
    request_body = RegisterRequest,
    responses(
        (status = 201, description = "Registration successful", body = AuthResponse),
        (status = 400, description = "Invalid request", body = ErrorResponse),
        (status = 409, description = "User already exists", body = ErrorResponse)
    )
)]
#[post("/auth/register")]
async fn register(state: web::Data<AppState>, req: web::Json<RegisterRequest>) -> impl Responder {
    if let Err(e) = req.validate() {
        return HttpResponse::BadRequest().json(ErrorResponse {
            error: format!("Validation error: {}", e),
        });
    }

    let password_hash = match bcrypt::hash(&req.password, bcrypt::DEFAULT_COST) {
        Ok(hash) => hash,
        Err(e) => {
            return HttpResponse::InternalServerError().json(ErrorResponse {
                error: format!("Failed to hash password: {}", e),
            })
        }
    };

    let user = match db_users::create_user(&state.db, &req.email, &req.username, &password_hash)
        .await
    {
        Ok(user) => user,
        Err(sqlx::Error::Database(db_err)) if db_err.is_unique_violation() => {
            return HttpResponse::Conflict().json(ErrorResponse {
                error: "User with this email or username already exists".to_string(),
            })
        }
        Err(e) => {
            return HttpResponse::InternalServerError().json(ErrorResponse {
                error: format!("Failed to create user: {}", e),
            })
        }
    };

    let claims = Claims::new(user.id, user.email.clone(), 24);
    let token = match encode(
        &Header::default(),
        &claims,
        &EncodingKey::from_secret(state.jwt_secret.as_bytes()),
    ) {
        Ok(t) => t,
        Err(e) => {
            return HttpResponse::InternalServerError().json(ErrorResponse {
                error: format!("Failed to generate token: {}", e),
            })
        }
    };

    HttpResponse::Created().json(AuthResponse { token, user })
}
