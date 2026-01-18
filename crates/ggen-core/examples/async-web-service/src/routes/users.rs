use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use uuid::Uuid;
use validator::Validate;

use crate::db::users as db_users;
use crate::middleware::auth::AuthenticatedUser;
use crate::models::{CreateUserRequest, ErrorResponse, UpdateUserRequest, User};
use crate::AppState;

pub fn configure(cfg: &mut web::ServiceConfig) {
    cfg.service(get_users)
        .service(get_user)
        .service(create_user)
        .service(update_user)
        .service(delete_user);
}

/// Get all users (paginated)
#[utoipa::path(
    get,
    path = "/api/v1/users",
    tag = "users",
    params(
        ("page" = Option<i64>, Query, description = "Page number"),
        ("per_page" = Option<i64>, Query, description = "Items per page")
    ),
    responses(
        (status = 200, description = "List of users", body = Vec<User>)
    ),
    security(
        ("bearer_auth" = [])
    )
)]
#[get("/users")]
async fn get_users(
    state: web::Data<AppState>,
    _auth: AuthenticatedUser,
    query: web::Query<PaginationParams>,
) -> impl Responder {
    let page = query.page.unwrap_or(1).max(1);
    let per_page = query.per_page.unwrap_or(10).min(100);
    let offset = (page - 1) * per_page;

    match db_users::get_users(&state.db, per_page, offset).await {
        Ok(users) => HttpResponse::Ok().json(users),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: format!("Failed to fetch users: {}", e),
        }),
    }
}

#[derive(serde::Deserialize)]
struct PaginationParams {
    page: Option<i64>,
    per_page: Option<i64>,
}

/// Get user by ID
#[utoipa::path(
    get,
    path = "/api/v1/users/{id}",
    tag = "users",
    params(
        ("id" = Uuid, Path, description = "User ID")
    ),
    responses(
        (status = 200, description = "User found", body = User),
        (status = 404, description = "User not found", body = ErrorResponse)
    ),
    security(
        ("bearer_auth" = [])
    )
)]
#[get("/users/{id}")]
async fn get_user(
    state: web::Data<AppState>,
    _auth: AuthenticatedUser,
    id: web::Path<Uuid>,
) -> impl Responder {
    match db_users::get_user_by_id(&state.db, *id).await {
        Ok(Some(user)) => HttpResponse::Ok().json(user),
        Ok(None) => HttpResponse::NotFound().json(ErrorResponse {
            error: "User not found".to_string(),
        }),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: format!("Database error: {}", e),
        }),
    }
}

/// Create new user
#[utoipa::path(
    post,
    path = "/api/v1/users",
    tag = "users",
    request_body = CreateUserRequest,
    responses(
        (status = 201, description = "User created", body = User),
        (status = 400, description = "Invalid request", body = ErrorResponse),
        (status = 409, description = "User already exists", body = ErrorResponse)
    )
)]
#[post("/users")]
async fn create_user(
    state: web::Data<AppState>,
    req: web::Json<CreateUserRequest>,
) -> impl Responder {
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

    match db_users::create_user(&state.db, &req.email, &req.username, &password_hash).await {
        Ok(user) => HttpResponse::Created().json(user),
        Err(sqlx::Error::Database(db_err)) if db_err.is_unique_violation() => {
            HttpResponse::Conflict().json(ErrorResponse {
                error: "User with this email or username already exists".to_string(),
            })
        }
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: format!("Failed to create user: {}", e),
        }),
    }
}

/// Update user
#[utoipa::path(
    put,
    path = "/api/v1/users/{id}",
    tag = "users",
    params(
        ("id" = Uuid, Path, description = "User ID")
    ),
    request_body = UpdateUserRequest,
    responses(
        (status = 200, description = "User updated", body = User),
        (status = 404, description = "User not found", body = ErrorResponse)
    ),
    security(
        ("bearer_auth" = [])
    )
)]
#[put("/users/{id}")]
async fn update_user(
    state: web::Data<AppState>,
    auth: AuthenticatedUser,
    id: web::Path<Uuid>,
    req: web::Json<UpdateUserRequest>,
) -> impl Responder {
    if auth.user_id != *id {
        return HttpResponse::Forbidden().json(ErrorResponse {
            error: "Cannot update another user's profile".to_string(),
        });
    }

    if let Err(e) = req.validate() {
        return HttpResponse::BadRequest().json(ErrorResponse {
            error: format!("Validation error: {}", e),
        });
    }

    match db_users::update_user(&state.db, *id, req.email.as_deref(), req.username.as_deref())
        .await
    {
        Ok(Some(user)) => HttpResponse::Ok().json(user),
        Ok(None) => HttpResponse::NotFound().json(ErrorResponse {
            error: "User not found".to_string(),
        }),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: format!("Failed to update user: {}", e),
        }),
    }
}

/// Delete user
#[utoipa::path(
    delete,
    path = "/api/v1/users/{id}",
    tag = "users",
    params(
        ("id" = Uuid, Path, description = "User ID")
    ),
    responses(
        (status = 204, description = "User deleted"),
        (status = 404, description = "User not found", body = ErrorResponse)
    ),
    security(
        ("bearer_auth" = [])
    )
)]
#[delete("/users/{id}")]
async fn delete_user(
    state: web::Data<AppState>,
    auth: AuthenticatedUser,
    id: web::Path<Uuid>,
) -> impl Responder {
    if auth.user_id != *id {
        return HttpResponse::Forbidden().json(ErrorResponse {
            error: "Cannot delete another user".to_string(),
        });
    }

    match db_users::delete_user(&state.db, *id).await {
        Ok(true) => HttpResponse::NoContent().finish(),
        Ok(false) => HttpResponse::NotFound().json(ErrorResponse {
            error: "User not found".to_string(),
        }),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: format!("Failed to delete user: {}", e),
        }),
    }
}
