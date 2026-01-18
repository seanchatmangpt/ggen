use actix_web::{dev::Payload, error::ErrorUnauthorized, Error, FromRequest, HttpRequest};
use futures::future::{ready, Ready};
use jsonwebtoken::{decode, DecodingKey, Validation};
use uuid::Uuid;

use crate::models::Claims;

/// Authenticated user extractor for protected endpoints
pub struct AuthenticatedUser {
    pub user_id: Uuid,
    pub email: String,
}

impl FromRequest for AuthenticatedUser {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, _: &mut Payload) -> Self::Future {
        let auth_header = match req.headers().get("Authorization") {
            Some(header) => header,
            None => return ready(Err(ErrorUnauthorized("Missing authorization header"))),
        };

        let auth_str = match auth_header.to_str() {
            Ok(s) => s,
            Err(_) => return ready(Err(ErrorUnauthorized("Invalid authorization header"))),
        };

        if !auth_str.starts_with("Bearer ") {
            return ready(Err(ErrorUnauthorized("Invalid authorization format")));
        }

        let token = &auth_str[7..];

        let app_state = match req.app_data::<actix_web::web::Data<std::sync::Arc<crate::AppState>>>()
        {
            Some(data) => data,
            None => return ready(Err(ErrorUnauthorized("Server configuration error"))),
        };

        let token_data = match decode::<Claims>(
            token,
            &DecodingKey::from_secret(app_state.jwt_secret.as_bytes()),
            &Validation::default(),
        ) {
            Ok(data) => data,
            Err(_) => return ready(Err(ErrorUnauthorized("Invalid token"))),
        };

        let user_id = match Uuid::parse_str(&token_data.claims.sub) {
            Ok(id) => id,
            Err(_) => return ready(Err(ErrorUnauthorized("Invalid user ID in token"))),
        };

        ready(Ok(AuthenticatedUser {
            user_id,
            email: token_data.claims.email,
        }))
    }
}
