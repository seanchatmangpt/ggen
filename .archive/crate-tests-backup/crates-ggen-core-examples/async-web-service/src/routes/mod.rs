use actix_web::web;

pub mod auth;
pub mod health;
pub mod users;

pub fn configure(cfg: &mut web::ServiceConfig) {
    cfg.service(
        web::scope("/api")
            .service(
                web::scope("/v1")
                    .configure(health::configure)
                    .configure(auth::configure)
                    .configure(users::configure),
            ),
    );
}
