// Generated GraphQL Schema with async-graphql
// Supports: Queries, Mutations, Subscriptions, DataLoaders

use async_graphql::*;
use async_graphql::dataloader::{DataLoader, Loader};
use sqlx::PgPool;
use std::collections::HashMap;
use futures::stream::Stream;

// ============================================================================
// Domain Models
// ============================================================================

#[derive(Clone, Debug)]
pub struct User {
    pub id: ID,
    pub name: String,
    pub email: String,
}

#[derive(Clone, Debug)]
pub struct Post {
    pub id: ID,
    pub title: String,
    pub content: String,
    pub author_id: ID,
}

// ============================================================================
// GraphQL Object Types
// ============================================================================

#[Object]
impl User {
    async fn id(&self) -> &ID {
        &self.id
    }

    async fn name(&self) -> &str {
        &self.name
    }

    async fn email(&self) -> &str {
        &self.email
    }

    #[graphql(
        guard = "RoleGuard::new(Role::User)",
        complexity = "posts_first * child_complexity"
    )]
    async fn posts(
        &self,
        ctx: &Context<'_>,
        first: Option<i32>,
        after: Option<String>,
    ) -> Result<Connection<String, Post>> {
        let loader = ctx.data_unchecked::<DataLoader<PostsByUserLoader>>();
        let posts = loader.load_one(self.id.clone()).await?
            .unwrap_or_default();

        let mut connection = Connection::new(false, false);
        for post in posts {
            connection.edges.push(Edge::new(
                format!("cursor_{}", post.id),
                post,
            ));
        }
        Ok(connection)
    }
}

#[Object]
impl Post {
    async fn id(&self) -> &ID {
        &self.id
    }

    async fn title(&self) -> &str {
        &self.title
    }

    async fn content(&self) -> &str {
        &self.content
    }

    async fn author(&self, ctx: &Context<'_>) -> Result<User> {
        let loader = ctx.data_unchecked::<DataLoader<UserByIdLoader>>();
        loader.load_one(self.author_id.clone()).await?
            .ok_or_else(|| Error::new("User not found"))
    }
}

// ============================================================================
// DataLoaders - Prevent N+1 Queries
// ============================================================================

pub struct UserByIdLoader {
    pool: PgPool,
}

#[async_trait::async_trait]
impl Loader<ID> for UserByIdLoader {
    type Value = User;
    type Error = async_graphql::Error;

    async fn load(&self, keys: &[ID]) -> Result<HashMap<ID, Self::Value>, Self::Error> {
        let ids: Vec<i32> = keys.iter()
            .filter_map(|id| id.parse::<i32>().ok())
            .collect();

        let users = sqlx::query_as::<_, (i32, String, String)>(
            "SELECT id, name, email FROM users WHERE id = ANY($1)"
        )
        .bind(&ids)
        .fetch_all(&self.pool)
        .await
        .map_err(|e| Error::new(format!("Database error: {}", e)))?;

        Ok(users.into_iter()
            .map(|(id, name, email)| {
                (ID::from(id), User {
                    id: ID::from(id),
                    name,
                    email,
                })
            })
            .collect())
    }
}

pub struct PostsByUserLoader {
    pool: PgPool,
}

#[async_trait::async_trait]
impl Loader<ID> for PostsByUserLoader {
    type Value = Vec<Post>;
    type Error = async_graphql::Error;

    async fn load(&self, keys: &[ID]) -> Result<HashMap<ID, Self::Value>, Self::Error> {
        let user_ids: Vec<i32> = keys.iter()
            .filter_map(|id| id.parse::<i32>().ok())
            .collect();

        let posts = sqlx::query_as::<_, (i32, String, String, i32)>(
            "SELECT id, title, content, author_id FROM posts WHERE author_id = ANY($1)"
        )
        .bind(&user_ids)
        .fetch_all(&self.pool)
        .await
        .map_err(|e| Error::new(format!("Database error: {}", e)))?;

        let mut result: HashMap<ID, Vec<Post>> = HashMap::new();
        for (id, title, content, author_id) in posts {
            result.entry(ID::from(author_id))
                .or_insert_with(Vec::new)
                .push(Post {
                    id: ID::from(id),
                    title,
                    content,
                    author_id: ID::from(author_id),
                });
        }
        Ok(result)
    }
}

// ============================================================================
// Input Types
// ============================================================================

#[derive(InputObject, Validate)]
pub struct CreateUserInput {
    #[validate(length(min = 1, max = 255))]
    pub name: String,

    #[validate(email, length(max = 255))]
    pub email: String,
}

#[derive(InputObject)]
pub struct UpdateUserInput {
    pub name: Option<String>,
    pub email: Option<String>,
}

// ============================================================================
// Authorization Guards
// ============================================================================

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum Role {
    Admin,
    User,
}

pub struct RoleGuard {
    role: Role,
}

impl RoleGuard {
    pub fn new(role: Role) -> Self {
        Self { role }
    }
}

#[async_trait::async_trait]
impl Guard for RoleGuard {
    async fn check(&self, ctx: &Context<'_>) -> Result<()> {
        let user_role = ctx.data_opt::<Role>()
            .ok_or_else(|| Error::new("Not authenticated"))?;

        if *user_role == self.role || *user_role == Role::Admin {
            Ok(())
        } else {
            Err(Error::new("Insufficient permissions"))
        }
    }
}

// ============================================================================
// Query Root
// ============================================================================

pub struct QueryRoot;

#[Object]
impl QueryRoot {
    async fn user(&self, ctx: &Context<'_>, id: ID) -> Result<Option<User>> {
        let loader = ctx.data_unchecked::<DataLoader<UserByIdLoader>>();
        loader.load_one(id).await
    }

    async fn users(
        &self,
        ctx: &Context<'_>,
        first: Option<i32>,
        after: Option<String>,
    ) -> Result<Connection<String, User>> {
        let pool = ctx.data_unchecked::<PgPool>();
        let limit = first.unwrap_or(10).min(100);

        let users = sqlx::query_as::<_, (i32, String, String)>(
            "SELECT id, name, email FROM users LIMIT $1"
        )
        .bind(limit)
        .fetch_all(pool)
        .await
        .map_err(|e| Error::new(format!("Database error: {}", e)))?;

        let mut connection = Connection::new(false, users.len() == limit as usize);
        for (id, name, email) in users {
            connection.edges.push(Edge::new(
                format!("cursor_{}", id),
                User {
                    id: ID::from(id),
                    name,
                    email,
                },
            ));
        }
        Ok(connection)
    }
}

// ============================================================================
// Mutation Root
// ============================================================================

pub struct MutationRoot;

#[Object]
impl MutationRoot {
    #[graphql(guard = "RoleGuard::new(Role::Admin)")]
    async fn create_user(
        &self,
        ctx: &Context<'_>,
        input: CreateUserInput,
    ) -> Result<User> {
        input.validate()
            .map_err(|e| Error::new(format!("Validation error: {}", e)))?;

        let pool = ctx.data_unchecked::<PgPool>();
        let (id, name, email) = sqlx::query_as::<_, (i32, String, String)>(
            "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
        )
        .bind(&input.name)
        .bind(&input.email)
        .fetch_one(pool)
        .await
        .map_err(|e| Error::new(format!("Database error: {}", e)))?;

        // Publish event for subscriptions
        let pubsub = ctx.data_unchecked::<SimpleBroker<UserEvent>>();
        SimpleBroker::publish(UserEvent::Created(User {
            id: ID::from(id),
            name: name.clone(),
            email: email.clone(),
        }));

        Ok(User {
            id: ID::from(id),
            name,
            email,
        })
    }

    #[graphql(guard = "RoleGuard::new(Role::User)")]
    async fn update_user(
        &self,
        ctx: &Context<'_>,
        id: ID,
        input: UpdateUserInput,
    ) -> Result<User> {
        let pool = ctx.data_unchecked::<PgPool>();

        // Update only provided fields
        let mut query = String::from("UPDATE users SET ");
        let mut params = Vec::new();

        if let Some(name) = input.name {
            params.push(format!("name = '{}'", name));
        }
        if let Some(email) = input.email {
            params.push(format!("email = '{}'", email));
        }

        query.push_str(&params.join(", "));
        query.push_str(&format!(" WHERE id = {} RETURNING id, name, email", id));

        let (id, name, email) = sqlx::query_as::<_, (i32, String, String)>(&query)
            .fetch_one(pool)
            .await
            .map_err(|e| Error::new(format!("Database error: {}", e)))?;

        Ok(User {
            id: ID::from(id),
            name,
            email,
        })
    }

    #[graphql(guard = "RoleGuard::new(Role::Admin)")]
    async fn delete_user(&self, ctx: &Context<'_>, id: ID) -> Result<bool> {
        let pool = ctx.data_unchecked::<PgPool>();

        let result = sqlx::query("DELETE FROM users WHERE id = $1")
            .bind(id.parse::<i32>().map_err(|_| Error::new("Invalid ID"))?)
            .execute(pool)
            .await
            .map_err(|e| Error::new(format!("Database error: {}", e)))?;

        Ok(result.rows_affected() > 0)
    }
}

// ============================================================================
// Subscription Root
// ============================================================================

#[derive(Clone)]
pub enum UserEvent {
    Created(User),
    Updated(User),
    Deleted(ID),
}

pub struct SubscriptionRoot;

#[Subscription]
impl SubscriptionRoot {
    #[graphql(guard = "RoleGuard::new(Role::User)")]
    async fn user_created(&self) -> impl Stream<Item = User> {
        SimpleBroker::<UserEvent>::subscribe()
            .filter_map(|event| async move {
                match event {
                    UserEvent::Created(user) => Some(user),
                    _ => None,
                }
            })
    }

    async fn post_updated(&self, user_id: Option<ID>) -> impl Stream<Item = Post> {
        SimpleBroker::<Post>::subscribe()
            .filter(move |post| {
                let matches = user_id.as_ref()
                    .map(|id| &post.author_id == id)
                    .unwrap_or(true);
                async move { matches }
            })
    }
}

// ============================================================================
// Schema Builder
// ============================================================================

pub type AppSchema = Schema<QueryRoot, MutationRoot, SubscriptionRoot>;

pub fn build_schema(pool: PgPool) -> AppSchema {
    Schema::build(QueryRoot, MutationRoot, SubscriptionRoot)
        .data(pool.clone())
        .data(DataLoader::new(
            UserByIdLoader { pool: pool.clone() },
            tokio::spawn,
        ))
        .data(DataLoader::new(
            PostsByUserLoader { pool },
            tokio::spawn,
        ))
        .finish()
}
