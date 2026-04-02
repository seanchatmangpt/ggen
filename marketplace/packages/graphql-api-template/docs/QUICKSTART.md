# GraphQL API Template - Quick Start

Get a production-ready GraphQL API running in minutes.

## Choose Your Language

### TypeScript (Recommended for Node.js)

```bash
# Install dependencies
npm install @apollo/server graphql dataloader pg graphql-subscriptions

# Copy template
cp templates/typescript/schema.ts ./src/

# Create server
cat > src/server.ts <<'EOF'
import { ApolloServer } from '@apollo/server';
import { startStandaloneServer } from '@apollo/server/standalone';
import { Pool } from 'pg';
import { PubSub } from 'graphql-subscriptions';
import { typeDefs, resolvers, createContext } from './schema';

const pool = new Pool({
  connectionString: process.env.DATABASE_URL || 'postgresql://localhost/myapp'
});

const pubsub = new PubSub();

const server = new ApolloServer({
  typeDefs,
  resolvers,
});

const { url } = await startStandaloneServer(server, {
  context: async ({ req }) => {
    // TODO: Add authentication
    const user = { id: '1', role: 'USER' };
    return createContext(pool, pubsub, user);
  },
  listen: { port: 4000 },
});

console.log(`🚀 GraphQL server ready at ${url}`);
EOF

# Run
npx tsx src/server.ts
```

### Python (Recommended for async Python)

```bash
# Install dependencies
pip install strawberry-graphql[fastapi] asyncpg uvicorn

# Copy template
cp templates/python/schema.py ./

# Create server
cat > app.py <<'EOF'
from fastapi import FastAPI
from strawberry.fastapi import GraphQLRouter
import asyncpg
from schema import schema, create_context, SimplePubSub

app = FastAPI()

pool = None
pubsub = SimplePubSub()

@app.on_event("startup")
async def startup():
    global pool
    pool = await asyncpg.create_pool(
        "postgresql://localhost/myapp"
    )

async def get_context():
    # TODO: Add authentication
    user = {"id": "1", "role": "USER"}
    return create_context(pool, pubsub, user)

graphql_app = GraphQLRouter(
    schema,
    context_getter=get_context,
)

app.include_router(graphql_app, prefix="/graphql")
EOF

# Run
uvicorn app:app --reload
```

### Rust (Recommended for high performance)

```bash
# Add dependencies to Cargo.toml
cat >> Cargo.toml <<'EOF'
[dependencies]
async-graphql = "7.0"
async-graphql-axum = "7.0"
axum = "0.7"
sqlx = { version = "0.7", features = ["postgres", "runtime-tokio"] }
tokio = { version = "1", features = ["full"] }
EOF

# Copy template
cp templates/rust/schema.rs ./src/

# Create main.rs
cat > src/main.rs <<'EOF'
use async_graphql::http::{GraphQLPlaygroundConfig, playground_source};
use async_graphql_axum::{GraphQLRequest, GraphQLResponse};
use axum::{
    response::{Html, IntoResponse},
    routing::get,
    Router,
};
use sqlx::postgres::PgPoolOptions;

mod schema;
use schema::build_schema;

async fn graphql_playground() -> impl IntoResponse {
    Html(playground_source(GraphQLPlaygroundConfig::new("/")))
}

async fn graphql_handler(
    schema: async_graphql::Schema<schema::QueryRoot, schema::MutationRoot, schema::SubscriptionRoot>,
    req: GraphQLRequest,
) -> GraphQLResponse {
    schema.execute(req.into_inner()).await.into()
}

#[tokio::main]
async fn main() {
    let pool = PgPoolOptions::new()
        .max_connections(5)
        .connect("postgresql://localhost/myapp")
        .await
        .expect("Failed to connect to database");

    let schema = build_schema(pool);

    let app = Router::new()
        .route("/", get(graphql_playground).post(move |req| graphql_handler(schema.clone(), req)));

    println!("🚀 GraphQL server ready at http://localhost:4000");

    axum::Server::bind(&"0.0.0.0:4000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
EOF

# Run
cargo run
```

## Database Setup

All implementations require PostgreSQL:

```sql
-- Create database
CREATE DATABASE myapp;

-- Connect to database
\c myapp

-- Create tables
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE posts (
    id SERIAL PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    content TEXT NOT NULL,
    author_id INTEGER REFERENCES users(id) ON DELETE CASCADE,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Insert sample data
INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');

INSERT INTO posts (title, content, author_id) VALUES
    ('First Post', 'Hello World!', 1),
    ('Second Post', 'GraphQL is awesome', 1),
    ('Third Post', 'Testing subscriptions', 2);
```

## Test Your API

### Using GraphQL Playground

Visit `http://localhost:4000` and try:

```graphql
# Query users with their posts
query {
  users(first: 10) {
    edges {
      node {
        id
        name
        email
        posts(first: 5) {
          edges {
            node {
              title
              content
            }
          }
        }
      }
    }
  }
}

# Create a user (requires ADMIN role)
mutation {
  createUser(input: {
    name: "Charlie"
    email: "charlie@example.com"
  }) {
    id
    name
    email
  }
}

# Subscribe to new users (WebSocket)
subscription {
  userCreated {
    id
    name
    email
  }
}
```

### Using curl

```bash
# Query
curl -X POST http://localhost:4000/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ users(first: 10) { edges { node { name email } } } }"}'

# Mutation
curl -X POST http://localhost:4000/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "mutation { createUser(input: { name: \"Test\", email: \"test@example.com\" }) { id name } }"}'
```

## Add Authentication

### TypeScript

```typescript
import jwt from 'jsonwebtoken';

const server = new ApolloServer({
  typeDefs,
  resolvers,
});

await startStandaloneServer(server, {
  context: async ({ req }) => {
    const token = req.headers.authorization?.replace('Bearer ', '');

    if (!token) {
      return createContext(pool, pubsub, null);
    }

    try {
      const decoded = jwt.verify(token, process.env.JWT_SECRET);
      const user = { id: decoded.sub, role: decoded.role };
      return createContext(pool, pubsub, user);
    } catch (err) {
      return createContext(pool, pubsub, null);
    }
  },
});
```

### Python

```python
from jose import jwt
from fastapi import Request

async def get_context(request: Request):
    token = request.headers.get("authorization", "").replace("Bearer ", "")

    if not token:
        return create_context(pool, pubsub, None)

    try:
        payload = jwt.decode(token, SECRET_KEY, algorithms=["HS256"])
        user = {"id": payload["sub"], "role": payload["role"]}
        return create_context(pool, pubsub, user)
    except:
        return create_context(pool, pubsub, None)
```

## Performance Tips

1. **Use DataLoaders** (already included)
   - Prevents N+1 queries automatically
   - Batches database calls

2. **Add Query Complexity Limits**
   ```typescript
   const server = new ApolloServer({
     typeDefs,
     resolvers,
     plugins: [
       ApolloServerPluginLandingPageGraphQLPlayground(),
       {
         requestDidStart: () => ({
           didResolveOperation({ request, document }) {
             const complexity = calculateComplexity({
               schema,
               query: document,
               variables: request.variables
             });
             if (complexity > 1000) {
               throw new Error('Query is too complex');
             }
           }
         })
       }
     ]
   });
   ```

3. **Add Caching**
   ```typescript
   import { KeyvAdapter } from '@apollo/utils.keyvadapter';
   import Keyv from 'keyv';

   const server = new ApolloServer({
     typeDefs,
     resolvers,
     cache: new KeyvAdapter(new Keyv('redis://localhost:6379')),
   });
   ```

4. **Enable Compression**
   ```typescript
   import compression from 'compression';
   app.use(compression());
   ```

## Production Checklist

- [ ] Add authentication/authorization
- [ ] Set up error logging (Sentry, etc.)
- [ ] Configure CORS properly
- [ ] Add rate limiting
- [ ] Set up monitoring (Prometheus, DataDog)
- [ ] Configure query complexity limits
- [ ] Add request validation
- [ ] Set up Redis for PubSub (multi-server)
- [ ] Configure connection pooling
- [ ] Add health check endpoint
- [ ] Set up CI/CD pipeline
- [ ] Configure environment variables
- [ ] Add API documentation
- [ ] Set up load balancing
- [ ] Configure SSL/TLS

## Next Steps

1. Review the [complete documentation](README.md)
2. Understand [resolver patterns](RESOLVERS.md)
3. Learn about [subscriptions](SUBSCRIPTIONS.md)
4. Run the test suite
5. Customize for your use case

## Support

- Report issues: [GitHub Issues](https://github.com/your-repo/issues)
- Documentation: See `/docs` folder
- Examples: See test suite for usage examples

## License

MIT
