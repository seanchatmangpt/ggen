// Generated GraphQL Schema with Apollo Server
// Supports: Queries, Mutations, Subscriptions, DataLoaders

import {
  GraphQLResolveInfo,
  GraphQLScalarType,
  GraphQLError,
} from 'graphql';
import DataLoader from 'dataloader';
import { PubSub } from 'graphql-subscriptions';
import { Pool } from 'pg';

// ============================================================================
// Type Definitions (SDL)
// ============================================================================

export const typeDefs = `#graphql
  type User {
    id: ID!
    name: String!
    email: String!
    posts(first: Int = 10, after: String): PostConnection!
  }

  type Post {
    id: ID!
    title: String!
    content: String!
    author: User!
  }

  type PostConnection {
    edges: [PostEdge!]!
    pageInfo: PageInfo!
  }

  type PostEdge {
    node: Post!
    cursor: String!
  }

  type PageInfo {
    hasNextPage: Boolean!
    hasPreviousPage: Boolean!
    startCursor: String
    endCursor: String
  }

  input CreateUserInput {
    name: String!
    email: String!
  }

  input UpdateUserInput {
    name: String
    email: String
  }

  type Query {
    user(id: ID!): User
    users(first: Int = 10, after: String): UserConnection!
    post(id: ID!): Post
  }

  type Mutation {
    createUser(input: CreateUserInput!): User! @auth(requires: ADMIN)
    updateUser(id: ID!, input: UpdateUserInput!): User! @auth(requires: USER)
    deleteUser(id: ID!): Boolean! @auth(requires: ADMIN)
  }

  type Subscription {
    userCreated: User!
    postUpdated(userId: ID): Post!
  }

  directive @auth(requires: Role!) on FIELD_DEFINITION

  enum Role {
    ADMIN
    USER
  }
`;

// ============================================================================
// TypeScript Types
// ============================================================================

export interface User {
  id: string;
  name: string;
  email: string;
}

export interface Post {
  id: string;
  title: string;
  content: string;
  authorId: string;
}

export interface Connection<T> {
  edges: Array<{ node: T; cursor: string }>;
  pageInfo: {
    hasNextPage: boolean;
    hasPreviousPage: boolean;
    startCursor?: string;
    endCursor?: string;
  };
}

export interface Context {
  pool: Pool;
  loaders: {
    userById: DataLoader<string, User | null>;
    postsByUser: DataLoader<string, Post[]>;
  };
  pubsub: PubSub;
  user?: {
    id: string;
    role: 'ADMIN' | 'USER';
  };
}

// ============================================================================
// DataLoaders - Prevent N+1 Queries
// ============================================================================

export function createUserByIdLoader(pool: Pool): DataLoader<string, User | null> {
  return new DataLoader(async (ids: readonly string[]) => {
    const result = await pool.query<User>(
      'SELECT id, name, email FROM users WHERE id = ANY($1)',
      [ids]
    );

    const userMap = new Map(result.rows.map(user => [user.id, user]));
    return ids.map(id => userMap.get(id) || null);
  });
}

export function createPostsByUserLoader(pool: Pool): DataLoader<string, Post[]> {
  return new DataLoader(async (userIds: readonly string[]) => {
    const result = await pool.query<Post>(
      'SELECT id, title, content, author_id as "authorId" FROM posts WHERE author_id = ANY($1)',
      [userIds]
    );

    const postsByUser = new Map<string, Post[]>();
    for (const userId of userIds) {
      postsByUser.set(userId, []);
    }

    for (const post of result.rows) {
      postsByUser.get(post.authorId)!.push(post);
    }

    return userIds.map(id => postsByUser.get(id) || []);
  });
}

// ============================================================================
// Authorization Directives
// ============================================================================

export function authDirective(
  next: any,
  _source: any,
  args: { requires: 'ADMIN' | 'USER' },
  context: Context
) {
  if (!context.user) {
    throw new GraphQLError('Not authenticated', {
      extensions: { code: 'UNAUTHENTICATED' },
    });
  }

  if (args.requires === 'ADMIN' && context.user.role !== 'ADMIN') {
    throw new GraphQLError('Insufficient permissions', {
      extensions: { code: 'FORBIDDEN' },
    });
  }

  return next();
}

// ============================================================================
// Resolvers
// ============================================================================

export const resolvers = {
  Query: {
    async user(
      _parent: unknown,
      { id }: { id: string },
      context: Context
    ): Promise<User | null> {
      return context.loaders.userById.load(id);
    },

    async users(
      _parent: unknown,
      { first = 10, after }: { first?: number; after?: string },
      context: Context
    ): Promise<Connection<User>> {
      const limit = Math.min(first, 100);
      const offset = after ? parseInt(Buffer.from(after, 'base64').toString()) : 0;

      const result = await context.pool.query<User>(
        'SELECT id, name, email FROM users ORDER BY id LIMIT $1 OFFSET $2',
        [limit + 1, offset]
      );

      const hasNextPage = result.rows.length > limit;
      const users = hasNextPage ? result.rows.slice(0, -1) : result.rows;

      const edges = users.map((user, index) => ({
        node: user,
        cursor: Buffer.from((offset + index).toString()).toString('base64'),
      }));

      return {
        edges,
        pageInfo: {
          hasNextPage,
          hasPreviousPage: offset > 0,
          startCursor: edges[0]?.cursor,
          endCursor: edges[edges.length - 1]?.cursor,
        },
      };
    },

    async post(
      _parent: unknown,
      { id }: { id: string },
      context: Context
    ): Promise<Post | null> {
      const result = await context.pool.query<Post>(
        'SELECT id, title, content, author_id as "authorId" FROM posts WHERE id = $1',
        [id]
      );
      return result.rows[0] || null;
    },
  },

  Mutation: {
    async createUser(
      _parent: unknown,
      { input }: { input: { name: string; email: string } },
      context: Context
    ): Promise<User> {
      // Validation
      if (!input.name || input.name.length === 0 || input.name.length > 255) {
        throw new GraphQLError('Name must be between 1 and 255 characters');
      }
      if (!input.email.includes('@') || input.email.length > 255) {
        throw new GraphQLError('Invalid email address');
      }

      const result = await context.pool.query<User>(
        'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email',
        [input.name, input.email]
      );

      const user = result.rows[0];

      // Publish event for subscriptions
      context.pubsub.publish('USER_CREATED', { userCreated: user });

      return user;
    },

    async updateUser(
      _parent: unknown,
      { id, input }: { id: string; input: { name?: string; email?: string } },
      context: Context
    ): Promise<User> {
      const updates: string[] = [];
      const values: any[] = [];
      let paramIndex = 1;

      if (input.name !== undefined) {
        updates.push(`name = $${paramIndex++}`);
        values.push(input.name);
      }
      if (input.email !== undefined) {
        updates.push(`email = $${paramIndex++}`);
        values.push(input.email);
      }

      values.push(id);

      const result = await context.pool.query<User>(
        `UPDATE users SET ${updates.join(', ')} WHERE id = $${paramIndex} RETURNING id, name, email`,
        values
      );

      if (result.rows.length === 0) {
        throw new GraphQLError('User not found');
      }

      return result.rows[0];
    },

    async deleteUser(
      _parent: unknown,
      { id }: { id: string },
      context: Context
    ): Promise<boolean> {
      const result = await context.pool.query(
        'DELETE FROM users WHERE id = $1',
        [id]
      );
      return result.rowCount ? result.rowCount > 0 : false;
    },
  },

  Subscription: {
    userCreated: {
      subscribe: (_parent: unknown, _args: unknown, context: Context) =>
        context.pubsub.asyncIterator(['USER_CREATED']),
    },

    postUpdated: {
      subscribe: (
        _parent: unknown,
        { userId }: { userId?: string },
        context: Context
      ) => {
        return context.pubsub.asyncIterator(['POST_UPDATED']);
      },
      resolve: (payload: { postUpdated: Post }, { userId }: { userId?: string }) => {
        if (userId && payload.postUpdated.authorId !== userId) {
          return null;
        }
        return payload.postUpdated;
      },
    },
  },

  User: {
    async posts(
      parent: User,
      { first = 10, after }: { first?: number; after?: string },
      context: Context
    ): Promise<Connection<Post>> {
      const posts = await context.loaders.postsByUser.load(parent.id);

      const limit = Math.min(first, 100);
      const offset = after ? parseInt(Buffer.from(after, 'base64').toString()) : 0;

      const hasNextPage = posts.length > offset + limit;
      const paginatedPosts = posts.slice(offset, offset + limit);

      const edges = paginatedPosts.map((post, index) => ({
        node: post,
        cursor: Buffer.from((offset + index).toString()).toString('base64'),
      }));

      return {
        edges,
        pageInfo: {
          hasNextPage,
          hasPreviousPage: offset > 0,
          startCursor: edges[0]?.cursor,
          endCursor: edges[edges.length - 1]?.cursor,
        },
      };
    },
  },

  Post: {
    async author(parent: Post, _args: unknown, context: Context): Promise<User> {
      const user = await context.loaders.userById.load(parent.authorId);
      if (!user) {
        throw new GraphQLError('Author not found');
      }
      return user;
    },
  },
};

// ============================================================================
// Context Factory
// ============================================================================

export function createContext(pool: Pool, pubsub: PubSub, user?: Context['user']): Context {
  return {
    pool,
    loaders: {
      userById: createUserByIdLoader(pool),
      postsByUser: createPostsByUserLoader(pool),
    },
    pubsub,
    user,
  };
}
