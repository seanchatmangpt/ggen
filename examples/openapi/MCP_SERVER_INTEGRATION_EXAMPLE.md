# MCP Server Integration Example

This guide shows how to use ggen-generated OpenAPI schemas and Zod validation with an MCP server.

---

## Quick Start

### 1. Import Generated Schemas

```javascript
import {
  commentSchema,
  postSchema,
  tagSchema,
  userSchema,
} from './lib/schemas/entities.mjs';

import {
  createPostRequestSchema,
  updatePostRequestSchema,
  createUserRequestSchema,
  updateUserRequestSchema,
} from './lib/schemas/requests.mjs';
```

### 2. Create MCP Tool Registry

```javascript
class MCPToolRegistry {
  constructor() {
    this.tools = new Map();
    this.handlers = new Map();
  }

  register(tool) {
    if (!tool.name || !tool.description) {
      throw new Error('Tool must have name and description');
    }
    this.tools.set(tool.name, tool);
  }

  registerHandler(toolName, handler) {
    this.handlers.set(toolName, handler);
  }

  async execute(toolName, input) {
    const tool = this.tools.get(toolName);
    if (!tool) {
      throw new Error(`Tool not found: ${toolName}`);
    }

    // Validate input
    if (tool.inputSchema) {
      const validatedInput = tool.inputSchema.parse(input);
      input = validatedInput;
    }

    // Execute handler
    const handler = this.handlers.get(toolName);
    if (!handler) {
      throw new Error(`Handler not found for: ${toolName}`);
    }

    const result = await handler(input);

    // Validate output
    if (tool.outputSchema) {
      return tool.outputSchema.parse(result);
    }

    return result;
  }

  getTool(name) {
    return this.tools.get(name);
  }

  listTools() {
    return Array.from(this.tools.values());
  }
}
```

### 3. Define Tool Handlers

```javascript
const registry = new MCPToolRegistry();

// User Tools
registry.registerHandler('listUsers', async () => {
  // Query database for users
  return [
    { id: 'u1', username: 'alice', email: 'alice@example.com' },
    { id: 'u2', username: 'bob', email: 'bob@example.com' },
  ];
});

registry.registerHandler('getUser', async ({ id }) => {
  // Query user by ID
  return { id, username: 'alice', email: 'alice@example.com' };
});

registry.registerHandler('createUser', async (input) => {
  // Validate with schema
  const validated = createUserRequestSchema.parse(input);
  // Create user in database
  return {
    id: 'u3',
    ...validated,
  };
});

registry.registerHandler('updateUser', async ({ id, ...updates }) => {
  // Validate with schema
  const validated = updateUserRequestSchema.parse(updates);
  // Update user in database
  return {
    id,
    username: 'alice-updated',
    email: 'alice.new@example.com',
    ...validated,
  };
});

registry.registerHandler('deleteUser', async ({ id }) => {
  // Delete user from database
  return { success: true, id };
});

// Post Tools
registry.registerHandler('listPosts', async () => {
  // Query database for posts
  return [
    {
      id: 'p1',
      authorId: 'u1',
      title: 'First Post',
      content: 'Hello World',
      published_at: '2024-01-01T00:00:00Z',
      tags: [],
      comments: [],
    },
  ];
});

registry.registerHandler('getPost', async ({ id }) => {
  // Query post by ID
  return {
    id,
    authorId: 'u1',
    title: 'First Post',
    content: 'Hello World',
    published_at: '2024-01-01T00:00:00Z',
    tags: [{ id: 't1', name: 'rust' }],
    comments: [
      { id: 'c1', authorId: 'u2', content: 'Great post!' },
    ],
  };
});

registry.registerHandler('createPost', async (input) => {
  // Validate with schema
  const validated = createPostRequestSchema.parse(input);
  // Create post in database
  return {
    id: 'p2',
    ...validated,
    published_at: new Date().toISOString(),
    tags: [],
    comments: [],
  };
});

registry.registerHandler('updatePost', async ({ id, ...updates }) => {
  // Validate with schema
  const validated = updatePostRequestSchema.parse(updates);
  // Update post in database
  return {
    id,
    authorId: 'u1',
    title: validated.title || 'First Post',
    content: validated.content || 'Hello World',
    published_at: '2024-01-01T00:00:00Z',
    tags: [],
    comments: [],
  };
});

registry.registerHandler('deletePost', async ({ id }) => {
  // Delete post from database
  return { success: true, id };
});

registry.registerHandler('listUserPosts', async ({ id }) => {
  // Query posts by user ID
  return [
    {
      id: 'p1',
      authorId: id,
      title: 'User Post',
      content: 'User content',
      published_at: '2024-01-01T00:00:00Z',
      tags: [],
      comments: [],
    },
  ];
});
```

### 4. Register Tools from OpenAPI

```javascript
// Define tools derived from OpenAPI spec
const mcpTools = [
  {
    name: 'listUsers',
    description: 'List all users',
    inputSchema: null,
    outputSchema: null, // Could be z.array(userSchema)
  },
  {
    name: 'getUser',
    description: 'Get a user by ID',
    inputSchema: null,
    outputSchema: userSchema,
  },
  {
    name: 'createUser',
    description: 'Create a user',
    inputSchema: createUserRequestSchema,
    outputSchema: userSchema,
  },
  {
    name: 'updateUser',
    description: 'Update a user',
    inputSchema: updateUserRequestSchema,
    outputSchema: userSchema,
  },
  {
    name: 'deleteUser',
    description: 'Delete a user',
    inputSchema: null,
    outputSchema: null,
  },
  {
    name: 'listPosts',
    description: 'List all posts',
    inputSchema: null,
    outputSchema: null, // Could be z.array(postSchema)
  },
  {
    name: 'getPost',
    description: 'Get a post by ID',
    inputSchema: null,
    outputSchema: postSchema,
  },
  {
    name: 'createPost',
    description: 'Create a post',
    inputSchema: createPostRequestSchema,
    outputSchema: postSchema,
  },
  {
    name: 'updatePost',
    description: 'Update a post',
    inputSchema: updatePostRequestSchema,
    outputSchema: postSchema,
  },
  {
    name: 'deletePost',
    description: 'Delete a post',
    inputSchema: null,
    outputSchema: null,
  },
  {
    name: 'listUserPosts',
    description: 'Get user\'s posts',
    inputSchema: null,
    outputSchema: null, // Could be z.array(postSchema)
  },
];

// Register all tools
mcpTools.forEach(tool => registry.register(tool));
```

### 5. Use in MCP Server

```javascript
// Example MCP server integration
class BlogAPIServer {
  constructor(registry) {
    this.registry = registry;
  }

  // MCP: List available tools
  tools() {
    return this.registry.listTools().map(tool => ({
      name: tool.name,
      description: tool.description,
      inputSchema: tool.inputSchema ? {
        type: 'object',
        // Convert Zod schema to JSON schema for MCP
      } : {},
    }));
  }

  // MCP: Call a tool
  async callTool(name, arguments) {
    try {
      const result = await this.registry.execute(name, arguments);
      return {
        type: 'text',
        text: JSON.stringify(result, null, 2),
      };
    } catch (error) {
      return {
        type: 'text',
        text: `Error: ${error.message}`,
        isError: true,
      };
    }
  }
}

const server = new BlogAPIServer(registry);

// Test calling tools
(async () => {
  // Get a user
  const user = await registry.execute('getUser', { id: 'u1' });
  console.log('User:', user);

  // Create a post
  const post = await registry.execute('createPost', {
    authorId: 'u1',
    title: 'New Post',
    content: 'This is a new post',
  });
  console.log('Created post:', post);

  // Update user
  const updated = await registry.execute('updateUser', {
    id: 'u1',
    bio: 'Updated bio',
  });
  console.log('Updated user:', updated);
})();
```

---

## Error Handling

### Validation Errors

```javascript
try {
  await registry.execute('createUser', {
    username: 'alice',
    // Missing email field
  });
} catch (error) {
  if (error.name === 'ZodError') {
    console.error('Validation failed:', error.issues);
    // Output: [{ code: 'invalid_type', path: ['email'], message: '...' }]
  } else {
    console.error('Execution error:', error.message);
  }
}
```

### Type-Safe Error Messages

```javascript
const result = createUserRequestSchema.safeParse(input);
if (!result.success) {
  // result.error contains detailed validation info
  result.error.issues.forEach(issue => {
    console.error(`Field '${issue.path.join('.')}': ${issue.message}`);
  });
} else {
  // result.data is type-safe
  const validatedInput = result.data;
}
```

---

## Advanced Patterns

### Custom Middleware

```javascript
class MiddlewareRegistry extends MCPToolRegistry {
  constructor() {
    super();
    this.middlewares = [];
  }

  use(middleware) {
    this.middlewares.push(middleware);
  }

  async execute(toolName, input) {
    // Run middlewares
    for (const middleware of this.middlewares) {
      await middleware(toolName, input);
    }
    return super.execute(toolName, input);
  }
}

// Add logging middleware
registry.use(async (toolName, input) => {
  console.log(`[${new Date().toISOString()}] Executing ${toolName}`, input);
});

// Add auth middleware
registry.use(async (toolName, input) => {
  if (toolName.startsWith('delete')) {
    // Require special permission
    if (!input.authorized) {
      throw new Error('Not authorized to delete');
    }
  }
});
```

### Batch Tool Execution

```javascript
async function executeBatch(toolCalls) {
  const results = [];
  for (const { name, arguments: args } of toolCalls) {
    try {
      const result = await registry.execute(name, args);
      results.push({ success: true, result });
    } catch (error) {
      results.push({ success: false, error: error.message });
    }
  }
  return results;
}

// Usage
const results = await executeBatch([
  { name: 'createUser', arguments: { username: 'alice', email: 'alice@example.com' } },
  { name: 'createPost', arguments: { authorId: 'u1', title: 'Post', content: 'Content' } },
  { name: 'listPosts', arguments: {} },
]);
```

### Tool Caching

```javascript
class CachedRegistry extends MCPToolRegistry {
  constructor() {
    super();
    this.cache = new Map();
    this.ttl = 5 * 60 * 1000; // 5 minutes
  }

  async execute(toolName, input) {
    const cacheKey = `${toolName}:${JSON.stringify(input)}`;

    const cached = this.cache.get(cacheKey);
    if (cached && Date.now() - cached.time < this.ttl) {
      return cached.result;
    }

    const result = await super.execute(toolName, input);

    this.cache.set(cacheKey, { result, time: Date.now() });
    return result;
  }
}
```

---

## Integration Checklist

- [ ] Import generated schemas
- [ ] Create MCPToolRegistry instance
- [ ] Define tool handlers for each endpoint
- [ ] Register handlers with registry
- [ ] Create tools array from OpenAPI spec
- [ ] Register tools with registry
- [ ] Test tool execution with sample data
- [ ] Add error handling and validation
- [ ] Integrate with MCP server framework
- [ ] Deploy and monitor

---

## Summary

The ggen-generated OpenAPI schemas and Zod validation provide:

✅ **Runtime Validation**: All inputs/outputs validated automatically
✅ **Type Safety**: Full TypeScript support via Zod
✅ **Error Handling**: Clear, actionable error messages
✅ **Tool Discovery**: Automatic from OpenAPI spec
✅ **MCP Ready**: Direct integration with MCP servers
✅ **Extensible**: Middleware, caching, batching patterns included

With this pattern, you can build robust MCP servers that:
- Validate all tool inputs
- Ensure type safety at runtime
- Provide clear error messages
- Discover tools from OpenAPI specs
- Handle concurrent requests safely
