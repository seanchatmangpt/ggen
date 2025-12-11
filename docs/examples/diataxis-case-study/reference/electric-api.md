# Reference: ElectricSQL API Documentation

Complete API reference for ElectricSQL local-first sync.

---

## Installation

```bash
npm install electric-sql wa-sqlite
```

---

## Core API

### `electrify(config)`

Initialize ElectricSQL connection.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `config` | `Object` | Yes | Configuration object |
| `config.appName` | `string` | Yes | Application name |
| `config.url` | `string` | No | Electric server URL (default: local-only) |
| `config.token` | `string` | No | Authentication token |

**Returns:** `Promise<ElectricClient>`

**Example:**

```javascript
import { electrify } from 'electric-sql/wa-sqlite';

const electric = await electrify({
  appName: 'my-app',
  url: 'ws://localhost:5133',
  token: 'eyJhbGc...',
});
```

**Errors:**

- `ConfigError` - Invalid configuration
- `ConnectionError` - Cannot connect to server
- `AuthError` - Invalid authentication token

---

### `ElectricClient`

Main client instance returned by `electrify()`.

**Properties:**

| Property | Type | Description |
|----------|------|-------------|
| `db` | `Database` | SQLite database instance |
| `sync` | `SyncEngine` | Sync engine instance |
| `isConnected` | `boolean` | Connection status |

**Example:**

```javascript
const { db, sync, isConnected } = await electrify(config);

console.log('Connected:', isConnected);
```

---

## Database API

### `db.exec(sql, params?)`

Execute SQL statement.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `sql` | `string` | Yes | SQL statement |
| `params` | `Array` | No | Bind parameters |

**Returns:** `Promise<QueryResult[]>`

**Example:**

```javascript
// No parameters
await db.exec('CREATE TABLE users (id TEXT PRIMARY KEY, name TEXT)');

// With parameters
await db.exec(
  'INSERT INTO users (id, name) VALUES (?, ?)',
  ['123', 'Alice']
);

// Select query
const result = await db.exec('SELECT * FROM users WHERE id = ?', ['123']);
console.log(result[0].values);  // [['123', 'Alice']]
```

**QueryResult:**

```javascript
{
  columns: string[],      // Column names
  values: any[][],        // Rows (array of arrays)
}
```

---

### `db.liveQuery(sql, params, callback)`

Subscribe to live query (reactive).

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `sql` | `string` | Yes | SQL SELECT statement |
| `params` | `Array` | Yes | Bind parameters |
| `callback` | `Function` | Yes | Called when data changes |

**Returns:** `{ unsubscribe: Function }`

**Example:**

```javascript
const { unsubscribe } = db.liveQuery(
  'SELECT * FROM todos WHERE completed = ?',
  [false],
  (result) => {
    const todos = result[0]?.values || [];
    console.log('Active todos:', todos.length);
  }
);

// Later: cleanup
unsubscribe();
```

**Callback signature:**

```javascript
/**
 * @param {QueryResult[]} results - Query results
 */
function callback(results) {
  // Called initially and whenever data changes
}
```

---

### `db.transaction(callback)`

Execute multiple statements atomically.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `callback` | `Function` | Yes | Transaction callback |

**Returns:** `Promise<any>` (callback return value)

**Example:**

```javascript
await db.transaction(async (tx) => {
  await tx.exec('INSERT INTO users (id, name) VALUES (?, ?)', ['1', 'Alice']);
  await tx.exec('INSERT INTO posts (user_id, text) VALUES (?, ?)', ['1', 'Hello']);

  // If error thrown here, both inserts rolled back

  return 'success';
});
```

---

## Sync API

### `sync.syncTables(tables)`

Start syncing specified tables.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `tables` | `string[]` | Yes | Table names to sync |

**Returns:** `Promise<SyncShape>`

**Example:**

```javascript
const shape = await sync.syncTables(['todos', 'users']);

// Wait for initial sync
await shape.synced;

console.log('Tables synced!');
```

---

### `SyncShape`

Represents a sync subscription.

**Properties:**

| Property | Type | Description |
|----------|------|-------------|
| `synced` | `Promise<void>` | Resolves when initial sync completes |
| `isActive` | `boolean` | Sync is active |
| `unsubscribe` | `Function` | Stop syncing |

**Example:**

```javascript
const shape = await sync.syncTables(['todos']);

// Wait for sync
await shape.synced;

// Stop syncing
shape.unsubscribe();
```

---

### `sync.subscribe(callback)`

Subscribe to sync events.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `callback` | `Function` | Yes | Event callback |

**Returns:** `{ unsubscribe: Function }`

**Events:**

| Event | Payload | Description |
|-------|---------|-------------|
| `connected` | `null` | Connected to server |
| `disconnected` | `null` | Disconnected from server |
| `syncing` | `{ table: string }` | Sync started |
| `synced` | `{ table: string }` | Sync completed |
| `error` | `{ error: Error }` | Sync error |

**Example:**

```javascript
const { unsubscribe } = sync.subscribe((event) => {
  console.log('Sync event:', event.type, event.payload);
});

// Later
unsubscribe();
```

---

## Schema API

### `db.getSchema()`

Get database schema information.

**Returns:** `Promise<SchemaInfo>`

**Example:**

```javascript
const schema = await db.getSchema();

console.log('Tables:', schema.tables);
// ['todos', 'users']

console.log('Columns:', schema.tables.todos.columns);
// [{ name: 'id', type: 'TEXT', ... }]
```

**SchemaInfo:**

```javascript
{
  tables: {
    [tableName]: {
      columns: [
        {
          name: string,
          type: string,
          nullable: boolean,
          primaryKey: boolean,
        }
      ],
      indexes: string[],
    }
  }
}
```

---

## Migration API

### `db.migrate(migrations)`

Apply database migrations.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `migrations` | `Migration[]` | Yes | Migration definitions |

**Returns:** `Promise<void>`

**Migration object:**

```javascript
{
  version: number,     // Migration version
  name: string,        // Migration name
  up: string,          // SQL to apply
  down: string,        // SQL to rollback (optional)
}
```

**Example:**

```javascript
await db.migrate([
  {
    version: 1,
    name: 'create_todos',
    up: 'CREATE TABLE todos (id TEXT PRIMARY KEY, text TEXT)',
    down: 'DROP TABLE todos',
  },
  {
    version: 2,
    name: 'add_completed',
    up: 'ALTER TABLE todos ADD COLUMN completed INTEGER DEFAULT 0',
  },
]);
```

---

## Utility API

### `generateUUID()`

Generate UUID v4.

**Returns:** `string`

**Example:**

```javascript
import { generateUUID } from 'electric-sql/util';

const id = generateUUID();
// '123e4567-e89b-12d3-a456-426614174000'
```

---

### `isElectricConnected()`

Check if connected to Electric server.

**Returns:** `boolean`

**Example:**

```javascript
import { isElectricConnected } from 'electric-sql/util';

if (isElectricConnected()) {
  console.log('Online: syncing to server');
} else {
  console.log('Offline: local-only mode');
}
```

---

## Configuration

### ElectricSQL Config

Complete configuration options:

```javascript
{
  // Required
  appName: string,           // Application name

  // Optional
  url: string,               // Electric server URL
  token: string,             // Auth token (JWT)

  // Advanced
  debug: boolean,            // Enable debug logs (default: false)
  timeout: number,           // Connection timeout ms (default: 5000)
  reconnect: boolean,        // Auto-reconnect (default: true)
  reconnectDelay: number,    // Reconnect delay ms (default: 1000)
  maxReconnectDelay: number, // Max reconnect delay ms (default: 30000)

  // Storage
  storage: {
    adapter: 'wa-sqlite',    // Storage adapter
    path: string,            // Database file path (optional)
  },

  // Sync
  sync: {
    batchSize: number,       // Sync batch size (default: 100)
    pollInterval: number,    // Poll interval ms (default: 5000)
  },
}
```

---

## Type Definitions (JSDoc)

### Import Library

All types are defined below using JSDoc annotations. Import the library normally:

```javascript
import { electrify } from 'electric-sql/wa-sqlite';
```

### Core Type Definitions

```javascript
/**
 * ElectricSQL client instance
 * @typedef {Object} ElectricClient
 * @property {Database} db - SQLite database instance
 * @property {SyncEngine} sync - Sync engine instance
 * @property {boolean} isConnected - Connection status
 */

/**
 * Database interface for executing queries and managing schema
 * @typedef {Object} Database
 * @property {function(string, Array=): Promise<QueryResult[]>} exec - Execute SQL query
 * @property {function(string, Array, Function): {unsubscribe: Function}} liveQuery - Reactive query with auto-updates
 * @property {function(Function): Promise<*>} transaction - Execute operations atomically
 * @property {function(): Promise<SchemaInfo>} getSchema - Get current schema information
 * @property {function(Migration[]): Promise<void>} migrate - Apply migrations
 */

/**
 * Sync engine for managing data synchronization
 * @typedef {Object} SyncEngine
 * @property {function(string[]): Promise<SyncShape>} syncTables - Start syncing specified tables
 * @property {function(Function): {unsubscribe: Function}} subscribe - Subscribe to sync events
 */

/**
 * Query result from database
 * @typedef {Object} QueryResult
 * @property {string[]} columns - Column names in result
 * @property {Array<Array>} values - Result rows (array of arrays)
 */

/**
 * Active sync shape for subscribed tables
 * @typedef {Object} SyncShape
 * @property {Promise<void>} synced - Resolves when initial sync completes
 * @property {boolean} isActive - Whether sync is currently active
 * @property {Function} unsubscribe - Stop syncing this shape
 */

/**
 * Migration definition for schema changes
 * @typedef {Object} Migration
 * @property {number} version - Migration version number
 * @property {string} name - Human-readable migration name
 * @property {string} up - SQL to apply migration
 * @property {string} [down] - SQL to rollback migration (optional)
 */

/**
 * Schema information from database
 * @typedef {Object} SchemaInfo
 * @property {string[]} tables - Table names
 * @property {Object<string, ColumnInfo[]>} columns - Columns by table
 */

/**
 * Column information
 * @typedef {Object} ColumnInfo
 * @property {string} name - Column name
 * @property {string} type - SQL type
 * @property {boolean} nullable - Whether NULL is allowed
 * @property {*} [default] - Default value (optional)
 */

/**
 * Sync event from sync engine
 * @typedef {Object} SyncEvent
 * @property {'connected'|'disconnected'|'synced'|'error'} type - Event type
 * @property {*} [data] - Event-specific data (optional)
 */
```

---

## Error Handling

### Error Types

| Error | Description | Recovery |
|-------|-------------|----------|
| `ConfigError` | Invalid configuration | Fix config |
| `ConnectionError` | Cannot connect to server | Check network, server URL |
| `AuthError` | Invalid auth token | Refresh token |
| `SyncError` | Sync operation failed | Retry sync |
| `QueryError` | SQL query failed | Check SQL syntax |
| `MigrationError` | Migration failed | Rollback migration |

### Error Example

```javascript
try {
  await db.exec('INSERT INTO users VALUES (?, ?)', ['1', 'Alice']);
} catch (error) {
  if (error.name === 'QueryError') {
    console.error('SQL error:', error.message);
  } else if (error.name === 'SyncError') {
    console.error('Sync failed, will retry');
  } else {
    throw error;
  }
}
```

---

## Best Practices

### 1. Always Use Transactions for Multiple Writes

```javascript
// ❌ BAD: Separate writes
await db.exec('INSERT INTO users ...');
await db.exec('INSERT INTO posts ...');

// ✅ GOOD: Transaction
await db.transaction(async (tx) => {
  await tx.exec('INSERT INTO users ...');
  await tx.exec('INSERT INTO posts ...');
});
```

### 2. Cleanup Subscriptions

```javascript
// ❌ BAD: Memory leak
db.liveQuery('SELECT * FROM todos', [], callback);

// ✅ GOOD: Cleanup
const { unsubscribe } = db.liveQuery('SELECT * FROM todos', [], callback);

// In cleanup (useEffect, componentWillUnmount)
unsubscribe();
```

### 3. Handle Offline Gracefully

```javascript
// ✅ GOOD: Check connection
if (!isElectricConnected()) {
  showOfflineNotice();
}

// Writes still work offline!
await db.exec('INSERT INTO todos ...');
```

---

## Versioning

**Current version**: v0.9.0

**Compatibility:**

| ElectricSQL | Node.js | Browser |
|-------------|---------|---------|
| 0.9.x | 18+ | Chrome 90+, Firefox 88+, Safari 15+ |
| 0.8.x | 16+ | Chrome 88+, Firefox 86+, Safari 14+ |

---

## Related Documentation

- [Tutorial: First Todo App](../tutorials/01-first-todo-app.md)
- [How-to: Setup Sync](../how-to/setup-electric-sync.md)
- [Explanation: Reactive Sync](../explanations/reactive-sync-patterns.md)

---

**Last Updated**: 2025-12-10
