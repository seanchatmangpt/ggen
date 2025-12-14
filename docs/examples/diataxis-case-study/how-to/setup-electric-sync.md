<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-to: Set Up ElectricSQL Sync with Postgres](#how-to-set-up-electricsql-sync-with-postgres)
  - [Prerequisites](#prerequisites)
  - [Overview](#overview)
  - [Step 1: Set Up Postgres with Docker](#step-1-set-up-postgres-with-docker)
  - [Step 2: Create Database Schema in Postgres](#step-2-create-database-schema-in-postgres)
  - [Step 3: Configure Client-Side Sync](#step-3-configure-client-side-sync)
  - [Step 4: Update Component for Real-Time Sync](#step-4-update-component-for-real-time-sync)
  - [Step 5: Test Real-Time Sync](#step-5-test-real-time-sync)
  - [Verification](#verification)
  - [Troubleshooting](#troubleshooting)
    - [Sync not working](#sync-not-working)
    - [Todos duplicate across devices](#todos-duplicate-across-devices)
    - [Changes don't sync immediately](#changes-dont-sync-immediately)
  - [Production Considerations](#production-considerations)
    - [1. Authentication](#1-authentication)
    - [2. Hosting](#2-hosting)
    - [3. Migrations](#3-migrations)
  - [What You Achieved](#what-you-achieved)
  - [Next Steps](#next-steps)
    - [Related How-tos](#related-how-tos)
    - [Explanations](#explanations)
    - [Reference](#reference)
  - [Meta-Lesson: How-to Guide Structure](#meta-lesson-how-to-guide-structure)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-to: Set Up ElectricSQL Sync with Postgres

**Problem**: You have a local-first app but need to sync data across devices and persist it on a server.

**Solution**: Configure ElectricSQL to sync your local SQLite database with a central Postgres database.

**Time**: 20 minutes | **Level**: Intermediate

---

## Prerequisites

- Completed [Tutorial 01: First Todo App](../tutorials/01-first-todo-app.md)
- Docker installed (for local Postgres)
- Basic understanding of SQL and databases

---

## Overview

```
┌──────────────┐         ┌──────────────┐         ┌──────────────┐
│   Browser    │         │  Electric    │         │   Postgres   │
│   (SQLite)   │ ◄─────► │    Sync      │ ◄─────► │   Database   │
│              │         │   Server     │         │              │
└──────────────┘         └──────────────┘         └──────────────┘
    Device 1                                          Centralized

┌──────────────┐               │
│   Browser    │               │
│   (SQLite)   │ ◄─────────────┘
│              │
└──────────────┘
    Device 2
```

**What we'll do:**
1. Set up Postgres database
2. Run Electric sync server
3. Configure client-side sync
4. Test real-time synchronization

---

## Step 1: Set Up Postgres with Docker

Create `docker-compose.yml` in your project root:

```yaml
version: "3.8"

services:
  postgres:
    image: postgres:15-alpine
    environment:
      POSTGRES_DB: electric
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: password
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data
    command:
      - "postgres"
      - "-c"
      - "wal_level=logical"  # Required for ElectricSQL

  electric:
    image: electricsql/electric:latest
    environment:
      DATABASE_URL: "postgresql://postgres:password@postgres:5432/electric"
      ELECTRIC_WRITE_TO_PG_MODE: "direct_writes"
      AUTH_MODE: "insecure"  # For development only!
    ports:
      - "5133:5133"  # Electric sync port
    depends_on:
      - postgres

volumes:
  postgres_data:
```

**Start the services:**

```bash
# Start Postgres + Electric
docker-compose up -d

# Check they're running
docker-compose ps

# Should show:
# postgres  running  0.0.0.0:5432->5432/tcp
# electric  running  0.0.0.0:5133->5133/tcp
```

---

## Step 2: Create Database Schema in Postgres

Create `migrations/001_create_todos.sql`:

```sql
-- Enable Electric replication
ALTER TABLE todos ENABLE ELECTRIC;

-- Create todos table (same as SQLite schema)
CREATE TABLE IF NOT EXISTS todos (
  id TEXT PRIMARY KEY NOT NULL,
  text TEXT NOT NULL,
  completed BOOLEAN NOT NULL DEFAULT false,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create index for performance
CREATE INDEX idx_todos_created_at ON todos(created_at DESC);

-- Enable Electric sync for this table
ALTER TABLE todos ENABLE ELECTRIC;
```

**Run the migration:**

```bash
# Install psql (if not already)
brew install postgresql  # macOS
# or
apt-get install postgresql-client  # Linux

# Run migration
psql postgresql://postgres:password@localhost:5432/electric -f migrations/001_create_todos.sql
```

**Verify it worked:**

```bash
# Connect to Postgres
psql postgresql://postgres:password@localhost:5432/electric

# List tables
\dt

# Should show: todos

# Check Electric is enabled
SELECT * FROM electric.ddl_commands;

# Should show: ALTER TABLE todos ENABLE ELECTRIC
```

---

## Step 3: Configure Client-Side Sync

Update `src/db/electric.js`:

```javascript
import { electrify } from 'electric-sql/wa-sqlite';
import { TODO_TABLE_SQL } from './schema.js';

/**
 * @typedef {Object} ElectricClient
 * @property {Object} db - Database connection
 * @property {Object} sync - Sync engine
 */

let electricClient = null;

/**
 * Initialize ElectricSQL with sync
 * @param {Object} options
 * @param {string} [options.token] - Auth token (optional)
 * @returns {Promise<ElectricClient>}
 */
export async function initElectric(options = {}) {
  if (electricClient) {
    return electricClient;
  }

  // Create local SQLite database with Electric
  const config = {
    appName: 'electric-todos',
    // Electric sync server URL
    url: process.env.NEXT_PUBLIC_ELECTRIC_URL || 'ws://localhost:5133',
    // Auth token (for production)
    token: options.token,
  };

  const electric = await electrify(config);
  const { db } = electric;

  // Create tables if they don't exist
  await db.exec(TODO_TABLE_SQL);

  // Start syncing
  const { synced } = await electric.sync.syncTables(['todos']);

  // Wait for initial sync to complete
  await synced;

  electricClient = {
    db,
    sync: electric.sync,
  };

  return electricClient;
}

/**
 * Subscribe to real-time changes
 * @param {Function} callback - Called when data changes
 * @returns {Function} Unsubscribe function
 */
export function subscribeToTodos(callback) {
  if (!electricClient) {
    throw new Error('ElectricSQL not initialized');
  }

  // Subscribe to live query
  const { unsubscribe } = electricClient.db.liveQuery(
    'SELECT * FROM todos ORDER BY created_at DESC',
    [],
    callback
  );

  return unsubscribe;
}
```

**Add environment variable:**

Create `.env.local`:

```bash
NEXT_PUBLIC_ELECTRIC_URL=ws://localhost:5133
```

---

## Step 4: Update Component for Real-Time Sync

Update `src/components/TodoList.jsx`:

```javascript
'use client';

import { useState, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Checkbox } from '@/components/ui/checkbox';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { initElectric, subscribeToTodos } from '@/db/electric';
import { TodoSchema } from '@/db/schema';

export default function TodoList() {
  const [todos, setTodos] = useState([]);
  const [newTodo, setNewTodo] = useState('');
  const [db, setDb] = useState(null);
  const [syncing, setSyncing] = useState(true);

  // Initialize database and subscribe to changes
  useEffect(() => {
    let unsubscribe;

    initElectric()
      .then(({ db: database }) => {
        setDb(database);
        setSyncing(false);

        // Subscribe to real-time updates
        unsubscribe = subscribeToTodos((result) => {
          const rows = result[0]?.values || [];
          const todoList = rows.map(row => ({
            id: row[0],
            text: row[1],
            completed: Boolean(row[2]),
            created_at: new Date(row[3]),
          }));
          setTodos(todoList);
        });
      })
      .catch(error => {
        console.error('Failed to initialize Electric:', error);
        setSyncing(false);
      });

    // Cleanup subscription
    return () => {
      if (unsubscribe) {
        unsubscribe();
      }
    };
  }, []);

  // ... rest of component code (addTodo, toggleTodo, deleteTodo) ...
  // Same as before - changes will automatically sync!

  return (
    <Card className="w-full max-w-2xl mx-auto mt-8">
      <CardHeader>
        <CardTitle className="flex items-center justify-between">
          <span>My Todos</span>
          {syncing ? (
            <span className="text-sm text-gray-500">Syncing...</span>
          ) : (
            <span className="text-sm text-green-500">✓ Synced</span>
          )}
        </CardTitle>
      </CardHeader>
      {/* ... rest of UI ... */}
    </Card>
  );
}
```

---

## Step 5: Test Real-Time Sync

**Open two browser windows:**

```bash
# Terminal 1: Start dev server
npm run dev

# Open two browser tabs
open http://localhost:3000
open http://localhost:3000
```

**Test sync:**

1. ✅ Add a todo in window 1
2. ✅ See it appear in window 2 (instantly!)
3. ✅ Check the todo in window 2
4. ✅ See it update in window 1
5. ✅ Delete in window 1
6. ✅ See it disappear from window 2

**Test offline:**

1. ✅ Open DevTools → Network → Offline
2. ✅ Add a todo (works offline!)
3. ✅ Go back online
4. ✅ Todo syncs automatically

---

## Verification

Check Postgres to see synced data:

```bash
# Connect to Postgres
psql postgresql://postgres:password@localhost:5432/electric

# Query todos
SELECT * FROM todos;

# Should show all your todos!
```

---

## Troubleshooting

### Sync not working

**Check Electric server logs:**

```bash
docker-compose logs electric

# Look for errors like:
# - "Connection refused" → Postgres not running
# - "Authentication failed" → Wrong credentials
```

**Check client connection:**

```javascript
// Add logging to electric.js
console.log('Connecting to Electric:', config.url);

// Check browser console for errors
```

### Todos duplicate across devices

**Cause**: Clock skew or race conditions

**Solution**: Use server-generated timestamps:

```sql
-- In Postgres migration
ALTER TABLE todos
  ALTER COLUMN created_at SET DEFAULT NOW();
```

### Changes don't sync immediately

**Cause**: Network latency or Electric backpressure

**Solution**: Check Electric server load:

```bash
docker-compose logs electric | grep "sync_delay"
```

---

## Production Considerations

### 1. Authentication

**Never use `AUTH_MODE: insecure` in production!**

Set up proper auth:

```javascript
// In electric.js
const token = await getAuthToken();  // From your auth provider

const electric = await electrify({
  url: process.env.NEXT_PUBLIC_ELECTRIC_URL,
  token,  // JWT token
});
```

### 2. Hosting

Deploy Electric to production:

```bash
# Use managed Electric service
# OR
# Deploy to Fly.io, Railway, etc.
fly deploy

# Point to production Postgres
# DATABASE_URL=postgresql://user:pass@prod-db.com/electric
```

### 3. Migrations

Use proper migration workflow:

```bash
# Create migration
npx electric-sql generate migration add_priority

# Review migration
cat migrations/002_add_priority.sql

# Apply to production
npx electric-sql migrate --prod
```

---

## What You Achieved

✅ **Set up** Postgres with Electric replication
✅ **Configured** Electric sync server
✅ **Connected** client to sync server
✅ **Implemented** real-time reactive queries
✅ **Tested** multi-device synchronization
✅ **Handled** offline scenarios

---

## Next Steps

### Related How-tos

- **[Implement Offline-First Patterns](offline-first-patterns.md)** - Handle conflicts and connectivity
- **[Optimize Real-Time Performance](optimize-realtime.md)** - Reduce latency and battery usage
- **[Set Up Production Deployment](production-deployment.md)** - Deploy to production

### Explanations

- **[Reactive Sync Patterns](../explanations/reactive-sync-patterns.md)** - How Electric achieves real-time sync
- **[Conflict Resolution](../explanations/conflict-resolution.md)** - How Electric handles concurrent edits

### Reference

- **[ElectricSQL API](../reference/electric-api.md)** - Complete API documentation
- **[Migration Commands](../reference/migrations.md)** - Migration reference

---

## Meta-Lesson: How-to Guide Structure

**Notice how this how-to was structured:**

1. **Problem** stated upfront
2. **Solution** overview
3. **Prerequisites** clearly listed
4. **Step-by-step** with verification
5. **Troubleshooting** common issues
6. **Production considerations**
7. **Related guides** linked

**This is the Diataxis how-to pattern.**

When you write how-tos:
- ✅ **Do** assume prerequisite knowledge
- ✅ **Do** focus on one specific task
- ✅ **Do** provide troubleshooting
- ✅ **Do** link to related guides
- ❌ **Don't** explain concepts in depth
- ❌ **Don't** teach from scratch
- ❌ **Don't** mix multiple tasks

**Want to learn more about writing how-tos?**

See our [Meta-Guide on How-to Writing](../META-GUIDE.md#writing-how-tos).
