# Tutorial: Build Your First Local-First Todo App

**Time**: 30 minutes | **Level**: Beginner | **Stack**: Next.js + shadcn/ui + ElectricSQL

## What You'll Build

A real-time collaborative todo app that works offline and syncs automatically when online.

**Features**:
- ✅ Add, complete, and delete todos
- ✅ Real-time sync across devices
- ✅ Offline-first (works without internet)
- ✅ Beautiful UI with shadcn/ui
- ✅ Type-safe with Zod validation

**By the end of this tutorial**, you'll have a working application and understand the basics of local-first development.

---

## Prerequisites

- Node.js 18+ installed
- Basic JavaScript knowledge
- Familiarity with React (helpful but not required)

---

## Step 1: Create Next.js Project (2 minutes)

```bash
# Create new Next.js app
npx create-next-app@latest electric-todos

# Choose these options:
# ✓ TypeScript? No (we use JavaScript + Zod + JSDoc)
# ✓ ESLint? Yes
# ✓ Tailwind CSS? Yes
# ✓ src/ directory? Yes
# ✓ App Router? Yes
# ✓ Turbopack? No

cd electric-todos
```

**What just happened?**

You created a new Next.js application with:
- Tailwind CSS for styling
- App Router for routing
- ESLint for code quality

---

## Step 2: Install shadcn/ui (3 minutes)

```bash
# Initialize shadcn/ui
npx shadcn-ui@latest init

# Choose these options:
# ✓ Style: Default
# ✓ Base color: Slate
# ✓ CSS variables: Yes

# Install components we'll use
npx shadcn-ui@latest add button
npx shadcn-ui@latest add checkbox
npx shadcn-ui@latest add input
npx shadcn-ui@latest add card
```

**What just happened?**

You installed shadcn/ui components. These are:
- **Not a library** - Components are copied to your project
- **Customizable** - Edit them directly in `src/components/ui/`
- **Accessible** - Built with Radix UI primitives

---

## Step 3: Install ElectricSQL (3 minutes)

```bash
# Install ElectricSQL client
npm install electric-sql

# Install Zod for schema validation
npm install zod

# Install wa-sqlite for local database
npm install wa-sqlite
```

**What just happened?**

You installed:
- **electric-sql**: Reactive sync layer
- **zod**: Runtime type validation
- **wa-sqlite**: SQLite compiled to WebAssembly (runs in browser)

---

## Step 4: Create Database Schema (5 minutes)

Create the file `src/db/schema.js`:

```javascript
import { z } from 'zod';

/**
 * @typedef {Object} Todo
 * @property {string} id - Unique identifier
 * @property {string} text - Todo text content
 * @property {boolean} completed - Completion status
 * @property {Date} created_at - Creation timestamp
 */

// Zod schema for runtime validation
export const TodoSchema = z.object({
  id: z.string().uuid(),
  text: z.string().min(1).max(500),
  completed: z.boolean(),
  created_at: z.date(),
});

// SQL schema for ElectricSQL
export const TODO_TABLE_SQL = `
  CREATE TABLE IF NOT EXISTS todos (
    id TEXT PRIMARY KEY NOT NULL,
    text TEXT NOT NULL,
    completed INTEGER NOT NULL DEFAULT 0,
    created_at INTEGER NOT NULL
  );
`;
```

**What just happened?**

You defined your data model **twice**:
1. **Zod schema** - For JavaScript validation
2. **SQL schema** - For database structure

This is normal in local-first apps. Later we'll sync them automatically.

---

## Step 5: Initialize ElectricSQL (5 minutes)

Create the file `src/db/electric.js`:

```javascript
import { electrify } from 'electric-sql/wa-sqlite';
import { TODO_TABLE_SQL } from './schema.js';

/**
 * @typedef {Object} ElectricClient
 * @property {Object} db - Database connection
 * @property {Function} sync - Sync function
 */

let electricClient = null;

/**
 * Initialize ElectricSQL connection
 * @returns {Promise<ElectricClient>}
 */
export async function initElectric() {
  if (electricClient) {
    return electricClient;
  }

  // Create local SQLite database
  const { db } = await electrify({
    appName: 'electric-todos',
    // For now, we're local-only (no server sync)
    // We'll add sync in a later how-to guide
  });

  // Create tables
  await db.exec(TODO_TABLE_SQL);

  electricClient = { db };
  return electricClient;
}
```

**What just happened?**

You created a function that:
- Initializes a local SQLite database in the browser
- Creates the `todos` table
- Returns a database connection

No server required yet! This works 100% offline.

---

## Step 6: Create Todo Component (7 minutes)

Create the file `src/components/TodoList.jsx`:

```javascript
'use client';

import { useState, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Checkbox } from '@/components/ui/checkbox';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { initElectric } from '@/db/electric';
import { TodoSchema } from '@/db/schema';

/**
 * @typedef {Object} Todo
 * @property {string} id
 * @property {string} text
 * @property {boolean} completed
 * @property {Date} created_at
 */

export default function TodoList() {
  /** @type {[Todo[], Function]} */
  const [todos, setTodos] = useState([]);
  const [newTodo, setNewTodo] = useState('');
  const [db, setDb] = useState(null);

  // Initialize database
  useEffect(() => {
    initElectric().then(({ db: database }) => {
      setDb(database);
      loadTodos(database);
    });
  }, []);

  /**
   * Load todos from database
   * @param {Object} database
   */
  async function loadTodos(database) {
    const result = await database.exec('SELECT * FROM todos ORDER BY created_at DESC');
    const rows = result[0]?.values || [];

    const todoList = rows.map(row => ({
      id: row[0],
      text: row[1],
      completed: Boolean(row[2]),
      created_at: new Date(row[3]),
    }));

    setTodos(todoList);
  }

  /**
   * Add new todo
   */
  async function addTodo() {
    if (!newTodo.trim() || !db) return;

    const todo = {
      id: crypto.randomUUID(),
      text: newTodo.trim(),
      completed: false,
      created_at: new Date(),
    };

    // Validate with Zod
    try {
      TodoSchema.parse(todo);
    } catch (error) {
      console.error('Validation failed:', error);
      return;
    }

    // Insert into database
    await db.exec(
      'INSERT INTO todos (id, text, completed, created_at) VALUES (?, ?, ?, ?)',
      [todo.id, todo.text, todo.completed ? 1 : 0, todo.created_at.getTime()]
    );

    // Reload todos
    await loadTodos(db);
    setNewTodo('');
  }

  /**
   * Toggle todo completion
   * @param {string} id
   */
  async function toggleTodo(id) {
    if (!db) return;

    const todo = todos.find(t => t.id === id);
    if (!todo) return;

    await db.exec(
      'UPDATE todos SET completed = ? WHERE id = ?',
      [todo.completed ? 0 : 1, id]
    );

    await loadTodos(db);
  }

  /**
   * Delete todo
   * @param {string} id
   */
  async function deleteTodo(id) {
    if (!db) return;

    await db.exec('DELETE FROM todos WHERE id = ?', [id]);
    await loadTodos(db);
  }

  return (
    <Card className="w-full max-w-2xl mx-auto mt-8">
      <CardHeader>
        <CardTitle>My Todos</CardTitle>
      </CardHeader>
      <CardContent>
        {/* Add todo form */}
        <div className="flex gap-2 mb-4">
          <Input
            type="text"
            placeholder="What needs to be done?"
            value={newTodo}
            onChange={(e) => setNewTodo(e.target.value)}
            onKeyPress={(e) => e.key === 'Enter' && addTodo()}
          />
          <Button onClick={addTodo}>Add</Button>
        </div>

        {/* Todo list */}
        <div className="space-y-2">
          {todos.length === 0 ? (
            <p className="text-center text-gray-500 py-8">
              No todos yet. Add one above!
            </p>
          ) : (
            todos.map(todo => (
              <div
                key={todo.id}
                className="flex items-center gap-2 p-3 border rounded hover:bg-gray-50"
              >
                <Checkbox
                  checked={todo.completed}
                  onCheckedChange={() => toggleTodo(todo.id)}
                />
                <span className={todo.completed ? 'line-through flex-1' : 'flex-1'}>
                  {todo.text}
                </span>
                <Button
                  variant="destructive"
                  size="sm"
                  onClick={() => deleteTodo(todo.id)}
                >
                  Delete
                </Button>
              </div>
            ))
          )}
        </div>

        {/* Stats */}
        <div className="mt-4 text-sm text-gray-600">
          {todos.filter(t => !t.completed).length} of {todos.length} remaining
        </div>
      </CardContent>
    </Card>
  );
}
```

**What just happened?**

You created a complete todo component with:
- **State management** with React hooks
- **Database operations** (create, read, update, delete)
- **Validation** with Zod schemas
- **Beautiful UI** with shadcn components
- **Real-time updates** (reload after each operation)

---

## Step 7: Add to Home Page (2 minutes)

Replace `src/app/page.js` with:

```javascript
import TodoList from '@/components/TodoList';

export default function Home() {
  return (
    <main className="min-h-screen p-8">
      <h1 className="text-4xl font-bold text-center mb-8">
        Electric Todos
      </h1>
      <TodoList />
    </main>
  );
}
```

**What just happened?**

You added the todo component to the home page. Simple!

---

## Step 8: Run the Application (3 minutes)

```bash
# Start development server
npm run dev

# Open browser
open http://localhost:3000
```

**Try it out**:

1. ✅ Add a todo
2. ✅ Check it as complete
3. ✅ Add more todos
4. ✅ Delete a todo
5. ✅ Refresh the page - todos persist!

**What just happened?**

You built a working application that:
- Stores data in a local SQLite database
- Validates data with Zod
- Renders a beautiful UI with shadcn
- Persists across page refreshes

---

## 🎉 Congratulations!

You've built your first local-first application!

### What You Learned

✅ **Set up** Next.js with shadcn/ui
✅ **Install** ElectricSQL for local-first sync
✅ **Define** data schemas with Zod
✅ **Create** database tables with SQL
✅ **Build** UI components with shadcn
✅ **Validate** data at runtime
✅ **Persist** data locally in the browser

### What You Built

- Local-first todo application
- Offline-capable (no server required)
- Type-safe with Zod validation
- Beautiful UI with shadcn components
- Persistent storage with SQLite

---

## 🚀 Next Steps

Now that you have the basics, explore:

### How-to Guides (Add Features)

- **[Set Up ElectricSQL Sync](../how-to/setup-electric-sync.md)** - Add real-time sync across devices
- **[Build Forms with Zod](../how-to/build-forms-zod.md)** - Advanced form validation
- **[Implement Offline Patterns](../how-to/offline-first-patterns.md)** - Handle connectivity loss gracefully

### Explanations (Understand Concepts)

- **[Local-First Architecture](../explanations/local-first-architecture.md)** - Why we built it this way
- **[Reactive Sync Patterns](../explanations/reactive-sync-patterns.md)** - How ElectricSQL works under the hood

### Reference (Look Up Details)

- **[ElectricSQL API](../reference/electric-api.md)** - Complete API documentation
- **[shadcn Components](../reference/shadcn-components.md)** - Component library reference

---

## 🔍 Troubleshooting

### Error: "Module not found: Can't resolve '@/components/ui/button'"

**Solution**: Run `npx shadcn-ui@latest add button` to install the component.

### Error: "Cannot find module 'electric-sql'"

**Solution**: Run `npm install electric-sql wa-sqlite zod`.

### Todos don't persist after refresh

**Solution**: Check browser console for database errors. Clear site data and try again.

### Database initialization fails

**Solution**: Make sure you're using a modern browser (Chrome, Firefox, Safari) with WebAssembly support.

---

## 📝 Meta-Lesson: Tutorial Structure

**Notice how this tutorial was structured:**

1. **Clear outcome** - You knew what you'd build
2. **Prerequisites** - What you need before starting
3. **Step-by-step** - Numbered steps in order
4. **What just happened?** - Explanation after each step
5. **Try it out** - Concrete actions to verify
6. **Next steps** - Where to go from here

**This is the Diataxis tutorial pattern.**

When you write tutorials:
- ✅ **Do** make them safe and predictable
- ✅ **Do** explain just enough to understand
- ✅ **Do** provide complete working code
- ❌ **Don't** explain every concept in detail
- ❌ **Don't** offer choices or alternatives
- ❌ **Don't** assume deep knowledge

**Want to learn more about writing tutorials?**

See our [Meta-Guide on Tutorial Writing](../META-GUIDE.md#writing-tutorials).

---

## 🎯 Complete Code

**GitHub**: [electric-todos-tutorial](https://github.com/example/electric-todos-tutorial)

**Download**: `git clone https://github.com/example/electric-todos-tutorial`

---

**Next Tutorial**: [Add Real-Time Sync](02-add-realtime-sync.md) (Coming soon!)
