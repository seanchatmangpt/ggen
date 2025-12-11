# How-to: Build Forms with shadcn/ui + Zod Validation

**Problem**: You need type-safe form validation with beautiful UI components.

**Solution**: Combine shadcn/ui components with Zod schemas for runtime validation.

**Time**: 15 minutes | **Level**: Intermediate

---

## Prerequisites

- Completed [Tutorial 01](../tutorials/01-first-todo-app.md)
- Basic understanding of React forms
- shadcn/ui components installed

---

## Overview

```
User Input → shadcn Form Component → Zod Validation → Database
                                    ↓ (errors)
                            Error Messages Display
```

---

## Step 1: Install Required Packages

```bash
# Install form dependencies
npm install zod
npm install react-hook-form
npm install @hookform/resolvers

# Install shadcn form components
npx shadcn-ui@latest add form
npx shadcn-ui@latest add label
npx shadcn-ui@latest add input
npx shadcn-ui@latest add textarea
npx shadcn-ui@latest add select
```

---

## Step 2: Define Zod Schema

Create `src/schemas/todo-form.js`:

```javascript
import { z } from 'zod';

/**
 * Todo form validation schema
 */
export const TodoFormSchema = z.object({
  text: z.string()
    .min(1, 'Todo text is required')
    .max(500, 'Todo must be less than 500 characters')
    .trim(),

  priority: z.enum(['low', 'medium', 'high'], {
    required_error: 'Please select a priority',
  }),

  dueDate: z.date({
    required_error: 'Due date is required',
  }).min(new Date(), 'Due date must be in the future'),

  tags: z.array(z.string()).optional(),
});

/**
 * @typedef {z.infer<typeof TodoFormSchema>} TodoFormData
 */
```

---

## Step 3: Create Form Component

Create `src/components/TodoForm.jsx`:

```javascript
'use client';

import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { TodoFormSchema } from '@/schemas/todo-form';
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/components/ui/form';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { Button } from '@/components/ui/button';
import { Calendar } from '@/components/ui/calendar';
import { Popover, PopoverContent, PopoverTrigger } from '@/components/ui/popover';
import { CalendarIcon } from 'lucide-react';
import { format } from 'date-fns';

/**
 * @typedef {import('@/schemas/todo-form').TodoFormData} TodoFormData
 */

/**
 * @param {Object} props
 * @param {Function} props.onSubmit - Form submission handler
 */
export function TodoForm({ onSubmit }) {
  const form = useForm({
    resolver: zodResolver(TodoFormSchema),
    defaultValues: {
      text: '',
      priority: 'medium',
      dueDate: new Date(),
      tags: [],
    },
  });

  /**
   * @param {TodoFormData} data
   */
  function handleSubmit(data) {
    // Validation passed! Safe to use data
    console.log('Valid data:', data);
    onSubmit(data);
    form.reset();
  }

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(handleSubmit)} className="space-y-6">
        {/* Text field */}
        <FormField
          control={form.control}
          name="text"
          render={({ field }) => (
            <FormItem>
              <FormLabel>Todo</FormLabel>
              <FormControl>
                <Textarea
                  placeholder="What needs to be done?"
                  {...field}
                />
              </FormControl>
              <FormDescription>
                Describe the task (max 500 characters)
              </FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />

        {/* Priority select */}
        <FormField
          control={form.control}
          name="priority"
          render={({ field }) => (
            <FormItem>
              <FormLabel>Priority</FormLabel>
              <Select onValueChange={field.onChange} defaultValue={field.value}>
                <FormControl>
                  <SelectTrigger>
                    <SelectValue placeholder="Select priority" />
                  </SelectTrigger>
                </FormControl>
                <SelectContent>
                  <SelectItem value="low">Low</SelectItem>
                  <SelectItem value="medium">Medium</SelectItem>
                  <SelectItem value="high">High</SelectItem>
                </SelectContent>
              </Select>
              <FormMessage />
            </FormItem>
          )}
        />

        {/* Due date picker */}
        <FormField
          control={form.control}
          name="dueDate"
          render={({ field }) => (
            <FormItem className="flex flex-col">
              <FormLabel>Due Date</FormLabel>
              <Popover>
                <PopoverTrigger asChild>
                  <FormControl>
                    <Button variant="outline" className="w-full pl-3 text-left font-normal">
                      {field.value ? (
                        format(field.value, 'PPP')
                      ) : (
                        <span>Pick a date</span>
                      )}
                      <CalendarIcon className="ml-auto h-4 w-4 opacity-50" />
                    </Button>
                  </FormControl>
                </PopoverTrigger>
                <PopoverContent className="w-auto p-0" align="start">
                  <Calendar
                    mode="single"
                    selected={field.value}
                    onSelect={field.onChange}
                    disabled={(date) => date < new Date()}
                    initialFocus
                  />
                </PopoverContent>
              </Popover>
              <FormMessage />
            </FormItem>
          )}
        />

        {/* Submit button */}
        <Button type="submit" className="w-full">
          Create Todo
        </Button>
      </form>
    </Form>
  );
}
```

---

## Step 4: Use Form in Component

Update `src/components/TodoList.jsx`:

```javascript
import { TodoForm } from '@/components/TodoForm';

export default function TodoList() {
  const [todos, setTodos] = useState([]);
  const [db, setDb] = useState(null);

  async function handleCreateTodo(data) {
    if (!db) return;

    const todo = {
      id: crypto.randomUUID(),
      text: data.text,
      priority: data.priority,
      due_date: data.dueDate.getTime(),
      completed: false,
      created_at: new Date().getTime(),
    };

    await db.exec(
      `INSERT INTO todos (id, text, priority, due_date, completed, created_at)
       VALUES (?, ?, ?, ?, ?, ?)`,
      [todo.id, todo.text, todo.priority, todo.due_date, 0, todo.created_at]
    );

    loadTodos(db);
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle>Create Todo</CardTitle>
      </CardHeader>
      <CardContent>
        <TodoForm onSubmit={handleCreateTodo} />
      </CardContent>
    </Card>
  );
}
```

---

## Verification

**Test validation:**

1. ✅ Leave text empty → See "Todo text is required"
2. ✅ Enter 501 characters → See "Todo must be less than 500 characters"
3. ✅ Select past date → See "Due date must be in the future"
4. ✅ Fill form correctly → Todo created successfully

---

## Advanced: Custom Validation

### Add Custom Validator

```javascript
export const TodoFormSchema = z.object({
  text: z.string()
    .min(1)
    .max(500)
    .refine(
      (val) => !val.toLowerCase().includes('spam'),
      { message: 'Todo cannot contain spam' }
    ),

  // Email field with custom validation
  assignee_email: z.string()
    .email('Invalid email address')
    .refine(
      async (email) => {
        // Check if user exists (async validation!)
        const user = await checkUserExists(email);
        return user !== null;
      },
      { message: 'User not found' }
    ),
});
```

### Conditional Fields

```javascript
export const TodoFormSchema = z.object({
  text: z.string().min(1),
  priority: z.enum(['low', 'medium', 'high']),

  // Only required if priority is 'high'
  escalation_notes: z.string()
    .optional()
    .refine((val, ctx) => {
      if (ctx.parent.priority === 'high' && !val) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: 'Escalation notes required for high priority',
        });
        return false;
      }
      return true;
    }),
});
```

---

## Troubleshooting

### Error: "resolver is not a function"

**Cause**: Missing `@hookform/resolvers`

**Solution**:
```bash
npm install @hookform/resolvers
```

### Validation Not Triggering

**Cause**: Missing `zodResolver` in `useForm`

**Solution**:
```javascript
const form = useForm({
  resolver: zodResolver(TodoFormSchema),  // ← Must include this!
  defaultValues: { ... },
});
```

### Date Picker Not Working

**Cause**: Missing calendar component

**Solution**:
```bash
npx shadcn-ui@latest add calendar
npm install date-fns
```

---

## Best Practices

### 1. Colocate Schemas with Forms

```
src/
  components/
    TodoForm.jsx
    TodoForm.schema.js  ← Schema next to component
```

### 2. Reuse Validation Logic

```javascript
// Shared validators
export const emailValidator = z.string().email();
export const passwordValidator = z.string().min(8);

// Use in multiple schemas
export const SignupSchema = z.object({
  email: emailValidator,
  password: passwordValidator,
});

export const LoginSchema = z.object({
  email: emailValidator,
  password: z.string(),  // No min length for login
});
```

### 3. Type Safety

```javascript
/**
 * @typedef {z.infer<typeof TodoFormSchema>} TodoFormData
 */

/**
 * @param {TodoFormData} data - Automatically typed!
 */
function handleSubmit(data) {
  // data.text is string
  // data.priority is 'low' | 'medium' | 'high'
  // data.dueDate is Date
}
```

---

## Related Guides

- [Implement Offline Patterns](offline-first-patterns.md) - Handle form submission offline
- [Tutorial: First App](../tutorials/01-first-todo-app.md) - Basic setup

---

## Reference

- [shadcn Form Component](../reference/shadcn-components.md#form)
- [Zod Documentation](https://zod.dev/)
- [React Hook Form](https://react-hook-form.com/)

---

**Meta-Lesson**: This is a task-focused how-to guide that assumes you know React and forms, solves one specific problem (form validation), and provides troubleshooting.
