# Advanced Features & Components Guide

This document describes the advanced components, hooks, and features available in the YAWL Editor application.

## 📦 Advanced Components

### UI Components (shadcn/ui)

#### Dialog
Modal dialog component for displaying overlays and forms.

```tsx
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogDescription,
  DialogFooter,
  DialogClose,
} from '@/components/ui/dialog'

<Dialog open={isOpen} onOpenChange={setIsOpen}>
  <DialogContent>
    <DialogHeader>
      <DialogTitle>Modal Title</DialogTitle>
      <DialogDescription>Modal description</DialogDescription>
    </DialogHeader>
    {/* Content */}
    <DialogFooter>
      <Button>Save</Button>
    </DialogFooter>
  </DialogContent>
</Dialog>
```

#### Form
React Hook Form integrated with shadcn/ui for powerful form handling.

```tsx
import { useForm } from 'react-hook-form'
import { zodResolver } from '@hookform/resolvers/zod'
import { Form, FormField, FormItem, FormLabel, FormControl, FormMessage } from '@/components/ui/form'
import { CreateCaseSchema } from '@/lib/validation'

const form = useForm({
  resolver: zodResolver(CreateCaseSchema),
  defaultValues: { processId: '' }
})

<Form {...form}>
  <FormField
    control={form.control}
    name="processId"
    render={({ field }) => (
      <FormItem>
        <FormLabel>Process</FormLabel>
        <FormControl>
          <Input {...field} />
        </FormControl>
        <FormMessage />
      </FormItem>
    )}
  />
</Form>
```

#### Select
Customizable select dropdown with search and keyboard navigation.

```tsx
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'

<Select onValueChange={onChange}>
  <SelectTrigger>
    <SelectValue placeholder="Select..." />
  </SelectTrigger>
  <SelectContent>
    <SelectItem value="opt1">Option 1</SelectItem>
    <SelectItem value="opt2">Option 2</SelectItem>
  </SelectContent>
</Select>
```

#### Textarea
Multi-line text input component.

```tsx
import { Textarea } from '@/components/ui/textarea'

<Textarea placeholder="Enter text..." />
```

#### Tabs
Organized content in tabbed interface.

```tsx
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs'

<Tabs defaultValue="tab1">
  <TabsList>
    <TabsTrigger value="tab1">Tab 1</TabsTrigger>
    <TabsTrigger value="tab2">Tab 2</TabsTrigger>
  </TabsList>
  <TabsContent value="tab1">Content 1</TabsContent>
  <TabsContent value="tab2">Content 2</TabsContent>
</Tabs>
```

### Data Table

Advanced sortable, filterable data table component.

```tsx
import { DataTable, Column } from '@/components/tables/data-table'

const columns: Column<Item>[] = [
  { key: 'id', label: 'ID', sortable: true },
  {
    key: 'status',
    label: 'Status',
    render: (value) => <Badge>{value}</Badge>
  },
]

<DataTable
  columns={columns}
  data={items}
  loading={loading}
  error={error}
  sortField={sortField}
  sortOrder={sortOrder}
  onSort={(field) => toggleSort(field)}
  getRowKey={(row) => row.id}
/>
```

### Forms

#### Case Form
Pre-built form for creating cases with validation.

```tsx
import { CaseForm } from '@/components/forms/case-form'

<CaseForm
  onSubmit={async (data) => {
    // Handle submission
  }}
  loading={false}
/>
```

### Modals

#### Create Case Modal
Modal dialog for creating new cases.

```tsx
import { CreateCaseModal } from '@/components/modals/create-case-modal'

<CreateCaseModal
  isOpen={isOpen}
  onClose={close}
  onSuccess={() => refetch()}
/>
```

## 🎣 Custom Hooks

### SPARQL Hooks (`hooks/use-sparql.ts`)

#### useSparql
Execute SPARQL queries with automatic caching and refetching.

```tsx
const { data, loading, error, variables, refetch } = useSparql(
  SPARQL_QUERIES.getAllCases,
  {
    skip: false,
    refetchInterval: 30000, // Refetch every 30 seconds
    onSuccess: (data) => console.log('Loaded', data),
    onError: (error) => console.error('Failed', error),
  }
)
```

#### useSparqlUpdate
Execute SPARQL UPDATE queries.

```tsx
const { execute, loading, error } = useSparqlUpdate()

await execute(SPARQL_QUERIES.updateCaseStatus(caseId, 'Running'))
```

#### useSparqlPaginated
Paginated SPARQL queries with automatic limit/offset.

```tsx
const {
  data,
  loading,
  page,
  pageSize,
  hasNextPage,
  nextPage,
  previousPage,
} = useSparqlPaginated(
  (offset, limit) => queryBuilder(offset, limit),
  10 // page size
)
```

#### useSparqlFiltered
SPARQL queries with dynamic filtering.

```tsx
const {
  data,
  filters,
  setFilters,
  addFilter,
  removeFilter,
  clearFilters,
} = useSparqlFiltered(baseQuery, filterBuilder)

addFilter('status', 'Running')
removeFilter('status')
clearFilters()
```

#### useSparqlSearch
Full-text search on SPARQL results.

```tsx
const {
  data: searchResults,
  searchTerm,
  setSearchTerm,
} = useSparqlSearch(query, ['name', 'description'])

setSearchTerm('loan') // Filters results
```

#### useSparqlSort
Sorting for SPARQL results.

```tsx
const {
  data: sortedData,
  sortField,
  sortOrder,
  toggleSort,
  setSortField,
  setSortOrder,
} = useSparqlSort(query, 'createdDate', 'desc')

toggleSort('processName')
```

### Modal Hooks (`hooks/use-modal.ts`)

#### useModal
Simple modal open/close state management.

```tsx
const { isOpen, open, close, toggle } = useModal(false)

<button onClick={open}>Open Modal</button>
<Modal open={isOpen} onClose={close} />
```

#### useModalWithData
Modal with associated data payload.

```tsx
const { isOpen, data, open, close, setData } = useModalWithData<Case>()

open(caseData) // Opens modal with data
const { data: caseData } = useModalWithData
```

#### useModals
Manage multiple modals independently.

```tsx
const { open, close, isOpen, closeAll } = useModals()

open('createCase')
open('editCase')
isOpen('createCase') // true
closeAll()
```

### Storage Hooks (`hooks/use-local-storage.ts`)

#### useLocalStorage
Persistent state in localStorage.

```tsx
const [value, setValue, removeValue] = useLocalStorage('key', defaultValue)

setValue(newValue)
removeValue() // Reset to default
```

#### useSessionStorage
Persistent state in sessionStorage (cleared on tab close).

```tsx
const [value, setValue, removeValue] = useSessionStorage('key', defaultValue)
```

### Async Hooks (`hooks/use-async.ts`)

#### useAsync
Handle async operations with loading and error states.

```tsx
const { data, loading, error, status, execute } = useAsync(
  async () => {
    const res = await fetch('/api/cases')
    return res.json()
  },
  true // execute immediately
)

// status: 'idle' | 'pending' | 'success' | 'error'
if (loading) return <Spinner />
if (error) return <Error>{error.message}</Error>
```

#### useAsyncDebounced
Debounced async operations for search.

```tsx
const { data, execute, searchTerm } = useAsyncDebounced(
  async (term) => {
    const res = await fetch(`/api/search?q=${term}`)
    return res.json()
  },
  500 // debounce delay
)

<Input onChange={(e) => execute(e.target.value)} />
```

#### useAsyncRetry
Async operations with automatic retry and exponential backoff.

```tsx
const { data, loading, error, execute, retryCount } = useAsyncRetry(
  async () => {
    const res = await fetch('/api/data')
    if (!res.ok) throw new Error('Failed')
    return res.json()
  },
  3, // max retries
  true // execute immediately
)

// Retries with delays: 1s, 2s, 4s
```

## 🛠️ Utility Functions (`lib/utils.ts`)

```tsx
import {
  cn, // Merge classNames
  formatDate, // Format Date object
  formatDateTime, // Format with time
  truncate, // Truncate string
  debounce, // Debounce function
  sleep, // Promise delay
  objectToQueryString, // Convert object to query params
  queryStringToObject, // Parse query string
  generateUUID, // Create UUID v4
  isEmpty, // Check if value is empty
  deepClone, // Deep clone objects
  deepMerge, // Recursive merge objects
} from '@/lib/utils'
```

## 📱 Advanced Features

### Sortable Tables
Click column headers to sort, click again to reverse order.

```tsx
<DataTable
  columns={columns}
  sortField={sortField}
  sortOrder={sortOrder}
  onSort={toggleSort}
  data={data}
  getRowKey={(row) => row.id}
/>
```

### Searchable Data
Real-time filtering with `useSparqlSearch` hook.

```tsx
const { data, searchTerm, setSearchTerm } = useSparqlSearch(
  query,
  ['id', 'name', 'description']
)

<Input value={searchTerm} onChange={(e) => setSearchTerm(e.target.value)} />
```

### Filterable Tables
Status filters and multi-field filtering.

```tsx
const [statusFilter, setStatusFilter] = useState('')

const filtered = statusFilter
  ? data.filter((item) => item.status === statusFilter)
  : data

<Select value={statusFilter} onValueChange={setStatusFilter}>
  <SelectContent>
    <SelectItem value="">All</SelectItem>
    <SelectItem value="Running">Running</SelectItem>
  </SelectContent>
</Select>
```

### Modal Forms
Combined modal + form for CRUD operations.

```tsx
<CreateCaseModal
  isOpen={isOpen}
  onClose={close}
  onSuccess={() => {
    refetch() // Refresh table
    close() // Close modal
  }}
/>
```

### API Integration
Seamless API route integration.

```tsx
// Client
const response = await fetch('/api/cases', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify(data),
})

// Server (app/api/cases/route.ts)
export async function POST(request: NextRequest) {
  const body = await request.json()
  // Process request
  return NextResponse.json({ success: true })
}
```

## 🔄 Common Patterns

### Loading States
```tsx
{loading && <Spinner />}
{error && <Alert variant="destructive">{error.message}</Alert>}
{!loading && data.length === 0 && <p>No data</p>}
```

### Optimistic Updates
```tsx
const optimisticData = [...data, newItem]
setData(optimisticData)

try {
  await api.createItem(newItem)
} catch (error) {
  setData(originalData) // Rollback
}
```

### Form Submission
```tsx
const onSubmit = async (data) => {
  setLoading(true)
  try {
    await api.create(data)
    onSuccess?.()
  } catch (error) {
    setError(error.message)
  } finally {
    setLoading(false)
  }
}
```

## 📚 Best Practices

1. **Use TypeScript**: All components and hooks are fully typed
2. **Validate Input**: Use Zod schemas from `lib/validation.ts`
3. **Handle Errors**: Always catch and display errors to users
4. **Show Loading States**: Indicate when data is being fetched
5. **Debounce Search**: Use `useSparqlSearch` for efficient queries
6. **Optimize Re-renders**: Use `useCallback` for event handlers
7. **Cache Results**: SPARQL queries cache by default
8. **Clean Up**: Hooks automatically cleanup on unmount

## 🚀 Performance Tips

- Use `useSparqlPaginated` for large datasets
- Enable `refetchInterval` only when necessary
- Use `useSparqlSearch` with debouncing
- Memoize column definitions to prevent re-renders
- Use `useCallback` for handlers passed to child components
- Implement virtual scrolling for huge tables (future enhancement)

## 🔗 Integration Examples

### Complete Case Management Page
See `app/cases/page.tsx` for a full example using:
- Advanced SPARQL hooks
- Modal management
- Data table with sorting
- Search and filter functionality
- API integration

### API Routes
- `app/api/cases/route.ts` - List and create cases
- `app/api/cases/[id]/route.ts` - Get, update, delete cases

## 📖 Additional Resources

- [shadcn/ui Documentation](https://ui.shadcn.com/)
- [React Hook Form Documentation](https://react-hook-form.com/)
- [Zod Documentation](https://zod.dev/)
- [Radix UI Documentation](https://www.radix-ui.com/)
- [Next.js Documentation](https://nextjs.org/docs)
