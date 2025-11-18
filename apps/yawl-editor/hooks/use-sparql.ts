import { useCallback, useEffect, useState } from 'react'
import { createDefaultSparqlClient } from '@/lib/sparql-client'
import type { QueryResult } from '@/lib/sparql-client'

interface UseSparqlOptions {
  skip?: boolean
  refetchInterval?: number
  onSuccess?: (data: any[]) => void
  onError?: (error: Error) => void
}

/**
 * Hook for executing SPARQL queries with caching and refetching
 */
export function useSparql<T = any>(
  query: string,
  options: UseSparqlOptions = {}
) {
  const { skip = false, refetchInterval, onSuccess, onError } = options

  const [data, setData] = useState<T[] | null>(null)
  const [loading, setLoading] = useState(!skip)
  const [error, setError] = useState<Error | null>(null)
  const [variables, setVariables] = useState<string[]>([])

  const client = createDefaultSparqlClient()

  const execute = useCallback(async () => {
    if (skip) return

    try {
      setLoading(true)
      setError(null)

      const result: QueryResult = await client.query(query)

      // Normalize results
      const normalized = result.bindings.map((binding: any) => {
        const item: Record<string, any> = {}
        for (const [key, value] of Object.entries(binding)) {
          if (typeof value === 'object' && value !== null && 'value' in value) {
            item[key] = (value as any).value
          } else {
            item[key] = value
          }
        }
        return item
      })

      setData(normalized as T[])
      setVariables(result.variables)
      onSuccess?.(normalized)
    } catch (err) {
      const error = err instanceof Error ? err : new Error(String(err))
      setError(error)
      onError?.(error)
    } finally {
      setLoading(false)
    }
  }, [query, skip, client, onSuccess, onError])

  useEffect(() => {
    execute()

    let interval: ReturnType<typeof setInterval> | undefined
    if (refetchInterval && refetchInterval > 0) {
      interval = setInterval(execute, refetchInterval)
    }

    return () => {
      if (interval) clearInterval(interval)
    }
  }, [execute, refetchInterval])

  return {
    data: data ?? [],
    loading,
    error,
    variables,
    refetch: execute,
  }
}

/**
 * Hook for executing SPARQL update queries
 */
export function useSparqlUpdate() {
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<Error | null>(null)

  const client = createDefaultSparqlClient()

  const execute = useCallback(
    async (updateQuery: string) => {
      try {
        setLoading(true)
        setError(null)

        const result = await client.update(updateQuery)
        return result
      } catch (err) {
        const error = err instanceof Error ? err : new Error(String(err))
        setError(error)
        throw error
      } finally {
        setLoading(false)
      }
    },
    [client]
  )

  return {
    execute,
    loading,
    error,
  }
}

/**
 * Hook for paginated SPARQL queries
 */
export function useSparqlPaginated<T = any>(
  queryBuilder: (offset: number, limit: number) => string,
  pageSize: number = 10,
  options: UseSparqlOptions = {}
) {
  const [page, setPage] = useState(0)

  const query = queryBuilder(page * pageSize, pageSize)
  const { data, loading, error, refetch } = useSparql<T>(query, options)

  return {
    data,
    loading,
    error,
    page,
    pageSize,
    setPage,
    hasNextPage: data.length === pageSize,
    hasPreviousPage: page > 0,
    nextPage: () => setPage((p) => p + 1),
    previousPage: () => setPage((p) => Math.max(0, p - 1)),
    refetch,
  }
}

/**
 * Hook for SPARQL queries with filters
 */
export function useSparqlFiltered<T = any>(
  baseQuery: string,
  filterBuilder?: (filters: Record<string, any>) => string,
  options: UseSparqlOptions = {}
) {
  const [filters, setFilters] = useState<Record<string, any>>({})

  const query = filterBuilder
    ? filterBuilder(filters)
    : baseQuery

  const { data, loading, error, refetch } = useSparql<T>(query, options)

  return {
    data,
    loading,
    error,
    filters,
    setFilters,
    addFilter: (key: string, value: any) => {
      setFilters((prev) => ({ ...prev, [key]: value }))
    },
    removeFilter: (key: string) => {
      setFilters((prev) => {
        const next = { ...prev }
        delete next[key]
        return next
      })
    },
    clearFilters: () => setFilters({}),
    refetch,
  }
}

/**
 * Hook for searching SPARQL results
 */
export function useSparqlSearch<T extends Record<string, any> = any>(
  query: string,
  searchFields: string[],
  options: UseSparqlOptions = {}
) {
  const [searchTerm, setSearchTerm] = useState('')
  const { data, loading, error, variables, refetch } = useSparql<T>(
    query,
    options
  )

  const filteredData = searchTerm
    ? data.filter((item) =>
        searchFields.some((field) => {
          const value = item[field]
          return (
            value &&
            String(value)
              .toLowerCase()
              .includes(searchTerm.toLowerCase())
          )
        })
      )
    : data

  return {
    data: filteredData,
    loading,
    error,
    variables,
    searchTerm,
    setSearchTerm,
    refetch,
  }
}

/**
 * Hook for sorting SPARQL results
 */
export function useSparqlSort<T extends Record<string, any> = any>(
  query: string,
  defaultSortField?: string,
  defaultSortOrder?: 'asc' | 'desc',
  options: UseSparqlOptions = {}
) {
  const [sortField, setSortField] = useState(defaultSortField || '')
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>(
    defaultSortOrder || 'asc'
  )

  const { data, loading, error, variables, refetch } = useSparql<T>(
    query,
    options
  )

  const sortedData = sortField
    ? [...data].sort((a, b) => {
        const aVal = a[sortField]
        const bVal = b[sortField]

        if (aVal === bVal) return 0

        let comparison = 0
        if (typeof aVal === 'number' && typeof bVal === 'number') {
          comparison = aVal - bVal
        } else {
          comparison = String(aVal).localeCompare(String(bVal))
        }

        return sortOrder === 'asc' ? comparison : -comparison
      })
    : data

  const toggleSort = (field: string) => {
    if (sortField === field) {
      setSortOrder((prev) => (prev === 'asc' ? 'desc' : 'asc'))
    } else {
      setSortField(field)
      setSortOrder('asc')
    }
  }

  return {
    data: sortedData,
    loading,
    error,
    variables,
    sortField,
    sortOrder,
    setSortField,
    setSortOrder,
    toggleSort,
    refetch,
  }
}
