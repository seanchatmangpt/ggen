/**
 * Backstage IDP Hooks
 * Advanced React hooks for developer platform operations
 */

import { useEffect, useState, useCallback, useRef } from 'react'
import type {
  ComponentMetadata,
  ServiceEntity,
  TemplateEntity,
  DeploymentPipeline,
  TeamInfo,
  APIDefinition,
  CatalogEntry,
} from '@/lib/backstage-types'

interface UseCatalogOptions {
  filters?: Record<string, string | string[]>
  sort?: string
  limit?: number
  skip?: boolean
}

/**
 * Hook for searching and filtering the component catalog
 */
export function useCatalog(options: UseCatalogOptions = {}) {
  const [items, setItems] = useState<ComponentMetadata[]>([])
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<Error | null>(null)
  const [totalCount, setTotalCount] = useState(0)

  const fetchCatalog = useCallback(async () => {
    if (options.skip) return

    setLoading(true)
    setError(null)

    try {
      const params = new URLSearchParams()
      if (options.filters) {
        Object.entries(options.filters).forEach(([key, value]) => {
          if (Array.isArray(value)) {
            value.forEach((v) => params.append(key, v))
          } else {
            params.append(key, value)
          }
        })
      }
      if (options.sort) params.append('sort', options.sort)
      if (options.limit) params.append('limit', options.limit.toString())

      const response = await fetch(`/api/backstage/catalog?${params}`)
      if (!response.ok) throw new Error('Failed to fetch catalog')

      const data = await response.json()
      setItems(data.items)
      setTotalCount(data.total)
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'))
    } finally {
      setLoading(false)
    }
  }, [options])

  useEffect(() => {
    fetchCatalog()
  }, [fetchCatalog])

  return {
    items,
    loading,
    error,
    totalCount,
    refetch: fetchCatalog,
  }
}

/**
 * Hook for managing services
 */
export function useServices() {
  const [services, setServices] = useState<ServiceEntity[]>([])
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<Error | null>(null)

  const fetchServices = useCallback(async () => {
    setLoading(true)
    try {
      const response = await fetch('/api/backstage/services')
      if (!response.ok) throw new Error('Failed to fetch services')
      const data = await response.json()
      setServices(data)
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'))
    } finally {
      setLoading(false)
    }
  }, [])

  const createService = useCallback(
    async (service: Omit<ServiceEntity, 'id' | 'metadata'>) => {
      try {
        const response = await fetch('/api/backstage/services', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(service),
        })
        if (!response.ok) throw new Error('Failed to create service')
        const created = await response.json()
        setServices((prev) => [...prev, created])
        return created
      } catch (err) {
        setError(err instanceof Error ? err : new Error('Unknown error'))
        throw err
      }
    },
    []
  )

  const updateService = useCallback(
    async (id: string, updates: Partial<ServiceEntity>) => {
      try {
        const response = await fetch(`/api/backstage/services/${id}`, {
          method: 'PUT',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(updates),
        })
        if (!response.ok) throw new Error('Failed to update service')
        const updated = await response.json()
        setServices((prev) =>
          prev.map((s) => (s.id === id ? updated : s))
        )
        return updated
      } catch (err) {
        setError(err instanceof Error ? err : new Error('Unknown error'))
        throw err
      }
    },
    []
  )

  const deleteService = useCallback(async (id: string) => {
    try {
      const response = await fetch(`/api/backstage/services/${id}`, {
        method: 'DELETE',
      })
      if (!response.ok) throw new Error('Failed to delete service')
      setServices((prev) => prev.filter((s) => s.id !== id))
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'))
      throw err
    }
  }, [])

  useEffect(() => {
    fetchServices()
  }, [fetchServices])

  return {
    services,
    loading,
    error,
    createService,
    updateService,
    deleteService,
    refetch: fetchServices,
  }
}

/**
 * Hook for managing deployment pipelines
 */
export function useDeploymentPipelines(serviceId?: string) {
  const [pipelines, setPipelines] = useState<DeploymentPipeline[]>([])
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<Error | null>(null)

  const fetchPipelines = useCallback(async () => {
    setLoading(true)
    try {
      const url = serviceId
        ? `/api/backstage/pipelines?serviceId=${serviceId}`
        : '/api/backstage/pipelines'
      const response = await fetch(url)
      if (!response.ok) throw new Error('Failed to fetch pipelines')
      const data = await response.json()
      setPipelines(data)
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'))
    } finally {
      setLoading(false)
    }
  }, [serviceId])

  const createPipeline = useCallback(
    async (pipeline: Omit<DeploymentPipeline, 'id'>) => {
      try {
        const response = await fetch('/api/backstage/pipelines', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(pipeline),
        })
        if (!response.ok) throw new Error('Failed to create pipeline')
        const created = await response.json()
        setPipelines((prev) => [created, ...prev])
        return created
      } catch (err) {
        setError(err instanceof Error ? err : new Error('Unknown error'))
        throw err
      }
    },
    []
  )

  const getPipelineStatus = useCallback(
    async (pipelineId: string) => {
      try {
        const response = await fetch(
          `/api/backstage/pipelines/${pipelineId}/status`
        )
        if (!response.ok) throw new Error('Failed to fetch pipeline status')
        return await response.json()
      } catch (err) {
        setError(err instanceof Error ? err : new Error('Unknown error'))
        throw err
      }
    },
    []
  )

  useEffect(() => {
    fetchPipelines()
  }, [fetchPipelines])

  return {
    pipelines,
    loading,
    error,
    createPipeline,
    getPipelineStatus,
    refetch: fetchPipelines,
  }
}

/**
 * Hook for managing teams
 */
export function useTeams() {
  const [teams, setTeams] = useState<TeamInfo[]>([])
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<Error | null>(null)

  const fetchTeams = useCallback(async () => {
    setLoading(true)
    try {
      const response = await fetch('/api/backstage/teams')
      if (!response.ok) throw new Error('Failed to fetch teams')
      const data = await response.json()
      setTeams(data)
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'))
    } finally {
      setLoading(false)
    }
  }, [])

  const createTeam = useCallback(async (team: Omit<TeamInfo, 'id'>) => {
    try {
      const response = await fetch('/api/backstage/teams', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(team),
      })
      if (!response.ok) throw new Error('Failed to create team')
      const created = await response.json()
      setTeams((prev) => [...prev, created])
      return created
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'))
      throw err
    }
  }, [])

  const addMember = useCallback(
    async (teamId: string, userId: string, role: string) => {
      try {
        const response = await fetch(`/api/backstage/teams/${teamId}/members`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ userId, role }),
        })
        if (!response.ok) throw new Error('Failed to add member')
        const updated = await response.json()
        setTeams((prev) =>
          prev.map((t) => (t.id === teamId ? updated : t))
        )
        return updated
      } catch (err) {
        setError(err instanceof Error ? err : new Error('Unknown error'))
        throw err
      }
    },
    []
  )

  useEffect(() => {
    fetchTeams()
  }, [fetchTeams])

  return {
    teams,
    loading,
    error,
    createTeam,
    addMember,
    refetch: fetchTeams,
  }
}

/**
 * Hook for managing APIs
 */
export function useAPICatalog() {
  const [apis, setAPIs] = useState<APIDefinition[]>([])
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<Error | null>(null)

  const fetchAPIs = useCallback(async () => {
    setLoading(true)
    try {
      const response = await fetch('/api/backstage/apis')
      if (!response.ok) throw new Error('Failed to fetch APIs')
      const data = await response.json()
      setAPIs(data)
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'))
    } finally {
      setLoading(false)
    }
  }, [])

  const registerAPI = useCallback(
    async (api: Omit<APIDefinition, 'id' | 'rating'>) => {
      try {
        const response = await fetch('/api/backstage/apis', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(api),
        })
        if (!response.ok) throw new Error('Failed to register API')
        const created = await response.json()
        setAPIs((prev) => [...prev, created])
        return created
      } catch (err) {
        setError(err instanceof Error ? err : new Error('Unknown error'))
        throw err
      }
    },
    []
  )

  const rateAPI = useCallback(async (apiId: string, rating: number) => {
    try {
      const response = await fetch(`/api/backstage/apis/${apiId}/rating`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ rating }),
      })
      if (!response.ok) throw new Error('Failed to rate API')
      const updated = await response.json()
      setAPIs((prev) =>
        prev.map((a) => (a.id === apiId ? updated : a))
      )
      return updated
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'))
      throw err
    }
  }, [])

  useEffect(() => {
    fetchAPIs()
  }, [fetchAPIs])

  return {
    apis,
    loading,
    error,
    registerAPI,
    rateAPI,
    refetch: fetchAPIs,
  }
}

/**
 * Hook for real-time search across catalog
 */
export function useCatalogSearch(query: string, debounceMs = 300) {
  const [results, setResults] = useState<CatalogEntry[]>([])
  const [isSearching, setIsSearching] = useState(false)
  const debounceTimer = useRef<NodeJS.Timeout>()

  useEffect(() => {
    if (debounceTimer.current) {
      clearTimeout(debounceTimer.current)
    }

    if (!query.trim()) {
      setResults([])
      return
    }

    setIsSearching(true)

    debounceTimer.current = setTimeout(async () => {
      try {
        const response = await fetch(
          `/api/backstage/search?q=${encodeURIComponent(query)}`
        )
        if (!response.ok) throw new Error('Search failed')
        const data = await response.json()
        setResults(data.results)
      } catch (err) {
        console.error('Search error:', err)
        setResults([])
      } finally {
        setIsSearching(false)
      }
    }, debounceMs)

    return () => {
      if (debounceTimer.current) {
        clearTimeout(debounceTimer.current)
      }
    }
  }, [query, debounceMs])

  return {
    results,
    isSearching,
    hasQuery: query.trim().length > 0,
  }
}
