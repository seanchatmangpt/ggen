import { useState, useCallback, useMemo } from 'react'
import type { Service } from '@/types'

interface CatalogFilters {
  search: string
  type: string[]
  language: string[]
  status: string[]
  team: string | null
}

export function useCatalog(services: Service[]) {
  const [filters, setFilters] = useState<CatalogFilters>({
    search: '',
    type: [],
    language: [],
    status: [],
    team: null,
  })

  const [sortBy, setSortBy] = useState<'name' | 'created' | 'updated'>('name')

  const filteredServices = useMemo(() => {
    return services
      .filter((service) => {
        if (filters.search && !service.name.toLowerCase().includes(filters.search.toLowerCase())) {
          return false
        }
        if (filters.type.length > 0 && !filters.type.includes(service.type)) {
          return false
        }
        if (filters.language.length > 0 && !service.language.some(l => filters.language.includes(l))) {
          return false
        }
        if (filters.status.length > 0 && !filters.status.includes(service.status)) {
          return false
        }
        if (filters.team && service.owner.id !== filters.team) {
          return false
        }
        return true
      })
      .sort((a, b) => {
        if (sortBy === 'name') return a.name.localeCompare(b.name)
        if (sortBy === 'created') return new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
        return new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime()
      })
  }, [services, filters, sortBy])

  const updateFilter = useCallback((key: keyof CatalogFilters, value: any) => {
    setFilters((prev) => ({ ...prev, [key]: value }))
  }, [])

  const stats = useMemo(() => ({
    total: services.length,
    filtered: filteredServices.length,
    byType: services.reduce((acc, s) => ({ ...acc, [s.type]: (acc[s.type] || 0) + 1 }), {} as Record<string, number>),
    byStatus: services.reduce((acc, s) => ({ ...acc, [s.status]: (acc[s.status] || 0) + 1 }), {} as Record<string, number>),
  }), [services, filteredServices])

  return {
    services: filteredServices,
    filters,
    updateFilter,
    sortBy,
    setSortBy,
    stats,
  }
}
