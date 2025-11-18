'use client'

import { useState, useMemo } from 'react'
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { useCatalog, useCatalogSearch } from '@/hooks/use-backstage'
import {
  Search,
  Filter,
  Star,
  Download,
  GitBranch,
  Users,
  Activity,
  TrendingUp,
} from 'lucide-react'
import type { ComponentMetadata } from '@/lib/backstage-types'

interface CatalogExplorerProps {
  initialFilters?: Record<string, string[]>
  onSelectComponent?: (component: ComponentMetadata) => void
}

/**
 * Component Catalog Explorer
 * Browse, search, and filter all available components and services
 */
export function CatalogExplorer({
  initialFilters = {},
  onSelectComponent,
}: CatalogExplorerProps) {
  const [filters, setFilters] = useState(initialFilters)
  const [searchQuery, setSearchQuery] = useState('')
  const [sortBy, setSortBy] = useState('relevance')

  const { items, loading, totalCount } = useCatalog({
    filters,
    sort: sortBy,
  })

  const { results: searchResults, isSearching } = useCatalogSearch(searchQuery)

  const displayItems = searchQuery.trim() ? searchResults : items

  const statusColors = {
    active: 'bg-green-100 text-green-800',
    deprecated: 'bg-red-100 text-red-800',
    experimental: 'bg-yellow-100 text-yellow-800',
  }

  const typeIcons = {
    service: '🔧',
    library: '📚',
    template: '📋',
    plugin: '🔌',
  }

  return (
    <div className="space-y-6">
      {/* Search and Filter Bar */}
      <Card>
        <CardContent className="pt-6">
          <div className="space-y-4">
            {/* Search Input */}
            <div className="relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-5 w-5 text-slate-400" />
              <Input
                placeholder="Search components, services, templates..."
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                className="pl-10"
              />
              {isSearching && (
                <div className="absolute right-3 top-1/2 transform -translate-y-1/2">
                  <div className="animate-spin h-4 w-4 border-2 border-blue-500 border-t-transparent rounded-full" />
                </div>
              )}
            </div>

            {/* Filters and Sort */}
            <div className="flex gap-2 flex-wrap">
              <div className="flex gap-1">
                {['service', 'library', 'template', 'plugin'].map((type) => (
                  <Badge
                    key={type}
                    variant={
                      filters[type]
                        ? 'default'
                        : 'outline'
                    }
                    className="cursor-pointer"
                    onClick={() => {
                      setFilters((prev) => {
                        const current = prev[type] || []
                        if (current.includes(type)) {
                          const newFilters = { ...prev }
                          delete newFilters[type]
                          return newFilters
                        }
                        return { ...prev, [type]: [type] }
                      })
                    }}
                  >
                    {type}
                  </Badge>
                ))}
              </div>

              <select
                value={sortBy}
                onChange={(e) => setSortBy(e.target.value)}
                className="px-3 py-1 text-sm border rounded-md"
              >
                <option value="relevance">Relevance</option>
                <option value="downloads">Most Downloaded</option>
                <option value="stars">Most Starred</option>
                <option value="recent">Recently Updated</option>
                <option value="health">Health Score</option>
              </select>
            </div>

            {/* Result Count */}
            <p className="text-sm text-slate-600">
              {loading ? 'Loading...' : `Showing ${displayItems.length} of ${totalCount} components`}
            </p>
          </div>
        </CardContent>
      </Card>

      {/* Component Grid */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {displayItems.map((component) => (
          <Card
            key={component.id}
            className="hover:shadow-lg transition-shadow cursor-pointer"
            onClick={() => onSelectComponent?.(component)}
          >
            <CardHeader>
              <div className="flex items-start justify-between mb-2">
                <div className="flex items-center gap-2">
                  <span className="text-2xl">
                    {typeIcons[component.type as keyof typeof typeIcons]}
                  </span>
                  <Badge
                    className={
                      statusColors[component.status as keyof typeof statusColors]
                    }
                  >
                    {component.status}
                  </Badge>
                </div>
              </div>
              <CardTitle className="text-lg">{component.name}</CardTitle>
              <CardDescription className="line-clamp-2">
                {component.description}
              </CardDescription>
            </CardHeader>

            <CardContent className="space-y-4">
              {/* Metadata */}
              <div className="grid grid-cols-2 gap-2 text-xs">
                <div>
                  <p className="text-slate-600">Version</p>
                  <p className="font-mono font-semibold">{component.version}</p>
                </div>
                <div>
                  <p className="text-slate-600">Language</p>
                  <p className="font-semibold">{component.language}</p>
                </div>
                <div>
                  <p className="text-slate-600">Owner</p>
                  <p className="font-semibold truncate">{component.owner}</p>
                </div>
                <div>
                  <p className="text-slate-600">Team</p>
                  <p className="font-semibold">{component.team}</p>
                </div>
              </div>

              {/* Tags */}
              {component.tags.length > 0 && (
                <div className="flex flex-wrap gap-1">
                  {component.tags.slice(0, 3).map((tag) => (
                    <Badge
                      key={tag}
                      variant="secondary"
                      className="text-xs"
                    >
                      {tag}
                    </Badge>
                  ))}
                  {component.tags.length > 3 && (
                    <Badge variant="secondary" className="text-xs">
                      +{component.tags.length - 3}
                    </Badge>
                  )}
                </div>
              )}

              {/* Stats */}
              <div className="flex gap-4 pt-2 border-t">
                <div className="flex items-center gap-1 text-xs">
                  <Download className="h-4 w-4 text-slate-600" />
                  <span className="font-semibold">
                    {component.downloads}
                  </span>
                </div>
                <div className="flex items-center gap-1 text-xs">
                  <Star className="h-4 w-4 text-yellow-600" />
                  <span className="font-semibold">{component.stars}</span>
                </div>
                <div className="flex items-center gap-1 text-xs">
                  <TrendingUp className="h-4 w-4 text-green-600" />
                  <span className="font-semibold">
                    {component.healthScore}%
                  </span>
                </div>
              </div>

              {/* Action Buttons */}
              <div className="flex gap-2 pt-2">
                <Button
                  size="sm"
                  variant="outline"
                  className="flex-1"
                >
                  View
                </Button>
                <Button
                  size="sm"
                  className="flex-1"
                >
                  Use
                </Button>
              </div>
            </CardContent>
          </Card>
        ))}
      </div>

      {displayItems.length === 0 && !loading && (
        <Card className="text-center py-12">
          <p className="text-slate-600">
            {searchQuery
              ? 'No components found matching your search'
              : 'No components available'}
          </p>
        </Card>
      )}
    </div>
  )
}
