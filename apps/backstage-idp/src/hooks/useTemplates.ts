import { useState, useCallback, useMemo } from 'react'
import type { Template } from '@/types'

export function useTemplates(templates: Template[]) {
  const [selectedTemplate, setSelectedTemplate] = useState<Template | null>(null)
  const [search, setSearch] = useState('')
  const [selectedCategory, setSelectedCategory] = useState<string | null>(null)

  const filteredTemplates = useMemo(() => {
    return templates
      .filter((t) => {
        if (search && !t.name.toLowerCase().includes(search.toLowerCase())) return false
        if (selectedCategory && t.category !== selectedCategory) return false
        return true
      })
      .sort((a, b) => b.downloads - a.downloads)
  }, [templates, search, selectedCategory])

  const categories = useMemo(() => {
    return [...new Set(templates.map((t) => t.category))].sort()
  }, [templates])

  return {
    templates: filteredTemplates,
    selectedTemplate,
    setSelectedTemplate,
    search,
    setSearch,
    categories,
    selectedCategory,
    setSelectedCategory,
  }
}
