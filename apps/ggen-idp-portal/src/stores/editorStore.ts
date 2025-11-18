import { create } from 'zustand'
import { EditorState, EditorError, AICodeSuggestion } from '@/types'

interface EditorStoreState extends EditorState {
  suggestions: AICodeSuggestion[]
  isGeneratingSuggestions: boolean
  selectedSuggestionId: string | null

  // Actions
  setContent: (content: string) => void
  setLanguage: (language: string) => void
  setDirty: (isDirty: boolean) => void
  setErrors: (errors: EditorError[]) => void
  addError: (error: EditorError) => void
  removeError: (index: number) => void
  clearErrors: () => void
  setSuggestions: (suggestions: AICodeSuggestion[]) => void
  setIsGeneratingSuggestions: (isGenerating: boolean) => void
  selectSuggestion: (suggestionId: string | null) => void
  applySuggestion: (suggestionId: string) => void
}

export const useEditorStore = create<EditorStoreState>((set, get) => ({
  content: '',
  language: 'yaml',
  isDirty: false,
  errors: [],
  suggestions: [],
  isGeneratingSuggestions: false,
  selectedSuggestionId: null,

  setContent: (content: string) => {
    set({ content, isDirty: true })
  },

  setLanguage: (language: string) => {
    set({ language })
  },

  setDirty: (isDirty: boolean) => {
    set({ isDirty })
  },

  setErrors: (errors: EditorError[]) => {
    set({ errors })
  },

  addError: (error: EditorError) => {
    set((state) => ({
      errors: [...state.errors, error],
    }))
  },

  removeError: (index: number) => {
    set((state) => ({
      errors: state.errors.filter((_, i) => i !== index),
    }))
  },

  clearErrors: () => {
    set({ errors: [] })
  },

  setSuggestions: (suggestions: AICodeSuggestion[]) => {
    set({ suggestions })
  },

  setIsGeneratingSuggestions: (isGenerating: boolean) => {
    set({ isGeneratingSuggestions: isGenerating })
  },

  selectSuggestion: (suggestionId: string | null) => {
    set({ selectedSuggestionId: suggestionId })
  },

  applySuggestion: (suggestionId: string) => {
    const { suggestions, content } = get()
    const suggestion = suggestions.find((s) => s.id === suggestionId)
    if (suggestion) {
      set({
        content: suggestion.code,
        isDirty: true,
        selectedSuggestionId: null,
      })
    }
  },
}))
