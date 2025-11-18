'use client'

import React, { useRef, useCallback, useEffect } from 'react'
import Editor from '@monaco-editor/react'
import { useEditorStore } from '@/stores/editorStore'
import { Lightbulb, Wand2, AlertCircle } from 'lucide-react'
import toast from 'react-hot-toast'

interface CodeEditorProps {
  value?: string
  language?: string
  onChange?: (value: string) => void
  onSuggestionsGenerated?: (suggestions: any[]) => void
  height?: string
  theme?: 'light' | 'dark'
  readOnly?: boolean
  showSuggestions?: boolean
}

export const CodeEditor: React.FC<CodeEditorProps> = ({
  value,
  language = 'yaml',
  onChange,
  onSuggestionsGenerated,
  height = '500px',
  theme = 'dark',
  readOnly = false,
  showSuggestions = true,
}) => {
  const editorRef = useRef<any>(null)
  const { content, setContent, errors, setErrors, suggestions, setSuggestions } =
    useEditorStore()

  const handleEditorChange = useCallback(
    (value: string | undefined) => {
      if (value !== undefined) {
        setContent(value)
        onChange?.(value)
      }
    },
    [setContent, onChange]
  )

  const handleEditorMount = (editor: any) => {
    editorRef.current = editor
    editor.focus()
  }

  const generateSuggestions = useCallback(async () => {
    if (!showSuggestions) return

    try {
      const response = await fetch('/api/ai/suggest', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          code: content,
          language,
          type: 'code-completion',
        }),
      })

      if (!response.ok) throw new Error('Failed to generate suggestions')

      const data = await response.json()
      setSuggestions(data.suggestions || [])
      onSuggestionsGenerated?.(data.suggestions || [])

      if (data.errors && data.errors.length > 0) {
        setErrors(data.errors)
      }

      toast.success(`Generated ${data.suggestions?.length || 0} suggestions`)
    } catch (error) {
      toast.error('Failed to generate suggestions')
      console.error('Suggestion generation failed:', error)
    }
  }, [content, language, setSuggestions, setErrors, onSuggestionsGenerated, showSuggestions])

  const validateCode = useCallback(async () => {
    try {
      const response = await fetch('/api/ai/validate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          code: content,
          language,
        }),
      })

      const data = await response.json()
      if (data.errors && data.errors.length > 0) {
        setErrors(data.errors)
        toast.error(`Found ${data.errors.length} validation issues`)
      } else {
        setErrors([])
        toast.success('Code is valid!')
      }
    } catch (error) {
      toast.error('Validation failed')
      console.error('Validation failed:', error)
    }
  }, [content, language, setErrors])

  return (
    <div className="flex flex-col h-full rounded-lg border border-slate-300 bg-white overflow-hidden">
      {/* Toolbar */}
      <div className="flex items-center justify-between px-4 py-3 bg-slate-100 border-b border-slate-300">
        <div className="flex items-center gap-2">
          <span className="text-sm font-medium text-slate-700">{language.toUpperCase()}</span>
        </div>
        <div className="flex items-center gap-2">
          {showSuggestions && (
            <button
              onClick={generateSuggestions}
              className="btn btn-secondary text-sm gap-1"
              title="Generate AI suggestions"
            >
              <Wand2 size={16} />
              Suggest
            </button>
          )}
          <button
            onClick={validateCode}
            className="btn btn-secondary text-sm gap-1"
            title="Validate code"
          >
            <AlertCircle size={16} />
            Validate
          </button>
        </div>
      </div>

      {/* Editor */}
      <Editor
        height={height}
        defaultLanguage={language}
        language={language}
        value={value || content}
        onChange={handleEditorChange}
        onMount={handleEditorMount}
        theme={theme === 'dark' ? 'vs-dark' : 'vs'}
        options={{
          minimap: { enabled: true },
          wordWrap: 'on',
          formatOnPaste: true,
          formatOnType: true,
          autoIndent: 'full',
          tabSize: 2,
          readOnly,
          scrollBeyondLastLine: false,
          fontSize: 13,
          fontFamily: "'Fira Code', Monaco, monospace",
          lineNumbers: 'on',
        }}
      />

      {/* Errors */}
      {errors.length > 0 && (
        <div className="border-t border-slate-300 bg-red-50 p-3">
          <div className="space-y-2">
            {errors.map((error, idx) => (
              <div key={idx} className="flex items-start gap-2 text-sm">
                <AlertCircle size={16} className="text-red-600 mt-0.5 flex-shrink-0" />
                <div>
                  <div className="font-medium text-red-800">
                    Line {error.line}, Column {error.column}
                  </div>
                  <div className="text-red-700">{error.message}</div>
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Suggestions */}
      {showSuggestions && suggestions.length > 0 && (
        <div className="border-t border-slate-300 bg-blue-50 p-3 max-h-40 overflow-y-auto">
          <div className="flex items-center gap-2 mb-2">
            <Lightbulb size={16} className="text-blue-600" />
            <span className="text-sm font-medium text-blue-900">AI Suggestions</span>
          </div>
          <div className="space-y-2">
            {suggestions.slice(0, 3).map((suggestion) => (
              <div
                key={suggestion.id}
                className="p-2 bg-white rounded border border-blue-200 cursor-pointer hover:bg-blue-100 transition-colors"
                onClick={() => {
                  setContent(suggestion.code)
                  onChange?.(suggestion.code)
                }}
              >
                <div className="text-sm font-medium text-blue-900">{suggestion.description}</div>
                <div className="text-xs text-blue-700 mt-1">{suggestion.explanation}</div>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}
