"use client"

import React, { useRef, useEffect, useState, useCallback } from "react"
import { useAIAssistant } from "@/hooks/useAIAssistant"
import { Copy, Loader, AlertCircle, CheckCircle, Zap } from "lucide-react"

interface MonacoEditorProps {
  value: string
  onChange: (value: string) => void
  language?: string
  height?: string
  enableAI?: boolean
  placeholder?: string
  aiContext?: string
  readOnly?: boolean
  theme?: "vs" | "vs-dark"
}

export function MonacoEditor({
  value,
  onChange,
  language = "yaml",
  height = "400px",
  enableAI = true,
  placeholder = "Enter your workflow definition here...",
  aiContext = "",
  readOnly = false,
  theme = "vs-dark",
}: MonacoEditorProps) {
  const containerRef = useRef<HTMLDivElement>(null)
  const editorRef = useRef<any>(null)
  const monacoRef = useRef<any>(null)
  const [copied, setCopied] = useState(false)

  const { completion, isLoading, error, generateSuggestions, generateWorkflowSuggestions, analyzeWorkflow } =
    useAIAssistant({
      context: aiContext,
    })

  // Load Monaco Editor
  useEffect(() => {
    const loadMonaco = async () => {
      try {
        const monaco = await import("monaco-editor")
        monacoRef.current = monaco

        if (containerRef.current && !editorRef.current) {
          editorRef.current = monaco.editor.create(containerRef.current, {
            value,
            language,
            theme: theme === "vs-dark" ? "vs-dark" : "vs",
            automaticLayout: true,
            readOnly,
            padding: { top: 16, bottom: 16 },
            minimap: { enabled: true },
            lineNumbers: "on",
            scrollBeyondLastLine: false,
            smoothScrolling: true,
            cursorStyle: "line",
            fontFamily: "'Fira Code', 'Monaco', monospace",
            fontSize: 13,
            lineHeight: 24,
            wordWrap: "on",
            formatOnPaste: true,
            formatOnType: true,
          })

          editorRef.current.onDidChangeModelContent(() => {
            const newValue = editorRef.current.getValue()
            onChange(newValue)
          })
        }
      } catch (err) {
        console.error("Failed to load Monaco Editor:", err)
      }
    }

    loadMonaco()

    return () => {
      if (editorRef.current) {
        editorRef.current.dispose()
        editorRef.current = null
      }
    }
  }, [onChange, language, theme, readOnly])

  // Keyboard shortcuts
  useEffect(() => {
    if (!editorRef.current || !enableAI) return

    const handleKeyDown = (e: any) => {
      if ((e.ctrlKey || e.metaKey) && e.key === "k") {
        e.preventDefault()
        const currentValue = editorRef.current.getValue()
        analyzeWorkflow(currentValue || placeholder)
      } else if ((e.ctrlKey || e.metaKey) && e.shiftKey && e.key === "S") {
        e.preventDefault()
        const currentValue = editorRef.current.getValue()
        generateWorkflowSuggestions(currentValue || placeholder)
      }
    }

    const subscription = editorRef.current.onKeyDown(handleKeyDown)

    return () => {
      if (subscription) subscription.dispose()
    }
  }, [enableAI, generateSuggestions, generateWorkflowSuggestions, analyzeWorkflow, placeholder])

  const handleCopy = useCallback(() => {
    navigator.clipboard.writeText(value)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }, [value])

  const applySuggestion = useCallback(() => {
    if (editorRef.current && completion) {
      editorRef.current.setValue(completion)
      onChange(completion)
    }
  }, [completion, onChange])

  return (
    <div className="w-full rounded-lg border border-border bg-background overflow-hidden shadow-sm">
      {/* Toolbar */}
      <div className="flex items-center justify-between px-4 py-2 bg-muted border-b border-border">
        <div className="flex items-center gap-2 text-xs text-muted-foreground">
          <span className="font-mono font-bold">{language.toUpperCase()}</span>
          {enableAI && (
            <>
              <span>•</span>
              <span className="flex items-center gap-1">
                <Zap className="h-3 w-3" />
                Ctrl+K to analyze
              </span>
              <span>•</span>
              <span>Ctrl+Shift+S for suggestions</span>
            </>
          )}
        </div>
        <div className="flex items-center gap-2">
          {enableAI && (
            <button
              onClick={() => analyzeWorkflow(value || placeholder)}
              disabled={isLoading}
              className="px-2 py-1 rounded text-xs bg-primary/10 hover:bg-primary/20 disabled:opacity-50 transition-colors flex items-center gap-1"
              title="Analyze workflow"
            >
              {isLoading ? (
                <>
                  <Loader className="h-3 w-3 animate-spin" />
                  <span>Analyzing...</span>
                </>
              ) : (
                <>
                  <Zap className="h-3 w-3" />
                  <span>Analyze</span>
                </>
              )}
            </button>
          )}
          <button
            onClick={handleCopy}
            className="px-2 py-1 rounded text-xs bg-secondary/50 hover:bg-secondary transition-colors flex items-center gap-1"
            title="Copy to clipboard"
          >
            <Copy className="h-3 w-3" />
            {copied ? "Copied" : "Copy"}
          </button>
        </div>
      </div>

      {/* Editor Container */}
      <div
        ref={containerRef}
        style={{ height }}
        className="relative"
      />

      {/* AI Suggestion Panel */}
      {enableAI && (completion || error) && (
        <div className={`border-t border-border p-4 ${error ? "bg-destructive/10" : "bg-primary/5"}`}>
          <div className="flex items-start gap-2 mb-2">
            {error ? (
              <>
                <AlertCircle className="h-4 w-4 text-destructive mt-0.5 flex-shrink-0" />
                <div className="text-sm text-destructive">{error.message || "Failed to analyze workflow"}</div>
              </>
            ) : completion ? (
              <>
                <CheckCircle className="h-4 w-4 text-green-600 mt-0.5 flex-shrink-0" />
                <div className="flex-1">
                  <div className="text-xs font-medium text-muted-foreground mb-2">AI Analysis & Suggestions:</div>
                  <div className="text-sm bg-background border border-border rounded p-3 font-mono max-h-40 overflow-y-auto whitespace-pre-wrap">
                    {completion}
                  </div>
                  <div className="flex gap-2 mt-2">
                    <button
                      onClick={applySuggestion}
                      className="px-3 py-1 rounded text-xs bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                    >
                      Apply Changes
                    </button>
                    <button
                      onClick={() => analyzeWorkflow(value)}
                      className="px-3 py-1 rounded text-xs bg-secondary hover:bg-secondary/80 transition-colors"
                    >
                      Re-Analyze
                    </button>
                  </div>
                </div>
              </>
            ) : null}
          </div>
        </div>
      )}
    </div>
  )
}
