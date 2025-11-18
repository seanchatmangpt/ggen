'use client'

import { useRef, useState, useCallback } from 'react'
import Editor from '@monaco-editor/react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Input } from '@/components/ui/input'
import {
  Wand2,
  RefreshCw,
  Copy,
  Download,
  Zap,
  MessageSquare,
  Settings,
  Eye,
  Loader,
  CheckCircle2,
  AlertCircle,
} from 'lucide-react'
import type * as Monaco from 'monaco-editor'

interface AICodeEditorProps {
  defaultLanguage?: string
  defaultValue?: string
  onChange?: (value: string) => void
  onGenerate?: (code: string) => void
  theme?: 'light' | 'dark'
  height?: string
  readOnly?: boolean
  showAIFeatures?: boolean
}

/**
 * Advanced Code Editor with AI Integration
 * Powered by Monaco Editor and Claude AI
 */
export function AICodeEditor({
  defaultLanguage = 'typescript',
  defaultValue = '',
  onChange,
  onGenerate,
  theme = 'dark',
  height = '600px',
  readOnly = false,
  showAIFeatures = true,
}: AICodeEditorProps) {
  const editorRef = useRef<Monaco.editor.IStandaloneCodeEditor | null>(null)
  const [code, setCode] = useState(defaultValue)
  const [language, setLanguage] = useState(defaultLanguage)
  const [isGenerating, setIsGenerating] = useState(false)
  const [prompt, setPrompt] = useState('')
  const [analysis, setAnalysis] = useState<string>('')
  const [showAnalysis, setShowAnalysis] = useState(false)
  const [showPreview, setShowPreview] = useState(false)
  const [selectedTab, setSelectedTab] = useState('editor')

  const languages = ['typescript', 'javascript', 'python', 'rust', 'go', 'java', 'csharp']

  const handleEditorChange = useCallback((value: string | undefined) => {
    if (value !== undefined) {
      setCode(value)
      onChange?.(value)
    }
  }, [onChange])

  const handleGenerateCode = useCallback(async () => {
    if (!prompt.trim()) return

    setIsGenerating(true)
    try {
      const response = await fetch('/api/monaco/generate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          language,
          description: prompt,
          type: 'service',
        }),
      })

      if (!response.ok) throw new Error('Failed to generate code')

      const reader = response.body?.getReader()
      if (!reader) throw new Error('No response body')

      let generatedCode = ''
      while (true) {
        const { done, value } = await reader.read()
        if (done) break

        const text = new TextDecoder().decode(value)
        generatedCode += text

        if (editorRef.current) {
          editorRef.current.setValue(generatedCode)
          setCode(generatedCode)
        }
      }

      onGenerate?.(generatedCode)
      setPrompt('')
    } catch (err) {
      console.error('Code generation failed:', err)
    } finally {
      setIsGenerating(false)
    }
  }, [prompt, language, onGenerate])

  const handleAnalyzeCode = useCallback(async () => {
    try {
      const response = await fetch('/api/monaco/analyze', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          code,
          language,
          type: 'refactor',
        }),
      })

      if (!response.ok) throw new Error('Failed to analyze code')

      const data = await response.json()
      setAnalysis(JSON.stringify(data, null, 2))
      setShowAnalysis(true)
    } catch (err) {
      console.error('Analysis failed:', err)
    }
  }, [code, language])

  const handleRefactorCode = useCallback(async () => {
    setIsGenerating(true)
    try {
      const response = await fetch('/api/monaco/refactor', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code, language }),
      })

      if (!response.ok) throw new Error('Failed to refactor code')

      const reader = response.body?.getReader()
      if (!reader) throw new Error('No response body')

      let refactoredCode = ''
      while (true) {
        const { done, value } = await reader.read()
        if (done) break

        const text = new TextDecoder().decode(value)
        refactoredCode += text

        if (editorRef.current) {
          editorRef.current.setValue(refactoredCode)
          setCode(refactoredCode)
        }
      }
    } catch (err) {
      console.error('Refactor failed:', err)
    } finally {
      setIsGenerating(false)
    }
  }, [code, language])

  const handleCopyCode = useCallback(() => {
    navigator.clipboard.writeText(code)
  }, [code])

  const handleDownloadCode = useCallback(() => {
    const element = document.createElement('a')
    const file = new Blob([code], { type: 'text/plain' })
    element.href = URL.createObjectURL(file)
    element.download = `code.${language}`
    document.body.appendChild(element)
    element.click()
    document.body.removeChild(element)
  }, [code, language])

  return (
    <div className="space-y-4">
      {/* Header with Controls */}
      <Card>
        <CardHeader className="pb-3">
          <div className="flex items-center justify-between">
            <div>
              <CardTitle>AI Code Editor</CardTitle>
              <CardDescription>
                Powered by Claude AI and Monaco Editor
              </CardDescription>
            </div>
            <div className="flex items-center gap-2">
              <select
                value={language}
                onChange={(e) => setLanguage(e.target.value)}
                className="px-3 py-2 text-sm border rounded-md bg-white"
                disabled={readOnly}
              >
                {languages.map((lang) => (
                  <option key={lang} value={lang}>
                    {lang}
                  </option>
                ))}
              </select>
              <Badge variant="outline">{code.length} characters</Badge>
            </div>
          </div>
        </CardHeader>

        {/* AI Controls */}
        {showAIFeatures && (
          <CardContent className="space-y-3">
            <div className="flex gap-2">
              <Input
                placeholder="Describe the code you want to generate..."
                value={prompt}
                onChange={(e) => setPrompt(e.target.value)}
                disabled={isGenerating || readOnly}
                onKeyPress={(e) => {
                  if (e.key === 'Enter') handleGenerateCode()
                }}
              />
              <Button
                onClick={handleGenerateCode}
                disabled={isGenerating || !prompt.trim() || readOnly}
                className="gap-2"
              >
                {isGenerating ? (
                  <Loader className="h-4 w-4 animate-spin" />
                ) : (
                  <Wand2 className="h-4 w-4" />
                )}
                Generate
              </Button>
            </div>

            {/* Action Buttons */}
            <div className="flex gap-2 flex-wrap">
              <Button
                size="sm"
                variant="outline"
                onClick={handleAnalyzeCode}
                disabled={!code || isGenerating}
                className="gap-2"
              >
                <MessageSquare className="h-4 w-4" />
                Analyze
              </Button>
              <Button
                size="sm"
                variant="outline"
                onClick={handleRefactorCode}
                disabled={!code || isGenerating || readOnly}
                className="gap-2"
              >
                <RefreshCw className="h-4 w-4" />
                Refactor
              </Button>
              <Button
                size="sm"
                variant="outline"
                onClick={handleCopyCode}
                disabled={!code}
                className="gap-2"
              >
                <Copy className="h-4 w-4" />
                Copy
              </Button>
              <Button
                size="sm"
                variant="outline"
                onClick={handleDownloadCode}
                disabled={!code}
                className="gap-2"
              >
                <Download className="h-4 w-4" />
                Download
              </Button>
            </div>
          </CardContent>
        )}
      </Card>

      {/* Tabs for Editor and Analysis */}
      <Tabs value={selectedTab} onValueChange={setSelectedTab}>
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="editor">Editor</TabsTrigger>
          <TabsTrigger value="analysis" disabled={!showAnalysis}>
            Analysis
          </TabsTrigger>
          <TabsTrigger value="preview">Preview</TabsTrigger>
        </TabsList>

        {/* Editor Tab */}
        <TabsContent value="editor" className="space-y-2">
          <Card>
            <CardContent className="p-0">
              <Editor
                height={height}
                defaultLanguage={language}
                language={language}
                value={code}
                onChange={handleEditorChange}
                theme={theme === 'dark' ? 'vs-dark' : 'vs'}
                onMount={(editor) => {
                  editorRef.current = editor
                }}
                options={{
                  minimap: { enabled: true },
                  formatOnPaste: true,
                  formatOnType: true,
                  automaticLayout: true,
                  readOnly,
                  scrollBeyondLastLine: false,
                  fontSize: 14,
                  lineNumbers: 'on',
                  fontFamily: 'Fira Code, monospace',
                  bracketPairColorization: true,
                  autoClosingBrackets: 'always',
                  autoClosingQuotes: 'always',
                  autoIndent: 'full',
                }}
              />
            </CardContent>
          </Card>
        </TabsContent>

        {/* Analysis Tab */}
        <TabsContent value="analysis" className="space-y-2">
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Code Analysis</CardTitle>
            </CardHeader>
            <CardContent>
              <pre className="bg-slate-900 text-slate-100 p-4 rounded overflow-auto max-h-96 text-sm">
                {analysis}
              </pre>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Preview Tab */}
        <TabsContent value="preview" className="space-y-2">
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Code Preview</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="bg-slate-50 dark:bg-slate-900 p-4 rounded border">
                <p className="text-sm text-slate-600 dark:text-slate-400 mb-3">
                  Syntax-highlighted preview of your code:
                </p>
                <pre className="overflow-auto max-h-96 text-sm font-mono">
                  <code>{code}</code>
                </pre>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}

/**
 * Minimal AI Code Editor for embedded usage
 */
export function CompactAICodeEditor({
  defaultLanguage = 'typescript',
  defaultValue = '',
  onSubmit,
}: {
  defaultLanguage?: string
  defaultValue?: string
  onSubmit?: (code: string) => void
}) {
  const [code, setCode] = useState(defaultValue)
  const [isSubmitting, setIsSubmitting] = useState(false)

  const handleSubmit = async () => {
    setIsSubmitting(true)
    try {
      onSubmit?.(code)
    } finally {
      setIsSubmitting(false)
    }
  }

  return (
    <div className="space-y-2">
      <Editor
        height="300px"
        defaultLanguage={defaultLanguage}
        language={defaultLanguage}
        value={code}
        onChange={(val) => setCode(val || '')}
        theme="vs-dark"
        options={{
          minimap: { enabled: false },
          scrollBeyondLastLine: false,
          fontSize: 12,
        }}
      />
      <div className="flex gap-2">
        <Button
          onClick={handleSubmit}
          disabled={isSubmitting || !code}
          className="gap-2"
        >
          {isSubmitting ? (
            <Loader className="h-4 w-4 animate-spin" />
          ) : (
            <CheckCircle2 className="h-4 w-4" />
          )}
          Submit
        </Button>
        <Button variant="outline" onClick={() => setCode(defaultValue)}>
          Reset
        </Button>
      </div>
    </div>
  )
}
