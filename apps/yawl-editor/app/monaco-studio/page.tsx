'use client'

import { useState } from 'react'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { AICodeEditor, CompactAICodeEditor } from '@/components/monaco/ai-code-editor'
import {
  codeTemplates,
  getTemplatesByCategory,
  searchTemplates,
} from '@/lib/code-templates'
import {
  Code2,
  Zap,
  BookOpen,
  Settings,
  Plus,
  Search,
  Copy,
  ExternalLink,
} from 'lucide-react'

/**
 * AI Code Studio
 * Integrated Monaco editor with Vercel AI for code generation
 */
export default function MonacoStudioPage() {
  const [activeTab, setActiveTab] = useState('editor')
  const [generatedCode, setGeneratedCode] = useState('')
  const [selectedTemplate, setSelectedTemplate] = useState(codeTemplates[0])
  const [searchQuery, setSearchQuery] = useState('')
  const [language, setLanguage] = useState('typescript')
  const [theme, setTheme] = useState<'light' | 'dark'>('dark')

  const filteredTemplates = searchQuery
    ? searchTemplates(searchQuery)
    : codeTemplates

  const handleTemplateSelect = (templateId: string) => {
    const template = codeTemplates.find((t) => t.id === templateId)
    if (template) {
      setSelectedTemplate(template)
      setLanguage(template.language)
      setGeneratedCode(template.code)
    }
  }

  const handleCodeGenerated = (code: string) => {
    setGeneratedCode(code)
    setActiveTab('editor')
  }

  const handleCopyCode = () => {
    navigator.clipboard.writeText(generatedCode)
  }

  return (
    <div className="min-h-screen bg-slate-50 dark:bg-slate-900 p-4 md:p-8">
      {/* Header */}
      <div className="mb-8 max-w-6xl mx-auto">
        <div className="flex items-center gap-3 mb-2">
          <div className="p-2 bg-gradient-to-br from-blue-100 to-purple-100 dark:from-blue-900 dark:to-purple-900 rounded-lg">
            <Code2 className="h-6 w-6 text-blue-600 dark:text-blue-400" />
          </div>
          <h1 className="text-4xl font-bold">AI Code Studio</h1>
        </div>
        <p className="text-slate-600 dark:text-slate-400">
          Monaco Editor powered by Claude AI - Generate, analyze, refactor, and manage code
        </p>
      </div>

      {/* Main Content */}
      <div className="max-w-7xl mx-auto space-y-6">
        <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-4">
          <TabsList className="grid w-full grid-cols-2 md:grid-cols-4">
            <TabsTrigger value="editor" className="gap-2">
              <Code2 className="h-4 w-4" />
              <span className="hidden sm:inline">Editor</span>
            </TabsTrigger>
            <TabsTrigger value="templates" className="gap-2">
              <BookOpen className="h-4 w-4" />
              <span className="hidden sm:inline">Templates</span>
            </TabsTrigger>
            <TabsTrigger value="generator" className="gap-2">
              <Zap className="h-4 w-4" />
              <span className="hidden sm:inline">Generate</span>
            </TabsTrigger>
            <TabsTrigger value="settings" className="gap-2">
              <Settings className="h-4 w-4" />
              <span className="hidden sm:inline">Settings</span>
            </TabsTrigger>
          </TabsList>

          {/* Editor Tab */}
          <TabsContent value="editor" className="space-y-4">
            <AICodeEditor
              defaultLanguage={language}
              defaultValue={generatedCode}
              onChange={setGeneratedCode}
              onGenerate={handleCodeGenerated}
              theme={theme}
              height="700px"
              showAIFeatures={true}
            />
          </TabsContent>

          {/* Templates Tab */}
          <TabsContent value="templates" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Code Templates</CardTitle>
                <CardDescription>
                  Pre-built templates for services, components, hooks, and more
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                {/* Search */}
                <div className="relative">
                  <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-5 w-5 text-slate-400" />
                  <Input
                    placeholder="Search templates..."
                    value={searchQuery}
                    onChange={(e) => setSearchQuery(e.target.value)}
                    className="pl-10"
                  />
                </div>

                {/* Template Grid */}
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  {filteredTemplates.map((template) => (
                    <Card
                      key={template.id}
                      className={`cursor-pointer hover:shadow-lg transition-shadow ${
                        selectedTemplate?.id === template.id
                          ? 'ring-2 ring-blue-500'
                          : ''
                      }`}
                      onClick={() => handleTemplateSelect(template.id)}
                    >
                      <CardHeader className="pb-3">
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <CardTitle className="text-base">
                              {template.name}
                            </CardTitle>
                            <CardDescription className="text-xs mt-1">
                              {template.description}
                            </CardDescription>
                          </div>
                          <Badge variant="outline" className="text-xs">
                            {template.category}
                          </Badge>
                        </div>
                      </CardHeader>
                      <CardContent>
                        <div className="flex gap-2 flex-wrap mb-3">
                          {template.tags.map((tag) => (
                            <Badge
                              key={tag}
                              variant="secondary"
                              className="text-xs"
                            >
                              {tag}
                            </Badge>
                          ))}
                        </div>
                        <div className="flex gap-2">
                          <Button
                            size="sm"
                            variant="outline"
                            className="flex-1"
                            onClick={(e) => {
                              e.stopPropagation()
                              handleTemplateSelect(template.id)
                              setActiveTab('editor')
                            }}
                          >
                            <Code2 className="h-3 w-3 mr-1" />
                            Use
                          </Button>
                          <Button
                            size="sm"
                            variant="outline"
                            onClick={(e) => {
                              e.stopPropagation()
                              navigator.clipboard.writeText(template.code)
                            }}
                          >
                            <Copy className="h-3 w-3" />
                          </Button>
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </div>

                {filteredTemplates.length === 0 && (
                  <div className="text-center py-8">
                    <p className="text-slate-600">No templates found</p>
                  </div>
                )}
              </CardContent>
            </Card>
          </TabsContent>

          {/* Generator Tab */}
          <TabsContent value="generator" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Code Generator</CardTitle>
                <CardDescription>
                  Generate code using AI from templates and requirements
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-6">
                {/* Quick Generate */}
                <div className="space-y-4">
                  <h3 className="font-semibold">Quick Generate</h3>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    {[
                      {
                        category: 'service',
                        label: 'Service',
                        icon: '🔧',
                      },
                      {
                        category: 'component',
                        label: 'React Component',
                        icon: '⚛️',
                      },
                      {
                        category: 'hook',
                        label: 'Custom Hook',
                        icon: '🪝',
                      },
                      {
                        category: 'test',
                        label: 'Test Suite',
                        icon: '✅',
                      },
                    ].map((item) => (
                      <Button
                        key={item.category}
                        variant="outline"
                        className="h-auto p-4 justify-start text-left"
                        onClick={() => {
                          const templates = getTemplatesByCategory(
                            item.category as any
                          )
                          if (templates.length > 0) {
                            handleTemplateSelect(templates[0].id)
                          }
                        }}
                      >
                        <span className="text-2xl mr-3">{item.icon}</span>
                        <div>
                          <p className="font-semibold text-sm">{item.label}</p>
                          <p className="text-xs text-slate-600">
                            Generate new {item.label.toLowerCase()}
                          </p>
                        </div>
                      </Button>
                    ))}
                  </div>
                </div>

                {/* Language Selection */}
                <div className="space-y-3">
                  <h3 className="font-semibold">Language</h3>
                  <div className="flex gap-2 flex-wrap">
                    {['typescript', 'javascript', 'python', 'rust', 'go'].map(
                      (lang) => (
                        <Badge
                          key={lang}
                          variant={language === lang ? 'default' : 'outline'}
                          className="cursor-pointer"
                          onClick={() => setLanguage(lang)}
                        >
                          {lang}
                        </Badge>
                      )
                    )}
                  </div>
                </div>

                {/* Theme Selection */}
                <div className="space-y-3">
                  <h3 className="font-semibold">Editor Theme</h3>
                  <div className="flex gap-2">
                    {(['light', 'dark'] as const).map((t) => (
                      <Button
                        key={t}
                        variant={theme === t ? 'default' : 'outline'}
                        onClick={() => setTheme(t)}
                        className="capitalize"
                      >
                        {t}
                      </Button>
                    ))}
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Settings Tab */}
          <TabsContent value="settings" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Editor Settings</CardTitle>
                <CardDescription>
                  Configure Monaco editor preferences
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="space-y-2">
                    <label className="font-medium text-sm">Theme</label>
                    <select
                      value={theme}
                      onChange={(e) => setTheme(e.target.value as any)}
                      className="w-full px-3 py-2 border rounded-md"
                    >
                      <option value="light">Light</option>
                      <option value="dark">Dark</option>
                    </select>
                  </div>

                  <div className="space-y-2">
                    <label className="font-medium text-sm">Language</label>
                    <select
                      value={language}
                      onChange={(e) => setLanguage(e.target.value)}
                      className="w-full px-3 py-2 border rounded-md"
                    >
                      <option value="typescript">TypeScript</option>
                      <option value="javascript">JavaScript</option>
                      <option value="python">Python</option>
                      <option value="rust">Rust</option>
                      <option value="go">Go</option>
                      <option value="java">Java</option>
                    </select>
                  </div>
                </div>

                <div className="bg-slate-50 dark:bg-slate-800 p-4 rounded-lg space-y-2">
                  <h3 className="font-semibold text-sm">Features</h3>
                  <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
                    <li>✓ AI-powered code generation</li>
                    <li>✓ Real-time code analysis</li>
                    <li>✓ Automatic refactoring suggestions</li>
                    <li>✓ Code formatting and highlighting</li>
                    <li>✓ Multiple language support</li>
                    <li>✓ Template library with pre-built code</li>
                  </ul>
                </div>

                <Button className="w-full">Save Settings</Button>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>

        {/* Code Preview */}
        {generatedCode && (
          <Card>
            <CardHeader className="pb-3">
              <div className="flex items-center justify-between">
                <CardTitle className="text-base">Generated Code</CardTitle>
                <Button
                  size="sm"
                  variant="outline"
                  onClick={handleCopyCode}
                  className="gap-2"
                >
                  <Copy className="h-4 w-4" />
                  Copy All
                </Button>
              </div>
            </CardHeader>
            <CardContent>
              <pre className="bg-slate-900 text-slate-100 p-4 rounded overflow-auto max-h-96 text-sm font-mono">
                <code>{generatedCode}</code>
              </pre>
            </CardContent>
          </Card>
        )}
      </div>
    </div>
  )
}
