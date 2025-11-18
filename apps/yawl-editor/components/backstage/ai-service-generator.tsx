'use client'

import { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { AICodeEditor } from '@/components/monaco/ai-code-editor'
import {
  Zap,
  Plus,
  GitBranch,
  Package,
  Code2,
  CheckCircle2,
  Loader,
} from 'lucide-react'

interface ServiceGeneratorProps {
  onServiceCreate?: (service: any) => void
}

/**
 * AI Service Generator
 * Generate complete services with Backstage integration
 */
export function AIServiceGenerator({ onServiceCreate }: ServiceGeneratorProps) {
  const [serviceName, setServiceName] = useState('')
  const [serviceDescription, setServiceDescription] = useState('')
  const [generatedCode, setGeneratedCode] = useState('')
  const [isGenerating, setIsGenerating] = useState(false)
  const [activeTab, setActiveTab] = useState('config')
  const [dependencies, setDependencies] = useState<string[]>([])
  const [apis, setApis] = useState<string[]>([])

  const handleGenerateService = async () => {
    if (!serviceName || !serviceDescription) return

    setIsGenerating(true)
    try {
      const response = await fetch('/api/monaco/generate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          language: 'typescript',
          type: 'service',
          description: `Create a ${serviceName} service that ${serviceDescription}`,
          requirements: [
            'Type-safe implementation',
            'Error handling',
            'Logging',
            `APIs: ${apis.join(', ') || 'None specified'}`,
            `Dependencies: ${dependencies.join(', ') || 'Standard'}`,
          ],
        }),
      })

      if (!response.ok) throw new Error('Failed to generate service')

      const reader = response.body?.getReader()
      if (!reader) throw new Error('No response body')

      let code = ''
      while (true) {
        const { done, value } = await reader.read()
        if (done) break

        code += new TextDecoder().decode(value)
        setGeneratedCode(code)
      }

      setActiveTab('code')
    } catch (err) {
      console.error('Generation failed:', err)
    } finally {
      setIsGenerating(false)
    }
  }

  const handleCreateService = async () => {
    if (!serviceName || !generatedCode) return

    try {
      const response = await fetch('/api/backstage/services', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: serviceName,
          namespace: 'generated',
          title: serviceName,
          description: serviceDescription,
          spec: {
            type: 'REST',
            lifecycle: 'development',
            owner: 'ai-generated',
            providesApis: apis,
          },
        }),
      })

      if (!response.ok) throw new Error('Failed to create service')

      const created = await response.json()
      onServiceCreate?.(created)

      // Reset form
      setServiceName('')
      setServiceDescription('')
      setGeneratedCode('')
      setDependencies([])
      setApis([])
      setActiveTab('config')
    } catch (err) {
      console.error('Service creation failed:', err)
    }
  }

  return (
    <div className="space-y-4">
      <Tabs value={activeTab} onValueChange={setActiveTab}>
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="config">Configuration</TabsTrigger>
          <TabsTrigger value="code" disabled={!generatedCode}>
            Generated Code
          </TabsTrigger>
          <TabsTrigger value="preview" disabled={!generatedCode}>
            Preview
          </TabsTrigger>
        </TabsList>

        {/* Configuration Tab */}
        <TabsContent value="config" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Service Configuration</CardTitle>
              <CardDescription>
                Configure your service before AI generation
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              {/* Service Name */}
              <div className="space-y-2">
                <label className="font-medium text-sm">Service Name</label>
                <Input
                  placeholder="e.g., user-service, payment-api"
                  value={serviceName}
                  onChange={(e) => setServiceName(e.target.value)}
                  disabled={isGenerating}
                />
              </div>

              {/* Description */}
              <div className="space-y-2">
                <label className="font-medium text-sm">Description</label>
                <textarea
                  placeholder="Describe what this service does..."
                  value={serviceDescription}
                  onChange={(e) => setServiceDescription(e.target.value)}
                  disabled={isGenerating}
                  className="w-full px-3 py-2 border rounded-md min-h-24"
                />
              </div>

              {/* Dependencies */}
              <div className="space-y-2">
                <label className="font-medium text-sm">Dependencies</label>
                <div className="space-y-2">
                  <Input
                    placeholder="Add dependency and press Enter..."
                    onKeyPress={(e) => {
                      if (e.key === 'Enter' && (e.target as HTMLInputElement).value) {
                        const newDeps = [...dependencies, (e.target as HTMLInputElement).value]
                        setDependencies(newDeps)
                        ;(e.target as HTMLInputElement).value = ''
                      }
                    }}
                    disabled={isGenerating}
                  />
                  <div className="flex flex-wrap gap-2">
                    {dependencies.map((dep) => (
                      <Badge
                        key={dep}
                        variant="secondary"
                        onClick={() =>
                          setDependencies(dependencies.filter((d) => d !== dep))
                        }
                        className="cursor-pointer"
                      >
                        {dep}
                        <span className="ml-1">✕</span>
                      </Badge>
                    ))}
                  </div>
                </div>
              </div>

              {/* APIs */}
              <div className="space-y-2">
                <label className="font-medium text-sm">Provided APIs</label>
                <div className="space-y-2">
                  <Input
                    placeholder="Add API endpoint and press Enter..."
                    onKeyPress={(e) => {
                      if (e.key === 'Enter' && (e.target as HTMLInputElement).value) {
                        const newApis = [...apis, (e.target as HTMLInputElement).value]
                        setApis(newApis)
                        ;(e.target as HTMLInputElement).value = ''
                      }
                    }}
                    disabled={isGenerating}
                  />
                  <div className="flex flex-wrap gap-2">
                    {apis.map((api) => (
                      <Badge
                        key={api}
                        variant="outline"
                        onClick={() => setApis(apis.filter((a) => a !== api))}
                        className="cursor-pointer"
                      >
                        {api}
                        <span className="ml-1">✕</span>
                      </Badge>
                    ))}
                  </div>
                </div>
              </div>

              {/* Generate Button */}
              <Button
                onClick={handleGenerateService}
                disabled={isGenerating || !serviceName || !serviceDescription}
                className="w-full gap-2"
              >
                {isGenerating ? (
                  <>
                    <Loader className="h-4 w-4 animate-spin" />
                    Generating...
                  </>
                ) : (
                  <>
                    <Zap className="h-4 w-4" />
                    Generate Service Code
                  </>
                )}
              </Button>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Code Tab */}
        <TabsContent value="code">
          <AICodeEditor
            defaultLanguage="typescript"
            defaultValue={generatedCode}
            onChange={setGeneratedCode}
            theme="dark"
            height="600px"
            readOnly={false}
            showAIFeatures={true}
          />
        </TabsContent>

        {/* Preview Tab */}
        <TabsContent value="preview">
          <Card>
            <CardHeader>
              <CardTitle>Service Preview</CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <p className="text-sm font-medium text-slate-600 mb-1">
                    Service Name
                  </p>
                  <p className="font-semibold">{serviceName}</p>
                </div>
                <div>
                  <p className="text-sm font-medium text-slate-600 mb-1">Status</p>
                  <Badge className="bg-blue-100 text-blue-800">Generated</Badge>
                </div>
              </div>

              <div>
                <p className="text-sm font-medium text-slate-600 mb-1">
                  Description
                </p>
                <p className="text-sm">{serviceDescription}</p>
              </div>

              {dependencies.length > 0 && (
                <div>
                  <p className="text-sm font-medium text-slate-600 mb-2">
                    Dependencies
                  </p>
                  <div className="flex flex-wrap gap-2">
                    {dependencies.map((dep) => (
                      <Badge key={dep} variant="secondary">
                        {dep}
                      </Badge>
                    ))}
                  </div>
                </div>
              )}

              {apis.length > 0 && (
                <div>
                  <p className="text-sm font-medium text-slate-600 mb-2">APIs</p>
                  <div className="space-y-1">
                    {apis.map((api) => (
                      <div key={api} className="text-sm flex items-center gap-2">
                        <GitBranch className="h-4 w-4 text-slate-600" />
                        {api}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              <Button
                onClick={handleCreateService}
                disabled={!generatedCode}
                className="w-full gap-2"
              >
                <CheckCircle2 className="h-4 w-4" />
                Create Service in Backstage
              </Button>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}
