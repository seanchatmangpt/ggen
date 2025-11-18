'use client'

import { useState, useCallback } from 'react'
import { BpmnDiagram } from '@/components/bpmn/bpmn-diagram'
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogDescription,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Card } from '@/components/ui/card'
import {
  Tabs,
  TabsContent,
  TabsList,
  TabsTrigger,
} from '@/components/ui/tabs'
import {
  parseBpmnXml,
  bpmnToYawlOntology,
  bpmnToSparqlInsert,
  exportAsBpmnXml,
} from '@/lib/bpmn-to-yawl'
import {
  analyzeProcess,
  generateOptimizations,
  detectProcessRisks,
  createProcessAnalysisReport,
} from '@/lib/ai-process-analyzer'
import { Download, Save, Zap } from 'lucide-react'

export default function DesignerPage() {
  const [diagramXml, setDiagramXml] = useState<string>('')
  const [showAnalysis, setShowAnalysis] = useState(false)
  const [showExport, setShowExport] = useState(false)
  const [exportFormat, setExportFormat] = useState<'bpmn' | 'yawl' | 'sparql'>('bpmn')
  const [exportedContent, setExportedContent] = useState<string>('')

  const handleDiagramChange = useCallback((xml: string) => {
    setDiagramXml(xml)
  }, [])

  const handleAnalyze = () => {
    if (!diagramXml) {
      alert('Please create or import a BPMN diagram first')
      return
    }
    setShowAnalysis(true)
  }

  const handleExport = (format: 'bpmn' | 'yawl' | 'sparql') => {
    if (!diagramXml) {
      alert('Please create or import a BPMN diagram first')
      return
    }

    const process = parseBpmnXml(diagramXml)
    if (!process) {
      alert('Could not parse BPMN diagram')
      return
    }

    let content = ''
    switch (format) {
      case 'yawl':
        content = bpmnToYawlOntology(process)
        break
      case 'sparql':
        content = bpmnToSparqlInsert(process)
        break
      case 'bpmn':
      default:
        content = exportAsBpmnXml(process)
    }

    setExportedContent(content)
    setExportFormat(format)
    setShowExport(true)
  }

  const handleDownload = () => {
    const element = document.createElement('a')
    const file = new Blob([exportedContent], {
      type: 'text/plain',
    })
    element.href = URL.createObjectURL(file)

    const extension = exportFormat === 'sparql' ? 'sparql' : exportFormat
    element.download = `process.${extension}`
    document.body.appendChild(element)
    element.click()
    document.body.removeChild(element)
  }

  const analysis = diagramXml ? (() => {
    const process = parseBpmnXml(diagramXml)
    return process ? createProcessAnalysisReport(process) : null
  })() : null

  return (
    <div className="flex h-screen flex-col gap-4 p-4 bg-slate-50">
      {/* Header */}
      <div className="space-y-2">
        <h1 className="text-3xl font-bold">Process Designer</h1>
        <p className="text-slate-600">
          Design BPMN workflows and convert to YAWL ontology
        </p>
      </div>

      {/* Toolbar */}
      <div className="flex gap-2 flex-wrap">
        <Button variant="default" onClick={handleAnalyze} className="gap-2">
          <Zap className="h-4 w-4" />
          Analyze Process
        </Button>

        <Button variant="outline" onClick={() => handleExport('bpmn')} className="gap-2">
          <Download className="h-4 w-4" />
          Export as BPMN
        </Button>

        <Button variant="outline" onClick={() => handleExport('yawl')} className="gap-2">
          <Download className="h-4 w-4" />
          Export as YAWL
        </Button>

        <Button variant="outline" onClick={() => handleExport('sparql')} className="gap-2">
          <Download className="h-4 w-4" />
          Export as SPARQL
        </Button>

        <Button variant="outline">
          <Save className="h-4 w-4 mr-2" />
          Save Process
        </Button>
      </div>

      {/* Main Editor */}
      <div className="flex-1 bg-white rounded-lg border overflow-hidden shadow-sm">
        <BpmnDiagram
          diagramXml={diagramXml}
          onDiagramChange={handleDiagramChange}
        />
      </div>

      {/* Analysis Dialog */}
      {analysis && (
        <Dialog open={showAnalysis} onOpenChange={setShowAnalysis}>
          <DialogContent className="max-w-2xl max-h-[80vh] overflow-y-auto">
            <DialogHeader>
              <DialogTitle>Process Analysis</DialogTitle>
              <DialogDescription>
                AI-powered insights and recommendations for your workflow
              </DialogDescription>
            </DialogHeader>

            <div className="space-y-6">
              {/* Summary */}
              <Card className="p-4 bg-blue-50 border-blue-200">
                <p className="text-sm text-blue-900">{analysis.summary}</p>
              </Card>

              {/* Metrics */}
              <div className="space-y-2">
                <h3 className="font-semibold">Metrics</h3>
                <div className="grid grid-cols-2 gap-3">
                  <div className="bg-slate-50 p-3 rounded">
                    <p className="text-xs text-slate-600">Total Tasks</p>
                    <p className="text-lg font-bold">{analysis.metrics.totalTasks}</p>
                  </div>
                  <div className="bg-slate-50 p-3 rounded">
                    <p className="text-xs text-slate-600">Total Gateways</p>
                    <p className="text-lg font-bold">{analysis.metrics.totalGateways}</p>
                  </div>
                  <div className="bg-slate-50 p-3 rounded">
                    <p className="text-xs text-slate-600">Process Depth</p>
                    <p className="text-lg font-bold">{analysis.metrics.depth}</p>
                  </div>
                  <div className="bg-slate-50 p-3 rounded">
                    <p className="text-xs text-slate-600">Complexity</p>
                    <Badge
                      className={
                        analysis.metrics.complexity === 'Low'
                          ? 'bg-green-100 text-green-800'
                          : analysis.metrics.complexity === 'Medium'
                            ? 'bg-yellow-100 text-yellow-800'
                            : analysis.metrics.complexity === 'High'
                              ? 'bg-orange-100 text-orange-800'
                              : 'bg-red-100 text-red-800'
                      }
                    >
                      {analysis.metrics.complexity}
                    </Badge>
                  </div>
                </div>
              </div>

              {/* Insights */}
              <div className="space-y-2">
                <h3 className="font-semibold">Key Insights</h3>
                <ul className="space-y-2">
                  {analysis.insights.map((insight, i) => (
                    <li key={i} className="flex gap-2 text-sm">
                      <span className="text-blue-500">•</span>
                      <span>{insight}</span>
                    </li>
                  ))}
                </ul>
              </div>

              {/* Suggestions */}
              <div className="space-y-2">
                <h3 className="font-semibold">Improvement Suggestions</h3>
                <ul className="space-y-2">
                  {analysis.suggestions.map((suggestion, i) => (
                    <li key={i} className="flex gap-2 text-sm">
                      <span className="text-green-500">✓</span>
                      <span>{suggestion}</span>
                    </li>
                  ))}
                </ul>
              </div>

              {/* Risks */}
              <div className="space-y-2">
                <h3 className="font-semibold">Potential Risks</h3>
                <ul className="space-y-2">
                  {analysis.risks.map((risk, i) => (
                    <li key={i} className="flex gap-2 text-sm">
                      <span className="text-red-500">⚠</span>
                      <span>{risk}</span>
                    </li>
                  ))}
                </ul>
              </div>
            </div>
          </DialogContent>
        </Dialog>
      )}

      {/* Export Dialog */}
      <Dialog open={showExport} onOpenChange={setShowExport}>
        <DialogContent className="max-w-2xl max-h-[80vh] overflow-y-auto">
          <DialogHeader>
            <DialogTitle>
              Export Process as {exportFormat.toUpperCase()}
            </DialogTitle>
          </DialogHeader>

          <Tabs defaultValue="preview" className="w-full">
            <TabsList>
              <TabsTrigger value="preview">Preview</TabsTrigger>
              <TabsTrigger value="raw">Raw</TabsTrigger>
            </TabsList>

            <TabsContent value="preview" className="space-y-4">
              <div className="max-h-96 overflow-auto bg-slate-50 p-4 rounded border font-mono text-xs whitespace-pre-wrap break-words">
                {exportedContent}
              </div>
            </TabsContent>

            <TabsContent value="raw">
              <textarea
                value={exportedContent}
                readOnly
                className="w-full h-96 p-4 font-mono text-xs border rounded"
              />
            </TabsContent>
          </Tabs>

          <div className="flex gap-2">
            <Button onClick={handleDownload} className="gap-2">
              <Download className="h-4 w-4" />
              Download
            </Button>
            <Button
              variant="outline"
              onClick={() => {
                navigator.clipboard.writeText(exportedContent)
                alert('Copied to clipboard!')
              }}
            >
              Copy to Clipboard
            </Button>
          </div>
        </DialogContent>
      </Dialog>
    </div>
  )
}
