'use client'

import { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Badge } from '@/components/ui/badge'
import { ThreeDVisualizer } from '@/components/visualization/three-d-visualizer'
import {
  Box,
  Boxes,
  Code2,
  Eye,
  Settings,
  Upload,
  Grid3x3,
  Zap,
  ArrowRight,
} from 'lucide-react'

/**
 * 3D Spatial Visualization Viewer
 * 2028 Innovation Phase: AR/VR Integration
 */
export default function ThreeDViewerPage() {
  const [activeTab, setActiveTab] = useState('process')
  const [selectedMode, setSelectedMode] = useState<'process' | 'code' | 'vr' | 'presentation'>(
    'process'
  )
  const [exportedData, setExportedData] = useState<string | null>(null)

  // Sample process data
  const sampleProcessData = {
    elements: [
      {
        id: 'task1',
        name: 'Initialize Process',
        type: 'TASK',
        outgoing: [{ targetRef: 'gateway1' }],
      },
      {
        id: 'gateway1',
        name: 'Decision Point',
        type: 'EXCLUSIVE_GATEWAY',
        outgoing: [
          { targetRef: 'task2' },
          { targetRef: 'task3' },
        ],
      },
      {
        id: 'task2',
        name: 'Process Path A',
        type: 'TASK',
        outgoing: [{ targetRef: 'endEvent' }],
      },
      {
        id: 'task3',
        name: 'Process Path B',
        type: 'TASK',
        outgoing: [{ targetRef: 'endEvent' }],
      },
      {
        id: 'endEvent',
        name: 'End',
        type: 'END_EVENT',
        outgoing: [],
      },
    ],
  }

  // Sample code data
  const sampleCodeData = {
    name: 'Application',
    type: 'module',
    children: [
      {
        name: 'UserService',
        type: 'class',
        children: [
          { name: 'authenticate', type: 'function' },
          { name: 'authorize', type: 'function' },
          { name: 'getCurrentUser', type: 'function' },
        ],
      },
      {
        name: 'DataService',
        type: 'class',
        children: [
          { name: 'fetchData', type: 'function' },
          { name: 'updateData', type: 'function' },
          { name: 'deleteData', type: 'function' },
        ],
      },
      {
        name: 'Utilities',
        type: 'module',
        children: [
          { name: 'formatDate', type: 'function' },
          { name: 'parseJson', type: 'function' },
          { name: 'validateEmail', type: 'function' },
        ],
      },
    ],
  }

  const handleExport = (data: string) => {
    setExportedData(data)
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800 p-4 md:p-8">
      {/* Header */}
      <div className="max-w-7xl mx-auto mb-8">
        <div className="flex items-center gap-4 mb-6">
          <div className="p-3 bg-gradient-to-br from-blue-100 to-purple-100 dark:from-blue-900 dark:to-purple-900 rounded-lg">
            <Box className="h-8 w-8 text-blue-600 dark:text-blue-400" />
          </div>
          <div>
            <h1 className="text-4xl font-bold">3D Spatial Visualization</h1>
            <p className="text-slate-600 dark:text-slate-400 text-lg">
              AR/VR-ready process and code structure visualization
            </p>
          </div>
        </div>

        {/* Features Overview */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
          <Card className="border-blue-200 dark:border-blue-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <Box className="h-5 w-5 text-blue-600" />
                3D BPMN
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-slate-600 dark:text-slate-400">
                Visualize workflow processes in interactive 3D space
              </p>
            </CardContent>
          </Card>

          <Card className="border-purple-200 dark:border-purple-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <Code2 className="h-5 w-5 text-purple-600" />
                Code Structure
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-slate-600 dark:text-slate-400">
                Explore code architecture as interactive 3D trees
              </p>
            </CardContent>
          </Card>

          <Card className="border-pink-200 dark:border-pink-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <Eye className="h-5 w-5 text-pink-600" />
                WebXR VR
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-slate-600 dark:text-slate-400">
                Immersive VR experience with WebXR support
              </p>
            </CardContent>
          </Card>

          <Card className="border-green-200 dark:border-green-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <Zap className="h-5 w-5 text-green-600" />
                Real-time
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-slate-600 dark:text-slate-400">
                Live rendering with Three.js at 60fps
              </p>
            </CardContent>
          </Card>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-7xl mx-auto">
        <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-4">
          <TabsList className="grid w-full grid-cols-2 md:grid-cols-4">
            <TabsTrigger value="process" className="gap-2">
              <Boxes className="h-4 w-4" />
              <span className="hidden sm:inline">Process</span>
            </TabsTrigger>
            <TabsTrigger value="code" className="gap-2">
              <Code2 className="h-4 w-4" />
              <span className="hidden sm:inline">Code</span>
            </TabsTrigger>
            <TabsTrigger value="vr" className="gap-2">
              <Eye className="h-4 w-4" />
              <span className="hidden sm:inline">VR</span>
            </TabsTrigger>
            <TabsTrigger value="docs" className="gap-2">
              <Grid3x3 className="h-4 w-4" />
              <span className="hidden sm:inline">Docs</span>
            </TabsTrigger>
          </TabsList>

          {/* Process Tab */}
          <TabsContent value="process" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>BPMN Process Visualization</CardTitle>
                <CardDescription>
                  Interactive 3D visualization of BPMN workflows with real-time rendering
                </CardDescription>
              </CardHeader>
              <CardContent>
                <ThreeDVisualizer
                  mode="process"
                  processData={sampleProcessData}
                  height="600px"
                  showStats={true}
                  onExport={handleExport}
                />
              </CardContent>
            </Card>
          </TabsContent>

          {/* Code Tab */}
          <TabsContent value="code" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Code Structure Visualization</CardTitle>
                <CardDescription>
                  Explore code architecture as interactive 3D tree structures with dependency graphs
                </CardDescription>
              </CardHeader>
              <CardContent>
                <ThreeDVisualizer
                  mode="code"
                  codeData={sampleCodeData}
                  height="600px"
                  showStats={true}
                  onExport={handleExport}
                />
              </CardContent>
            </Card>
          </TabsContent>

          {/* VR Tab */}
          <TabsContent value="vr" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Immersive VR Experience</CardTitle>
                <CardDescription>
                  Experience 3D visualizations in virtual reality with WebXR support
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <ThreeDVisualizer
                  mode="vr"
                  processData={sampleProcessData}
                  height="600px"
                  showStats={true}
                />

                <div className="bg-purple-50 dark:bg-purple-900/20 border border-purple-200 dark:border-purple-900/50 rounded-lg p-4">
                  <h3 className="font-semibold mb-2 flex items-center gap-2">
                    <Eye className="h-4 w-4" />
                    VR Requirements
                  </h3>
                  <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
                    <li>✓ WebXR-compatible VR headset (Meta Quest, HTC Vive, etc.)</li>
                    <li>✓ Modern browser with WebXR support</li>
                    <li>✓ HTTPS connection for security</li>
                    <li>✓ Optional: Hand-tracking capabilities</li>
                  </ul>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Docs Tab */}
          <TabsContent value="docs" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Documentation & Features</CardTitle>
              </CardHeader>
              <CardContent className="space-y-6">
                {/* API Section */}
                <div>
                  <h3 className="font-semibold mb-3">Core API</h3>
                  <div className="space-y-2">
                    <div className="bg-slate-100 dark:bg-slate-800 p-3 rounded text-sm font-mono overflow-x-auto">
                      <code>
                        new Visualization3D(canvasId, config)
                      </code>
                    </div>
                    <div className="bg-slate-100 dark:bg-slate-800 p-3 rounded text-sm font-mono overflow-x-auto">
                      <code>
                        visualizer.addBpmnProcess(process)
                      </code>
                    </div>
                    <div className="bg-slate-100 dark:bg-slate-800 p-3 rounded text-sm font-mono overflow-x-auto">
                      <code>
                        visualizer.addCodeStructure(ast)
                      </code>
                    </div>
                    <div className="bg-slate-100 dark:bg-slate-800 p-3 rounded text-sm font-mono overflow-x-auto">
                      <code>
                        visualizer.enableVrMode()
                      </code>
                    </div>
                  </div>
                </div>

                {/* Features Grid */}
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div>
                    <h3 className="font-semibold mb-2">Rendering</h3>
                    <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
                      <li>✓ Three.js-based 3D engine</li>
                      <li>✓ 60 FPS real-time rendering</li>
                      <li>✓ Multiple lighting presets</li>
                      <li>✓ Auto-rotation support</li>
                    </ul>
                  </div>
                  <div>
                    <h3 className="font-semibold mb-2">Interaction</h3>
                    <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
                      <li>✓ Mouse-based camera control</li>
                      <li>✓ Touch gesture support</li>
                      <li>✓ Object selection & highlighting</li>
                      <li>✓ Zoom to fit functionality</li>
                    </ul>
                  </div>
                  <div>
                    <h3 className="font-semibold mb-2">Export</h3>
                    <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
                      <li>✓ JSON scene export</li>
                      <li>✓ GLTF 3D model export</li>
                      <li>✓ Screenshot capture</li>
                      <li>✓ Animation recording</li>
                    </ul>
                  </div>
                  <div>
                    <h3 className="font-semibold mb-2">Immersion</h3>
                    <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
                      <li>✓ WebXR VR support</li>
                      <li>✓ Hand-tracking ready</li>
                      <li>✓ Spatial audio integration</li>
                      <li>✓ Multi-platform support</li>
                    </ul>
                  </div>
                </div>

                {/* Presets */}
                <div>
                  <h3 className="font-semibold mb-3">Visualization Presets</h3>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                    {[
                      {
                        name: 'Process Flow',
                        desc: 'Optimized for BPMN diagrams',
                        icon: Boxes,
                      },
                      {
                        name: 'Code Structure',
                        desc: 'Optimized for code trees',
                        icon: Code2,
                      },
                      {
                        name: 'VR Experience',
                        desc: 'Optimized for immersion',
                        icon: Eye,
                      },
                      {
                        name: 'Presentation',
                        desc: 'Optimized for demos',
                        icon: Settings,
                      },
                    ].map((preset) => {
                      const Icon = preset.icon
                      return (
                        <div
                          key={preset.name}
                          className="flex items-start gap-3 p-3 bg-slate-100 dark:bg-slate-800 rounded"
                        >
                          <Icon className="h-5 w-5 text-blue-600 dark:text-blue-400 flex-shrink-0 mt-0.5" />
                          <div className="text-sm">
                            <p className="font-medium">{preset.name}</p>
                            <p className="text-slate-600 dark:text-slate-400">{preset.desc}</p>
                          </div>
                        </div>
                      )
                    })}
                  </div>
                </div>

                {/* Export Info */}
                {exportedData && (
                  <div>
                    <h3 className="font-semibold mb-2">Last Export</h3>
                    <div className="bg-green-50 dark:bg-green-900/20 border border-green-200 dark:border-green-900/50 rounded-lg p-4 text-sm">
                      <p className="text-slate-600 dark:text-slate-400 mb-2">
                        Scene exported successfully at{' '}
                        {new Date().toLocaleTimeString()}
                      </p>
                      <pre className="bg-slate-900 text-slate-100 p-3 rounded text-xs overflow-x-auto max-h-48 overflow-y-auto">
                        {JSON.stringify(JSON.parse(exportedData), null, 2).substring(0, 500)}
                        ...
                      </pre>
                    </div>
                  </div>
                )}
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </div>

      {/* Footer */}
      <div className="max-w-7xl mx-auto mt-12 pt-8 border-t border-slate-200 dark:border-slate-800">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
          <div>
            <h3 className="font-semibold mb-2">Technology</h3>
            <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
              <li>Three.js 3D Engine</li>
              <li>WebXR API</li>
              <li>GLTF Standard</li>
              <li>TypeScript</li>
            </ul>
          </div>
          <div>
            <h3 className="font-semibold mb-2">Browser Support</h3>
            <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
              <li>Chrome 90+</li>
              <li>Firefox 88+</li>
              <li>Safari 15+</li>
              <li>Edge 90+</li>
            </ul>
          </div>
          <div>
            <h3 className="font-semibold mb-2">VR Devices</h3>
            <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
              <li>Meta Quest 2/3</li>
              <li>HTC Vive Focus</li>
              <li>PlayStation VR</li>
              <li>Windows Mixed Reality</li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  )
}
