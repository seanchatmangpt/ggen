'use client'

import React, { useEffect, useRef, useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import {
  Visualization3D,
  Visualization3DPresets,
  Node3D,
  Edge3D,
} from '@/lib/three-d-visualization'
import {
  Box,
  Maximize2,
  RotateCcw,
  VolumeX,
  Download,
  Eye,
  Grid3x3,
  Zap,
} from 'lucide-react'

interface ThreeDVisualizerProps {
  mode?: 'process' | 'code' | 'vr' | 'presentation'
  processData?: any
  codeData?: any
  height?: string
  showStats?: boolean
  onExport?: (data: string) => void
}

/**
 * 3D Visualizer Component
 * Renders 3D spatial visualizations of processes and code structures
 */
export function ThreeDVisualizer({
  mode = 'process',
  processData,
  codeData,
  height = '600px',
  showStats = true,
  onExport,
}: ThreeDVisualizerProps) {
  const canvasRef = useRef<HTMLDivElement>(null)
  const vizRef = useRef<Visualization3D | null>(null)
  const [isLoading, setIsLoading] = useState(true)
  const [isVrSupported, setIsVrSupported] = useState(false)
  const [isVrActive, setIsVrActive] = useState(false)
  const [stats, setStats] = useState({ nodeCount: 0, edgeCount: 0, meshCount: 0 })
  const [isAnimating, setIsAnimating] = useState(true)

  // Initialize visualizer
  useEffect(() => {
    if (!canvasRef.current) return

    try {
      // Get preset configuration
      const baseConfig = {
        width: canvasRef.current.clientWidth,
        height: parseInt(height),
        backgroundColor: '#f5f5f5',
      }

      const preset = Visualization3DPresets[mode as keyof typeof Visualization3DPresets]
      const config = preset ? preset(baseConfig) : baseConfig

      // Create visualizer
      const viz = new Visualization3D('3d-canvas', config as any)
      vizRef.current = viz

      // Load data
      if (mode === 'process' && processData) {
        viz.addBpmnProcess(processData)
      } else if (mode === 'code' && codeData) {
        viz.addCodeStructure(codeData)
      }

      // Start animation
      if (isAnimating) {
        viz.animate(() => {
          setStats(viz.getStatistics())
        })
      }

      // Check VR support
      if (navigator.xr) {
        navigator.xr?.isSessionSupported('immersive-vr').then((supported) => {
          setIsVrSupported(supported)
        })
      }

      setIsLoading(false)
    } catch (error) {
      console.error('Failed to initialize visualizer:', error)
      setIsLoading(false)
    }

    return () => {
      if (vizRef.current) {
        vizRef.current.dispose()
      }
    }
  }, [mode, processData, codeData, height, isAnimating])

  const handleZoomToFit = () => {
    vizRef.current?.zoomToFit()
  }

  const handleResetView = () => {
    vizRef.current?.resetView()
  }

  const handleToggleAnimation = () => {
    if (vizRef.current) {
      if (isAnimating) {
        vizRef.current.stopAnimation()
      } else {
        vizRef.current.animate()
      }
      setIsAnimating(!isAnimating)
    }
  }

  const handleToggleVr = async () => {
    if (!vizRef.current) return

    try {
      if (isVrActive) {
        await vizRef.current.exitVrMode()
        setIsVrActive(false)
      } else {
        await vizRef.current.enableVrMode()
        setIsVrActive(true)
      }
    } catch (error) {
      console.error('VR mode toggle failed:', error)
    }
  }

  const handleExport = () => {
    if (!vizRef.current) return

    const data = vizRef.current.exportAsJson()
    const blob = new Blob([data], { type: 'application/json' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `3d-visualization-${Date.now()}.json`
    a.click()
    URL.revokeObjectURL(url)

    onExport?.(data)
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="p-2 bg-gradient-to-br from-blue-100 to-purple-100 dark:from-blue-900 dark:to-purple-900 rounded-lg">
              <Box className="h-5 w-5 text-blue-600 dark:text-blue-400" />
            </div>
            <div>
              <CardTitle className="flex items-center gap-2">
                3D Spatial Visualizer
                {isVrActive && (
                  <Badge className="bg-purple-600">VR Active</Badge>
                )}
              </CardTitle>
              <CardDescription>
                {mode === 'process' && 'BPMN Process Flow Visualization'}
                {mode === 'code' && 'Code Structure Tree Visualization'}
                {mode === 'vr' && 'Immersive VR Experience'}
                {mode === 'presentation' && 'Presentation Mode'}
              </CardDescription>
            </div>
          </div>

          {/* Stats */}
          {showStats && !isLoading && (
            <div className="flex gap-4 text-xs">
              <div className="text-center">
                <p className="text-slate-600 dark:text-slate-400">Nodes</p>
                <p className="font-bold">{stats.nodeCount}</p>
              </div>
              <div className="text-center">
                <p className="text-slate-600 dark:text-slate-400">Edges</p>
                <p className="font-bold">{stats.edgeCount}</p>
              </div>
              <div className="text-center">
                <p className="text-slate-600 dark:text-slate-400">Meshes</p>
                <p className="font-bold">{stats.meshCount}</p>
              </div>
            </div>
          )}
        </div>
      </CardHeader>

      <CardContent className="space-y-4">
        {/* 3D Canvas */}
        <div
          ref={canvasRef}
          style={{ height }}
          className="relative bg-gradient-to-br from-slate-100 to-slate-50 dark:from-slate-800 dark:to-slate-900 rounded-lg overflow-hidden border border-slate-200 dark:border-slate-700"
        >
          {isLoading && (
            <div className="absolute inset-0 flex items-center justify-center bg-white/80 dark:bg-slate-900/80 backdrop-blur-sm">
              <div className="text-center">
                <div className="animate-spin mb-4">
                  <Box className="h-8 w-8 text-blue-600" />
                </div>
                <p className="text-sm font-medium">Loading 3D Visualization...</p>
              </div>
            </div>
          )}

          {/* Placeholder for Three.js canvas */}
          <div id="3d-canvas" className="w-full h-full" />

          {/* Watermark */}
          {!isLoading && (
            <div className="absolute bottom-2 right-2 text-xs text-slate-400 dark:text-slate-600 pointer-events-none">
              3D Spatial Visualizer (2028)
            </div>
          )}
        </div>

        {/* Controls */}
        <div className="flex flex-wrap gap-2">
          <Button
            size="sm"
            variant="outline"
            onClick={handleZoomToFit}
            className="gap-2"
            disabled={isLoading}
          >
            <Maximize2 className="h-4 w-4" />
            Zoom Fit
          </Button>

          <Button
            size="sm"
            variant="outline"
            onClick={handleResetView}
            className="gap-2"
            disabled={isLoading}
          >
            <RotateCcw className="h-4 w-4" />
            Reset View
          </Button>

          <Button
            size="sm"
            variant="outline"
            onClick={handleToggleAnimation}
            className="gap-2"
            disabled={isLoading}
          >
            <Zap className="h-4 w-4" />
            {isAnimating ? 'Pause' : 'Play'}
          </Button>

          {isVrSupported && (
            <Button
              size="sm"
              variant={isVrActive ? 'default' : 'outline'}
              onClick={handleToggleVr}
              className="gap-2"
              disabled={isLoading}
            >
              <Eye className="h-4 w-4" />
              {isVrActive ? 'Exit VR' : 'Enter VR'}
            </Button>
          )}

          <Button
            size="sm"
            variant="outline"
            onClick={handleExport}
            className="gap-2"
            disabled={isLoading}
          >
            <Download className="h-4 w-4" />
            Export
          </Button>
        </div>

        {/* Info Panel */}
        {!isLoading && (
          <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-900/50 rounded-lg p-4 text-sm">
            <div className="flex gap-4">
              <div>
                <p className="font-semibold mb-1 flex items-center gap-2">
                  <Grid3x3 className="h-4 w-4" />
                  Interaction
                </p>
                <ul className="text-xs text-slate-600 dark:text-slate-400 space-y-1">
                  <li>• Mouse: Drag to rotate</li>
                  <li>• Scroll: Zoom in/out</li>
                  <li>• Right-click: Pan camera</li>
                </ul>
              </div>
              <div>
                <p className="font-semibold mb-1">Features</p>
                <ul className="text-xs text-slate-600 dark:text-slate-400 space-y-1">
                  <li>• Real-time 3D rendering</li>
                  <li>• WebXR VR support</li>
                  <li>• GLTF export</li>
                  <li>• Auto-rotation mode</li>
                </ul>
              </div>
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  )
}

/**
 * Compact 3D Visualizer variant
 */
export function CompactThreeDVisualizer(props: Omit<ThreeDVisualizerProps, 'height'>) {
  return <ThreeDVisualizer {...props} height="300px" showStats={false} />
}
