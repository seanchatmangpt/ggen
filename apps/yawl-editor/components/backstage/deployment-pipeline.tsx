'use client'

import { useDeploymentPipelines } from '@/hooks/use-backstage'
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import {
  Play,
  Pause,
  RotateCcw,
  ChevronDown,
  AlertCircle,
  CheckCircle2,
  Clock,
  Zap,
} from 'lucide-react'
import { useState } from 'react'
import type { DeploymentPipeline, PipelineStage } from '@/lib/backstage-types'

interface DeploymentPipelineProps {
  serviceId?: string
}

/**
 * Deployment Pipeline Manager
 * View, manage, and monitor CI/CD pipelines
 */
export function DeploymentPipeline({ serviceId }: DeploymentPipelineProps) {
  const { pipelines, loading, createPipeline } = useDeploymentPipelines(serviceId)
  const [expandedPipeline, setExpandedPipeline] = useState<string | null>(null)

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'success':
        return <CheckCircle2 className="h-5 w-5 text-green-600" />
      case 'failed':
        return <AlertCircle className="h-5 w-5 text-red-600" />
      case 'running':
        return <Zap className="h-5 w-5 text-blue-600 animate-pulse" />
      case 'pending':
        return <Clock className="h-5 w-5 text-slate-600" />
      default:
        return null
    }
  }

  const getStageStatus = (stage: PipelineStage) => {
    const colors = {
      success: 'bg-green-100 text-green-800',
      failed: 'bg-red-100 text-red-800',
      running: 'bg-blue-100 text-blue-800 animate-pulse',
      pending: 'bg-slate-100 text-slate-800',
      skipped: 'bg-gray-100 text-gray-800',
    }
    return colors[stage.status as keyof typeof colors] || colors.pending
  }

  const formatDuration = (ms: number) => {
    const seconds = Math.floor(ms / 1000)
    const minutes = Math.floor(seconds / 60)
    if (minutes > 0) return `${minutes}m ${seconds % 60}s`
    return `${seconds}s`
  }

  if (loading) {
    return (
      <Card>
        <CardContent className="pt-6">
          <p className="text-slate-600">Loading pipelines...</p>
        </CardContent>
      </Card>
    )
  }

  return (
    <div className="space-y-4">
      {/* Pipeline List */}
      <div className="space-y-3">
        {pipelines.map((pipeline) => (
          <Card key={pipeline.id}>
            <CardHeader
              className="cursor-pointer hover:bg-slate-50"
              onClick={() =>
                setExpandedPipeline(
                  expandedPipeline === pipeline.id ? null : pipeline.id
                )
              }
            >
              <div className="flex items-center justify-between">
                <div className="flex items-center gap-3 flex-1">
                  {getStatusIcon(pipeline.status)}
                  <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2">
                      <CardTitle className="text-base">
                        {pipeline.name}
                      </CardTitle>
                      <Badge variant="outline">{pipeline.environment}</Badge>
                    </div>
                    <CardDescription className="text-xs">
                      Triggered by {pipeline.triggeredBy} •{' '}
                      {new Date(pipeline.createdAt).toLocaleString()}
                    </CardDescription>
                  </div>
                </div>

                <div className="flex items-center gap-2">
                  <Badge
                    className={
                      pipeline.status === 'success'
                        ? 'bg-green-100 text-green-800'
                        : pipeline.status === 'failed'
                          ? 'bg-red-100 text-red-800'
                          : pipeline.status === 'running'
                            ? 'bg-blue-100 text-blue-800'
                            : 'bg-slate-100 text-slate-800'
                    }
                  >
                    {pipeline.status}
                  </Badge>
                  <ChevronDown
                    className={`h-5 w-5 transition-transform ${
                      expandedPipeline === pipeline.id ? 'rotate-180' : ''
                    }`}
                  />
                </div>
              </div>
            </CardHeader>

            {/* Pipeline Details */}
            {expandedPipeline === pipeline.id && (
              <CardContent className="space-y-4">
                {/* Progress Stages */}
                <div className="space-y-3">
                  {pipeline.stages.map((stage, idx) => (
                    <div key={idx} className="space-y-2">
                      <div className="flex items-center justify-between">
                        <div className="flex items-center gap-2">
                          <span className="text-sm font-semibold">
                            {stage.name}
                          </span>
                          <Badge className={getStageStatus(stage)}>
                            {stage.status}
                          </Badge>
                        </div>
                        <span className="text-xs text-slate-600">
                          {formatDuration(stage.duration)}
                        </span>
                      </div>

                      {/* Progress Bar */}
                      <div className="h-2 bg-slate-200 rounded-full overflow-hidden">
                        <div
                          className={`h-full transition-all ${
                            stage.status === 'success'
                              ? 'bg-green-500'
                              : stage.status === 'failed'
                                ? 'bg-red-500'
                                : stage.status === 'running'
                                  ? 'bg-blue-500'
                                  : 'bg-slate-400'
                          }`}
                          style={{
                            width:
                              stage.status === 'success'
                                ? '100%'
                                : stage.status === 'failed'
                                  ? '100%'
                                  : stage.status === 'running'
                                    ? '75%'
                                    : '0%',
                          }}
                        />
                      </div>

                      {/* Logs Preview */}
                      {stage.logs && (
                        <div className="bg-slate-900 text-slate-100 p-2 rounded text-xs font-mono max-h-32 overflow-y-auto">
                          {stage.logs.split('\n').slice(0, 5).join('\n')}
                          {stage.logs.split('\n').length > 5 && '...'}
                        </div>
                      )}
                    </div>
                  ))}
                </div>

                {/* Artifacts */}
                {pipeline.artifacts.length > 0 && (
                  <div className="border-t pt-4">
                    <h4 className="font-semibold text-sm mb-2">Artifacts</h4>
                    <div className="space-y-1">
                      {pipeline.artifacts.map((artifact) => (
                        <div
                          key={artifact.id}
                          className="flex items-center justify-between p-2 bg-slate-50 rounded text-sm"
                        >
                          <span className="font-mono">{artifact.name}</span>
                          <span className="text-xs text-slate-600">
                            {(artifact.size / 1024 / 1024).toFixed(2)} MB
                          </span>
                        </div>
                      ))}
                    </div>
                  </div>
                )}

                {/* Pipeline Controls */}
                <div className="flex gap-2 pt-2 border-t">
                  {pipeline.status === 'running' ? (
                    <>
                      <Button size="sm" variant="outline" className="gap-2">
                        <Pause className="h-4 w-4" />
                        Pause
                      </Button>
                      <Button size="sm" variant="outline" className="gap-2">
                        <RotateCcw className="h-4 w-4" />
                        Abort
                      </Button>
                    </>
                  ) : (
                    <Button size="sm" className="gap-2">
                      <Play className="h-4 w-4" />
                      Rerun
                    </Button>
                  )}
                </div>
              </CardContent>
            )}
          </Card>
        ))}
      </div>

      {pipelines.length === 0 && (
        <Card className="text-center py-8">
          <p className="text-slate-600">No pipelines found</p>
          <Button size="sm" className="mt-4">
            Create Pipeline
          </Button>
        </Card>
      )}
    </div>
  )
}
