'use client'

import React, { useState, useCallback } from 'react'
import { Plus, Edit2, Trash2, Play, Save } from 'lucide-react'
import { AuthFlow } from '@/types'
import { CodeEditor } from '@/components/editor/CodeEditor'
import { authFlowTemplate } from '@/lib/templates/authFlows'
import { idpClient } from '@/api/client'
import toast from 'react-hot-toast'

const mockFlows: AuthFlow[] = [
  {
    id: 'login-basic',
    name: 'Standard Login',
    description: 'Email/password authentication with optional MFA',
    organizationId: '',
    flowType: 'BasicLogin',
    steps: [],
    createdAt: new Date().toISOString(),
  },
  {
    id: 'oauth2-callback',
    name: 'OAuth2 Callback',
    description: 'OAuth2 authorization code callback handler',
    organizationId: '',
    flowType: 'OauthCallback',
    steps: [],
    createdAt: new Date().toISOString(),
  },
]

export default function AuthFlowsPage() {
  const [flows, setFlows] = useState<AuthFlow[]>(mockFlows)
  const [selectedFlow, setSelectedFlow] = useState<AuthFlow | null>(null)
  const [isEditing, setIsEditing] = useState(false)
  const [editorContent, setEditorContent] = useState(authFlowTemplate)
  const [isLoading, setIsLoading] = useState(false)

  const handleCreateFlow = useCallback(() => {
    setSelectedFlow(null)
    setEditorContent(authFlowTemplate)
    setIsEditing(true)
  }, [])

  const handleSelectFlow = useCallback((flow: AuthFlow) => {
    setSelectedFlow(flow)
    // Load flow YAML representation
    setEditorContent(flowToYaml(flow))
  }, [])

  const handleSaveFlow = useCallback(async () => {
    setIsLoading(true)
    try {
      // Parse YAML and validate
      const parsedFlow = parseFlowYaml(editorContent)

      if (selectedFlow?.id) {
        // Update existing flow
        await idpClient.updateAuthFlow(selectedFlow.id, parsedFlow)
        toast.success('Auth flow updated')
      } else {
        // Create new flow
        const newFlow = await idpClient.createAuthFlow('org-id', parsedFlow)
        setFlows([...flows, newFlow])
        toast.success('Auth flow created')
      }
      setIsEditing(false)
    } catch (error) {
      toast.error('Failed to save flow')
      console.error('Save error:', error)
    } finally {
      setIsLoading(false)
    }
  }, [selectedFlow, editorContent, flows])

  const handleDeleteFlow = useCallback(async (flowId: string) => {
    if (!confirm('Delete this auth flow?')) return

    try {
      await idpClient.deleteAuthFlow(flowId)
      setFlows(flows.filter((f) => f.id !== flowId))
      if (selectedFlow?.id === flowId) {
        setSelectedFlow(null)
      }
      toast.success('Auth flow deleted')
    } catch (error) {
      toast.error('Failed to delete flow')
    }
  }, [flows, selectedFlow])

  const handleTestFlow = useCallback(async () => {
    if (!selectedFlow) return

    try {
      const result = await idpClient.executeAuthFlow(selectedFlow.id, {
        username: 'test@example.com',
        password: 'TestPass123!',
      })
      toast.success('Flow executed successfully')
      console.log('Flow result:', result)
    } catch (error) {
      toast.error('Flow execution failed')
    }
  }, [selectedFlow])

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-slate-900">Authentication Flows</h1>
          <p className="text-slate-600 mt-1">
            BPMN-based authentication workflows with AI-powered suggestions
          </p>
        </div>
        <button onClick={handleCreateFlow} className="btn btn-primary gap-2">
          <Plus size={20} />
          New Flow
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-4 gap-6">
        {/* Flow List */}
        <div className="lg:col-span-1">
          <div className="card space-y-2 max-h-[600px] overflow-y-auto">
            <h2 className="font-bold text-slate-900 mb-4">Flows</h2>
            {flows.map((flow) => (
              <div
                key={flow.id}
                onClick={() => handleSelectFlow(flow)}
                className={`p-3 rounded-lg cursor-pointer transition-all ${
                  selectedFlow?.id === flow.id
                    ? 'bg-blue-50 border border-blue-300'
                    : 'bg-slate-50 hover:bg-slate-100'
                }`}
              >
                <div className="font-medium text-slate-900 text-sm">{flow.name}</div>
                <div className="text-xs text-slate-500 truncate">{flow.description}</div>
              </div>
            ))}
          </div>
        </div>

        {/* Editor */}
        <div className="lg:col-span-3 space-y-4">
          {isEditing ? (
            <>
              <CodeEditor
                value={editorContent}
                language="yaml"
                onChange={setEditorContent}
                height="500px"
                showSuggestions={true}
              />
              <div className="flex gap-2">
                <button onClick={handleSaveFlow} className="btn btn-primary gap-2" disabled={isLoading}>
                  <Save size={16} />
                  {isLoading ? 'Saving...' : 'Save'}
                </button>
                <button
                  onClick={() => setIsEditing(false)}
                  className="btn btn-secondary"
                >
                  Cancel
                </button>
                {selectedFlow && (
                  <button onClick={handleTestFlow} className="btn btn-secondary gap-2 ml-auto">
                    <Play size={16} />
                    Test
                  </button>
                )}
              </div>
            </>
          ) : selectedFlow ? (
            <>
              <div className="card space-y-4">
                <div className="space-y-2">
                  <h2 className="text-2xl font-bold text-slate-900">{selectedFlow.name}</h2>
                  <p className="text-slate-600">{selectedFlow.description}</p>
                </div>

                <div className="grid grid-cols-2 gap-4 text-sm">
                  <div>
                    <span className="text-slate-600">Type:</span>
                    <p className="font-medium text-slate-900">{selectedFlow.flowType}</p>
                  </div>
                  <div>
                    <span className="text-slate-600">Steps:</span>
                    <p className="font-medium text-slate-900">{selectedFlow.steps.length}</p>
                  </div>
                </div>

                <div className="pt-4 border-t border-slate-200 flex gap-2">
                  <button
                    onClick={() => setIsEditing(true)}
                    className="btn btn-primary gap-2 flex-1"
                  >
                    <Edit2 size={16} />
                    Edit
                  </button>
                  <button onClick={handleTestFlow} className="btn btn-secondary gap-2">
                    <Play size={16} />
                    Test
                  </button>
                  <button
                    onClick={() => handleDeleteFlow(selectedFlow.id)}
                    className="btn btn-danger gap-2"
                  >
                    <Trash2 size={16} />
                    Delete
                  </button>
                </div>
              </div>

              {/* Flow Visualization */}
              <div className="card">
                <h3 className="font-bold text-slate-900 mb-4">Flow Steps</h3>
                <div className="space-y-3">
                  {selectedFlow.steps.length > 0 ? (
                    selectedFlow.steps.map((step, idx) => (
                      <div key={step.id} className="p-3 bg-slate-50 rounded-lg">
                        <div className="flex items-start gap-3">
                          <div className="flex-shrink-0 w-8 h-8 rounded-full bg-blue-600 text-white flex items-center justify-center text-sm font-bold">
                            {idx + 1}
                          </div>
                          <div className="flex-1 min-w-0">
                            <div className="font-medium text-slate-900">{step.name}</div>
                            <div className="text-sm text-slate-600">{step.stepType}</div>
                          </div>
                        </div>
                      </div>
                    ))
                  ) : (
                    <p className="text-slate-500 text-sm">No steps defined</p>
                  )}
                </div>
              </div>
            </>
          ) : (
            <div className="card h-[500px] flex items-center justify-center">
              <p className="text-slate-500">Select a flow or create a new one</p>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}

/**
 * Convert AuthFlow to YAML representation
 */
function flowToYaml(flow: AuthFlow): string {
  return `id: ${flow.id}
name: ${flow.name}
description: ${flow.description || ''}
flowType: ${flow.flowType}
steps: ${JSON.stringify(flow.steps, null, 2)}`
}

/**
 * Parse YAML to AuthFlow
 */
function parseFlowYaml(yaml: string): Partial<AuthFlow> {
  // Simple implementation - in production use proper YAML parser
  const lines = yaml.split('\n')
  const result: any = {}

  for (const line of lines) {
    const [key, ...valueParts] = line.split(':')
    const value = valueParts.join(':').trim()

    if (key === 'id') result.id = value
    if (key === 'name') result.name = value
    if (key === 'description') result.description = value
    if (key === 'flowType') result.flowType = value
  }

  return result
}
