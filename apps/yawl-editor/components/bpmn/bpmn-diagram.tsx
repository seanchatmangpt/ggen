'use client'

import { useEffect, useRef, useState } from 'react'
import BpmnModeler from 'bpmn-js/lib/Modeler'
import { BpmnPropertiesPanel, BpmnPropertiesPanelModule } from 'bpmn-js-properties-panel'
import 'bpmn-js/dist/assets/diagram-js.css'
import 'bpmn-js/dist/assets/bpmn-font/font/bpmn.css'
import 'bpmn-js-properties-panel/dist/assets/bpmn-js-properties-panel.css'

interface BpmnDiagramProps {
  diagramXml?: string
  onDiagramChange?: (xml: string) => void
  readOnly?: boolean
}

const defaultBpmnXml = `<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL"
                   xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
                   xmlns:dc="http://www.omg.org/spec/DD/20100524/DC"
                   xmlns:di="http://www.omg.org/spec/DD/20100524/DI"
                   id="Definitions_1"
                   targetNamespace="http://bpmn.io/schema/bpmn">
  <bpmn:process id="Process_1" isExecutable="false">
    <bpmn:startEvent id="StartEvent_1"/>
  </bpmn:process>
  <bpmndi:BPMNDiagram id="BPMNDiagram_1">
    <bpmndi:BPMNPlane id="BPMNPlane_1" bpmnElement="Process_1">
      <bpmndi:BPMNShape id="BPMNShape_StartEvent_1" bpmnElement="StartEvent_1">
        <dc:Bounds x="150" y="100" width="36" height="36"/>
      </bpmndi:BPMNShape>
    </bpmndi:BPMNPlane>
  </bpmndi:BPMNDiagram>
</bpmn:definitions>`

export function BpmnDiagram({
  diagramXml = defaultBpmnXml,
  onDiagramChange,
  readOnly = false,
}: BpmnDiagramProps) {
  const containerRef = useRef<HTMLDivElement>(null)
  const modelerRef = useRef<BpmnModeler | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [isLoading, setIsLoading] = useState(true)

  useEffect(() => {
    if (!containerRef.current) return

    const initBpmn = async () => {
      try {
        setIsLoading(true)
        setError(null)

        // Create modeler instance
        const modeler = new BpmnModeler({
          container: containerRef.current,
          propertiesPanel: {
            parent: '#properties',
          },
          additionalModules: [BpmnPropertiesPanelModule],
        })

        modelerRef.current = modeler

        // Import diagram
        await modeler.importXML(diagramXml)

        // Auto-fit viewport
        const canvas = modeler.get('canvas')
        canvas.zoom('fit-viewport')

        // Setup event listeners
        if (!readOnly) {
          modeler.on('commandStack.changed', async () => {
            try {
              const { xml } = await modeler.saveXML({ format: true })
              onDiagramChange?.(xml)
            } catch (error) {
              console.error('Error saving diagram:', error)
            }
          })
        }

        setIsLoading(false)
      } catch (err) {
        const message = err instanceof Error ? err.message : 'Failed to load diagram'
        setError(message)
        setIsLoading(false)
      }
    }

    initBpmn()

    return () => {
      if (modelerRef.current) {
        modelerRef.current.destroy()
      }
    }
  }, [diagramXml, onDiagramChange, readOnly])

  return (
    <div className="flex h-full gap-4">
      <div className="flex-1 bg-white border rounded-lg overflow-hidden">
        {error ? (
          <div className="flex items-center justify-center h-full bg-red-50 border border-red-200">
            <div className="text-center">
              <p className="text-red-800 font-semibold mb-2">Error loading diagram</p>
              <p className="text-red-600 text-sm">{error}</p>
            </div>
          </div>
        ) : isLoading ? (
          <div className="flex items-center justify-center h-full">
            <div className="text-center">
              <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-500 mx-auto mb-2"></div>
              <p className="text-slate-600">Loading diagram...</p>
            </div>
          </div>
        ) : (
          <div ref={containerRef} className="w-full h-full" />
        )}
      </div>

      {!readOnly && (
        <div
          id="properties"
          className="w-80 bg-white border-l overflow-y-auto"
        />
      )}
    </div>
  )
}
