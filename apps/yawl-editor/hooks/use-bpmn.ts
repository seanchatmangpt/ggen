import { useCallback, useState } from 'react'
import type BpmnModeler from 'bpmn-js/lib/Modeler'
import {
  parseBpmnXml,
  bpmnToYawlOntology,
  bpmnToSparqlInsert,
  exportAsBpmnXml,
  type BpmnProcess,
} from '@/lib/bpmn-to-yawl'

interface UseBpmnOptions {
  initialXml?: string
  onSave?: (xml: string) => Promise<void>
}

/**
 * Hook for managing BPMN diagram operations
 */
export function useBpmn(options: UseBpmnOptions = {}) {
  const [xml, setXml] = useState(options.initialXml || '')
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<Error | null>(null)
  const [modeler, setModeler] = useState<BpmnModeler | null>(null)

  const updateDiagram = useCallback((newXml: string) => {
    setXml(newXml)
  }, [])

  const save = useCallback(async () => {
    if (!options.onSave) return

    try {
      setLoading(true)
      setError(null)
      await options.onSave(xml)
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Save failed')
      setError(error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [xml, options])

  const parseProcess = useCallback((): BpmnProcess | null => {
    if (!xml) return null

    try {
      return parseBpmnXml(xml)
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Parse failed')
      setError(error)
      return null
    }
  }, [xml])

  const exportAsYawl = useCallback((): string | null => {
    const process = parseProcess()
    if (!process) return null
    return bpmnToYawlOntology(process)
  }, [parseProcess])

  const exportAsSparql = useCallback((): string | null => {
    const process = parseProcess()
    if (!process) return null
    return bpmnToSparqlInsert(process)
  }, [parseProcess])

  const exportAsFile = useCallback((format: 'bpmn' | 'yawl' | 'sparql'): string | null => {
    const process = parseProcess()
    if (!process) return null

    switch (format) {
      case 'bpmn':
        return exportAsBpmnXml(process)
      case 'yawl':
        return exportAsYawl()
      case 'sparql':
        return exportAsSparql()
      default:
        return null
    }
  }, [parseProcess, exportAsYawl, exportAsSparql])

  const downloadFile = useCallback(
    (format: 'bpmn' | 'yawl' | 'sparql', filename?: string) => {
      const content = exportAsFile(format)
      if (!content) return

      const element = document.createElement('a')
      const file = new Blob([content], { type: 'text/plain' })
      element.href = URL.createObjectURL(file)
      element.download = filename || `process.${format}`
      document.body.appendChild(element)
      element.click()
      document.body.removeChild(element)
    },
    [exportAsFile]
  )

  return {
    xml,
    loading,
    error,
    modeler,
    setModeler,
    updateDiagram,
    save,
    parseProcess,
    exportAsYawl,
    exportAsSparql,
    exportAsFile,
    downloadFile,
  }
}

/**
 * Hook for BPMN diagram validation
 */
export function useBpmnValidation(xml: string) {
  const [isValid, setIsValid] = useState(true)
  const [errors, setErrors] = useState<string[]>([])
  const [warnings, setWarnings] = useState<string[]>([])

  const validate = useCallback(() => {
    const newErrors: string[] = []
    const newWarnings: string[] = []

    try {
      const process = parseBpmnXml(xml)
      if (!process) {
        newErrors.push('Invalid BPMN XML')
        setIsValid(false)
        setErrors(newErrors)
        return
      }

      // Check for start events
      const startEvents = process.elements.filter((e) => e.type === 'startEvent')
      if (startEvents.length === 0) {
        newErrors.push('No start event found')
      } else if (startEvents.length > 1) {
        newWarnings.push('Multiple start events detected')
      }

      // Check for end events
      const endEvents = process.elements.filter((e) => e.type === 'endEvent')
      if (endEvents.length === 0) {
        newErrors.push('No end event found')
      } else if (endEvents.length > 1) {
        newWarnings.push('Multiple end events detected')
      }

      // Check for isolated tasks
      for (const el of process.elements) {
        if (el.type !== 'startEvent' && (!el.incoming || el.incoming.length === 0)) {
          newWarnings.push(`Task "${el.name}" has no incoming connections`)
        }
        if (el.type !== 'endEvent' && (!el.outgoing || el.outgoing.length === 0)) {
          newWarnings.push(`Task "${el.name}" has no outgoing connections`)
        }
      }

      setIsValid(newErrors.length === 0)
      setErrors(newErrors)
      setWarnings(newWarnings)
    } catch (error) {
      newErrors.push(error instanceof Error ? error.message : 'Validation error')
      setIsValid(false)
      setErrors(newErrors)
    }
  }, [xml])

  return {
    isValid,
    errors,
    warnings,
    validate,
  }
}
