"use client"

import React from "react"
import { ValidationError } from "@/hooks/useWorkflowValidation"
import { AlertCircle, AlertTriangle, Info } from "lucide-react"

interface ValidationReportProps {
  errors: ValidationError[]
  onErrorClick?: (error: ValidationError) => void
}

export function ValidationReport({
  errors,
  onErrorClick,
}: ValidationReportProps) {
  if (errors.length === 0) {
    return (
      <div className="flex items-center gap-2 p-3 bg-green-50 border border-green-200 rounded-lg">
        <Info className="h-4 w-4 text-green-600" />
        <span className="text-sm font-medium text-green-700">
          Workflow is valid
        </span>
      </div>
    )
  }

  const errorsByType = {
    error: errors.filter((e) => e.type === "error"),
    warning: errors.filter((e) => e.type === "warning"),
    info: errors.filter((e) => e.type === "info"),
  }

  return (
    <div className="space-y-2">
      {errorsByType.error.length > 0 && (
        <div className="space-y-2">
          <h4 className="text-sm font-semibold text-red-700 flex items-center gap-2">
            <AlertCircle className="h-4 w-4" />
            Errors ({errorsByType.error.length})
          </h4>
          <div className="space-y-1">
            {errorsByType.error.map((error) => (
              <div
                key={error.id}
                className="p-2 bg-red-50 border border-red-200 rounded text-sm text-red-700 cursor-pointer hover:bg-red-100 transition-colors"
                onClick={() => onErrorClick?.(error)}
              >
                {error.message}
              </div>
            ))}
          </div>
        </div>
      )}

      {errorsByType.warning.length > 0 && (
        <div className="space-y-2">
          <h4 className="text-sm font-semibold text-yellow-700 flex items-center gap-2">
            <AlertTriangle className="h-4 w-4" />
            Warnings ({errorsByType.warning.length})
          </h4>
          <div className="space-y-1">
            {errorsByType.warning.map((error) => (
              <div
                key={error.id}
                className="p-2 bg-yellow-50 border border-yellow-200 rounded text-sm text-yellow-700 cursor-pointer hover:bg-yellow-100 transition-colors"
                onClick={() => onErrorClick?.(error)}
              >
                {error.message}
              </div>
            ))}
          </div>
        </div>
      )}

      {errorsByType.info.length > 0 && (
        <div className="space-y-2">
          <h4 className="text-sm font-semibold text-blue-700 flex items-center gap-2">
            <Info className="h-4 w-4" />
            Info ({errorsByType.info.length})
          </h4>
          <div className="space-y-1">
            {errorsByType.info.map((error) => (
              <div
                key={error.id}
                className="p-2 bg-blue-50 border border-blue-200 rounded text-sm text-blue-700 cursor-pointer hover:bg-blue-100 transition-colors"
                onClick={() => onErrorClick?.(error)}
              >
                {error.message}
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}
