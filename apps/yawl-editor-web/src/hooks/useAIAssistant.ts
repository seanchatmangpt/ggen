import { useCallback, useState } from "react"
import { useCompletion } from "ai/react"

interface AIAssistantOptions {
  context?: string
  maxTokens?: number
}

export function useAIAssistant(options: AIAssistantOptions = {}) {
  const [suggestions, setSuggestions] = useState<string[]>([])

  const { complete, completion, isLoading, error } = useCompletion({
    api: "/api/ai/completion",
    body: {
      context: options.context || "",
    },
  })

  const generateSuggestions = useCallback(
    async (prompt: string) => {
      try {
        await complete(prompt)
      } catch (err) {
        console.error("Error generating suggestions:", err)
      }
    },
    [complete]
  )

  const generateWorkflowSuggestions = useCallback(
    async (workflowDescription: string) => {
      const prompt = `Generate a YAWL workflow JSON for: ${workflowDescription}\n\nReturn only valid JSON.`
      await generateSuggestions(prompt)
    },
    [generateSuggestions]
  )

  const generateNodeDescription = useCallback(
    async (nodeType: string, nodeName: string) => {
      const prompt = `Generate a brief description for a ${nodeType} node named "${nodeName}" in a workflow.`
      await generateSuggestions(prompt)
    },
    [generateSuggestions]
  )

  const suggestWorkflowOptimizations = useCallback(
    async (workflowJSON: string) => {
      const prompt = `Suggest optimizations for this workflow:\n${workflowJSON}`
      await generateSuggestions(prompt)
    },
    [generateSuggestions]
  )

  const analyzeWorkflow = useCallback(
    async (workflowJSON: string) => {
      const prompt = `Analyze this workflow and identify potential issues:\n${workflowJSON}`
      await generateSuggestions(prompt)
    },
    [generateSuggestions]
  )

  return {
    completion,
    isLoading,
    error,
    suggestions,
    setSuggestions,
    generateSuggestions,
    generateWorkflowSuggestions,
    generateNodeDescription,
    suggestWorkflowOptimizations,
    analyzeWorkflow,
  }
}
