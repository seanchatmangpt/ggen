import { useCallback, useState } from "react"
import { useCompletion } from "ai/react"

interface AIAssistantOptions {
  context?: string
  maxTokens?: number
  type?: "service" | "template" | "documentation" | "general"
}

export function useAIAssistant(options: AIAssistantOptions = {}) {
  const [suggestions, setSuggestions] = useState<string[]>([])

  const { complete, completion, isLoading, error } = useCompletion({
    api: "/api/ai/completion",
    body: {
      context: options.context || "",
      type: options.type || "general",
      maxTokens: options.maxTokens || 500,
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

  const generateServiceDescription = useCallback(
    async (serviceName: string, serviceType: string) => {
      const prompt = `Generate a detailed description for a ${serviceType} service named "${serviceName}" in our Internal Developer Platform. Include its purpose, capabilities, and integration points.`
      await generateSuggestions(prompt)
    },
    [generateSuggestions]
  )

  const generateTemplateScaffold = useCallback(
    async (templateName: string, language: string) => {
      const prompt = `Generate a project template scaffold for "${templateName}" in ${language}. Include directory structure, essential files, configuration, and dependencies.`
      await generateSuggestions(prompt)
    },
    [generateSuggestions]
  )

  const generateDocumentation = useCallback(
    async (topic: string, audience: string = "developers") => {
      const prompt = `Generate comprehensive technical documentation for "${topic}" aimed at ${audience}. Include overview, usage examples, best practices, and troubleshooting.`
      await generateSuggestions(prompt)
    },
    [generateSuggestions]
  )

  const suggestServiceOptimizations = useCallback(
    async (serviceInfo: string) => {
      const prompt = `Review this service configuration and suggest optimizations:\\n${serviceInfo}\\n\\nProvide specific recommendations for performance, scalability, and maintainability.`
      await generateSuggestions(prompt)
    },
    [generateSuggestions]
  )

  const analyzeArchitecture = useCallback(
    async (architectureDescription: string) => {
      const prompt = `Analyze this system architecture and identify potential issues:\\n${architectureDescription}\\n\\nProvide detailed feedback on reliability, scalability, and integration patterns.`
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
    generateServiceDescription,
    generateTemplateScaffold,
    generateDocumentation,
    suggestServiceOptimizations,
    analyzeArchitecture,
  }
}
