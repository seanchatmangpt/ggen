import { useCallback, useState } from "react"

export interface HistoryEntry<T> {
  state: T
  timestamp: number
}

export function useHistory<T>(initialState: T, maxHistory: number = 50) {
  const [history, setHistory] = useState<HistoryEntry<T>[]>([
    { state: initialState, timestamp: Date.now() },
  ])
  const [currentIndex, setCurrentIndex] = useState(0)

  const push = useCallback((state: T) => {
    setHistory((prev) => {
      const newHistory = prev.slice(0, currentIndex + 1)
      newHistory.push({ state, timestamp: Date.now() })
      if (newHistory.length > maxHistory) {
        newHistory.shift()
      }
      return newHistory
    })
    setCurrentIndex((prev) => Math.min(prev + 1, maxHistory - 1))
  }, [currentIndex, maxHistory])

  const undo = useCallback(() => {
    setCurrentIndex((prev) => Math.max(prev - 1, 0))
  }, [])

  const redo = useCallback(() => {
    setCurrentIndex((prev) => Math.min(prev + 1, history.length - 1))
  }, [history.length])

  const canUndo = currentIndex > 0
  const canRedo = currentIndex < history.length - 1
  const currentState = history[currentIndex]?.state || null
  const timelineLength = history.length

  const reset = useCallback((state: T) => {
    setHistory([{ state, timestamp: Date.now() }])
    setCurrentIndex(0)
  }, [])

  const clear = useCallback(() => {
    setHistory([{ state: initialState, timestamp: Date.now() }])
    setCurrentIndex(0)
  }, [initialState])

  return {
    state: currentState,
    push,
    undo,
    redo,
    canUndo,
    canRedo,
    reset,
    clear,
    history,
    currentIndex,
    timelineLength,
  }
}
