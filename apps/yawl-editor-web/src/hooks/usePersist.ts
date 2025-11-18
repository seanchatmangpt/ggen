import { useCallback, useState } from "react"

export function usePersist<T>(
  key: string,
  initialValue: T
): [T, (value: T) => void, () => void] {
  const [value, setValue] = useState<T>(() => {
    try {
      const stored = typeof window !== "undefined" ? localStorage.getItem(key) : null
      return stored ? JSON.parse(stored) : initialValue
    } catch {
      console.error(`Error reading from localStorage: ${key}`)
      return initialValue
    }
  })

  const save = useCallback(
    (newValue: T) => {
      try {
        setValue(newValue)
        if (typeof window !== "undefined") {
          localStorage.setItem(key, JSON.stringify(newValue))
        }
      } catch {
        console.error(`Error writing to localStorage: ${key}`)
      }
    },
    [key]
  )

  const clear = useCallback(() => {
    try {
      setValue(initialValue)
      if (typeof window !== "undefined") {
        localStorage.removeItem(key)
      }
    } catch {
      console.error(`Error clearing localStorage: ${key}`)
    }
  }, [key, initialValue])

  return [value, save, clear]
}
