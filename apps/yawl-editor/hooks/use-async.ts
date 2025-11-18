import { useCallback, useEffect, useRef, useState } from 'react'

interface AsyncState<T, E = Error> {
  status: 'idle' | 'pending' | 'success' | 'error'
  data: T | null
  error: E | null
}

/**
 * Hook for handling async operations with loading and error states
 */
export function useAsync<T, E = Error>(
  asyncFunction: () => Promise<T>,
  immediate: boolean = true
): AsyncState<T, E> & {
  execute: () => Promise<T>
} {
  const [state, setState] = useState<AsyncState<T, E>>({
    status: 'idle',
    data: null,
    error: null,
  })

  const isMounted = useRef(true)

  const execute = useCallback(async (): Promise<T> => {
    setState({ status: 'pending', data: null, error: null })
    try {
      const response = await asyncFunction()
      if (isMounted.current) {
        setState({ status: 'success', data: response, error: null })
      }
      return response
    } catch (error) {
      if (isMounted.current) {
        setState({
          status: 'error',
          data: null,
          error: error as E,
        })
      }
      throw error
    }
  }, [asyncFunction])

  useEffect(() => {
    if (immediate) {
      execute()
    }

    return () => {
      isMounted.current = false
    }
  }, [execute, immediate])

  return { ...state, execute }
}

/**
 * Hook for debounced async operations
 */
export function useAsyncDebounced<T, E = Error>(
  asyncFunction: (term: string) => Promise<T>,
  delay: number = 500
) {
  const [state, setState] = useState<AsyncState<T, E>>({
    status: 'idle',
    data: null,
    error: null,
  })

  const [searchTerm, setSearchTerm] = useState('')
  const timeoutRef = useRef<ReturnType<typeof setTimeout>>()
  const isMounted = useRef(true)

  const execute = useCallback(
    (term: string) => {
      setSearchTerm(term)

      if (timeoutRef.current) {
        clearTimeout(timeoutRef.current)
      }

      if (!term.trim()) {
        setState({ status: 'idle', data: null, error: null })
        return
      }

      setState({ status: 'pending', data: null, error: null })

      timeoutRef.current = setTimeout(async () => {
        try {
          const response = await asyncFunction(term)
          if (isMounted.current) {
            setState({ status: 'success', data: response, error: null })
          }
        } catch (error) {
          if (isMounted.current) {
            setState({
              status: 'error',
              data: null,
              error: error as E,
            })
          }
        }
      }, delay)
    },
    [asyncFunction, delay]
  )

  useEffect(() => {
    return () => {
      isMounted.current = false
      if (timeoutRef.current) {
        clearTimeout(timeoutRef.current)
      }
    }
  }, [])

  return { ...state, execute, searchTerm }
}

/**
 * Hook for handling async operations with retry logic
 */
export function useAsyncRetry<T, E = Error>(
  asyncFunction: () => Promise<T>,
  maxRetries: number = 3,
  immediate: boolean = true
) {
  const [state, setState] = useState<AsyncState<T, E>>({
    status: 'idle',
    data: null,
    error: null,
  })

  const [retryCount, setRetryCount] = useState(0)
  const isMounted = useRef(true)

  const execute = useCallback(async (): Promise<T> => {
    setState({ status: 'pending', data: null, error: null })
    setRetryCount(0)

    let lastError: E | null = null

    for (let i = 0; i <= maxRetries; i++) {
      try {
        const response = await asyncFunction()
        if (isMounted.current) {
          setState({ status: 'success', data: response, error: null })
        }
        return response
      } catch (error) {
        lastError = error as E
        setRetryCount(i + 1)

        if (i < maxRetries) {
          // Exponential backoff
          await new Promise((resolve) =>
            setTimeout(resolve, Math.pow(2, i) * 1000)
          )
        }
      }
    }

    if (isMounted.current) {
      setState({ status: 'error', data: null, error: lastError })
    }
    throw lastError
  }, [asyncFunction, maxRetries])

  useEffect(() => {
    if (immediate) {
      execute()
    }

    return () => {
      isMounted.current = false
    }
  }, [execute, immediate])

  return { ...state, execute, retryCount, maxRetries }
}
