import { useCallback, useState } from 'react'

/**
 * Hook for managing modal open/close state
 */
export function useModal(initialOpen: boolean = false) {
  const [isOpen, setIsOpen] = useState(initialOpen)

  const open = useCallback(() => setIsOpen(true), [])
  const close = useCallback(() => setIsOpen(false), [])
  const toggle = useCallback(() => setIsOpen((prev) => !prev), [])

  return {
    isOpen,
    open,
    close,
    toggle,
  }
}

/**
 * Hook for managing modal with data
 */
export function useModalWithData<T = any>(initialOpen: boolean = false) {
  const [isOpen, setIsOpen] = useState(initialOpen)
  const [data, setData] = useState<T | null>(null)

  const open = useCallback((data?: T) => {
    if (data) setData(data)
    setIsOpen(true)
  }, [])

  const close = useCallback(() => {
    setIsOpen(false)
    setData(null)
  }, [])

  const toggle = useCallback(() => setIsOpen((prev) => !prev), [])

  return {
    isOpen,
    data,
    open,
    close,
    toggle,
    setData,
  }
}

/**
 * Hook for managing multiple modals
 */
export function useModals() {
  const [openModals, setOpenModals] = useState<Set<string>>(new Set())

  const open = useCallback((id: string) => {
    setOpenModals((prev) => new Set([...prev, id]))
  }, [])

  const close = useCallback((id: string) => {
    setOpenModals((prev) => {
      const next = new Set(prev)
      next.delete(id)
      return next
    })
  }, [])

  const toggle = useCallback((id: string) => {
    setOpenModals((prev) => {
      const next = new Set(prev)
      if (next.has(id)) {
        next.delete(id)
      } else {
        next.add(id)
      }
      return next
    })
  }, [])

  const isOpen = useCallback((id: string) => openModals.has(id), [openModals])

  return {
    openModals,
    open,
    close,
    toggle,
    isOpen,
    closeAll: () => setOpenModals(new Set()),
  }
}
