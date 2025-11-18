'use client'

import React, { ReactNode } from 'react'

interface RootProviderProps {
  children: ReactNode
}

export default function RootProvider({ children }: RootProviderProps) {
  return <>{children}</>
}
