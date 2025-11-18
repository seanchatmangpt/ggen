import type { Metadata } from 'next'
import { Toaster } from 'react-hot-toast'
import './globals.css'
import RootProvider from '@/components/providers/RootProvider'

export const metadata: Metadata = {
  title: 'ggen IDP Portal | Identity Provider Management',
  description: 'Enterprise identity management for BPMN.js Marketplace with AI-powered auth flows',
  viewport: 'width=device-width, initial-scale=1',
  icons: {
    icon: '/favicon.ico',
  },
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body className="antialiased">
        <RootProvider>
          {children}
          <Toaster position="bottom-right" />
        </RootProvider>
      </body>
    </html>
  )
}
