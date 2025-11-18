import type { Metadata } from "next"
import "./globals.css"

export const metadata: Metadata = {
  title: "Backstage IDP - Internal Developer Platform",
  description: "Enterprise Internal Developer Platform for Service Management, Catalog, and Documentation",
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body className="bg-background text-foreground">{children}</body>
    </html>
  )
}
