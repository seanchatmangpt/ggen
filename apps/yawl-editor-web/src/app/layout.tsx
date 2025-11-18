import type { Metadata } from "next"
import "./globals.css"

export const metadata: Metadata = {
  title: "YAWL Editor",
  description: "A modern web-based YAWL workflow process editor",
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body className="font-sans">{children}</body>
    </html>
  )
}
