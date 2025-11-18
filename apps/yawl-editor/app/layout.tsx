import type { Metadata } from "next"
import { Inter } from "next/font/google"
import Link from "next/link"
import "./globals.css"

const inter = Inter({ subsets: ["latin"] })

export const metadata: Metadata = {
  title: "YAWL Editor - Workflow Management",
  description: "Next.js + shadcn/ui YAWL Editor with SPARQL integration",
}

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode
}>) {
  return (
    <html lang="en">
      <body className={inter.className}>
        <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100">
          <nav className="bg-white border-b border-slate-200 shadow-sm sticky top-0 z-50">
            <div className="container mx-auto px-4">
              <div className="flex items-center justify-between h-16">
                <div className="flex items-center space-x-8">
                  <Link href="/" className="text-xl font-bold text-slate-900">
                    ⚙️ YAWL Editor
                  </Link>
                  <div className="flex space-x-4">
                    <Link
                      href="/dashboard"
                      className="text-slate-600 hover:text-slate-900 px-3 py-2 rounded-md text-sm font-medium hover:bg-slate-100 transition"
                    >
                      Dashboard
                    </Link>
                    <Link
                      href="/cases"
                      className="text-slate-600 hover:text-slate-900 px-3 py-2 rounded-md text-sm font-medium hover:bg-slate-100 transition"
                    >
                      Cases
                    </Link>
                    <Link
                      href="/workitems"
                      className="text-slate-600 hover:text-slate-900 px-3 py-2 rounded-md text-sm font-medium hover:bg-slate-100 transition"
                    >
                      Workitems
                    </Link>
                    <Link
                      href="/processes"
                      className="text-slate-600 hover:text-slate-900 px-3 py-2 rounded-md text-sm font-medium hover:bg-slate-100 transition"
                    >
                      Processes
                    </Link>
                    <Link
                      href="/resources"
                      className="text-slate-600 hover:text-slate-900 px-3 py-2 rounded-md text-sm font-medium hover:bg-slate-100 transition"
                    >
                      Resources
                    </Link>
                  </div>
                </div>
                <div className="text-sm text-slate-500">
                  SPARQL-Driven Workflow Engine
                </div>
              </div>
            </div>
          </nav>
          <main className="container mx-auto px-4 py-8">
            {children}
          </main>
        </div>
      </body>
    </html>
  )
}
