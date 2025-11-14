import type { Metadata } from "next"
import { Inter } from "next/font/google"
import Link from "next/link"
import "./globals.css"

const inter = Inter({ subsets: ["latin"] })

export const metadata: Metadata = {
  title: "Task Management - Ontology-Driven App",
  description: "Next.js app demonstrating ontology-driven development with automatic UI generation",
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
          <nav className="bg-white border-b border-slate-200 shadow-sm">
            <div className="container mx-auto px-4">
              <div className="flex items-center justify-between h-16">
                <div className="flex items-center space-x-8">
                  <Link href="/" className="text-xl font-bold text-slate-900">
                    🧠 Ontology-Driven App
                  </Link>
                  <div className="flex space-x-4">
                    <Link
                      href="/tasks"
                      className="text-slate-600 hover:text-slate-900 px-3 py-2 rounded-md text-sm font-medium hover:bg-slate-100 transition"
                    >
                      Tasks
                    </Link>
                    <Link
                      href="/projects"
                      className="text-slate-600 hover:text-slate-900 px-3 py-2 rounded-md text-sm font-medium hover:bg-slate-100 transition"
                    >
                      Projects
                    </Link>
                  </div>
                </div>
                <div className="text-sm text-slate-500">
                  Generated from ontology/task-management.ttl
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
