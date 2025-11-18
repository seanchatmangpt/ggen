'use client'

import { QuantumSecurityPanel } from '@/components/quantum/quantum-security-panel'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Shield, Zap, Lock, CheckCircle2, ArrowRight } from 'lucide-react'
import Link from 'next/link'

/**
 * Quantum Security Dashboard
 * 2028 Innovation Phase: Post-Quantum Cryptography
 */
export default function QuantumSecurityPage() {
  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800 p-4 md:p-8">
      {/* Hero Section */}
      <div className="max-w-6xl mx-auto mb-12">
        <div className="flex items-center gap-4 mb-6">
          <div className="p-3 bg-gradient-to-br from-purple-100 to-pink-100 dark:from-purple-900 dark:to-pink-900 rounded-lg">
            <Shield className="h-8 w-8 text-purple-600 dark:text-purple-400" />
          </div>
          <div>
            <h1 className="text-4xl font-bold">Quantum-Ready Security</h1>
            <p className="text-slate-600 dark:text-slate-400 text-lg">
              Post-quantum cryptography for tomorrow's threats
            </p>
          </div>
        </div>

        {/* Overview */}
        <div className="mb-8">
          <p className="text-slate-700 dark:text-slate-300 mb-4">
            The YAWL Editor platform is secured with <strong>post-quantum cryptography</strong>,
            protecting your data against both classical and quantum computing threats. This 2028
            Innovation Phase implementation uses hybrid encryption combining proven classical
            algorithms with quantum-resistant lattice-based cryptography.
          </p>
        </div>

        {/* Key Features Grid */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-8">
          <Card className="border-purple-200 dark:border-purple-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <Lock className="h-5 w-5 text-purple-600" />
                Hybrid Encryption
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-slate-600 dark:text-slate-400">
                Combines RSA-2048 classical with Kyber-like post-quantum for defense in depth
              </p>
            </CardContent>
          </Card>

          <Card className="border-pink-200 dark:border-pink-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <Zap className="h-5 w-5 text-pink-600" />
                Future-Proof
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-slate-600 dark:text-slate-400">
                Resistant to quantum attacks from future quantum computers (2030+)
              </p>
            </CardContent>
          </Card>

          <Card className="border-blue-200 dark:border-blue-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <CheckCircle2 className="h-5 w-5 text-blue-600" />
                Authenticated
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-slate-600 dark:text-slate-400">
                Multi-layer digital signatures ensure integrity and non-repudiation
              </p>
            </CardContent>
          </Card>
        </div>

        {/* Technical Stack */}
        <Card className="mb-8">
          <CardHeader>
            <CardTitle>Technical Stack</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <h3 className="font-semibold mb-3">Encryption</h3>
                <ul className="space-y-2 text-sm text-slate-600 dark:text-slate-400">
                  <li className="flex items-center gap-2">
                    <span className="w-2 h-2 bg-purple-600 rounded-full"></span>
                    AES-256-GCM (Authenticated)
                  </li>
                  <li className="flex items-center gap-2">
                    <span className="w-2 h-2 bg-purple-600 rounded-full"></span>
                    Kyber-like KEM (Post-Quantum)
                  </li>
                  <li className="flex items-center gap-2">
                    <span className="w-2 h-2 bg-purple-600 rounded-full"></span>
                    HKDF-SHA512 (Key Derivation)
                  </li>
                </ul>
              </div>
              <div>
                <h3 className="font-semibold mb-3">Digital Signatures</h3>
                <ul className="space-y-2 text-sm text-slate-600 dark:text-slate-400">
                  <li className="flex items-center gap-2">
                    <span className="w-2 h-2 bg-pink-600 rounded-full"></span>
                    RSA-PSS (2048-bit)
                  </li>
                  <li className="flex items-center gap-2">
                    <span className="w-2 h-2 bg-pink-600 rounded-full"></span>
                    Hash-based (Merkle-like)
                  </li>
                  <li className="flex items-center gap-2">
                    <span className="w-2 h-2 bg-pink-600 rounded-full"></span>
                    Multi-layer Validation
                  </li>
                </ul>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* API Endpoints */}
        <Card className="mb-8">
          <CardHeader>
            <CardTitle>API Endpoints</CardTitle>
            <CardDescription>
              Quantum-safe encryption and key management APIs
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-3">
              <div className="bg-slate-100 dark:bg-slate-800 p-4 rounded-lg">
                <code className="text-sm font-mono">POST /api/quantum/key-exchange</code>
                <p className="text-xs text-slate-600 dark:text-slate-400 mt-1">
                  Register and exchange quantum-safe public keys
                </p>
              </div>
              <div className="bg-slate-100 dark:bg-slate-800 p-4 rounded-lg">
                <code className="text-sm font-mono">POST /api/quantum/encrypt</code>
                <p className="text-xs text-slate-600 dark:text-slate-400 mt-1">
                  Encrypt data with quantum-safe encryption
                </p>
              </div>
              <div className="bg-slate-100 dark:bg-slate-800 p-4 rounded-lg">
                <code className="text-sm font-mono">POST /api/quantum/sign</code>
                <p className="text-xs text-slate-600 dark:text-slate-400 mt-1">
                  Create and verify quantum-resistant digital signatures
                </p>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Documentation Link */}
        <Card className="bg-gradient-to-r from-purple-50 to-pink-50 dark:from-purple-950 dark:to-pink-950 border-purple-200 dark:border-purple-900">
          <CardContent className="pt-6">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="font-semibold mb-1">Complete Documentation</h3>
                <p className="text-sm text-slate-600 dark:text-slate-400">
                  Read the detailed implementation guide and integration examples
                </p>
              </div>
              <Link href="/QUANTUM_CRYPTO_IMPLEMENTATION.md" className="shrink-0">
                <button className="flex items-center gap-2 px-4 py-2 bg-purple-600 text-white rounded-lg hover:bg-purple-700 transition-colors text-sm font-medium">
                  View Docs
                  <ArrowRight className="h-4 w-4" />
                </button>
              </Link>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Security Panel */}
      <div className="max-w-6xl mx-auto">
        <QuantumSecurityPanel userId="user-demo" userName="Demo User" />
      </div>

      {/* Footer */}
      <div className="max-w-6xl mx-auto mt-12 pt-8 border-t border-slate-200 dark:border-slate-800">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
          <div>
            <h3 className="font-semibold mb-2">Timeline</h3>
            <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
              <li>✓ 2024-2026: Hybrid mode (current)</li>
              <li>2026-2028: PQC transition</li>
              <li>2028+: Pure post-quantum</li>
            </ul>
          </div>
          <div>
            <h3 className="font-semibold mb-2">Security Level</h3>
            <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
              <li>Classical: 112-bits</li>
              <li>Post-Quantum: 256-bits</li>
              <li>Combined: Unbreakable*</li>
            </ul>
          </div>
          <div>
            <h3 className="font-semibold mb-2">Standards</h3>
            <ul className="text-sm text-slate-600 dark:text-slate-400 space-y-1">
              <li>NIST SP 800-56C</li>
              <li>FIPS 197 (AES)</li>
              <li>NIST PQC Roadmap</li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  )
}
