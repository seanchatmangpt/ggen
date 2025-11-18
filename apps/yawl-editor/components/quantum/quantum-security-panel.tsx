'use client'

import React, { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import {
  Shield,
  Key,
  Lock,
  Unlock,
  Copy,
  RefreshCw,
  Check,
  AlertCircle,
  Clock,
  Zap,
  Download,
} from 'lucide-react'
import { QuantumCryptoService } from '@/lib/quantum-crypto'
import { QuantumCollaborationService } from '@/lib/quantum-collaboration'

interface QuantumSecurityPanelProps {
  userId: string
  userName: string
}

/**
 * Quantum Security Panel Component
 * UI for managing post-quantum cryptographic keys and operations
 */
export function QuantumSecurityPanel({
  userId,
  userName,
}: QuantumSecurityPanelProps) {
  const [cryptoService] = useState(() => new QuantumCryptoService())
  const [collabService] = useState(
    () => new QuantumCollaborationService(userId, userName, cryptoService)
  )

  const [keyPair, setKeyPair] = useState(cryptoService.generateKeyPair({
    userId,
    purpose: 'both',
    expiryDays: 90,
  }))

  const [activeTab, setActiveTab] = useState('overview')
  const [encryptionTest, setEncryptionTest] = useState({
    plaintext: 'Secret message here',
    ciphertext: '',
    decrypted: '',
  })
  const [copyFeedback, setCopyFeedback] = useState<string | null>(null)
  const [isConnected, setIsConnected] = useState(false)

  // Simulate connection status
  useEffect(() => {
    const timer = setTimeout(() => setIsConnected(true), 500)
    return () => clearTimeout(timer)
  }, [])

  const handleGenerateNewKey = () => {
    const newKeyPair = cryptoService.generateKeyPair({
      userId,
      purpose: 'both',
      expiryDays: 90,
    })
    setKeyPair(newKeyPair)
  }

  const handleRotateKey = () => {
    const rotatedKey = cryptoService.rotateKeyPair(keyPair.keyId)
    if (rotatedKey) {
      setKeyPair(rotatedKey)
    }
  }

  const handleEncryptionTest = () => {
    try {
      const encrypted = cryptoService.encrypt(
        encryptionTest.plaintext,
        keyPair.publicKey,
        keyPair.keyId
      )

      const decrypted = cryptoService.decrypt(
        encrypted,
        keyPair.privateKey
      )

      setEncryptionTest({
        ...encryptionTest,
        ciphertext: encrypted.ciphertext.substring(0, 50) + '...',
        decrypted: decrypted.toString('utf-8'),
      })
    } catch (error) {
      console.error('Encryption test failed:', error)
    }
  }

  const handleCopyToClipboard = (text: string, label: string) => {
    navigator.clipboard.writeText(text)
    setCopyFeedback(label)
    setTimeout(() => setCopyFeedback(null), 2000)
  }

  const handleDownloadKeyPair = () => {
    const data = {
      keyId: keyPair.keyId,
      publicKey: keyPair.publicKey,
      algorithm: keyPair.algorithm,
      createdAt: keyPair.createdAt,
      expiresAt: keyPair.expiresAt,
    }

    const blob = new Blob([JSON.stringify(data, null, 2)], {
      type: 'application/json',
    })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `quantum-key-${keyPair.keyId}.json`
    a.click()
    URL.revokeObjectURL(url)
  }

  const daysUntilExpiry = keyPair.expiresAt
    ? Math.ceil((keyPair.expiresAt.getTime() - Date.now()) / (1000 * 60 * 60 * 24))
    : null

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center gap-3 mb-6">
        <div className="p-3 bg-gradient-to-br from-purple-100 to-pink-100 dark:from-purple-900 dark:to-pink-900 rounded-lg">
          <Shield className="h-6 w-6 text-purple-600 dark:text-purple-400" />
        </div>
        <div>
          <h2 className="text-2xl font-bold">Quantum Security Panel</h2>
          <p className="text-sm text-slate-600 dark:text-slate-400">
            Post-quantum cryptography key management and testing
          </p>
        </div>
      </div>

      {/* Connection Status */}
      <Card className={isConnected ? 'border-green-200 dark:border-green-900' : 'border-yellow-200 dark:border-yellow-900'}>
        <CardContent className="pt-6">
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-3">
              {isConnected ? (
                <>
                  <Check className="h-5 w-5 text-green-600" />
                  <span className="font-medium">Quantum-Safe Connection Active</span>
                </>
              ) : (
                <>
                  <AlertCircle className="h-5 w-5 text-yellow-600" />
                  <span className="font-medium">Connecting...</span>
                </>
              )}
            </div>
            <Badge variant={isConnected ? 'default' : 'secondary'}>
              {isConnected ? 'Connected' : 'Connecting'}
            </Badge>
          </div>
        </CardContent>
      </Card>

      {/* Main Tabs */}
      <Tabs value={activeTab} onValueChange={setActiveTab}>
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="overview">Overview</TabsTrigger>
          <TabsTrigger value="keys">Key Management</TabsTrigger>
          <TabsTrigger value="test">Test Operations</TabsTrigger>
        </TabsList>

        {/* Overview Tab */}
        <TabsContent value="overview" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {/* Key Status */}
            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-base flex items-center gap-2">
                  <Key className="h-4 w-4" />
                  Key Status
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-3">
                <div>
                  <p className="text-xs text-slate-600 dark:text-slate-400 mb-1">Key ID</p>
                  <p className="font-mono text-sm break-all">{keyPair.keyId}</p>
                </div>
                <div>
                  <p className="text-xs text-slate-600 dark:text-slate-400 mb-1">Algorithm</p>
                  <Badge variant="outline">{keyPair.algorithm}</Badge>
                </div>
                <div>
                  <p className="text-xs text-slate-600 dark:text-slate-400 mb-1">Purpose</p>
                  <Badge variant="secondary">{keyPair.metadata?.purpose}</Badge>
                </div>
                <div>
                  <p className="text-xs text-slate-600 dark:text-slate-400 mb-1">Status</p>
                  {cryptoService.isKeyValid(keyPair.keyId) ? (
                    <Badge className="bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-100">
                      Valid
                    </Badge>
                  ) : (
                    <Badge variant="destructive">Expired</Badge>
                  )}
                </div>
              </CardContent>
            </Card>

            {/* Timeline */}
            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-base flex items-center gap-2">
                  <Clock className="h-4 w-4" />
                  Timeline
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-3">
                <div>
                  <p className="text-xs text-slate-600 dark:text-slate-400 mb-1">Created</p>
                  <p className="text-sm">
                    {keyPair.createdAt.toLocaleDateString()} at{' '}
                    {keyPair.createdAt.toLocaleTimeString()}
                  </p>
                </div>
                <div>
                  <p className="text-xs text-slate-600 dark:text-slate-400 mb-1">Expires</p>
                  {keyPair.expiresAt ? (
                    <p className="text-sm">
                      {keyPair.expiresAt.toLocaleDateString()}
                      {daysUntilExpiry && daysUntilExpiry <= 30 && (
                        <span className="ml-2 text-xs text-orange-600 font-medium">
                          ({daysUntilExpiry} days)
                        </span>
                      )}
                    </p>
                  ) : (
                    <p className="text-sm text-slate-500">No expiration</p>
                  )}
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Security Features */}
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Security Features</CardTitle>
            </CardHeader>
            <CardContent className="space-y-2">
              <div className="flex items-center gap-2">
                <Check className="h-4 w-4 text-green-600" />
                <span className="text-sm">Hybrid Classical + Post-Quantum</span>
              </div>
              <div className="flex items-center gap-2">
                <Check className="h-4 w-4 text-green-600" />
                <span className="text-sm">AES-256-GCM Authenticated Encryption</span>
              </div>
              <div className="flex items-center gap-2">
                <Check className="h-4 w-4 text-green-600" />
                <span className="text-sm">Multi-Layer Digital Signatures</span>
              </div>
              <div className="flex items-center gap-2">
                <Check className="h-4 w-4 text-green-600" />
                <span className="text-sm">Automatic Key Rotation Support</span>
              </div>
              <div className="flex items-center gap-2">
                <Check className="h-4 w-4 text-green-600" />
                <span className="text-sm">Post-Quantum Resistant (2028+)</span>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Key Management Tab */}
        <TabsContent value="keys" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Public Key</CardTitle>
              <CardDescription>
                Share this key with others for encrypted communication
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="bg-slate-100 dark:bg-slate-800 p-4 rounded-lg break-all font-mono text-xs max-h-40 overflow-y-auto">
                {keyPair.publicKey.substring(0, 200)}...
              </div>
              <div className="flex gap-2">
                <Button
                  size="sm"
                  variant="outline"
                  onClick={() =>
                    handleCopyToClipboard(keyPair.publicKey, 'Public Key')
                  }
                  className="flex-1 gap-2"
                >
                  <Copy className="h-4 w-4" />
                  {copyFeedback === 'Public Key' ? 'Copied!' : 'Copy'}
                </Button>
                <Button
                  size="sm"
                  variant="outline"
                  onClick={handleDownloadKeyPair}
                  className="gap-2"
                >
                  <Download className="h-4 w-4" />
                  Download
                </Button>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Key Management</CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <Button onClick={handleGenerateNewKey} className="w-full gap-2">
                <Zap className="h-4 w-4" />
                Generate New Key Pair
              </Button>
              <Button
                onClick={handleRotateKey}
                variant="outline"
                className="w-full gap-2"
              >
                <RefreshCw className="h-4 w-4" />
                Rotate Current Key
              </Button>
              <p className="text-xs text-slate-600 dark:text-slate-400">
                Key rotation creates a new key pair and marks the current one as expired.
                This is recommended every 30-60 days for maximum security.
              </p>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Test Operations Tab */}
        <TabsContent value="test" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Lock className="h-4 w-4" />
                Encryption Test
              </CardTitle>
              <CardDescription>Test encryption and decryption</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-2">
                <label className="text-sm font-medium">Plaintext</label>
                <textarea
                  value={encryptionTest.plaintext}
                  onChange={(e) =>
                    setEncryptionTest({
                      ...encryptionTest,
                      plaintext: e.target.value,
                    })
                  }
                  className="w-full px-3 py-2 border rounded-md text-sm"
                  rows={3}
                />
              </div>

              <Button onClick={handleEncryptionTest} className="w-full gap-2">
                <Zap className="h-4 w-4" />
                Test Encryption
              </Button>

              {encryptionTest.ciphertext && (
                <>
                  <div className="space-y-2">
                    <label className="text-sm font-medium">Ciphertext (truncated)</label>
                    <div className="bg-slate-100 dark:bg-slate-800 p-3 rounded-md text-xs break-all font-mono">
                      {encryptionTest.ciphertext}
                    </div>
                  </div>

                  <div className="space-y-2">
                    <label className="text-sm font-medium">Decrypted</label>
                    <div className="bg-green-50 dark:bg-green-900/20 p-3 rounded-md text-sm border border-green-200 dark:border-green-900">
                      ✓ {encryptionTest.decrypted}
                    </div>
                  </div>
                </>
              )}
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Unlock className="h-4 w-4" />
                Security Info
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-3 text-sm">
              <div>
                <p className="font-medium mb-1">Encryption Algorithm</p>
                <p className="text-slate-600 dark:text-slate-400">
                  AES-256-GCM with Kyber-like hybrid key encapsulation
                </p>
              </div>
              <div>
                <p className="font-medium mb-1">Signature Algorithm</p>
                <p className="text-slate-600 dark:text-slate-400">
                  RSA-PSS (2048-bit) + Hash-based (Dilithium-like)
                </p>
              </div>
              <div>
                <p className="font-medium mb-1">Post-Quantum Ready</p>
                <p className="text-slate-600 dark:text-slate-400">
                  Resistant to quantum attacks from future quantum computers
                </p>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}
