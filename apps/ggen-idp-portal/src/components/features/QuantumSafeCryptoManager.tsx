'use client'

import React, { useState } from 'react'
import { Plus, Shield, Zap, Lock } from 'lucide-react'
import { QuantumSafeKeyPair, HybridKeyPair, KeyMigrationPlan } from '@/types'
import toast from 'react-hot-toast'

export const QuantumSafeCryptoManager: React.FC = () => {
  const [keyPairs, setKeyPairs] = useState<QuantumSafeKeyPair[]>([
    {
      id: 'qsk-001',
      algorithm: 'Kyber-768',
      publicKey: 'MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE...',
      createdAt: new Date().toISOString(),
      keySize: 1184,
      securityLevel: 3,
    },
  ])

  const [hybridKeyPairs, setHybridKeyPairs] = useState<HybridKeyPair[]>([
    {
      id: 'hkp-001',
      classicalKeyPair: {
        algorithm: 'Ed25519',
        publicKey: 'Classical pub key...',
      },
      quantumSafeKeyPair: keyPairs[0],
      createdAt: new Date().toISOString(),
    },
  ])

  const [migrationPlan, setMigrationPlan] = useState<KeyMigrationPlan | null>({
    id: 'kmp-001',
    organizationId: 'org-001',
    status: 'InProgress',
    currentPhase: 'DualSignatures',
    targetAlgorithm: 'Kyber-768',
    completionPercentage: 45,
    estimatedCompletionDate: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
    startedAt: new Date(Date.now() - 15 * 24 * 60 * 60 * 1000).toISOString(),
    updatedAt: new Date().toISOString(),
  })

  const [showCreateKey, setShowCreateKey] = useState(false)
  const [keyType, setKeyType] = useState<'quantum-safe' | 'hybrid'>('hybrid')
  const [algorithm, setAlgorithm] = useState('Kyber-768')

  const handleCreateKey = () => {
    if (keyType === 'quantum-safe') {
      const newKey: QuantumSafeKeyPair = {
        id: `qsk-${Date.now()}`,
        algorithm: algorithm as any,
        publicKey: `Generated ${algorithm} public key...`,
        createdAt: new Date().toISOString(),
        keySize: algorithm === 'Kyber-768' ? 1184 : 1344,
        securityLevel: 3,
      }
      setKeyPairs([...keyPairs, newKey])
      toast.success('Quantum-safe key pair generated')
    } else {
      const newQSKey: QuantumSafeKeyPair = {
        id: `qsk-${Date.now()}`,
        algorithm: algorithm as any,
        publicKey: `Generated ${algorithm} public key...`,
        createdAt: new Date().toISOString(),
        keySize: 1184,
        securityLevel: 3,
      }
      const newHybrid: HybridKeyPair = {
        id: `hkp-${Date.now()}`,
        classicalKeyPair: {
          algorithm: 'Ed25519',
          publicKey: 'Generated Ed25519 public key...',
        },
        quantumSafeKeyPair: newQSKey,
        createdAt: new Date().toISOString(),
      }
      setHybridKeyPairs([...hybridKeyPairs, newHybrid])
      toast.success('Hybrid key pair generated')
    }
    setShowCreateKey(false)
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold text-slate-900">Quantum-Safe Cryptography</h2>
        <button
          onClick={() => setShowCreateKey(true)}
          className="btn btn-primary gap-2"
        >
          <Plus size={20} />
          Generate Keys
        </button>
      </div>

      {/* Overview */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="card">
          <div className="flex items-center gap-2 mb-2">
            <Shield className="text-blue-600" size={20} />
            <h3 className="font-bold text-slate-900">Quantum-Safe Keys</h3>
          </div>
          <div className="text-3xl font-bold text-blue-600">{keyPairs.length}</div>
          <p className="text-sm text-slate-600 mt-1">NIST-approved algorithms</p>
        </div>

        <div className="card">
          <div className="flex items-center gap-2 mb-2">
            <Lock className="text-green-600" size={20} />
            <h3 className="font-bold text-slate-900">Hybrid Keys</h3>
          </div>
          <div className="text-3xl font-bold text-green-600">{hybridKeyPairs.length}</div>
          <p className="text-sm text-slate-600 mt-1">Classical + Quantum-safe</p>
        </div>

        <div className="card">
          <div className="flex items-center gap-2 mb-2">
            <Zap className="text-yellow-600" size={20} />
            <h3 className="font-bold text-slate-900">Migration Status</h3>
          </div>
          <div className="text-3xl font-bold text-yellow-600">{migrationPlan?.completionPercentage}%</div>
          <p className="text-sm text-slate-600 mt-1">{migrationPlan?.currentPhase}</p>
        </div>
      </div>

      {/* Quantum-Safe Keys */}
      <div className="card">
        <h3 className="text-xl font-bold text-slate-900 mb-4">Quantum-Safe Key Pairs</h3>
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="border-b border-slate-200">
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">ID</th>
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Algorithm</th>
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Security Level</th>
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Key Size</th>
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Created</th>
              </tr>
            </thead>
            <tbody>
              {keyPairs.map((key) => (
                <tr key={key.id} className="border-b border-slate-200 hover:bg-slate-50">
                  <td className="px-4 py-2 text-sm font-mono text-slate-900">{key.id}</td>
                  <td className="px-4 py-2 text-sm">
                    <span className="px-2 py-1 bg-blue-100 text-blue-800 rounded text-xs font-semibold">
                      {key.algorithm}
                    </span>
                  </td>
                  <td className="px-4 py-2 text-sm text-slate-900">Level {key.securityLevel}</td>
                  <td className="px-4 py-2 text-sm text-slate-600">{key.keySize} bytes</td>
                  <td className="px-4 py-2 text-sm text-slate-600">
                    {new Date(key.createdAt).toLocaleDateString()}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>

      {/* Hybrid Keys */}
      <div className="card">
        <h3 className="text-xl font-bold text-slate-900 mb-4">Hybrid Key Pairs (Classical + Quantum-Safe)</h3>
        <div className="space-y-4">
          {hybridKeyPairs.map((hybrid) => (
            <div key={hybrid.id} className="p-4 border border-slate-200 rounded-lg">
              <div className="flex items-center justify-between mb-3">
                <h4 className="font-semibold text-slate-900">{hybrid.id}</h4>
                <span className="text-xs bg-green-100 text-green-800 px-2 py-1 rounded-full">
                  ✓ Active
                </span>
              </div>
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <div className="text-sm font-medium text-slate-700">Classical Algorithm</div>
                  <div className="text-sm text-slate-900 mt-1">{hybrid.classicalKeyPair.algorithm}</div>
                </div>
                <div>
                  <div className="text-sm font-medium text-slate-700">Quantum-Safe Algorithm</div>
                  <div className="text-sm text-slate-900 mt-1">{hybrid.quantumSafeKeyPair.algorithm}</div>
                </div>
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* Migration Plan */}
      {migrationPlan && (
        <div className="card">
          <h3 className="text-xl font-bold text-slate-900 mb-4 flex items-center gap-2">
            <Zap size={20} />
            Key Migration Plan
          </h3>

          <div className="space-y-4">
            <div>
              <div className="flex items-center justify-between mb-2">
                <span className="text-sm font-medium text-slate-700">Overall Progress</span>
                <span className="text-sm font-bold text-slate-900">{migrationPlan.completionPercentage}%</span>
              </div>
              <div className="w-full bg-slate-200 rounded-full h-3">
                <div
                  className="h-3 bg-gradient-to-r from-blue-500 to-purple-500 rounded-full transition-all"
                  style={{ width: `${migrationPlan.completionPercentage}%` }}
                />
              </div>
            </div>

            <div className="grid grid-cols-2 gap-4">
              <div>
                <label className="text-sm font-medium text-slate-700">Current Phase</label>
                <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm font-semibold text-slate-900">
                  {migrationPlan.currentPhase}
                </div>
              </div>
              <div>
                <label className="text-sm font-medium text-slate-700">Target Algorithm</label>
                <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm font-semibold text-slate-900">
                  {migrationPlan.targetAlgorithm}
                </div>
              </div>
            </div>

            <div className="bg-blue-50 border border-blue-200 rounded-lg p-3">
              <div className="text-sm text-blue-900">
                <strong>Estimated Completion:</strong> {new Date(migrationPlan.estimatedCompletionDate).toLocaleDateString()}
              </div>
            </div>

            <div className="flex gap-2">
              <button className="btn btn-secondary flex-1">Pause Migration</button>
              <button className="btn btn-primary flex-1">View Details</button>
            </div>
          </div>
        </div>
      )}

      {/* Create Key Modal */}
      {showCreateKey && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
          <div className="bg-white rounded-lg shadow-lg max-w-md w-full p-6 space-y-4">
            <h3 className="text-xl font-bold text-slate-900">Generate Cryptographic Keys</h3>

            <div>
              <label className="text-sm font-medium text-slate-700">Key Type</label>
              <select
                value={keyType}
                onChange={(e) => setKeyType(e.target.value as any)}
                className="input mt-1"
              >
                <option value="hybrid">Hybrid (Recommended)</option>
                <option value="quantum-safe">Quantum-Safe Only</option>
              </select>
            </div>

            <div>
              <label className="text-sm font-medium text-slate-700">Quantum-Safe Algorithm</label>
              <select
                value={algorithm}
                onChange={(e) => setAlgorithm(e.target.value)}
                className="input mt-1"
              >
                <option value="Kyber-768">CRYSTALS-Kyber-768 (Recommended)</option>
                <option value="Dilithium-3">CRYSTALS-Dilithium-3</option>
                <option value="Falcon-1024">Falcon-1024</option>
                <option value="SPHINCS+">SPHINCS+</option>
              </select>
              <p className="text-xs text-slate-500 mt-1">
                All algorithms are NIST-approved for post-quantum cryptography
              </p>
            </div>

            <div className="flex gap-2 pt-4">
              <button
                onClick={handleCreateKey}
                className="btn btn-primary flex-1"
              >
                Generate
              </button>
              <button
                onClick={() => setShowCreateKey(false)}
                className="btn btn-secondary flex-1"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  )
}
