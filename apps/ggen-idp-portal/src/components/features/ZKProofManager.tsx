'use client'

import React, { useState } from 'react'
import { Plus, CheckCircle, AlertCircle, BarChart3 } from 'lucide-react'
import { ZeroKnowledgeProof, PrivacyPool } from '@/types'
import toast from 'react-hot-toast'

export const ZKProofManager: React.FC = () => {
  const [proofs, setProofs] = useState<ZeroKnowledgeProof[]>([
    {
      id: 'zk-proof-001',
      type: 'AgeInRange',
      commitment: '0x1234567890abcdef...',
      provingSystem: 'Groth16',
      circuitName: 'age_range_proof',
      verified: true,
      verifiedAt: new Date().toISOString(),
      timestamp: new Date().toISOString(),
    },
  ])

  const [privacyPools, setPrivacyPools] = useState<PrivacyPool[]>([
    {
      id: 'pool-001',
      name: 'Age Verification Pool',
      size: 1000,
      anonymitySet: [],
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    },
  ])

  const [selectedProof, setSelectedProof] = useState<ZeroKnowledgeProof | null>(proofs[0])
  const [showCreateProof, setShowCreateProof] = useState(false)
  const [proofType, setProofType] = useState<'AgeInRange' | 'Membership' | 'Balance'>('AgeInRange')
  const [provingSystem, setProvingSystem] = useState('Groth16')

  const handleGenerateProof = () => {
    const newProof: ZeroKnowledgeProof = {
      id: `zk-proof-${Date.now()}`,
      type: proofType,
      commitment: `0x${Math.random().toString(16).substr(2)}...`,
      provingSystem: provingSystem as any,
      circuitName: `${proofType.toLowerCase()}_circuit`,
      verified: true,
      verifiedAt: new Date().toISOString(),
      timestamp: new Date().toISOString(),
    }
    setProofs([...proofs, newProof])
    setSelectedProof(newProof)
    setShowCreateProof(false)
    toast.success('Zero-Knowledge Proof generated successfully')
  }

  const proofTypeDescriptions: Record<string, string> = {
    AgeInRange: 'Prove age is within range without revealing actual age',
    Membership: 'Prove membership in a set without revealing identity',
    Balance: 'Prove account balance without revealing the amount',
    Credential: 'Prove credential authenticity without disclosure',
    Attribute: 'Prove attribute satisfies condition',
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold text-slate-900">Zero-Knowledge Proofs</h2>
        <button
          onClick={() => setShowCreateProof(true)}
          className="btn btn-primary gap-2"
        >
          <Plus size={20} />
          Generate Proof
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Proof List */}
        <div className="lg:col-span-1">
          <div className="card space-y-2 max-h-[500px] overflow-y-auto">
            <h3 className="font-bold text-slate-900 mb-4">Proofs</h3>
            {proofs.map((proof) => (
              <div
                key={proof.id}
                onClick={() => setSelectedProof(proof)}
                className={`p-3 rounded-lg cursor-pointer transition-all ${
                  selectedProof?.id === proof.id
                    ? 'bg-blue-50 border border-blue-300'
                    : 'bg-slate-50 hover:bg-slate-100'
                }`}
              >
                <div className="flex items-center gap-2 mb-1">
                  {proof.verified ? (
                    <CheckCircle size={14} className="text-green-600" />
                  ) : (
                    <AlertCircle size={14} className="text-yellow-600" />
                  )}
                  <span className="font-medium text-slate-900 text-sm">{proof.type}</span>
                </div>
                <div className="text-xs text-slate-500 truncate">{proof.id}</div>
                <div className="text-xs text-slate-600 mt-1">{proof.provingSystem}</div>
              </div>
            ))}
          </div>
        </div>

        {/* Proof Details */}
        <div className="lg:col-span-2 space-y-4">
          {selectedProof ? (
            <>
              <div className="card">
                <div className="flex items-center justify-between mb-4">
                  <h3 className="text-xl font-bold text-slate-900">Proof Details</h3>
                  <div className="flex items-center gap-2">
                    {selectedProof.verified ? (
                      <span className="px-3 py-1 bg-green-100 text-green-800 rounded-full text-xs font-medium">
                        ✓ Verified
                      </span>
                    ) : (
                      <span className="px-3 py-1 bg-yellow-100 text-yellow-800 rounded-full text-xs font-medium">
                        ⏳ Pending
                      </span>
                    )}
                  </div>
                </div>

                <div className="grid grid-cols-2 gap-4 mb-4">
                  <div>
                    <label className="text-sm font-medium text-slate-700">Proof Type</label>
                    <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm font-semibold text-blue-600">
                      {selectedProof.type}
                    </div>
                  </div>
                  <div>
                    <label className="text-sm font-medium text-slate-700">Proving System</label>
                    <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm">
                      {selectedProof.provingSystem}
                    </div>
                  </div>
                </div>

                <div>
                  <label className="text-sm font-medium text-slate-700">Commitment Hash</label>
                  <code className="block mt-1 bg-slate-50 p-2 rounded text-xs font-mono break-all">
                    {selectedProof.commitment}
                  </code>
                </div>
              </div>

              <div className="card">
                <h3 className="font-bold text-slate-900 mb-3 flex items-center gap-2">
                  <BarChart3 size={18} />
                  Proof Statistics
                </h3>
                <div className="grid grid-cols-3 gap-4">
                  <div className="text-center">
                    <div className="text-2xl font-bold text-blue-600">32KB</div>
                    <div className="text-xs text-slate-600">Proof Size</div>
                  </div>
                  <div className="text-center">
                    <div className="text-2xl font-bold text-green-600">≈50ms</div>
                    <div className="text-xs text-slate-600">Generation Time</div>
                  </div>
                  <div className="text-center">
                    <div className="text-2xl font-bold text-purple-600">≈20ms</div>
                    <div className="text-xs text-slate-600">Verification Time</div>
                  </div>
                </div>
              </div>
            </>
          ) : (
            <div className="card h-[400px] flex items-center justify-center">
              <p className="text-slate-500">Select a proof to view details</p>
            </div>
          )}
        </div>
      </div>

      {/* Privacy Pools */}
      <div className="card">
        <h3 className="text-xl font-bold text-slate-900 mb-4">Privacy Pools</h3>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {privacyPools.map((pool) => (
            <div key={pool.id} className="p-4 border border-slate-200 rounded-lg">
              <div className="font-bold text-slate-900 mb-2">{pool.name}</div>
              <div className="text-sm text-slate-600 space-y-1">
                <div>Pool Size: <span className="font-semibold">{pool.size} agents</span></div>
                <div>Anonymity Set: <span className="font-semibold">{pool.anonymitySet.length}</span></div>
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* Create Proof Modal */}
      {showCreateProof && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
          <div className="bg-white rounded-lg shadow-lg max-w-md w-full p-6 space-y-4">
            <h3 className="text-xl font-bold text-slate-900">Generate Zero-Knowledge Proof</h3>

            <div>
              <label className="text-sm font-medium text-slate-700">Proof Type</label>
              <select
                value={proofType}
                onChange={(e) => setProofType(e.target.value as any)}
                className="input mt-1"
              >
                <option value="AgeInRange">Age In Range (Recommended)</option>
                <option value="Membership">Membership Proof</option>
                <option value="Balance">Balance Proof</option>
              </select>
              <p className="text-xs text-slate-500 mt-1">
                {proofTypeDescriptions[proofType]}
              </p>
            </div>

            <div>
              <label className="text-sm font-medium text-slate-700">Proving System</label>
              <select
                value={provingSystem}
                onChange={(e) => setProvingSystem(e.target.value)}
                className="input mt-1"
              >
                <option value="Groth16">Groth16 (Recommended)</option>
                <option value="Plonk">Plonk</option>
                <option value="STARK">STARK (Quantum-Safe)</option>
              </select>
            </div>

            <div className="flex gap-2 pt-4">
              <button
                onClick={handleGenerateProof}
                className="btn btn-primary flex-1"
              >
                Generate
              </button>
              <button
                onClick={() => setShowCreateProof(false)}
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
