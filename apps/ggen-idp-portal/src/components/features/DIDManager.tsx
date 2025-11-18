'use client'

import React, { useState } from 'react'
import { Plus, Copy, Trash2, Key, CheckCircle, AlertCircle } from 'lucide-react'
import { DIDDocument, VerifiableCredential } from '@/types'
import toast from 'react-hot-toast'

export const DIDManager: React.FC = () => {
  const [dids, setDids] = useState<DIDDocument[]>([
    {
      id: 'did-agent-001',
      subject: 'Agent-Issuer-001',
      didMethod: 'web',
      publicKeys: [
        {
          id: '#key-1',
          type: 'Ed25519',
          publicKeyPem: 'MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE...',
          createdAt: new Date().toISOString(),
        },
      ],
      authentication: ['#key-1'],
      assertionMethod: ['#key-1'],
      serviceEndpoints: [
        {
          id: '#agent-endpoint',
          type: 'VerifiableCredentialService',
          serviceEndpoint: 'https://idp.ggen.dev/did/agent-001/credentials',
        },
      ],
    },
  ])

  const [selectedDID, setSelectedDID] = useState<DIDDocument | null>(dids[0])
  const [showCreate, setShowCreate] = useState(false)
  const [newDIDMethod, setNewDIDMethod] = useState<'web' | 'key' | 'ethereum'>('web')

  const handleCreateDID = () => {
    const newDID: DIDDocument = {
      id: `did-${Date.now()}`,
      subject: `Agent-${Math.random().toString(36).substr(2, 9)}`,
      didMethod: newDIDMethod,
      publicKeys: [
        {
          id: '#key-1',
          type: 'Ed25519',
          publicKeyPem: 'Generated public key...',
          createdAt: new Date().toISOString(),
        },
      ],
      authentication: ['#key-1'],
      assertionMethod: ['#key-1'],
      serviceEndpoints: [],
    }
    setDids([...dids, newDID])
    setSelectedDID(newDID)
    setShowCreate(false)
    toast.success('DID created successfully')
  }

  const handleDeleteDID = (id: string) => {
    setDids(dids.filter((d) => d.id !== id))
    if (selectedDID?.id === id) setSelectedDID(null)
    toast.success('DID deleted')
  }

  const handleCopyDID = (did: string) => {
    navigator.clipboard.writeText(did)
    toast.success('DID copied to clipboard')
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold text-slate-900">Decentralized Identities (DIDs)</h2>
        <button
          onClick={() => setShowCreate(true)}
          className="btn btn-primary gap-2"
        >
          <Plus size={20} />
          Create DID
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* DID List */}
        <div className="lg:col-span-1">
          <div className="card space-y-2 max-h-[600px] overflow-y-auto">
            <h3 className="font-bold text-slate-900 mb-4">Your DIDs</h3>
            {dids.length === 0 ? (
              <p className="text-slate-500 text-sm">No DIDs created yet</p>
            ) : (
              dids.map((did) => (
                <div
                  key={did.id}
                  onClick={() => setSelectedDID(did)}
                  className={`p-3 rounded-lg cursor-pointer transition-all ${
                    selectedDID?.id === did.id
                      ? 'bg-blue-50 border border-blue-300'
                      : 'bg-slate-50 hover:bg-slate-100'
                  }`}
                >
                  <div className="font-medium text-slate-900 text-sm truncate">{did.subject}</div>
                  <div className="text-xs text-slate-500 truncate">{did.id}</div>
                  <div className="text-xs text-slate-600 mt-1">
                    Method: {did.didMethod.toUpperCase()}
                  </div>
                </div>
              ))
            )}
          </div>
        </div>

        {/* DID Details */}
        <div className="lg:col-span-2 space-y-4">
          {selectedDID ? (
            <>
              <div className="card">
                <div className="flex items-center justify-between mb-4">
                  <h3 className="text-xl font-bold text-slate-900">DID Details</h3>
                  <button
                    onClick={() => handleDeleteDID(selectedDID.id)}
                    className="text-red-600 hover:text-red-700"
                  >
                    <Trash2 size={20} />
                  </button>
                </div>

                <div className="space-y-4">
                  <div>
                    <label className="text-sm font-medium text-slate-700">DID Identifier</label>
                    <div className="flex items-center gap-2 mt-1">
                      <code className="flex-1 bg-slate-50 p-2 rounded text-sm font-mono break-all">
                        {selectedDID.id}
                      </code>
                      <button
                        onClick={() => handleCopyDID(selectedDID.id)}
                        className="text-slate-600 hover:text-slate-900"
                      >
                        <Copy size={18} />
                      </button>
                    </div>
                  </div>

                  <div>
                    <label className="text-sm font-medium text-slate-700">DID Method</label>
                    <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm font-semibold text-blue-600">
                      did:{selectedDID.didMethod}
                    </div>
                  </div>

                  <div>
                    <label className="text-sm font-medium text-slate-700">Subject</label>
                    <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm">
                      {selectedDID.subject}
                    </div>
                  </div>
                </div>
              </div>

              {/* Public Keys */}
              <div className="card">
                <h3 className="font-bold text-slate-900 mb-3 flex items-center gap-2">
                  <Key size={18} />
                  Public Keys
                </h3>
                <div className="space-y-2">
                  {selectedDID.publicKeys.map((key) => (
                    <div key={key.id} className="p-3 bg-slate-50 rounded-lg">
                      <div className="flex items-center gap-2 mb-2">
                        <CheckCircle size={16} className="text-green-600" />
                        <span className="font-medium text-slate-900">{key.type}</span>
                      </div>
                      <code className="text-xs bg-white p-2 rounded block font-mono break-all">
                        {key.publicKeyPem}
                      </code>
                    </div>
                  ))}
                </div>
              </div>

              {/* Service Endpoints */}
              <div className="card">
                <h3 className="font-bold text-slate-900 mb-3">Service Endpoints</h3>
                {selectedDID.serviceEndpoints.length === 0 ? (
                  <p className="text-slate-500 text-sm">No service endpoints configured</p>
                ) : (
                  <div className="space-y-2">
                    {selectedDID.serviceEndpoints.map((endpoint) => (
                      <div key={endpoint.id} className="p-3 bg-slate-50 rounded-lg">
                        <div className="text-sm font-medium text-slate-900">{endpoint.type}</div>
                        <div className="text-xs text-slate-600 mt-1 break-all">
                          {endpoint.serviceEndpoint}
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            </>
          ) : (
            <div className="card h-[400px] flex items-center justify-center">
              <p className="text-slate-500">Select a DID to view details</p>
            </div>
          )}
        </div>
      </div>

      {/* Create DID Modal */}
      {showCreate && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
          <div className="bg-white rounded-lg shadow-lg max-w-md w-full p-6 space-y-4">
            <h3 className="text-xl font-bold text-slate-900">Create New DID</h3>

            <div>
              <label className="text-sm font-medium text-slate-700">DID Method</label>
              <select
                value={newDIDMethod}
                onChange={(e) => setNewDIDMethod(e.target.value as any)}
                className="input mt-1"
              >
                <option value="web">did:web (Recommended)</option>
                <option value="key">did:key</option>
                <option value="ethereum">did:ethereum</option>
                <option value="polygon">did:polygon</option>
              </select>
              <p className="text-xs text-slate-500 mt-1">
                {newDIDMethod === 'web' &&
                  'Web-based DID, ideal for agents with web endpoints'}
                {newDIDMethod === 'key' &&
                  'Key-based DID, derived from a public key'}
                {newDIDMethod === 'ethereum' &&
                  'Ethereum blockchain-based DID'}
                {newDIDMethod === 'polygon' &&
                  'Polygon blockchain-based DID'}
              </p>
            </div>

            <div className="flex gap-2 pt-4">
              <button
                onClick={handleCreateDID}
                className="btn btn-primary flex-1"
              >
                Create
              </button>
              <button
                onClick={() => setShowCreate(false)}
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
