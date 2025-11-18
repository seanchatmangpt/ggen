'use client'

import React, { useState } from 'react'
import { Zap, Key, Brain, Shield } from 'lucide-react'

export default function FeaturesPage2028() {
  const [activeTab, setActiveTab] = useState<'did' | 'zk' | 'agents' | 'quantum'>('did')

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold text-slate-900">2028 Features</h1>
        <p className="text-slate-600 mt-1">
          Forward-looking identity features: Decentralized Identity, Zero-Knowledge Proofs, Autonomous Agents
        </p>
      </div>

      {/* Tabs */}
      <div className="flex gap-2 border-b border-slate-200">
        {[
          { id: 'did', label: 'Decentralized Identity (DIDs)', icon: Key },
          { id: 'zk', label: 'Zero-Knowledge Proofs', icon: Shield },
          { id: 'agents', label: 'Autonomous Agents', icon: Brain },
          { id: 'quantum', label: 'Quantum-Safe Crypto', icon: Zap },
        ].map(({ id, label, icon: Icon }) => (
          <button
            key={id}
            onClick={() => setActiveTab(id as any)}
            className={`px-4 py-3 font-medium text-sm border-b-2 transition-colors flex items-center gap-2 ${
              activeTab === id
                ? 'border-blue-600 text-blue-600'
                : 'border-transparent text-slate-600 hover:text-slate-900'
            }`}
          >
            <Icon size={18} />
            {label}
          </button>
        ))}
      </div>

      {/* DID Content */}
      {activeTab === 'did' && <DIDSection />}

      {/* ZK Proofs Content */}
      {activeTab === 'zk' && <ZKProofsSection />}

      {/* Autonomous Agents Content */}
      {activeTab === 'agents' && <AutonomousAgentsSection />}

      {/* Quantum-Safe Content */}
      {activeTab === 'quantum' && <QuantumSafeSection />}
    </div>
  )
}

function DIDSection() {
  return (
    <div className="space-y-6">
      <div className="card">
        <h2 className="text-2xl font-bold mb-4">Decentralized Identity (DIDs)</h2>
        <p className="text-slate-600 mb-4">
          W3C Decentralized Identifier standard for self-sovereign identity management
        </p>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div className="space-y-3">
            <h3 className="font-bold text-slate-900">Key Features</h3>
            <ul className="space-y-2 text-slate-700">
              <li>✓ Self-sovereign identity (no central authority)</li>
              <li>✓ Blockchain-agnostic (Ethereum, Polygon, Solana, etc.)</li>
              <li>✓ Verifiable credentials (VCs)</li>
              <li>✓ Portable across organizations</li>
              <li>✓ Privacy-preserving claims</li>
              <li>✓ Zero-knowledge credential proofs</li>
            </ul>
          </div>

          <div className="bg-slate-50 p-4 rounded-lg font-mono text-sm">
            <div className="text-blue-600">did:web:ggen.dev/users/alice</div>
            <div className="mt-2 text-slate-700"># DID Document</div>
            <div className="mt-2 text-slate-600">
              <div>@context: https://w3id.org/did/v1</div>
              <div>id: did:web:ggen.dev/users/alice</div>
              <div>publicKey:</div>
              <div>  - id: #key-1</div>
              <div>    type: Ed25519VerificationKey2020</div>
            </div>
          </div>
        </div>
      </div>

      <div className="card">
        <h3 className="font-bold text-slate-900 mb-3">Create DID</h3>
        <div className="space-y-3">
          <div>
            <label className="text-sm font-medium text-slate-700">DID Method</label>
            <select className="input mt-1">
              <option>did:web (Recommended)</option>
              <option>did:key</option>
              <option>did:ethereum</option>
              <option>did:polygon</option>
            </select>
          </div>
          <div>
            <label className="text-sm font-medium text-slate-700">Public Key Type</label>
            <select className="input mt-1">
              <option>Ed25519</option>
              <option>ECDSA (P-256)</option>
              <option>RSA-2048</option>
            </select>
          </div>
          <button className="btn btn-primary">Generate DID</button>
        </div>
      </div>
    </div>
  )
}

function ZKProofsSection() {
  return (
    <div className="space-y-6">
      <div className="card">
        <h2 className="text-2xl font-bold mb-4">Zero-Knowledge Proofs</h2>
        <p className="text-slate-600 mb-4">
          Prove claims about user data without revealing the underlying information
        </p>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div className="space-y-3">
            <h3 className="font-bold text-slate-900">Proof Types</h3>
            <div className="space-y-2">
              <div className="p-3 bg-blue-50 rounded-lg">
                <div className="font-medium text-blue-900">Age Proof</div>
                <div className="text-sm text-blue-700">Prove age ≥ 18 without revealing birth date</div>
              </div>
              <div className="p-3 bg-green-50 rounded-lg">
                <div className="font-medium text-green-900">Membership Proof</div>
                <div className="text-sm text-green-700">Prove membership without membership list</div>
              </div>
              <div className="p-3 bg-purple-50 rounded-lg">
                <div className="font-medium text-purple-900">Balance Proof</div>
                <div className="text-sm text-purple-700">Prove account balance without revealing amount</div>
              </div>
              <div className="p-3 bg-amber-50 rounded-lg">
                <div className="font-medium text-amber-900">Credential Proof</div>
                <div className="text-sm text-amber-700">Verify credential authenticity without disclosure</div>
              </div>
            </div>
          </div>

          <div className="bg-slate-50 p-4 rounded-lg font-mono text-sm">
            <div className="text-slate-600">type: age_proof</div>
            <div className="text-slate-600">age_range: [18, 65]</div>
            <div className="text-slate-600">protocol: MimC7</div>
            <div className="text-slate-600">commitment: 0x...</div>
            <div className="text-slate-600 mt-2"># Prover can verify without</div>
            <div className="text-slate-600"># revealing actual age</div>
          </div>
        </div>
      </div>

      <div className="card">
        <h3 className="font-bold text-slate-900 mb-3">Create Zero-Knowledge Proof</h3>
        <div className="space-y-3">
          <div>
            <label className="text-sm font-medium text-slate-700">Proof Type</label>
            <select className="input mt-1">
              <option>Age Verification</option>
              <option>Membership Verification</option>
              <option>Balance Verification</option>
              <option>Credential Verification</option>
            </select>
          </div>
          <div>
            <label className="text-sm font-medium text-slate-700">Circuit</label>
            <select className="input mt-1">
              <option>MimC7 (Recommended)</option>
              <option>Pedersen</option>
              <option>Poseidon</option>
            </select>
          </div>
          <button className="btn btn-primary">Generate Proof</button>
        </div>
      </div>
    </div>
  )
}

function AutonomousAgentsSection() {
  return (
    <div className="space-y-6">
      <div className="card">
        <h2 className="text-2xl font-bold mb-4">Autonomous Agents</h2>
        <p className="text-slate-600 mb-4">
          AI-powered agents that manage identity operations autonomously
        </p>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div className="space-y-3">
            <h3 className="font-bold text-slate-900">Agent Types</h3>
            <div className="space-y-2">
              <div className="p-3 bg-blue-50 rounded-lg">
                <div className="font-medium text-blue-900">Credential Issuer Agent</div>
                <div className="text-sm text-blue-700">Autonomously issues credentials based on rules</div>
              </div>
              <div className="p-3 bg-green-50 rounded-lg">
                <div className="font-medium text-green-900">Verifier Agent</div>
                <div className="text-sm text-green-700">Verifies credentials and proofs automatically</div>
              </div>
              <div className="p-3 bg-purple-50 rounded-lg">
                <div className="font-medium text-purple-900">Delegated Signer Agent</div>
                <div className="text-sm text-purple-700">Signs transactions on user's behalf with limits</div>
              </div>
              <div className="p-3 bg-amber-50 rounded-lg">
                <div className="font-medium text-amber-900">Risk Assessment Agent</div>
                <div className="text-sm text-amber-700">Evaluates authentication risk in real-time</div>
              </div>
            </div>
          </div>

          <div className="bg-slate-50 p-4 rounded-lg font-mono text-sm">
            <div className="text-slate-600">id: agent-issuer-001</div>
            <div className="text-slate-600">type: credential_issuer</div>
            <div className="text-slate-600">capabilities:</div>
            <div className="text-slate-600">  - issue_credentials</div>
            <div className="text-slate-600">  - revoke_credentials</div>
            <div className="text-slate-600">  - verify_identity</div>
            <div className="text-slate-600">rate_limit: 1000/hour</div>
          </div>
        </div>
      </div>

      <div className="card">
        <h3 className="font-bold text-slate-900 mb-3">Create Autonomous Agent</h3>
        <div className="space-y-3">
          <div>
            <label className="text-sm font-medium text-slate-700">Agent Type</label>
            <select className="input mt-1">
              <option>Credential Issuer</option>
              <option>Verifier</option>
              <option>Delegated Signer</option>
              <option>Risk Assessment</option>
            </select>
          </div>
          <div>
            <label className="text-sm font-medium text-slate-700">Rate Limit (requests/hour)</label>
            <input type="number" defaultValue="1000" className="input mt-1" />
          </div>
          <div>
            <label className="text-sm font-medium text-slate-700">LLM Model</label>
            <select className="input mt-1">
              <option>GPT-4 Turbo</option>
              <option>Claude 3 Opus</option>
              <option>Mistral Large</option>
            </select>
          </div>
          <button className="btn btn-primary">Create Agent</button>
        </div>
      </div>
    </div>
  )
}

function QuantumSafeSection() {
  return (
    <div className="space-y-6">
      <div className="card">
        <h2 className="text-2xl font-bold mb-4">Quantum-Safe Cryptography</h2>
        <p className="text-slate-600 mb-4">
          Post-quantum cryptography algorithms resistant to quantum computing attacks
        </p>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div className="space-y-3">
            <h3 className="font-bold text-slate-900">Algorithms</h3>
            <div className="space-y-2">
              <div className="p-3 bg-blue-50 rounded-lg">
                <div className="font-medium text-blue-900">CRYSTALS-Kyber (IND-CCA2)</div>
                <div className="text-sm text-blue-700">Key encapsulation mechanism</div>
              </div>
              <div className="p-3 bg-green-50 rounded-lg">
                <div className="font-medium text-green-900">CRYSTALS-Dilithium (QROM)</div>
                <div className="text-sm text-green-700">Digital signature scheme</div>
              </div>
              <div className="p-3 bg-purple-50 rounded-lg">
                <div className="font-medium text-purple-900">FALCON</div>
                <div className="text-sm text-purple-700">Fast lattice-based signatures</div>
              </div>
              <div className="p-3 bg-amber-50 rounded-lg">
                <div className="font-medium text-amber-900">SPHINCS+</div>
                <div className="text-sm text-amber-700">Hash-based digital signatures</div>
              </div>
            </div>
          </div>

          <div className="bg-slate-50 p-4 rounded-lg font-mono text-sm">
            <div className="text-slate-600">algorithm: CRYSTALS-Kyber-768</div>
            <div className="text-slate-600">security_level: 3 (256-bit equiv)</div>
            <div className="text-slate-600">key_size: 1184 bytes</div>
            <div className="text-slate-600">ciphertext_size: 1088 bytes</div>
            <div className="text-slate-600 mt-2"># Resistant to</div>
            <div className="text-slate-600"># quantum attacks</div>
          </div>
        </div>
      </div>

      <div className="card">
        <h3 className="font-bold text-slate-900 mb-3">Enable Quantum-Safe Cryptography</h3>
        <div className="space-y-3">
          <div>
            <label className="text-sm font-medium text-slate-700">Algorithm</label>
            <select className="input mt-1">
              <option>CRYSTALS-Kyber-768</option>
              <option>CRYSTALS-Dilithium</option>
              <option>FALCON</option>
              <option>SPHINCS+</option>
            </select>
          </div>
          <div className="flex items-center gap-2">
            <input type="checkbox" id="hybrid" defaultChecked />
            <label htmlFor="hybrid" className="text-sm font-medium text-slate-700">
              Hybrid mode (classical + quantum-safe)
            </label>
          </div>
          <button className="btn btn-primary">Enable</button>
        </div>
      </div>
    </div>
  )
}
