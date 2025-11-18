'use client'

import React, { useState } from 'react'
import { Zap, Key, Brain, Shield, Fingerprint, Users, TrendingUp, Lock } from 'lucide-react'
import { DIDManager } from '@/components/features/DIDManager'
import { ZKProofManager } from '@/components/features/ZKProofManager'
import { AutonomousAgentManager } from '@/components/features/AutonomousAgentManager'
import { QuantumSafeCryptoManager } from '@/components/features/QuantumSafeCryptoManager'
import { SwarmCoordinationManager } from '@/components/features/SwarmCoordinationManager'
import { ReputationManager } from '@/components/features/ReputationManager'
import { BiometricManager } from '@/components/features/BiometricManager'

export default function FeaturesPage2028() {
  const [activeTab, setActiveTab] = useState<'did' | 'zk' | 'agents' | 'quantum' | 'swarm' | 'reputation' | 'biometric'>('did')

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold text-slate-900">2028 Features & Innovations</h1>
        <p className="text-slate-600 mt-1">
          Advanced identity infrastructure: DIDs, Zero-Knowledge Proofs, Autonomous Agents, Quantum-Safe Crypto,
          Swarm Coordination, Reputation Systems, and Biometric Authentication
        </p>
      </div>

      {/* Tabs */}
      <div className="overflow-x-auto">
        <div className="flex gap-2 border-b border-slate-200 whitespace-nowrap pb-0">
          {[
            { id: 'did', label: 'Decentralized Identity', icon: Key },
            { id: 'zk', label: 'Zero-Knowledge Proofs', icon: Shield },
            { id: 'agents', label: 'Autonomous Agents', icon: Brain },
            { id: 'quantum', label: 'Quantum-Safe Crypto', icon: Zap },
            { id: 'swarm', label: 'Swarm Coordination', icon: Users },
            { id: 'reputation', label: 'Reputation System', icon: TrendingUp },
            { id: 'biometric', label: 'Biometric Auth', icon: Fingerprint },
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
      </div>

      {/* Feature Content */}
      {activeTab === 'did' && <DIDManager />}
      {activeTab === 'zk' && <ZKProofManager />}
      {activeTab === 'agents' && <AutonomousAgentManager />}
      {activeTab === 'quantum' && <QuantumSafeCryptoManager />}
      {activeTab === 'swarm' && <SwarmCoordinationManager />}
      {activeTab === 'reputation' && <ReputationManager />}
      {activeTab === 'biometric' && <BiometricManager />}
    </div>
  )
}
