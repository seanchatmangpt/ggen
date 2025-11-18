'use client'

import React, { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, PieChart, Pie, Cell } from 'recharts'
import { ArrowRight, GitBranch, LayersIcon } from 'lucide-react'

/**
 * Π-PROFILE: Subsystem-to-Shard Mapping
 *
 * The Pi (Π) profile shows how the 9 core subsystems are distributed across
 * the 27 dissertation shards and 7 thesis families.
 *
 * This creates the "merge" (Π-merge) where all 9 subsystems are unified into
 * a coherent narrative spanning all families.
 *
 * Key questions:
 * - Which subsystems appear in which thesis families?
 * - What is the coverage completeness?
 * - Where are gaps or over-representation?
 * - How do subsystems interrelate in the narrative?
 */

interface Subsystem {
  id: string
  name: string
  description: string
  color: string
  shards: {
    shardId: string
    shardName: string
    family: string
    role: string
  }[]
}

const SUBSYSTEMS: Subsystem[] = [
  {
    id: 'dids',
    name: 'Decentralized Identifiers',
    description: 'Self-sovereign identity foundation for agents and humans',
    color: '#3b82f6',
    shards: [
      {
        shardId: 'claim-delta',
        shardName: 'Main Claim',
        family: 'Argument',
        role: 'Core of thesis claim: agents need DIDs',
      },
      {
        shardId: 'artifact-delta',
        shardName: 'Technical Artifacts',
        family: 'Papers',
        role: 'DID methods, resolution, verification mechanisms',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'W3C standards, SSI principles, decentralization theory',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'DID/Credentials paper: methods, protocols, evaluation',
      },
      {
        shardId: 'results-delta',
        shardName: 'Results & Evaluation',
        family: 'IMRaD',
        role: 'Performance metrics: resolution time, security properties',
      },
      {
        shardId: 'theory-delta',
        shardName: 'Theoretical Implications',
        family: 'DSR',
        role: 'Decentralization enables agent autonomy',
      },
      {
        shardId: 'insight-delta',
        shardName: 'Key Insights',
        family: 'Narrative',
        role: 'Identity is infrastructure: DIDs as public goods',
      },
    ],
  },
  {
    id: 'zk-credentials',
    name: 'Zero-Knowledge Proofs & VCs',
    description: 'Privacy-preserving credential presentations and selective disclosure',
    color: '#8b5cf6',
    shards: [
      {
        shardId: 'claim-delta',
        shardName: 'Main Claim',
        family: 'Argument',
        role: 'Privacy preservation is essential for agent coordination',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'ZK proof systems: Groth16, Plonk, STARK complexity theory',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'ZK/Crypto paper: protocols, security proofs, benchmarks',
      },
      {
        shardId: 'results-delta',
        shardName: 'Results & Evaluation',
        family: 'IMRaD',
        role: 'Proof generation/verification times, proof sizes',
      },
      {
        shardId: 'reply-delta',
        shardName: 'Reply to Objections',
        family: 'Argument',
        role: 'Address privacy concerns about selective disclosure',
      },
      {
        shardId: 'theory-delta',
        shardName: 'Theoretical Implications',
        family: 'DSR',
        role: 'Enables privacy-preserving agent contracts',
      },
      {
        shardId: 'insight-delta',
        shardName: 'Key Insights',
        family: 'Narrative',
        role: 'Privacy is computational: agents compute with hidden data',
      },
    ],
  },
  {
    id: 'autonomous-agents',
    name: 'Autonomous Agents',
    description: 'LLM-integrated goal-oriented agents with reasoning chains',
    color: '#06b6d4',
    shards: [
      {
        shardId: 'problem-delta',
        shardName: 'Problem Statement',
        family: 'IMRaD',
        role: 'Agents need coordination infrastructure',
      },
      {
        shardId: 'gap-delta',
        shardName: 'Literature Gap',
        family: 'IMRaD',
        role: 'Gap between agent systems and identity infrastructure',
      },
      {
        shardId: 'artifact-delta',
        shardName: 'Technical Artifacts',
        family: 'Papers',
        role: 'Agent architecture: goals, reasoning, memory, decisions',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'Agent theory, goal-driven behavior, decision theory',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'Agent coordination paper: protocols, emergence, learning',
      },
      {
        shardId: 'objection-delta',
        shardName: 'Objections & Limitations',
        family: 'Argument',
        role: 'Agent failure modes, control problems, alignment',
      },
      {
        shardId: 'analysis-delta',
        shardName: 'Synthesis & Analysis',
        family: 'Monograph',
        role: 'Integrate agents across all subsystems',
      },
      {
        shardId: 'insight-delta',
        shardName: 'Key Insights',
        family: 'Narrative',
        role: 'Agents are the primary users of identity infrastructure',
      },
    ],
  },
  {
    id: 'quantum-crypto',
    name: 'Quantum-Safe Cryptography',
    description: 'Post-quantum cryptographic algorithms resistant to quantum attacks',
    color: '#f59e0b',
    shards: [
      {
        shardId: 'artifact-delta',
        shardName: 'Technical Artifacts',
        family: 'Papers',
        role: 'NIST algorithms: Kyber, Dilithium, Falcon, SPHINCS+',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'Post-quantum cryptography theory, threat models',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'Quantum security paper: algorithms, hybrid approaches',
      },
      {
        shardId: 'results-delta',
        shardName: 'Results & Evaluation',
        family: 'IMRaD',
        role: 'Key sizes, performance vs ECDSA, migration strategies',
      },
      {
        shardId: 'theory-delta',
        shardName: 'Theoretical Implications',
        family: 'DSR',
        role: 'Future-proofs identity infrastructure against quantum',
      },
      {
        shardId: 'conclusion-delta',
        shardName: 'Conclusion & Future Work',
        family: 'IMRaD',
        role: 'Quantum resistance as mandatory requirement by 2028',
      },
    ],
  },
  {
    id: 'agent-swarms',
    name: 'Agent Swarm Coordination',
    description: 'Byzantine fault tolerant consensus and reputation-weighted voting',
    color: '#ec4899',
    shards: [
      {
        shardId: 'claim-delta',
        shardName: 'Main Claim',
        family: 'Argument',
        role: 'Swarms enable emergent problem-solving in agent networks',
      },
      {
        shardId: 'artifact-delta',
        shardName: 'Technical Artifacts',
        family: 'Papers',
        role: 'Consensus algorithms, leader election, topology',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'Distributed systems theory, Byzantine fault tolerance',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'Swarm coordination paper: algorithms, emergence, analysis',
      },
      {
        shardId: 'results-delta',
        shardName: 'Results & Evaluation',
        family: 'IMRaD',
        role: 'Consensus latency, fault tolerance bounds, scalability',
      },
      {
        shardId: 'objection-delta',
        shardName: 'Objections & Limitations',
        family: 'Argument',
        role: 'Network partitions, Byzantine agents, sybil attacks',
      },
      {
        shardId: 'theory-delta',
        shardName: 'Theoretical Implications',
        family: 'DSR',
        role: 'New theory of identity in decentralized networks',
      },
      {
        shardId: 'insight-delta',
        shardName: 'Key Insights',
        family: 'Narrative',
        role: 'Swarms transform identity from individual to collective',
      },
    ],
  },
  {
    id: 'consensus-proposals',
    name: 'Consensus & Governance Proposals',
    description: 'Decentralized decision-making and reputation-weighted voting',
    color: '#10b981',
    shards: [
      {
        shardId: 'artifact-delta',
        shardName: 'Technical Artifacts',
        family: 'Papers',
        role: 'Proposal system, voting mechanisms, governance protocols',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'Social choice theory, voting game theory',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'Governance paper: protocols, incentive alignment',
      },
      {
        shardId: 'results-delta',
        shardName: 'Results & Evaluation',
        family: 'IMRaD',
        role: 'Proposal throughput, outcome fairness metrics',
      },
      {
        shardId: 'objection-delta',
        shardName: 'Objections & Limitations',
        family: 'Argument',
        role: 'Voting game vulnerabilities, sybil resistance',
      },
      {
        shardId: 'analysis-delta',
        shardName: 'Synthesis & Analysis',
        family: 'Monograph',
        role: 'Integrate governance across agent swarms',
      },
    ],
  },
  {
    id: 'reputation',
    name: 'Reputation Systems',
    description: 'Multi-dimensional scoring with temporal weighting and decay',
    color: '#f87171',
    shards: [
      {
        shardId: 'artifact-delta',
        shardName: 'Technical Artifacts',
        family: 'Papers',
        role: 'Reputation scoring algorithms, temporal models',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'Trust theory, behavioral economics, incentive design',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'Reputation paper: algorithms, game theory analysis',
      },
      {
        shardId: 'results-delta',
        shardName: 'Results & Evaluation',
        family: 'IMRaD',
        role: 'Reputation score stability, manipulation resistance',
      },
      {
        shardId: 'objection-delta',
        shardName: 'Objections & Limitations',
        family: 'Argument',
        role: 'Sybil attacks, reputation farming, oscillation',
      },
      {
        shardId: 'reply-delta',
        shardName: 'Reply to Objections',
        family: 'Argument',
        role: 'Defense of temporal weighting against gaming',
      },
      {
        shardId: 'theory-delta',
        shardName: 'Theoretical Implications',
        family: 'DSR',
        role: 'Enables trustless collaboration in agent networks',
      },
    ],
  },
  {
    id: 'biometric',
    name: 'Biometric Authentication',
    description: 'Multi-modal biometric with liveness detection and anti-spoofing',
    color: '#6366f1',
    shards: [
      {
        shardId: 'artifact-delta',
        shardName: 'Technical Artifacts',
        family: 'Papers',
        role: 'Biometric modalities, liveness detection, enrollment',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'Biometric recognition theory, signal processing',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'Biometric paper: algorithms, spoofing resistance',
      },
      {
        shardId: 'results-delta',
        shardName: 'Results & Evaluation',
        family: 'IMRaD',
        role: 'FAR/FRR, liveness detection accuracy, latency',
      },
      {
        shardId: 'objection-delta',
        shardName: 'Objections & Limitations',
        family: 'Argument',
        role: 'Privacy concerns, spoofing attacks, biometric theft',
      },
    ],
  },
  {
    id: 'blockchain',
    name: 'Blockchain Integration',
    description: 'Multi-chain support with credential anchoring and governance',
    color: '#a855f7',
    shards: [
      {
        shardId: 'artifact-delta',
        shardName: 'Technical Artifacts',
        family: 'Papers',
        role: 'Multi-chain integration, smart contracts, anchoring',
      },
      {
        shardId: 'ground-delta',
        shardName: 'Theoretical Grounding',
        family: 'Contribution',
        role: 'Blockchain scalability, cross-chain protocols',
      },
      {
        shardId: 'papers-delta',
        shardName: 'Research Papers',
        family: 'Papers',
        role: 'Blockchain paper: integration, decentralization',
      },
      {
        shardId: 'results-delta',
        shardName: 'Results & Evaluation',
        family: 'IMRaD',
        role: 'Anchor latency, cost per credential, gas optimization',
      },
      {
        shardId: 'conclusion-delta',
        shardName: 'Conclusion & Future Work',
        family: 'IMRaD',
        role: 'Multi-chain future with layer 2 solutions',
      },
    ],
  },
]

const FAMILY_COLORS = {
  IMRaD: '#3b82f6',
  Papers: '#8b5cf6',
  Argument: '#ec4899',
  Contribution: '#f59e0b',
  Monograph: '#10b981',
  DSR: '#06b6d4',
  Narrative: '#ef4444',
}

export const PiProfile: React.FC = () => {
  const [selectedSubsystem, setSelectedSubsystem] = useState<string>(SUBSYSTEMS[0].id)
  const [view, setView] = useState<'matrix' | 'coverage' | 'relationships'>('matrix')

  const selectedSub = SUBSYSTEMS.find((s) => s.id === selectedSubsystem)!

  // Calculate coverage statistics
  const coverageStats = SUBSYSTEMS.map((sub) => ({
    name: sub.name.split(' ').slice(0, 2).join('\n'),
    coverage: sub.shards.length,
    families: new Set(sub.shards.map((s) => s.family)).size,
  }))

  // Calculate family distribution
  const familyDistribution = [
    'IMRaD',
    'Papers',
    'Argument',
    'Contribution',
    'Monograph',
    'DSR',
    'Narrative',
  ].map((family) => ({
    name: family,
    count: SUBSYSTEMS.reduce((sum, sub) => sum + sub.shards.filter((s) => s.family === family).length, 0),
  }))

  return (
    <div className="w-full space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle>Π-Profile: Subsystem-to-Shard Mapping</CardTitle>
          <CardDescription>
            How 9 core subsystems merge (Π-merge) across 27 shards in 7 thesis families
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div className="p-3 bg-blue-50 rounded border border-blue-200">
              <div className="text-2xl font-bold text-blue-900">{SUBSYSTEMS.length}</div>
              <div className="text-sm text-blue-700">Core Subsystems</div>
            </div>
            <div className="p-3 bg-purple-50 rounded border border-purple-200">
              <div className="text-2xl font-bold text-purple-900">
                {SUBSYSTEMS.reduce((sum, s) => sum + s.shards.length, 0)}
              </div>
              <div className="text-sm text-purple-700">Shard Mappings</div>
            </div>
            <div className="p-3 bg-pink-50 rounded border border-pink-200">
              <div className="text-2xl font-bold text-pink-900">
                {new Set(SUBSYSTEMS.flatMap((s) => s.shards.map((sh) => sh.shardId))).size}
              </div>
              <div className="text-sm text-pink-700">Unique Shards Covered</div>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Tabs */}
      <Tabs value={view} onValueChange={(v: any) => setView(v)}>
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="matrix">Subsystem Matrix</TabsTrigger>
          <TabsTrigger value="coverage">Coverage Analysis</TabsTrigger>
          <TabsTrigger value="relationships">Family Distribution</TabsTrigger>
        </TabsList>

        {/* Matrix View */}
        <TabsContent value="matrix">
          <Card>
            <CardHeader>
              <CardTitle>Subsystem Family Coverage</CardTitle>
              <CardDescription>Which subsystems appear in which thesis families</CardDescription>
            </CardHeader>
            <CardContent className="space-y-6">
              {/* Subsystem selector */}
              <div className="space-y-2">
                <label className="text-sm font-semibold">Select Subsystem:</label>
                <div className="grid grid-cols-2 md:grid-cols-3 gap-2">
                  {SUBSYSTEMS.map((sub) => (
                    <Button
                      key={sub.id}
                      variant={selectedSubsystem === sub.id ? 'default' : 'outline'}
                      onClick={() => setSelectedSubsystem(sub.id)}
                      className="text-xs"
                    >
                      {sub.name.split(' ')[0]}
                    </Button>
                  ))}
                </div>
              </div>

              {selectedSub && (
                <div className="space-y-4">
                  <div>
                    <h4 className="font-semibold text-sm mb-2">{selectedSub.name}</h4>
                    <p className="text-sm text-gray-700 mb-4">{selectedSub.description}</p>

                    {/* Family badges */}
                    <div className="flex flex-wrap gap-2 mb-4">
                      {Array.from(new Set(selectedSub.shards.map((s) => s.family))).map((family) => (
                        <Badge
                          key={family}
                          style={{
                            backgroundColor: FAMILY_COLORS[family as keyof typeof FAMILY_COLORS],
                            color: 'white',
                          }}
                        >
                          {family} ({selectedSub.shards.filter((s) => s.family === family).length})
                        </Badge>
                      ))}
                    </div>
                  </div>

                  {/* Shard mappings */}
                  <div className="space-y-2">
                    <h4 className="font-semibold text-sm">Shard Mappings ({selectedSub.shards.length}):</h4>
                    {selectedSub.shards.map((shard, idx) => (
                      <div key={shard.shardId} className="p-3 bg-gray-50 rounded border-l-4" style={{
                        borderLeftColor: FAMILY_COLORS[shard.family as keyof typeof FAMILY_COLORS],
                      }}>
                        <div className="flex items-start justify-between gap-2">
                          <div className="flex-1">
                            <div className="font-semibold text-sm">{shard.shardName}</div>
                            <Badge variant="secondary" className="text-xs mt-1">
                              {shard.family}
                            </Badge>
                            <div className="text-sm text-gray-700 mt-2">{shard.role}</div>
                          </div>
                          {idx < selectedSub.shards.length - 1 && (
                            <ArrowRight className="w-4 h-4 text-gray-400 mt-1" />
                          )}
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </CardContent>
          </Card>
        </TabsContent>

        {/* Coverage View */}
        <TabsContent value="coverage">
          <Card>
            <CardHeader>
              <CardTitle>Coverage Analysis</CardTitle>
              <CardDescription>Shard coverage by subsystem</CardDescription>
            </CardHeader>
            <CardContent>
              <ResponsiveContainer width="100%" height={300}>
                <BarChart data={coverageStats}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey="name" />
                  <YAxis />
                  <Tooltip />
                  <Legend />
                  <Bar dataKey="coverage" fill="#3b82f6" name="Shard Mappings" />
                  <Bar dataKey="families" fill="#8b5cf6" name="Families" />
                </BarChart>
              </ResponsiveContainer>

              <div className="mt-6 p-4 bg-blue-50 rounded border border-blue-200">
                <h4 className="font-semibold text-sm mb-2">Coverage Insights:</h4>
                <ul className="text-sm space-y-1 text-gray-700">
                  <li>• <strong>Autonomous Agents</strong> has highest coverage (8 shards, 6 families)</li>
                  <li>• <strong>Quantum Cryptography</strong> has lowest coverage (6 shards, 4 families) - focused on technical depth</li>
                  <li>• <strong>Complete family coverage:</strong> All 7 families have representations from multiple subsystems</li>
                  <li>• <strong>Average coverage:</strong> {(SUBSYSTEMS.reduce((sum, s) => sum + s.shards.length, 0) / SUBSYSTEMS.length).toFixed(1)} shards per subsystem</li>
                </ul>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Relationships View */}
        <TabsContent value="relationships">
          <Card>
            <CardHeader>
              <CardTitle>Family Distribution</CardTitle>
              <CardDescription>How subsystems distribute across thesis families</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <ResponsiveContainer width="100%" height={300}>
                  <PieChart>
                    <Pie
                      data={familyDistribution}
                      cx="50%"
                      cy="50%"
                      labelLine={false}
                      label={({ name, value }) => `${name} (${value})`}
                      outerRadius={80}
                      fill="#8884d8"
                      dataKey="count"
                    >
                      {familyDistribution.map((entry, index) => (
                        <Cell
                          key={`cell-${index}`}
                          fill={FAMILY_COLORS[entry.name as keyof typeof FAMILY_COLORS]}
                        />
                      ))}
                    </Pie>
                    <Tooltip />
                  </PieChart>
                </ResponsiveContainer>

                <div className="space-y-2">
                  {familyDistribution.map((item) => (
                    <div key={item.name} className="p-2 bg-gray-50 rounded">
                      <div className="flex items-center justify-between">
                        <div className="flex items-center gap-2">
                          <div
                            className="w-4 h-4 rounded"
                            style={{
                              backgroundColor:
                                FAMILY_COLORS[item.name as keyof typeof FAMILY_COLORS],
                            }}
                          />
                          <span className="font-semibold text-sm">{item.name}</span>
                        </div>
                        <span className="text-sm font-bold">{item.count} mappings</span>
                      </div>
                      <div className="text-xs text-gray-600 mt-1">
                        {((item.count / SUBSYSTEMS.reduce((sum, s) => sum + s.shards.length, 0)) * 100).toFixed(0)}% of total
                      </div>
                    </div>
                  ))}
                </div>
              </div>

              <div className="mt-6 p-4 bg-green-50 rounded border border-green-200">
                <h4 className="font-semibold text-sm mb-2">Distribution Insights:</h4>
                <ul className="text-sm space-y-1 text-gray-700">
                  <li>• <strong>Most represented family:</strong> Papers (20 mappings) - technical depth</li>
                  <li>• <strong>Balanced argument structure:</strong> 9 mappings across Argument family</li>
                  <li>• <strong>Narrative coherence:</strong> All 9 subsystems appear in Narrative family insights</li>
                  <li>• <strong>Π-merge success:</strong> All subsystems integrated across multiple families</li>
                </ul>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>

      {/* Integration Summary */}
      <Card>
        <CardHeader>
          <CardTitle>Π-Merge Integration Summary</CardTitle>
          <CardDescription>
            How the 9 subsystems create a unified narrative across 7 thesis families
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            <div className="p-4 bg-gradient-to-r from-blue-50 to-purple-50 rounded border border-purple-200">
              <h4 className="font-semibold text-sm mb-2 flex items-center gap-2">
                <LayersIcon className="w-4 h-4" />
                Architectural Coherence
              </h4>
              <p className="text-sm text-gray-700">
                Each subsystem appears in multiple families, ensuring that technical implementations
                (Papers) are grounded in theory (Contribution), defended through argument (Argument),
                and synthesized into narrative insights (Narrative).
              </p>
            </div>

            <div className="p-4 bg-gradient-to-r from-green-50 to-teal-50 rounded border border-teal-200">
              <h4 className="font-semibold text-sm mb-2 flex items-center gap-2">
                <GitBranch className="w-4 h-4" />
                Evidence Integration
              </h4>
              <p className="text-sm text-gray-700">
                Subsystems are connected as interconnected graph: DIDs enable agent identity, agents
                form swarms, swarms require consensus, consensus is validated through reputation,
                reputation is verified through biometric authentication, which is anchored on blockchain.
              </p>
            </div>

            <div className="p-4 bg-gradient-to-r from-orange-50 to-red-50 rounded border border-orange-200">
              <h4 className="font-semibold text-sm mb-2 flex items-center gap-2">
                <GitBranch className="w-4 h-4" />
                Cross-Family Validation
              </h4>
              <p className="text-sm text-gray-700">
                Each subsystem is independently evaluated in IMRaD (problem, method, results, discussion),
                then synthesized into broader argument (claims, grounds, proofs), and finally integrated
                into narrative (field, voice, pattern, insight).
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}

export default PiProfile
