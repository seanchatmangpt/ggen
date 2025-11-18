'use client'

import React, { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Badge } from '@/components/ui/badge'
import { AlertCircle, CheckCircle, TrendingUp } from 'lucide-react'
import LambdaScheduler from './LambdaScheduler'
import PiProfile from './PiProfile'
import GammaChecker from './GammaChecker'

/**
 * HTF DASHBOARD - Unified Hyper-Thesis Framework Interface
 *
 * Integrates three specialized validators:
 * - Λ-Scheduler: Chapter planning with sequential constraints
 * - Π-Profile: Subsystem-to-shard mapping and family distribution
 * - Γ-Checker: Coherence validation and drift detection
 *
 * This dashboard provides a comprehensive view of thesis structure,
 * progress, and coherence across all validation dimensions.
 */

export const HTFDashboard: React.FC = () => {
  const [activeTab, setActiveTab] = useState('overview')

  // Calculate overall metrics
  const metrics = {
    chaptersComplete: 5,
    chaptersTotal: 14,
    progressPercentage: (5 / 14) * 100,
    wordsWritten: 5950,
    wordsTarget: 12000,
    coherenceScore: 92,
    driftStatus: 'healthy' as const,
  }

  return (
    <div className="w-full space-y-6">
      {/* Main Header */}
      <Card className="bg-gradient-to-r from-blue-50 to-purple-50 border-blue-200">
        <CardHeader>
          <div className="space-y-2">
            <div className="flex items-center gap-2">
              <h1 className="text-3xl font-bold text-gray-900">Hyper-Thesis Framework</h1>
              <Badge variant="secondary" className="text-base px-3 py-1">
                HTF v1.0
              </Badge>
            </div>
            <p className="text-gray-700">
              Unified dissertation framework merging 7 thesis modes (IMRaD, Papers, Argument,
              Contribution, Monograph, DSR, Narrative) across 9 subsystems via Λ-scheduling,
              Π-merge, and Γ-gluing.
            </p>
          </div>
        </CardHeader>
      </Card>

      {/* Quick Metrics */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <Card>
          <CardContent className="pt-6">
            <div className="space-y-2">
              <p className="text-sm text-gray-600">Chapters Complete</p>
              <div className="text-3xl font-bold text-blue-600">
                {metrics.chaptersComplete}/{metrics.chaptersTotal}
              </div>
              <div className="w-full bg-gray-200 rounded-full h-2">
                <div
                  className="bg-blue-600 h-2 rounded-full"
                  style={{ width: `${metrics.progressPercentage}%` }}
                />
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="space-y-2">
              <p className="text-sm text-gray-600">Words Written</p>
              <div className="text-3xl font-bold text-purple-600">
                {metrics.wordsWritten.toLocaleString()}
              </div>
              <p className="text-xs text-gray-500">
                of {metrics.wordsTarget.toLocaleString()} target
              </p>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="space-y-2">
              <p className="text-sm text-gray-600">Coherence Score</p>
              <div className="text-3xl font-bold text-green-600">{metrics.coherenceScore}%</div>
              <div className="w-full bg-gray-200 rounded-full h-2">
                <div
                  className="bg-green-600 h-2 rounded-full"
                  style={{ width: `${metrics.coherenceScore}%` }}
                />
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="space-y-2">
              <p className="text-sm text-gray-600">Drift Status</p>
              <div className="flex items-center gap-2 mt-2">
                <CheckCircle className="w-5 h-5 text-green-600" />
                <span className="font-bold text-green-600 capitalize">{metrics.driftStatus}</span>
              </div>
              <p className="text-xs text-gray-500 mt-2">All core claims maintained</p>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Main Navigation */}
      <Card>
        <CardHeader className="pb-3">
          <CardTitle>Thesis Validation & Planning</CardTitle>
        </CardHeader>
        <CardContent>
          <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full">
            <TabsList className="grid w-full grid-cols-4">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="scheduler">Λ-Scheduler</TabsTrigger>
              <TabsTrigger value="profile">Π-Profile</TabsTrigger>
              <TabsTrigger value="checker">Γ-Checker</TabsTrigger>
            </TabsList>

            {/* Overview Tab */}
            <TabsContent value="overview" className="space-y-4">
              <div className="space-y-4">
                <div className="p-4 bg-blue-50 rounded-lg border border-blue-200">
                  <h3 className="font-semibold text-sm mb-2 flex items-center gap-2">
                    <TrendingUp className="w-4 h-4" />
                    What is the Hyper-Thesis Framework?
                  </h3>
                  <p className="text-sm text-gray-700 mb-3">
                    HTF is a unified architecture for organizing dissertations that simultaneously
                    embody multiple thesis modes. Instead of choosing between IMRaD, Papers, Argument,
                    Contribution, Monograph, Design Science Research, and Narrative structures, HTF
                    enables all seven to coexist coherently.
                  </p>
                  <ul className="text-sm space-y-2 text-gray-700">
                    <li>
                      <strong>Λ-Scheduler:</strong> Enforces total ordering of chapters (sequential
                      constraints)
                    </li>
                    <li>
                      <strong>Π-Profile:</strong> Maps 9 subsystems across 27 shards to ensure complete
                      coverage
                    </li>
                    <li>
                      <strong>Γ-Checker:</strong> Validates coherence and detects drift from core claims
                    </li>
                  </ul>
                </div>

                <div className="p-4 bg-purple-50 rounded-lg border border-purple-200">
                  <h3 className="font-semibold text-sm mb-2 flex items-center gap-2">
                    <CheckCircle className="w-4 h-4" />
                    How HTF Works
                  </h3>
                  <ol className="text-sm space-y-2 text-gray-700 list-decimal list-inside">
                    <li>
                      <strong>Δ-Shards (27):</strong> Discrete thesis components across 7 families
                    </li>
                    <li>
                      <strong>Λ-Total Order:</strong> 20 constraints ensuring logical flow (problem ≺
                      gap ≺ claim ≺ ... ≺ insight)
                    </li>
                    <li>
                      <strong>Π-Merge:</strong> All 9 subsystems integrated across multiple families for
                      complete coverage
                    </li>
                    <li>
                      <strong>Γ-Gluing:</strong> Sheaf-like validation ensuring family coherence and no
                      contradictions
                    </li>
                    <li>
                      <strong>Q-Invariants:</strong> 6 global properties that must hold for thesis validity
                    </li>
                  </ol>
                </div>

                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <Card className="border-blue-200">
                    <CardHeader className="pb-3">
                      <CardTitle className="text-base">Λ-Scheduling</CardTitle>
                    </CardHeader>
                    <CardContent className="text-sm space-y-2">
                      <p className="text-gray-700">Chapter planning with sequential constraints</p>
                      <ul className="text-gray-600 space-y-1 text-xs">
                        <li>• 14 chapters in total order</li>
                        <li>• 3-10 day estimates per chapter</li>
                        <li>• Critical path analysis</li>
                        <li>• Bottleneck detection</li>
                      </ul>
                      <button className="text-blue-600 hover:underline text-xs font-semibold mt-2">
                        View Scheduler →
                      </button>
                    </CardContent>
                  </Card>

                  <Card className="border-purple-200">
                    <CardHeader className="pb-3">
                      <CardTitle className="text-base">Π-Mapping</CardTitle>
                    </CardHeader>
                    <CardContent className="text-sm space-y-2">
                      <p className="text-gray-700">Subsystem-to-shard integration</p>
                      <ul className="text-gray-600 space-y-1 text-xs">
                        <li>• 9 core subsystems</li>
                        <li>• 27 dissertation shards</li>
                        <li>• 7 thesis families</li>
                        <li>• 63+ mappings total</li>
                      </ul>
                      <button className="text-purple-600 hover:underline text-xs font-semibold mt-2">
                        View Profile →
                      </button>
                    </CardContent>
                  </Card>

                  <Card className="border-green-200">
                    <CardHeader className="pb-3">
                      <CardTitle className="text-base">Γ-Validation</CardTitle>
                    </CardHeader>
                    <CardContent className="text-sm space-y-2">
                      <p className="text-gray-700">Coherence & drift detection</p>
                      <ul className="text-gray-600 space-y-1 text-xs">
                        <li>• 6 global invariants</li>
                        <li>• 7 family coherence checks</li>
                        <li>• 5 drift detection rules</li>
                        <li>• Real-time validation</li>
                      </ul>
                      <button className="text-green-600 hover:underline text-xs font-semibold mt-2">
                        View Checker →
                      </button>
                    </CardContent>
                  </Card>
                </div>
              </div>
            </TabsContent>

            {/* Scheduler Tab */}
            <TabsContent value="scheduler" className="py-4">
              <LambdaScheduler />
            </TabsContent>

            {/* Profile Tab */}
            <TabsContent value="profile" className="py-4">
              <PiProfile />
            </TabsContent>

            {/* Checker Tab */}
            <TabsContent value="checker" className="py-4">
              <GammaChecker />
            </TabsContent>
          </Tabs>
        </CardContent>
      </Card>

      {/* HTF Statistics */}
      <Card>
        <CardHeader>
          <CardTitle>HTF Statistics & Architecture</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
            <div className="p-3 bg-gray-50 rounded">
              <p className="text-xs text-gray-600 uppercase tracking-wide">Total Shards (Δ)</p>
              <p className="text-2xl font-bold text-gray-900 mt-1">27</p>
            </div>
            <div className="p-3 bg-gray-50 rounded">
              <p className="text-xs text-gray-600 uppercase tracking-wide">Families</p>
              <p className="text-2xl font-bold text-gray-900 mt-1">7</p>
            </div>
            <div className="p-3 bg-gray-50 rounded">
              <p className="text-xs text-gray-600 uppercase tracking-wide">Λ Constraints</p>
              <p className="text-2xl font-bold text-gray-900 mt-1">20</p>
            </div>
            <div className="p-3 bg-gray-50 rounded">
              <p className="text-xs text-gray-600 uppercase tracking-wide">Q-Invariants</p>
              <p className="text-2xl font-bold text-gray-900 mt-1">6</p>
            </div>
            <div className="p-3 bg-gray-50 rounded">
              <p className="text-xs text-gray-600 uppercase tracking-wide">Core Subsystems</p>
              <p className="text-2xl font-bold text-gray-900 mt-1">9</p>
            </div>
            <div className="p-3 bg-gray-50 rounded">
              <p className="text-xs text-gray-600 uppercase tracking-wide">Shard Mappings</p>
              <p className="text-2xl font-bold text-gray-900 mt-1">63+</p>
            </div>
            <div className="p-3 bg-gray-50 rounded">
              <p className="text-xs text-gray-600 uppercase tracking-wide">Family Checks</p>
              <p className="text-2xl font-bold text-gray-900 mt-1">7</p>
            </div>
            <div className="p-3 bg-gray-50 rounded">
              <p className="text-xs text-gray-600 uppercase tracking-wide">Drift Rules</p>
              <p className="text-2xl font-bold text-gray-900 mt-1">5</p>
            </div>
          </div>

          <div className="p-4 bg-indigo-50 rounded border border-indigo-200">
            <h4 className="font-semibold text-sm mb-2">Thesis Families (7 Modes):</h4>
            <div className="text-sm text-gray-700 space-y-1">
              <p>
                <strong>1. IMRaD</strong> (Introduction, Method, Results, Discussion) - 4 shards
              </p>
              <p>
                <strong>2. Papers</strong> (Research papers with synthesis) - 4 shards
              </p>
              <p>
                <strong>3. Argument</strong> (Claims, grounds, proofs, objections, replies) - 5 shards
              </p>
              <p>
                <strong>4. Contribution</strong> (Gap, design, evaluation, impact) - 4 shards
              </p>
              <p>
                <strong>5. Monograph</strong> (Context, canon, method, analysis, conclusion) - 5 shards
              </p>
              <p>
                <strong>6. DSR</strong> (Design Science Research: problem, artifact, evaluation, theory)
                - 4 shards
              </p>
              <p>
                <strong>7. Narrative</strong> (Field, voice, pattern, insight) - 4 shards
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Quick Guide */}
      <Card>
        <CardHeader>
          <CardTitle>Quick Navigation Guide</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-3 text-sm">
            <div className="flex gap-3">
              <div className="w-8 h-8 rounded-full bg-blue-100 text-blue-600 flex items-center justify-center font-semibold flex-shrink-0">
                1
              </div>
              <div>
                <p className="font-semibold">Start with Λ-Scheduler</p>
                <p className="text-gray-600">See your chapter planning and critical path. Identify writing priorities.</p>
              </div>
            </div>
            <div className="flex gap-3">
              <div className="w-8 h-8 rounded-full bg-purple-100 text-purple-600 flex items-center justify-center font-semibold flex-shrink-0">
                2
              </div>
              <div>
                <p className="font-semibold">Review Π-Profile</p>
                <p className="text-gray-600">
                  Understand how your 9 subsystems map to dissertation shards. Ensure complete coverage.
                </p>
              </div>
            </div>
            <div className="flex gap-3">
              <div className="w-8 h-8 rounded-full bg-green-100 text-green-600 flex items-center justify-center font-semibold flex-shrink-0">
                3
              </div>
              <div>
                <p className="font-semibold">Run Γ-Checker periodically</p>
                <p className="text-gray-600">
                  Validate coherence after writing substantial content. Detect drift from core claims.
                </p>
              </div>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}

export default HTFDashboard
