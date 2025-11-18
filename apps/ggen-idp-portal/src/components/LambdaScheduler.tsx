'use client'

import React, { useState, useMemo } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { ArrowRight, CheckCircle, AlertTriangle, Clock } from 'lucide-react'

/**
 * Λ-SCHEDULER: Chapter Planning with Sequential Constraints
 *
 * The Lambda (Λ) scheduler enforces a total ordering over dissertation chapters.
 * This ensures logical flow from problem definition through insight generation.
 *
 * Key principles:
 * - Primordial ordering: problem ≺ gap ≺ claim ≺ introduction
 * - Evidence chain: artifact ≺ ground ≺ papers ≺ results
 * - Synthesis: reply ≺ theory ≺ analysis ≺ conclusion ≺ insight
 * - All constraints form a directed acyclic graph (DAG)
 */

interface ScheduledChapter {
  id: string
  title: string
  description: string
  targetWordCount: number
  currentWordCount: number
  status: 'not-started' | 'in-progress' | 'draft' | 'complete'
  estimatedDays: number
  dependencies: string[] // IDs of chapters that must come first
  family: 'IMRaD' | 'Papers' | 'Argument' | 'Contribution' | 'Monograph' | 'DSR' | 'Narrative'
  color: string
}

interface CriticalPath {
  chapters: ScheduledChapter[]
  totalDays: number
  bottlenecks: string[]
}

const SCHEDULED_CHAPTERS: ScheduledChapter[] = [
  // PRIMORDIAL ORDERING
  {
    id: 'problem-delta',
    title: '1. Problem Statement',
    description: 'Define the core problem: agent swarms need decentralized identity',
    targetWordCount: 800,
    currentWordCount: 850,
    status: 'complete',
    estimatedDays: 3,
    dependencies: [],
    family: 'IMRaD',
    color: 'bg-red-100 border-red-300',
  },
  {
    id: 'gap-delta',
    title: '2. Literature Gap Analysis',
    description: 'Identify gaps in existing SSI + agent systems integration',
    targetWordCount: 1200,
    currentWordCount: 1180,
    status: 'complete',
    estimatedDays: 5,
    dependencies: ['problem-delta'],
    family: 'IMRaD',
    color: 'bg-orange-100 border-orange-300',
  },
  {
    id: 'claim-delta',
    title: '3. Main Thesis Claim',
    description: 'Decentralized identity is fundamental infrastructure for agent coordination',
    targetWordCount: 600,
    currentWordCount: 620,
    status: 'complete',
    estimatedDays: 2,
    dependencies: ['gap-delta'],
    family: 'Argument',
    color: 'bg-yellow-100 border-yellow-300',
  },
  {
    id: 'intro-delta',
    title: '4. Introduction & Context',
    description: 'Set up narrative frame, research questions, scope',
    targetWordCount: 1000,
    currentWordCount: 980,
    status: 'draft',
    estimatedDays: 3,
    dependencies: ['claim-delta'],
    family: 'IMRaD',
    color: 'bg-green-100 border-green-300',
  },

  // EVIDENCE CHAIN
  {
    id: 'artifact-delta',
    title: '5. Technical Artifacts',
    description: 'Present DIDs, VCs, ZK proofs, quantum cryptography implementation',
    targetWordCount: 2500,
    currentWordCount: 2450,
    status: 'draft',
    estimatedDays: 7,
    dependencies: ['intro-delta'],
    family: 'Papers',
    color: 'bg-blue-100 border-blue-300',
  },
  {
    id: 'ground-delta',
    title: '6. Theoretical Grounding',
    description: 'Connect artifacts to SSI, cryptography, swarm consensus theory',
    targetWordCount: 1800,
    currentWordCount: 1600,
    status: 'in-progress',
    estimatedDays: 6,
    dependencies: ['artifact-delta'],
    family: 'Contribution',
    color: 'bg-cyan-100 border-cyan-300',
  },
  {
    id: 'papers-delta',
    title: '7. Research Papers',
    description: 'Three papers: DID/Credentials, ZK/Crypto, Swarm Coordination + Synthesis',
    targetWordCount: 3000,
    currentWordCount: 2800,
    status: 'in-progress',
    estimatedDays: 10,
    dependencies: ['ground-delta'],
    family: 'Papers',
    color: 'bg-indigo-100 border-indigo-300',
  },
  {
    id: 'results-delta',
    title: '8. Results & Evaluation',
    description: 'Performance benchmarks, security analysis, deployment scenarios',
    targetWordCount: 1500,
    currentWordCount: 1200,
    status: 'in-progress',
    estimatedDays: 5,
    dependencies: ['papers-delta'],
    family: 'IMRaD',
    color: 'bg-purple-100 border-purple-300',
  },

  // SYNTHESIS & CONCLUSION
  {
    id: 'objection-delta',
    title: '9. Objections & Limitations',
    description: 'Address counterarguments, limitations, failure modes, uncertainties',
    targetWordCount: 1200,
    currentWordCount: 1100,
    status: 'draft',
    estimatedDays: 4,
    dependencies: ['results-delta'],
    family: 'Argument',
    color: 'bg-pink-100 border-pink-300',
  },
  {
    id: 'reply-delta',
    title: '10. Reply to Objections',
    description: 'Defend thesis, provide counterarguments, strengthen claims',
    targetWordCount: 800,
    currentWordCount: 700,
    status: 'draft',
    estimatedDays: 3,
    dependencies: ['objection-delta'],
    family: 'Argument',
    color: 'bg-rose-100 border-rose-300',
  },
  {
    id: 'theory-delta',
    title: '11. Theoretical Implications',
    description: 'What new theory does this work establish? How does it advance the field?',
    targetWordCount: 1000,
    currentWordCount: 800,
    status: 'draft',
    estimatedDays: 4,
    dependencies: ['reply-delta'],
    family: 'DSR',
    color: 'bg-teal-100 border-teal-300',
  },
  {
    id: 'analysis-delta',
    title: '12. Synthesis & Analysis',
    description: 'Integrate findings across all 7 families, show coherence',
    targetWordCount: 1500,
    currentWordCount: 900,
    status: 'draft',
    estimatedDays: 5,
    dependencies: ['theory-delta'],
    family: 'Monograph',
    color: 'bg-lime-100 border-lime-300',
  },
  {
    id: 'conclusion-delta',
    title: '13. Conclusion & Future Work',
    description: 'Summarize contributions, state implications, propose next steps',
    targetWordCount: 800,
    currentWordCount: 600,
    status: 'not-started',
    estimatedDays: 3,
    dependencies: ['analysis-delta'],
    family: 'IMRaD',
    color: 'bg-amber-100 border-amber-300',
  },
  {
    id: 'insight-delta',
    title: '14. Key Insights & Impact',
    description: 'The narrative payoff: why this matters for AI/identity in 2028',
    targetWordCount: 600,
    currentWordCount: 400,
    status: 'not-started',
    estimatedDays: 2,
    dependencies: ['conclusion-delta'],
    family: 'Narrative',
    color: 'bg-violet-100 border-violet-300',
  },
]

export const LambdaScheduler: React.FC = () => {
  const [selectedChapter, setSelectedChapter] = useState<ScheduledChapter | null>(null)
  const [view, setView] = useState<'timeline' | 'critical-path' | 'details'>('timeline')

  // Calculate critical path (longest dependency chain)
  const criticalPath = useMemo((): CriticalPath => {
    const chapterMap = new Map(SCHEDULED_CHAPTERS.map((ch) => [ch.id, ch]))
    const paths = new Map<string, { chapters: ScheduledChapter[]; days: number }>()

    const calculatePath = (chapterId: string): { chapters: ScheduledChapter[]; days: number } => {
      if (paths.has(chapterId)) return paths.get(chapterId)!

      const chapter = chapterMap.get(chapterId)!
      let maxDays = chapter.estimatedDays
      let maxPath: ScheduledChapter[] = [chapter]

      for (const depId of chapter.dependencies) {
        const depPath = calculatePath(depId)
        if (depPath.days + chapter.estimatedDays > maxDays) {
          maxDays = depPath.days + chapter.estimatedDays
          maxPath = [...depPath.chapters, chapter]
        }
      }

      paths.set(chapterId, { chapters: maxPath, days: maxDays })
      return paths.get(chapterId)!
    }

    // Find the longest path
    let longestPath = { chapters: [] as ScheduledChapter[], days: 0 }
    for (const chapter of SCHEDULED_CHAPTERS) {
      const path = calculatePath(chapter.id)
      if (path.days > longestPath.days) {
        longestPath = path
      }
    }

    // Identify bottlenecks (chapters with high fan-out or long duration)
    const bottlenecks = SCHEDULED_CHAPTERS.filter(
      (ch) =>
        (ch.estimatedDays >= 7 ||
          SCHEDULED_CHAPTERS.filter((other) => other.dependencies.includes(ch.id)).length > 2) &&
        longestPath.chapters.includes(ch)
    ).map((ch) => ch.title)

    return {
      chapters: longestPath.chapters,
      totalDays: longestPath.days,
      bottlenecks,
    }
  }, [])

  // Calculate progress
  const progress = useMemo(() => {
    const totalWords = SCHEDULED_CHAPTERS.reduce((sum, ch) => sum + ch.currentWordCount, 0)
    const targetWords = SCHEDULED_CHAPTERS.reduce((sum, ch) => sum + ch.targetWordCount, 0)
    const percentComplete = (totalWords / targetWords) * 100
    const completed = SCHEDULED_CHAPTERS.filter((ch) => ch.status === 'complete').length

    return { totalWords, targetWords, percentComplete, completed, total: SCHEDULED_CHAPTERS.length }
  }, [])

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'complete':
        return <CheckCircle className="w-4 h-4 text-green-600" />
      case 'draft':
        return <Clock className="w-4 h-4 text-blue-600" />
      case 'in-progress':
        return <Clock className="w-4 h-4 text-yellow-600 animate-spin" />
      default:
        return <AlertTriangle className="w-4 h-4 text-gray-400" />
    }
  }

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'complete':
        return 'bg-green-50'
      case 'draft':
        return 'bg-blue-50'
      case 'in-progress':
        return 'bg-yellow-50'
      default:
        return 'bg-gray-50'
    }
  }

  return (
    <div className="w-full space-y-6">
      {/* Header with progress */}
      <Card>
        <CardHeader>
          <CardTitle>Λ-Scheduler: Thesis Chapter Planning</CardTitle>
          <CardDescription>
            Sequential constraints ensure logical flow from problem through insight
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            <div className="p-3 bg-gradient-to-br from-blue-50 to-blue-100 rounded border border-blue-200">
              <div className="text-2xl font-bold text-blue-900">{progress.completed}</div>
              <div className="text-sm text-blue-700">Completed Chapters</div>
            </div>
            <div className="p-3 bg-gradient-to-br from-green-50 to-green-100 rounded border border-green-200">
              <div className="text-2xl font-bold text-green-900">
                {Math.round(progress.percentComplete)}%
              </div>
              <div className="text-sm text-green-700">Overall Progress</div>
            </div>
            <div className="p-3 bg-gradient-to-br from-purple-50 to-purple-100 rounded border border-purple-200">
              <div className="text-2xl font-bold text-purple-900">{criticalPath.totalDays}</div>
              <div className="text-sm text-purple-700">Critical Path (days)</div>
            </div>
            <div className="p-3 bg-gradient-to-br from-orange-50 to-orange-100 rounded border border-orange-200">
              <div className="text-2xl font-bold text-orange-900">{progress.totalWords}</div>
              <div className="text-sm text-orange-700">Words Written</div>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* View selector */}
      <div className="flex gap-2">
        <Button
          variant={view === 'timeline' ? 'default' : 'outline'}
          onClick={() => setView('timeline')}
          className="flex-1"
        >
          Timeline View
        </Button>
        <Button
          variant={view === 'critical-path' ? 'default' : 'outline'}
          onClick={() => setView('critical-path')}
          className="flex-1"
        >
          Critical Path
        </Button>
        <Button
          variant={view === 'details' ? 'default' : 'outline'}
          onClick={() => setView('details')}
          className="flex-1"
        >
          Details
        </Button>
      </div>

      {/* Timeline View */}
      {view === 'timeline' && (
        <Card>
          <CardHeader>
            <CardTitle>Writing Schedule</CardTitle>
            <CardDescription>Λ-order constraints visualized as dependency chain</CardDescription>
          </CardHeader>
          <CardContent className="space-y-3">
            {SCHEDULED_CHAPTERS.map((chapter, idx) => (
              <div key={chapter.id} className="space-y-1">
                <div className={`p-3 rounded-lg border-l-4 cursor-pointer transition ${chapter.color}`}
                     onClick={() => setSelectedChapter(chapter)}>
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-3">
                      {getStatusIcon(chapter.status)}
                      <div className="flex-1">
                        <div className="font-semibold text-sm">{chapter.title}</div>
                        <div className="text-xs text-gray-600">{chapter.description}</div>
                      </div>
                    </div>
                    <div className="flex items-center gap-2">
                      <Badge variant="secondary">{chapter.estimatedDays}d</Badge>
                      <Badge variant="outline">{chapter.status}</Badge>
                    </div>
                  </div>
                  <div className="mt-2 w-full bg-gray-200 rounded-full h-2">
                    <div
                      className="bg-blue-600 h-2 rounded-full transition-all"
                      style={{
                        width: `${(chapter.currentWordCount / chapter.targetWordCount) * 100}%`,
                      }}
                    />
                  </div>
                  <div className="text-xs text-gray-500 mt-1">
                    {chapter.currentWordCount} / {chapter.targetWordCount} words
                  </div>
                </div>

                {/* Show dependency arrows */}
                {idx < SCHEDULED_CHAPTERS.length - 1 && (
                  <div className="flex justify-center">
                    <ArrowRight className="w-4 h-4 text-gray-400 rotate-90" />
                  </div>
                )}
              </div>
            ))}
          </CardContent>
        </Card>
      )}

      {/* Critical Path View */}
      {view === 'critical-path' && (
        <Card>
          <CardHeader>
            <CardTitle>Critical Path Analysis</CardTitle>
            <CardDescription>Longest dependency chain determines minimum completion time</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <Alert className="border-purple-200 bg-purple-50">
              <AlertTriangle className="h-4 w-4" />
              <AlertDescription>
                <strong>Critical Path: {criticalPath.totalDays} days</strong>
                {criticalPath.bottlenecks.length > 0 && (
                  <div className="mt-2">
                    <strong>Bottlenecks:</strong> {criticalPath.bottlenecks.join(', ')}
                  </div>
                )}
              </AlertDescription>
            </Alert>

            <div className="space-y-2">
              <h4 className="font-semibold text-sm">Critical Path Chapters:</h4>
              {criticalPath.chapters.map((chapter) => (
                <div
                  key={chapter.id}
                  className={`p-3 rounded-lg border-l-4 ${chapter.color} cursor-pointer`}
                  onClick={() => setSelectedChapter(chapter)}
                >
                  <div className="flex items-center justify-between">
                    <div>
                      <div className="font-semibold text-sm">{chapter.title}</div>
                      <div className="text-xs text-gray-600">{chapter.estimatedDays} days</div>
                    </div>
                    {getStatusIcon(chapter.status)}
                  </div>
                </div>
              ))}
            </div>

            <div className="p-3 bg-blue-50 rounded border border-blue-200">
              <h4 className="font-semibold text-sm mb-2">Optimization Suggestions:</h4>
              <ul className="text-sm space-y-1 text-gray-700">
                <li>• Parallelize independent chapters: {
                  SCHEDULED_CHAPTERS
                    .filter(ch => !criticalPath.chapters.some(cp => cp.dependencies.includes(ch.id)) &&
                                  !criticalPath.chapters.some(cp => ch.dependencies.includes(cp.id)))
                    .slice(0, 2)
                    .map(ch => ch.title.split('.')[1].trim())
                    .join(', ')
                }</li>
                <li>• Prioritize critical path chapters: {criticalPath.bottlenecks[0]}</li>
                <li>• Consider splitting long chapters (&gt;7 days) for faster iteration</li>
              </ul>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Details View */}
      {view === 'details' && selectedChapter && (
        <Card>
          <CardHeader>
            <CardTitle>{selectedChapter.title}</CardTitle>
            <CardDescription>{selectedChapter.family}</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div>
              <h4 className="font-semibold text-sm mb-2">Description</h4>
              <p className="text-sm text-gray-700">{selectedChapter.description}</p>
            </div>

            <div className="grid grid-cols-2 gap-4">
              <div className="p-3 bg-gray-50 rounded">
                <div className="text-xs text-gray-600 uppercase tracking-wide">Status</div>
                <div className="flex items-center gap-2 mt-1">
                  {getStatusIcon(selectedChapter.status)}
                  <span className="font-semibold text-sm">{selectedChapter.status}</span>
                </div>
              </div>
              <div className="p-3 bg-gray-50 rounded">
                <div className="text-xs text-gray-600 uppercase tracking-wide">Timeline</div>
                <div className="font-semibold text-sm mt-1">{selectedChapter.estimatedDays} days estimated</div>
              </div>
              <div className="p-3 bg-gray-50 rounded">
                <div className="text-xs text-gray-600 uppercase tracking-wide">Word Count</div>
                <div className="font-semibold text-sm mt-1">
                  {selectedChapter.currentWordCount} / {selectedChapter.targetWordCount}
                </div>
              </div>
              <div className="p-3 bg-gray-50 rounded">
                <div className="text-xs text-gray-600 uppercase tracking-wide">Dependencies</div>
                <div className="font-semibold text-sm mt-1">
                  {selectedChapter.dependencies.length === 0
                    ? 'None (start here)'
                    : selectedChapter.dependencies.length}
                </div>
              </div>
            </div>

            {selectedChapter.dependencies.length > 0 && (
              <div>
                <h4 className="font-semibold text-sm mb-2">Must Complete First:</h4>
                <div className="space-y-2">
                  {selectedChapter.dependencies.map((depId) => {
                    const dep = SCHEDULED_CHAPTERS.find((ch) => ch.id === depId)
                    return (
                      <div key={depId} className="p-2 bg-amber-50 rounded border border-amber-200 text-sm">
                        <div className="font-semibold">{dep?.title}</div>
                        <div className="text-xs text-gray-600">{dep?.description}</div>
                      </div>
                    )
                  })}
                </div>
              </div>
            )}

            <div className="p-3 bg-blue-50 rounded border border-blue-200">
              <h4 className="font-semibold text-sm mb-2">Writing Prompt:</h4>
              <p className="text-sm text-gray-700">
                {selectedChapter.id === 'problem-delta'
                  ? 'Define the core technical and societal problem that motivates this research.'
                  : selectedChapter.id === 'gap-delta'
                    ? 'What existing literature addresses parts of this problem? What gaps remain?'
                    : selectedChapter.id === 'claim-delta'
                      ? 'State your main thesis as a single, falsifiable claim.'
                      : selectedChapter.id === 'intro-delta'
                        ? 'Introduce the research context, questions, and scope.'
                        : selectedChapter.id === 'artifact-delta'
                          ? 'Present your technical implementation and design decisions.'
                          : selectedChapter.id === 'ground-delta'
                            ? 'Connect your artifacts to relevant theory and prior work.'
                            : selectedChapter.id === 'papers-delta'
                              ? 'Develop research paper(s) contributing novel insights.'
                              : selectedChapter.id === 'results-delta'
                                ? 'Present empirical results, benchmarks, and evaluations.'
                                : selectedChapter.id === 'objection-delta'
                                  ? 'Identify objections to your thesis and limitations of your work.'
                                  : selectedChapter.id === 'reply-delta'
                                    ? 'Respond to objections and strengthen your argument.'
                                    : selectedChapter.id === 'theory-delta'
                                      ? 'What new theory emerges from your work?'
                                      : selectedChapter.id === 'analysis-delta'
                                        ? 'Synthesize findings across all 7 thesis families.'
                                        : selectedChapter.id === 'conclusion-delta'
                                          ? 'Summarize contributions and propose future work.'
                                          : 'What are the key insights and real-world impact?'}
              </p>
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  )
}

export default LambdaScheduler
