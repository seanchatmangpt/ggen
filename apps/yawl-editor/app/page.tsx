'use client'

import Link from "next/link"
import { Card } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { Badge } from "@/components/ui/badge"
import {
  Workflow,
  Zap,
  Users,
  BarChart3,
  ArrowRight,
  Sparkles,
  CheckCircle2,
  Boxes,
  Code2,
  Shield,
} from 'lucide-react'

export default function Home() {
  const features = [
    {
      icon: Code2,
      title: 'Monaco AI Studio',
      description: 'AI-powered code editor with generation, analysis, and refactoring',
      href: '/monaco-studio',
    },
    {
      icon: Boxes,
      title: 'Backstage IDP',
      description: 'Internal Developer Platform with catalog, services, and deployments',
      href: '/backstage',
    },
    {
      icon: Shield,
      title: 'Quantum Security',
      description: 'Post-quantum cryptography for tomorrow\'s security threats',
      href: '/quantum-security',
    },
    {
      icon: Workflow,
      title: 'Process Designer',
      description: 'Design BPMN workflows with automatic conversion to YAWL ontology',
      href: '/designer',
    },
    {
      icon: Zap,
      title: 'Case Management',
      description: 'Create, track, and manage workflow case instances in real-time',
      href: '/cases',
    },
    {
      icon: Users,
      title: 'Team Collaboration',
      description: 'Real-time workspace with presence tracking and activity feeds',
      href: '/collaboration',
    },
  ]

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 via-blue-50 to-slate-100">
      {/* Hero Section */}
      <div className="px-4 py-12 md:py-20 lg:py-32">
        <div className="max-w-6xl mx-auto space-y-6 md:space-y-8">
          {/* Main Headline */}
          <div className="space-y-3 md:space-y-4">
            <div className="inline-flex items-center gap-2 px-4 py-2 rounded-full bg-blue-100 text-blue-700 text-sm font-medium">
              <Sparkles className="h-4 w-4" />
              Innovation Hub - All-in-One Platform
            </div>
            <h1 className="text-4xl md:text-5xl lg:text-6xl font-bold tracking-tight text-slate-900">
              Modern Workflow Management
            </h1>
            <p className="text-lg md:text-xl text-slate-600 max-w-3xl">
              Next-generation YAWL editor with real-time collaboration, BPMN design,
              advanced analytics, and AI-powered insights. Built with Next.js, TypeScript, and SPARQL integration.
            </p>
          </div>

          {/* CTA Buttons */}
          <div className="flex flex-col sm:flex-row gap-3 pt-4">
            <Link href="/innovation-hub">
              <Button size="lg" className="w-full sm:w-auto gap-2">
                Launch Innovation Hub
                <ArrowRight className="h-4 w-4" />
              </Button>
            </Link>
            <Link href="/designer">
              <Button size="lg" variant="outline" className="w-full sm:w-auto gap-2">
                Start Designing
              </Button>
            </Link>
          </div>
        </div>
      </div>

      {/* Stats Section */}
      <div className="px-4 py-8 md:py-12">
        <div className="max-w-6xl mx-auto">
          <div className="grid grid-cols-2 md:grid-cols-4 gap-3 md:gap-4">
            {[
              { label: 'Active Processes', value: '24' },
              { label: 'Cases Managed', value: '156' },
              { label: 'Team Members', value: '12' },
              { label: 'Uptime', value: '99.9%' },
            ].map((stat, idx) => (
              <Card key={idx} className="p-4 md:p-6 bg-white">
                <p className="text-xs md:text-sm font-medium text-slate-600">{stat.label}</p>
                <p className="text-2xl md:text-3xl font-bold mt-1">{stat.value}</p>
              </Card>
            ))}
          </div>
        </div>
      </div>

      {/* Features Grid */}
      <div className="px-4 py-12 md:py-16">
        <div className="max-w-6xl mx-auto">
          <div className="text-center mb-8 md:mb-12">
            <h2 className="text-2xl md:text-3xl lg:text-4xl font-bold text-slate-900">
              Integrated Features
            </h2>
            <p className="text-slate-600 mt-2 text-sm md:text-base">
              Everything you need for modern workflow management
            </p>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 md:gap-6">
            {features.map((feature) => {
              const Icon = feature.icon
              return (
                <Link key={feature.href} href={feature.href}>
                  <Card className="h-full p-6 hover:shadow-lg transition-shadow cursor-pointer">
                    <div className="flex flex-col h-full">
                      <div className="p-3 bg-gradient-to-br from-blue-100 to-purple-100 rounded-lg w-fit mb-4">
                        <Icon className="h-6 w-6 text-blue-600" />
                      </div>
                      <h3 className="text-lg font-semibold text-slate-900 mb-2">
                        {feature.title}
                      </h3>
                      <p className="text-sm text-slate-600 flex-1">
                        {feature.description}
                      </p>
                      <div className="pt-4 mt-4 border-t">
                        <Button
                          variant="ghost"
                          size="sm"
                          className="gap-2 group"
                        >
                          Explore
                          <ArrowRight className="h-4 w-4 group-hover:translate-x-1 transition-transform" />
                        </Button>
                      </div>
                    </div>
                  </Card>
                </Link>
              )
            })}
          </div>
        </div>
      </div>

      {/* Technology Stack */}
      <div className="px-4 py-12 md:py-16 bg-white">
        <div className="max-w-6xl mx-auto">
          <div className="text-center mb-8 md:mb-12">
            <h2 className="text-2xl md:text-3xl lg:text-4xl font-bold text-slate-900">
              Modern Tech Stack
            </h2>
            <p className="text-slate-600 mt-2 text-sm md:text-base">
              Built with industry-leading technologies
            </p>
          </div>

          <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-3 md:gap-4">
            {[
              { name: 'Next.js 14', desc: 'React Framework' },
              { name: 'TypeScript', desc: 'Type Safety' },
              { name: 'SPARQL', desc: 'Data Queries' },
              { name: 'RDF/OWL', desc: 'Semantics' },
              { name: 'Tailwind CSS', desc: 'Styling' },
              { name: 'shadcn/ui', desc: 'Components' },
              { name: 'BPMN.js', desc: 'Diagrams' },
              { name: 'WebSocket', desc: 'Real-time' },
              { name: 'Zod', desc: 'Validation' },
              { name: 'React Hooks', desc: 'Logic' },
              { name: 'Quantum Crypto', desc: 'Post-Quantum' },
              { name: 'Vercel', desc: 'Hosting' },
            ].map((tech, idx) => (
              <Card key={idx} className="p-3 md:p-4 text-center hover:shadow-md transition-shadow">
                <p className="font-semibold text-sm">{tech.name}</p>
                <p className="text-xs text-slate-500 mt-1">{tech.desc}</p>
              </Card>
            ))}
          </div>
        </div>
      </div>

      {/* Key Features */}
      <div className="px-4 py-12 md:py-16">
        <div className="max-w-6xl mx-auto">
          <div className="text-center mb-8 md:mb-12">
            <h2 className="text-2xl md:text-3xl lg:text-4xl font-bold text-slate-900">
              Powerful Features
            </h2>
            <p className="text-slate-600 mt-2 text-sm md:text-base">
              Everything needed for enterprise workflow management
            </p>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6 md:gap-8">
            {[
              {
                icon: Code2,
                title: 'AI Code Studio (Monaco Editor)',
                items: [
                  'AI-powered code generation',
                  'Real-time code analysis & suggestions',
                  'Automatic refactoring & optimization',
                  'Multiple language support',
                ],
              },
              {
                icon: Boxes,
                title: 'Developer Platform (Backstage IDP)',
                items: [
                  'Component catalog with discovery',
                  'Service registry management',
                  'CI/CD deployment pipelines',
                  'Team organization & API management',
                ],
              },
              {
                icon: CheckCircle2,
                title: 'Visual Process Design',
                items: [
                  'BPMN 2.0 standard support',
                  'Automatic YAWL conversion',
                  'Real-time diagram editing',
                  'AI-powered suggestions',
                ],
              },
              {
                icon: Zap,
                title: 'Real-Time Collaboration',
                items: [
                  'Live presence tracking',
                  'Team chat integration',
                  'Activity feed monitoring',
                  'Workspace sharing',
                ],
              },
              {
                icon: BarChart3,
                title: 'Advanced Analytics',
                items: [
                  'Performance metrics',
                  'Team utilization tracking',
                  'Process optimization insights',
                  'Exportable reports',
                ],
              },
              {
                icon: Workflow,
                title: 'Case Management',
                items: [
                  'Full lifecycle tracking',
                  'Status management',
                  'Resource allocation',
                  'Historical reporting',
                ],
              },
              {
                icon: Shield,
                title: 'Quantum-Ready Security',
                items: [
                  'Post-quantum cryptography',
                  'Hybrid encryption (RSA + Kyber)',
                  'Digital signatures (Multi-layer)',
                  'Quantum-safe key management',
                ],
              },
            ].map((feature, idx) => {
              const Icon = feature.icon
              return (
                <Card key={idx} className="p-6 md:p-8">
                  <div className="flex items-start gap-4">
                    <div className="p-3 bg-gradient-to-br from-blue-100 to-purple-100 rounded-lg">
                      <Icon className="h-6 w-6 text-blue-600" />
                    </div>
                    <div className="flex-1">
                      <h3 className="text-lg font-semibold text-slate-900 mb-4">
                        {feature.title}
                      </h3>
                      <ul className="space-y-2">
                        {feature.items.map((item, i) => (
                          <li key={i} className="flex gap-2 text-sm text-slate-700">
                            <span className="text-blue-600 font-bold">•</span>
                            {item}
                          </li>
                        ))}
                      </ul>
                    </div>
                  </div>
                </Card>
              )
            })}
          </div>
        </div>
      </div>

      {/* CTA Section */}
      <div className="px-4 py-12 md:py-16 bg-gradient-to-r from-blue-600 to-purple-600">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-2xl md:text-3xl lg:text-4xl font-bold text-white mb-4">
            Ready to Transform Your Workflows?
          </h2>
          <p className="text-blue-100 mb-8 text-sm md:text-base">
            Get started with the Innovation Hub today and experience the future of workflow management
          </p>
          <Link href="/innovation-hub">
            <Button
              size="lg"
              className="bg-white text-blue-600 hover:bg-blue-50 gap-2"
            >
              Launch Innovation Hub
              <ArrowRight className="h-4 w-4" />
            </Button>
          </Link>
        </div>
      </div>

      {/* Footer */}
      <div className="px-4 py-8 md:py-12 border-t bg-white">
        <div className="max-w-6xl mx-auto">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            <div>
              <h3 className="font-semibold text-slate-900 mb-4">Product</h3>
              <ul className="space-y-2 text-sm text-slate-600">
                <li><Link href="/designer" className="hover:text-blue-600">Designer</Link></li>
                <li><Link href="/cases" className="hover:text-blue-600">Cases</Link></li>
                <li><Link href="/analytics" className="hover:text-blue-600">Analytics</Link></li>
                <li><Link href="/collaboration" className="hover:text-blue-600">Collaboration</Link></li>
              </ul>
            </div>
            <div>
              <h3 className="font-semibold text-slate-900 mb-4">Documentation</h3>
              <ul className="space-y-2 text-sm text-slate-600">
                <li><a href="#" className="hover:text-blue-600">Getting Started</a></li>
                <li><a href="#" className="hover:text-blue-600">API Reference</a></li>
                <li><a href="#" className="hover:text-blue-600">Mobile Guide</a></li>
                <li><a href="#" className="hover:text-blue-600">FAQ</a></li>
              </ul>
            </div>
            <div>
              <h3 className="font-semibold text-slate-900 mb-4">Community</h3>
              <ul className="space-y-2 text-sm text-slate-600">
                <li><a href="#" className="hover:text-blue-600">GitHub</a></li>
                <li><a href="#" className="hover:text-blue-600">Discord</a></li>
                <li><a href="#" className="hover:text-blue-600">Forum</a></li>
                <li><a href="#" className="hover:text-blue-600">Issues</a></li>
              </ul>
            </div>
            <div>
              <h3 className="font-semibold text-slate-900 mb-4">Legal</h3>
              <ul className="space-y-2 text-sm text-slate-600">
                <li><a href="#" className="hover:text-blue-600">Privacy</a></li>
                <li><a href="#" className="hover:text-blue-600">Terms</a></li>
                <li><a href="#" className="hover:text-blue-600">License</a></li>
                <li><a href="#" className="hover:text-blue-600">Contact</a></li>
              </ul>
            </div>
          </div>
          <div className="border-t mt-8 pt-8 text-center text-sm text-slate-600">
            <p>&copy; 2024 YAWL Innovation Hub. Built with Next.js, TypeScript, and SPARQL.</p>
          </div>
        </div>
      </div>
    </div>
  )
}
