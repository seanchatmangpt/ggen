'use client'

import React, { useState } from 'react'
import { Fingerprint, Eye, Mic, Plus, CheckCircle, AlertCircle } from 'lucide-react'
import { BiometricEnrollment, BiometricVerification } from '@/types'
import toast from 'react-hot-toast'

export const BiometricManager: React.FC = () => {
  const [enrollments, setEnrollments] = useState<BiometricEnrollment[]>([
    {
      id: 'bio-enroll-001',
      userId: 'user-001',
      biometricType: 'Fingerprint',
      templateHash: 'hash_fp_001',
      status: 'Complete',
      qualityScore: 98,
      livenessDetected: true,
      createdAt: new Date().toISOString(),
    },
    {
      id: 'bio-enroll-002',
      userId: 'user-001',
      biometricType: 'FacialRecognition',
      templateHash: 'hash_face_001',
      status: 'Complete',
      qualityScore: 94,
      livenessDetected: true,
      createdAt: new Date().toISOString(),
    },
  ])

  const [verifications, setVerifications] = useState<BiometricVerification[]>([
    {
      id: 'bio-ver-001',
      enrollmentId: 'bio-enroll-001',
      matchScore: 99.2,
      livenessScore: 98.5,
      spoofingRisk: 'Low',
      isMatched: true,
      verifiedAt: new Date().toISOString(),
    },
  ])

  const [selectedBiometric, setSelectedBiometric] = useState<BiometricEnrollment | null>(enrollments[0])
  const [showEnroll, setShowEnroll] = useState(false)
  const [biometricType, setBiometricType] = useState<'Fingerprint' | 'FacialRecognition' | 'VoiceRecognition'>('Fingerprint')
  const [enrollmentStatus, setEnrollmentStatus] = useState<'idle' | 'enrolling' | 'complete'>('idle')

  const handleStartEnrollment = () => {
    setEnrollmentStatus('enrolling')
    // Simulate enrollment process
    setTimeout(() => {
      const newEnrollment: BiometricEnrollment = {
        id: `bio-enroll-${Date.now()}`,
        userId: 'user-current',
        biometricType,
        templateHash: `hash_${biometricType.toLowerCase()}_${Date.now()}`,
        status: 'Complete',
        qualityScore: Math.floor(Math.random() * 15) + 85,
        livenessDetected: true,
        createdAt: new Date().toISOString(),
      }
      setEnrollments([...enrollments, newEnrollment])
      setSelectedBiometric(newEnrollment)
      setEnrollmentStatus('complete')
      toast.success(`${biometricType} biometric enrolled successfully`)
      setTimeout(() => setShowEnroll(false), 2000)
    }, 3000)
  }

  const handleVerify = (enrollmentId: string) => {
    const newVerification: BiometricVerification = {
      id: `bio-ver-${Date.now()}`,
      enrollmentId,
      matchScore: Math.floor(Math.random() * 10) + 90,
      livenessScore: Math.floor(Math.random() * 8) + 90,
      spoofingRisk: 'Low',
      isMatched: true,
      verifiedAt: new Date().toISOString(),
    }
    setVerifications([...verifications, newVerification])
    toast.success('Biometric verification successful')
  }

  const biometricIcons: Record<string, React.ReactNode> = {
    Fingerprint: <Fingerprint size={20} />,
    FacialRecognition: <Eye size={20} />,
    VoiceRecognition: <Mic size={20} />,
    IrisRecognition: <Eye size={20} />,
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold text-slate-900">Biometric Authentication</h2>
        <button
          onClick={() => setShowEnroll(true)}
          className="btn btn-primary gap-2"
        >
          <Plus size={20} />
          Enroll Biometric
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Enrollment List */}
        <div className="lg:col-span-1">
          <div className="card space-y-2 max-h-[500px] overflow-y-auto">
            <h3 className="font-bold text-slate-900 mb-4">Biometric Enrollments</h3>
            {enrollments.map((enrollment) => (
              <div
                key={enrollment.id}
                onClick={() => setSelectedBiometric(enrollment)}
                className={`p-3 rounded-lg cursor-pointer transition-all ${
                  selectedBiometric?.id === enrollment.id
                    ? 'bg-blue-50 border border-blue-300'
                    : 'bg-slate-50 hover:bg-slate-100'
                }`}
              >
                <div className="flex items-center gap-2 mb-1">
                  <span className="text-slate-600">
                    {biometricIcons[enrollment.biometricType] || '●'}
                  </span>
                  <span className="font-medium text-slate-900 text-sm">{enrollment.biometricType}</span>
                </div>
                <div className="flex items-center gap-2 text-xs">
                  {enrollment.status === 'Complete' ? (
                    <CheckCircle size={12} className="text-green-600" />
                  ) : (
                    <AlertCircle size={12} className="text-yellow-600" />
                  )}
                  <span className="text-slate-600">{enrollment.status}</span>
                </div>
                <div className="text-xs text-slate-500 mt-1">Quality: {enrollment.qualityScore}%</div>
              </div>
            ))}
          </div>
        </div>

        {/* Enrollment Details */}
        <div className="lg:col-span-2 space-y-4">
          {selectedBiometric ? (
            <>
              <div className="card">
                <div className="flex items-center justify-between mb-4">
                  <h3 className="text-xl font-bold text-slate-900 flex items-center gap-2">
                    {biometricIcons[selectedBiometric.biometricType]}
                    {selectedBiometric.biometricType}
                  </h3>
                  <span
                    className={`px-3 py-1 rounded-full text-xs font-medium ${
                      selectedBiometric.status === 'Complete'
                        ? 'bg-green-100 text-green-800'
                        : 'bg-yellow-100 text-yellow-800'
                    }`}
                  >
                    {selectedBiometric.status}
                  </span>
                </div>

                <div className="space-y-3">
                  <div>
                    <label className="text-sm font-medium text-slate-700">Quality Score</label>
                    <div className="mt-1 flex items-center gap-2">
                      <div className="flex-1 bg-slate-200 rounded-full h-2">
                        <div
                          className={`h-2 rounded-full ${
                            selectedBiometric.qualityScore >= 90
                              ? 'bg-green-600'
                              : selectedBiometric.qualityScore >= 80
                              ? 'bg-yellow-600'
                              : 'bg-red-600'
                          }`}
                          style={{ width: `${selectedBiometric.qualityScore}%` }}
                        />
                      </div>
                      <span className="text-sm font-bold text-slate-900">
                        {selectedBiometric.qualityScore}%
                      </span>
                    </div>
                  </div>

                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <label className="text-sm font-medium text-slate-700">Liveness Detected</label>
                      <div className="mt-1 text-sm">
                        {selectedBiometric.livenessDetected ? (
                          <span className="text-green-600 font-semibold">✓ Yes</span>
                        ) : (
                          <span className="text-red-600 font-semibold">✗ No</span>
                        )}
                      </div>
                    </div>
                    <div>
                      <label className="text-sm font-medium text-slate-700">Anti-Spoofing</label>
                      <div className="mt-1 text-sm">
                        <span className="bg-green-100 text-green-800 px-2 py-1 rounded text-xs font-semibold">
                          Verified
                        </span>
                      </div>
                    </div>
                  </div>
                </div>

                <div className="mt-4 flex gap-2">
                  <button
                    onClick={() => handleVerify(selectedBiometric.id)}
                    className="btn btn-primary flex-1"
                  >
                    Verify Now
                  </button>
                  <button className="btn btn-secondary flex-1">Re-enroll</button>
                </div>
              </div>

              {/* Recent Verifications */}
              <div className="card">
                <h3 className="font-bold text-slate-900 mb-3">Recent Verifications</h3>
                <div className="space-y-2 max-h-[250px] overflow-y-auto">
                  {verifications
                    .filter((v) => v.enrollmentId === selectedBiometric.id)
                    .slice(0, 5)
                    .map((verification) => (
                      <div
                        key={verification.id}
                        className={`p-3 rounded-lg ${
                          verification.isMatched
                            ? 'bg-green-50 border border-green-200'
                            : 'bg-red-50 border border-red-200'
                        }`}
                      >
                        <div className="flex items-center justify-between">
                          <div>
                            <div className="text-sm font-semibold text-slate-900">
                              Match Score:{' '}
                              <span className={verification.isMatched ? 'text-green-600' : 'text-red-600'}>
                                {verification.matchScore.toFixed(1)}%
                              </span>
                            </div>
                            <div className="text-xs text-slate-600 mt-1">
                              Liveness: {verification.livenessScore.toFixed(1)}% | Risk: {verification.spoofingRisk}
                            </div>
                          </div>
                          {verification.isMatched ? (
                            <CheckCircle size={20} className="text-green-600" />
                          ) : (
                            <AlertCircle size={20} className="text-red-600" />
                          )}
                        </div>
                      </div>
                    ))}
                </div>
              </div>
            </>
          ) : (
            <div className="card h-[400px] flex items-center justify-center">
              <p className="text-slate-500">Select a biometric enrollment to view details</p>
            </div>
          )}
        </div>
      </div>

      {/* Enrollment Modal */}
      {showEnroll && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
          <div className="bg-white rounded-lg shadow-lg max-w-md w-full p-6 space-y-4">
            <h3 className="text-xl font-bold text-slate-900">Enroll Biometric</h3>

            {enrollmentStatus === 'idle' ? (
              <>
                <div>
                  <label className="text-sm font-medium text-slate-700">Biometric Type</label>
                  <select
                    value={biometricType}
                    onChange={(e) => setBiometricType(e.target.value as any)}
                    className="input mt-1"
                  >
                    <option value="Fingerprint">Fingerprint (Recommended)</option>
                    <option value="FacialRecognition">Facial Recognition</option>
                    <option value="VoiceRecognition">Voice Recognition</option>
                  </select>
                </div>

                <div className="bg-blue-50 border border-blue-200 rounded-lg p-3 text-sm text-blue-900">
                  <strong>Note:</strong> Ensure proper lighting and positioning for best results. The enrollment
                  process will verify liveness detection and anti-spoofing measures.
                </div>

                <div className="flex gap-2 pt-4">
                  <button
                    onClick={handleStartEnrollment}
                    className="btn btn-primary flex-1"
                  >
                    Start Enrollment
                  </button>
                  <button
                    onClick={() => setShowEnroll(false)}
                    className="btn btn-secondary flex-1"
                  >
                    Cancel
                  </button>
                </div>
              </>
            ) : enrollmentStatus === 'enrolling' ? (
              <div className="text-center space-y-4 py-8">
                <div className="inline-block">
                  <div className="w-16 h-16 border-4 border-blue-200 border-t-blue-600 rounded-full animate-spin" />
                </div>
                <p className="text-slate-600">Enrolling {biometricType}...</p>
                <p className="text-xs text-slate-500">Verifying liveness and quality...</p>
              </div>
            ) : (
              <div className="text-center space-y-4 py-8">
                <div className="inline-block bg-green-100 rounded-full p-3">
                  <CheckCircle size={32} className="text-green-600" />
                </div>
                <p className="text-lg font-semibold text-slate-900">Enrollment Complete!</p>
                <p className="text-sm text-slate-600">Your biometric has been securely enrolled and is ready for use.</p>
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  )
}
