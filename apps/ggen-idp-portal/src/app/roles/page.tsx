'use client'

import React, { useState } from 'react'
import { Plus, Edit2, Trash2, Save } from 'lucide-react'
import { Role } from '@/types'
import { CodeEditor } from '@/components/editor/CodeEditor'

const mockRoles: Role[] = [
  {
    id: 'admin',
    name: 'Admin',
    description: 'Full administrative access',
    organizationId: '',
    permissions: [
      { resource: 'organization', action: 'admin' },
      { resource: 'pack', action: 'publish' },
      { resource: 'user', action: 'manage' },
    ],
    createdAt: new Date().toISOString(),
  },
  {
    id: 'editor',
    name: 'Editor',
    description: 'Can edit and publish packs',
    organizationId: '',
    permissions: [
      { resource: 'pack', action: 'write' },
      { resource: 'pack', action: 'publish' },
    ],
    createdAt: new Date().toISOString(),
  },
]

export default function RolesPage() {
  const [roles, setRoles] = useState<Role[]>(mockRoles)
  const [selectedRole, setSelectedRole] = useState<Role | null>(null)
  const [isEditing, setIsEditing] = useState(false)
  const [editorContent, setEditorContent] = useState('')

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-slate-900">RBAC Roles</h1>
          <p className="text-slate-600 mt-1">
            Manage roles with Relation-Based Access Control (ReBAC)
          </p>
        </div>
        <button className="btn btn-primary gap-2">
          <Plus size={20} />
          New Role
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-4 gap-6">
        {/* Role List */}
        <div className="lg:col-span-1">
          <div className="card space-y-2 max-h-[600px] overflow-y-auto">
            <h2 className="font-bold text-slate-900 mb-4">Roles</h2>
            {roles.map((role) => (
              <div
                key={role.id}
                onClick={() => {
                  setSelectedRole(role)
                  setEditorContent(roleToYaml(role))
                }}
                className={`p-3 rounded-lg cursor-pointer transition-all ${
                  selectedRole?.id === role.id
                    ? 'bg-blue-50 border border-blue-300'
                    : 'bg-slate-50 hover:bg-slate-100'
                }`}
              >
                <div className="font-medium text-slate-900 text-sm">{role.name}</div>
                <div className="text-xs text-slate-500 truncate">{role.description}</div>
                <div className="text-xs text-slate-600 mt-1">{role.permissions.length} permissions</div>
              </div>
            ))}
          </div>
        </div>

        {/* Editor */}
        <div className="lg:col-span-3 space-y-4">
          {selectedRole ? (
            <>
              <CodeEditor
                value={editorContent}
                language="yaml"
                onChange={setEditorContent}
                height="500px"
                showSuggestions={true}
              />
              <div className="flex gap-2">
                <button className="btn btn-primary gap-2">
                  <Save size={16} />
                  Save
                </button>
                <button className="btn btn-secondary">Cancel</button>
              </div>
            </>
          ) : (
            <div className="card h-[500px] flex items-center justify-center">
              <p className="text-slate-500">Select a role or create a new one</p>
            </div>
          )}
        </div>
      </div>

      {/* Role Details Table */}
      <div className="card">
        <h2 className="text-xl font-bold mb-4">Role Permissions</h2>
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="border-b border-slate-200">
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Resource</th>
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Action</th>
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Constraints</th>
                <th className="text-right px-4 py-2 text-sm font-semibold text-slate-700">Actions</th>
              </tr>
            </thead>
            <tbody>
              {selectedRole?.permissions.map((perm, idx) => (
                <tr key={idx} className="border-b border-slate-200 hover:bg-slate-50">
                  <td className="px-4 py-2 text-sm text-slate-900">{perm.resource}</td>
                  <td className="px-4 py-2 text-sm text-slate-900">{perm.action}</td>
                  <td className="px-4 py-2 text-sm text-slate-600">
                    {perm.constraints ? (
                      <span className="text-xs bg-slate-100 px-2 py-1 rounded">
                        {perm.constraints.ownerOnly ? 'Owner only' : 'Org level'}
                      </span>
                    ) : (
                      <span className="text-slate-400">None</span>
                    )}
                  </td>
                  <td className="px-4 py-2 text-right">
                    <button className="text-blue-600 hover:text-blue-700 text-sm">Edit</button>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}

function roleToYaml(role: Role): string {
  return `id: ${role.id}
name: ${role.name}
description: ${role.description || ''}
permissions:
${role.permissions.map((p) => `  - resource: ${p.resource}
    action: ${p.action}`).join('\n')}
`
}
