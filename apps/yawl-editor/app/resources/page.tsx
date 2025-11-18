'use client'

import { useState } from 'react'
import { Card } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import type { User, Role } from '@/lib/types'

const mockUsers: User[] = [
  {
    id: 'user-001',
    name: 'John Smith',
    email: 'john.smith@example.com',
    type: 'User',
    roles: [
      { id: 'role-001', name: 'Loan Officer', type: 'Role', email: undefined },
    ],
  },
  {
    id: 'user-002',
    name: 'Jane Doe',
    email: 'jane.doe@example.com',
    type: 'User',
    roles: [
      { id: 'role-002', name: 'Document Reviewer', type: 'Role', email: undefined },
    ],
  },
  {
    id: 'user-003',
    name: 'Bob Johnson',
    email: 'bob.johnson@example.com',
    type: 'User',
    roles: [
      { id: 'role-001', name: 'Loan Officer', type: 'Role', email: undefined },
      { id: 'role-003', name: 'Manager', type: 'Role', email: undefined },
    ],
  },
]

const mockRoles: Role[] = [
  {
    id: 'role-001',
    name: 'Loan Officer',
    type: 'Role',
    description: 'Responsible for loan processing and approvals',
  },
  {
    id: 'role-002',
    name: 'Document Reviewer',
    type: 'Role',
    description: 'Reviews and validates submitted documents',
  },
  {
    id: 'role-003',
    name: 'Manager',
    type: 'Role',
    description: 'Manages team and approves final decisions',
  },
]

export default function ResourcesPage() {
  const [tab, setTab] = useState<'users' | 'roles'>('users')
  const [users, setUsers] = useState<User[]>(mockUsers)
  const [roles, setRoles] = useState<Role[]>(mockRoles)

  return (
    <div className="space-y-6">
      <div className="space-y-2">
        <h1 className="text-3xl font-bold">Resources</h1>
        <p className="text-slate-600">Manage users, roles, and resource allocation</p>
      </div>

      <div className="flex gap-2">
        <Button
          variant={tab === 'users' ? 'default' : 'outline'}
          onClick={() => setTab('users')}
        >
          Users
        </Button>
        <Button
          variant={tab === 'roles' ? 'default' : 'outline'}
          onClick={() => setTab('roles')}
        >
          Roles
        </Button>
      </div>

      {tab === 'users' && (
        <div className="space-y-4">
          <div className="flex gap-2">
            <Button>Create New User</Button>
            <Button variant="outline">Refresh</Button>
          </div>

          <Card className="p-6">
            <div className="overflow-x-auto">
              <table className="w-full text-sm">
                <thead>
                  <tr className="border-b">
                    <th className="text-left py-3 px-4 font-semibold text-slate-700">Name</th>
                    <th className="text-left py-3 px-4 font-semibold text-slate-700">Email</th>
                    <th className="text-left py-3 px-4 font-semibold text-slate-700">Roles</th>
                    <th className="text-left py-3 px-4 font-semibold text-slate-700">Actions</th>
                  </tr>
                </thead>
                <tbody>
                  {users.map((user) => (
                    <tr key={user.id} className="border-b hover:bg-slate-50">
                      <td className="py-3 px-4">{user.name}</td>
                      <td className="py-3 px-4 text-xs">{user.email || '-'}</td>
                      <td className="py-3 px-4">
                        <div className="flex gap-1">
                          {user.roles?.map((role) => (
                            <Badge key={role.id} variant="secondary">
                              {role.name}
                            </Badge>
                          ))}
                        </div>
                      </td>
                      <td className="py-3 px-4">
                        <Button variant="ghost" size="sm">Edit</Button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </Card>

          <div className="text-sm text-slate-500">
            Showing {users.length} users. Data will load from SPARQL endpoint when configured.
          </div>
        </div>
      )}

      {tab === 'roles' && (
        <div className="space-y-4">
          <div className="flex gap-2">
            <Button>Create New Role</Button>
            <Button variant="outline">Refresh</Button>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {roles.map((role) => (
              <Card key={role.id} className="p-6">
                <div className="space-y-3">
                  <div>
                    <h3 className="text-lg font-semibold">{role.name}</h3>
                    <p className="text-sm text-slate-600">{role.description}</p>
                  </div>
                  <div className="flex gap-2">
                    <Button variant="ghost" size="sm">Edit</Button>
                    <Button variant="ghost" size="sm">Delete</Button>
                  </div>
                </div>
              </Card>
            ))}
          </div>

          <div className="text-sm text-slate-500">
            Showing {roles.length} roles. Data will load from SPARQL endpoint when configured.
          </div>
        </div>
      )}
    </div>
  )
}
