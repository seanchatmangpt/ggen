'use client'

import { useState } from 'react'
import { useServices } from '@/hooks/use-backstage'
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import {
  Plus,
  ExternalLink,
  GitBranch,
  Users,
  AlertCircle,
  CheckCircle2,
} from 'lucide-react'
import type { ServiceEntity } from '@/lib/backstage-types'

interface ServiceRegistryProps {
  onCreateService?: () => void
}

/**
 * Service Registry
 * View and manage all registered services and APIs
 */
export function ServiceRegistry({ onCreateService }: ServiceRegistryProps) {
  const { services, loading } = useServices()
  const [selectedService, setSelectedService] = useState<ServiceEntity | null>(
    null
  )

  const lifecycleColors = {
    production: 'bg-green-100 text-green-800',
    staging: 'bg-yellow-100 text-yellow-800',
    development: 'bg-blue-100 text-blue-800',
  }

  if (loading) {
    return (
      <Card>
        <CardContent className="pt-6">
          <p className="text-slate-600">Loading services...</p>
        </CardContent>
      </Card>
    )
  }

  return (
    <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
      {/* Services List */}
      <div className="lg:col-span-2 space-y-4">
        <div className="flex items-center justify-between">
          <h3 className="text-lg font-semibold">Services ({services.length})</h3>
          <Button
            size="sm"
            className="gap-2"
            onClick={onCreateService}
          >
            <Plus className="h-4 w-4" />
            Register Service
          </Button>
        </div>

        <div className="space-y-3">
          {services.map((service) => (
            <Card
              key={service.id}
              className={`cursor-pointer hover:shadow-md transition-shadow ${
                selectedService?.id === service.id ? 'ring-2 ring-blue-500' : ''
              }`}
              onClick={() => setSelectedService(service)}
            >
              <CardHeader className="pb-3">
                <div className="flex items-start justify-between">
                  <div className="flex-1">
                    <div className="flex items-center gap-2 mb-1">
                      <CardTitle className="text-base">
                        {service.name}
                      </CardTitle>
                      <Badge
                        className={
                          lifecycleColors[
                            service.spec
                              .lifecycle as keyof typeof lifecycleColors
                          ]
                        }
                      >
                        {service.spec.lifecycle}
                      </Badge>
                    </div>
                    <CardDescription className="text-xs">
                      {service.namespace} • Owner: {service.spec.owner}
                    </CardDescription>
                  </div>
                  <CheckCircle2 className="h-5 w-5 text-green-600" />
                </div>
              </CardHeader>
              <CardContent>
                <p className="text-sm text-slate-700 mb-3">
                  {service.description}
                </p>
                <div className="flex gap-2 text-xs">
                  {service.spec.providesApis && (
                    <span className="text-slate-600">
                      Provides {service.spec.providesApis.length} APIs
                    </span>
                  )}
                  {service.spec.consumesApis && (
                    <span className="text-slate-600">
                      Consumes {service.spec.consumesApis.length} APIs
                    </span>
                  )}
                </div>
              </CardContent>
            </Card>
          ))}
        </div>

        {services.length === 0 && (
          <Card className="text-center py-8">
            <AlertCircle className="h-8 w-8 text-slate-600 mx-auto mb-2" />
            <p className="text-slate-600">No services registered yet</p>
            <Button size="sm" className="mt-4" onClick={onCreateService}>
              Register First Service
            </Button>
          </Card>
        )}
      </div>

      {/* Service Details */}
      {selectedService && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">{selectedService.name}</CardTitle>
          </CardHeader>
          <CardContent className="space-y-4">
            {/* Service Info */}
            <div>
              <p className="text-xs font-semibold text-slate-600 mb-1">
                Service Type
              </p>
              <p className="text-sm font-medium">{selectedService.spec.type}</p>
            </div>

            <div>
              <p className="text-xs font-semibold text-slate-600 mb-1">
                Owner
              </p>
              <div className="flex items-center gap-1 text-sm">
                <Users className="h-4 w-4" />
                <span>{selectedService.spec.owner}</span>
              </div>
            </div>

            {selectedService.spec.domain && (
              <div>
                <p className="text-xs font-semibold text-slate-600 mb-1">
                  Domain
                </p>
                <p className="text-sm">{selectedService.spec.domain}</p>
              </div>
            )}

            {/* APIs */}
            {selectedService.spec.providesApis &&
              selectedService.spec.providesApis.length > 0 && (
                <div className="border-t pt-3">
                  <p className="text-xs font-semibold text-slate-600 mb-2">
                    Provides APIs
                  </p>
                  <div className="space-y-1">
                    {selectedService.spec.providesApis.map((api) => (
                      <div
                        key={api}
                        className="text-xs p-2 bg-slate-50 rounded flex items-center justify-between"
                      >
                        <span>{api}</span>
                        <ExternalLink className="h-3 w-3 text-slate-600" />
                      </div>
                    ))}
                  </div>
                </div>
              )}

            {/* Actions */}
            <div className="flex gap-2 pt-3 border-t">
              <Button size="sm" variant="outline" className="flex-1">
                View
              </Button>
              <Button size="sm" variant="outline" className="flex-1">
                Edit
              </Button>
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  )
}
