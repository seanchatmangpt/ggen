'use client'

import { useForm } from 'react-hook-form'
import { zodResolver } from '@hookform/resolvers/zod'
import { Button } from '@/components/ui/button'
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/components/ui/form'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { Card } from '@/components/ui/card'
import { CreateCaseSchema, type CreateCase } from '@/lib/validation'
import { CaseStatus } from '@/lib/types'

interface CaseFormProps {
  onSubmit: (data: CreateCase) => Promise<void>
  loading?: boolean
}

export function CaseForm({ onSubmit, loading = false }: CaseFormProps) {
  const form = useForm<CreateCase>({
    resolver: zodResolver(CreateCaseSchema),
    defaultValues: {
      processId: '',
      ownerId: '',
    },
  })

  const handleSubmit = form.handleSubmit(async (data) => {
    try {
      await onSubmit(data)
      form.reset()
    } catch (error) {
      console.error('Form submission error:', error)
    }
  })

  return (
    <Card className="p-6 w-full max-w-2xl">
      <Form {...form}>
        <form onSubmit={handleSubmit} className="space-y-6">
          <div className="space-y-2">
            <h3 className="text-lg font-semibold">Create New Case</h3>
            <p className="text-sm text-slate-600">
              Create a new workflow case instance
            </p>
          </div>

          <FormField
            control={form.control}
            name="processId"
            render={({ field }) => (
              <FormItem>
                <FormLabel>Process</FormLabel>
                <Select onValueChange={field.onChange} defaultValue={field.value}>
                  <FormControl>
                    <SelectTrigger>
                      <SelectValue placeholder="Select a process" />
                    </SelectTrigger>
                  </FormControl>
                  <SelectContent>
                    <SelectItem value="proc-001">Loan Application</SelectItem>
                    <SelectItem value="proc-002">Insurance Claim</SelectItem>
                    <SelectItem value="proc-003">Customer Onboarding</SelectItem>
                  </SelectContent>
                </Select>
                <FormDescription>
                  Select the process for this case
                </FormDescription>
                <FormMessage />
              </FormItem>
            )}
          />

          <FormField
            control={form.control}
            name="ownerId"
            render={({ field }) => (
              <FormItem>
                <FormLabel>Owner (Optional)</FormLabel>
                <Select onValueChange={field.onChange} defaultValue={field.value || ''}>
                  <FormControl>
                    <SelectTrigger>
                      <SelectValue placeholder="Select an owner" />
                    </SelectTrigger>
                  </FormControl>
                  <SelectContent>
                    <SelectItem value="user-001">John Smith</SelectItem>
                    <SelectItem value="user-002">Jane Doe</SelectItem>
                    <SelectItem value="user-003">Bob Johnson</SelectItem>
                  </SelectContent>
                </Select>
                <FormDescription>
                  Optionally assign an owner to this case
                </FormDescription>
                <FormMessage />
              </FormItem>
            )}
          />

          <div className="flex gap-3">
            <Button type="submit" disabled={loading}>
              {loading ? 'Creating...' : 'Create Case'}
            </Button>
            <Button
              type="button"
              variant="outline"
              onClick={() => form.reset()}
            >
              Clear
            </Button>
          </div>
        </form>
      </Form>
    </Card>
  )
}
