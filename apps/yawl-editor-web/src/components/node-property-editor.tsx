"use client"

import React, { useState, useEffect } from "react"
import { Node } from "reactflow"
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogFooter,
} from "@/components/ui/dialog"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { Textarea } from "@/components/ui/textarea"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Badge } from "@/components/ui/badge"

interface NodePropertyEditorProps {
  node: Node | null
  isOpen: boolean
  onClose: () => void
  onSave: (nodeId: string, data: any) => void
}

export function NodePropertyEditor({
  node,
  isOpen,
  onClose,
  onSave,
}: NodePropertyEditorProps) {
  const [label, setLabel] = useState("")
  const [description, setDescription] = useState("")
  const [customProperties, setCustomProperties] = useState<
    Record<string, string>
  >({})
  const [newPropKey, setNewPropKey] = useState("")
  const [newPropValue, setNewPropValue] = useState("")

  useEffect(() => {
    if (node) {
      setLabel(node.data?.label || "")
      setDescription(node.data?.description || "")
      setCustomProperties(node.data?.properties || {})
    }
  }, [node])

  const handleSave = () => {
    if (node) {
      onSave(node.id, {
        label,
        description,
        properties: customProperties,
      })
      onClose()
    }
  }

  const addProperty = () => {
    if (newPropKey && newPropValue) {
      setCustomProperties((prev) => ({
        ...prev,
        [newPropKey]: newPropValue,
      }))
      setNewPropKey("")
      setNewPropValue("")
    }
  }

  const removeProperty = (key: string) => {
    setCustomProperties((prev) => {
      const updated = { ...prev }
      delete updated[key]
      return updated
    })
  }

  return (
    <Dialog open={isOpen} onOpenChange={onClose}>
      <DialogContent className="max-w-md">
        <DialogHeader>
          <DialogTitle>Edit Node Properties</DialogTitle>
          <DialogDescription>
            {node && (
              <div className="flex items-center gap-2 mt-2">
                <span>Node ID: {node.id}</span>
                <Badge variant="secondary" className="capitalize">
                  {node.type}
                </Badge>
              </div>
            )}
          </DialogDescription>
        </DialogHeader>

        {node && (
          <Tabs defaultValue="basic" className="w-full">
            <TabsList className="grid w-full grid-cols-2">
              <TabsTrigger value="basic">Basic</TabsTrigger>
              <TabsTrigger value="advanced">Advanced</TabsTrigger>
            </TabsList>

            <TabsContent value="basic" className="space-y-4">
              <div className="space-y-2">
                <Label htmlFor="label">Label</Label>
                <Input
                  id="label"
                  value={label}
                  onChange={(e) => setLabel(e.target.value)}
                  placeholder="Node label"
                />
              </div>

              <div className="space-y-2">
                <Label htmlFor="description">Description</Label>
                <Textarea
                  id="description"
                  value={description}
                  onChange={(e) => setDescription(e.target.value)}
                  placeholder="Node description"
                  className="resize-none"
                  rows={3}
                />
              </div>
            </TabsContent>

            <TabsContent value="advanced" className="space-y-4">
              <div className="space-y-3">
                <h4 className="font-semibold text-sm">Custom Properties</h4>

                <div className="space-y-2">
                  {Object.entries(customProperties).map(([key, value]) => (
                    <div
                      key={key}
                      className="flex items-center gap-2 p-2 bg-muted rounded"
                    >
                      <div className="flex-1">
                        <div className="text-xs font-semibold">{key}</div>
                        <div className="text-xs text-muted-foreground">
                          {value}
                        </div>
                      </div>
                      <Button
                        size="sm"
                        variant="ghost"
                        onClick={() => removeProperty(key)}
                      >
                        ✕
                      </Button>
                    </div>
                  ))}
                </div>

                <div className="space-y-2 pt-2 border-t">
                  <Label htmlFor="prop-key">Property Key</Label>
                  <Input
                    id="prop-key"
                    value={newPropKey}
                    onChange={(e) => setNewPropKey(e.target.value)}
                    placeholder="e.g., maxRetries"
                    size={1}
                  />

                  <Label htmlFor="prop-value">Property Value</Label>
                  <Input
                    id="prop-value"
                    value={newPropValue}
                    onChange={(e) => setNewPropValue(e.target.value)}
                    placeholder="e.g., 3"
                    size={1}
                  />

                  <Button
                    size="sm"
                    onClick={addProperty}
                    className="w-full"
                    variant="outline"
                  >
                    Add Property
                  </Button>
                </div>
              </div>
            </TabsContent>
          </Tabs>
        )}

        <DialogFooter>
          <Button variant="outline" onClick={onClose}>
            Cancel
          </Button>
          <Button onClick={handleSave}>Save Changes</Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
}
