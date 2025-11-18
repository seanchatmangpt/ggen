"use client"

import React from "react"
import { Button } from "@/components/ui/button"
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
  DropdownMenuSeparator,
} from "@/components/ui/dropdown-menu"
import {
  Undo2,
  Redo2,
  Download,
  Upload,
  Trash2,
  Menu,
  Copy,
  Clipboard,
  ZoomIn,
  ZoomOut,
  Focus,
} from "lucide-react"

interface EditorToolbarProps {
  canUndo: boolean
  canRedo: boolean
  hasSelection: boolean
  isDirty: boolean
  onUndo: () => void
  onRedo: () => void
  onExport: () => void
  onImport: () => void
  onDelete: () => void
  onCopy: () => void
  onPaste: () => void
  onZoomIn: () => void
  onZoomOut: () => void
  onFitView: () => void
}

export function EditorToolbar({
  canUndo,
  canRedo,
  hasSelection,
  isDirty,
  onUndo,
  onRedo,
  onExport,
  onImport,
  onDelete,
  onCopy,
  onPaste,
  onZoomIn,
  onZoomOut,
  onFitView,
}: EditorToolbarProps) {
  return (
    <div className="flex items-center gap-2 p-2 bg-card border-b border-border">
      {/* Undo/Redo */}
      <div className="flex items-center gap-1">
        <Button
          size="icon"
          variant="ghost"
          onClick={onUndo}
          disabled={!canUndo}
          title="Undo (Ctrl+Z)"
        >
          <Undo2 className="h-4 w-4" />
        </Button>
        <Button
          size="icon"
          variant="ghost"
          onClick={onRedo}
          disabled={!canRedo}
          title="Redo (Ctrl+Y)"
        >
          <Redo2 className="h-4 w-4" />
        </Button>
      </div>

      <div className="w-px h-6 bg-border" />

      {/* Zoom Controls */}
      <div className="flex items-center gap-1">
        <Button
          size="icon"
          variant="ghost"
          onClick={onZoomIn}
          title="Zoom In (Ctrl+Plus)"
        >
          <ZoomIn className="h-4 w-4" />
        </Button>
        <Button
          size="icon"
          variant="ghost"
          onClick={onZoomOut}
          title="Zoom Out (Ctrl+Minus)"
        >
          <ZoomOut className="h-4 w-4" />
        </Button>
        <Button
          size="icon"
          variant="ghost"
          onClick={onFitView}
          title="Fit View (Ctrl+0)"
        >
          <Focus className="h-4 w-4" />
        </Button>
      </div>

      <div className="w-px h-6 bg-border" />

      {/* Clipboard Operations */}
      <div className="flex items-center gap-1">
        <Button
          size="icon"
          variant="ghost"
          onClick={onCopy}
          disabled={!hasSelection}
          title="Copy (Ctrl+C)"
        >
          <Copy className="h-4 w-4" />
        </Button>
        <Button
          size="icon"
          variant="ghost"
          onClick={onPaste}
          title="Paste (Ctrl+V)"
        >
          <Clipboard className="h-4 w-4" />
        </Button>
      </div>

      <div className="w-px h-6 bg-border" />

      {/* File Operations */}
      <div className="flex items-center gap-1">
        <Button
          size="icon"
          variant="ghost"
          onClick={onExport}
          title="Export Workflow"
        >
          <Download className="h-4 w-4" />
        </Button>
        <Button
          size="icon"
          variant="ghost"
          onClick={onImport}
          title="Import Workflow"
        >
          <Upload className="h-4 w-4" />
        </Button>
      </div>

      <div className="w-px h-6 bg-border" />

      {/* Edit Operations */}
      <Button
        size="icon"
        variant="ghost"
        onClick={onDelete}
        disabled={!hasSelection}
        className="text-destructive hover:text-destructive hover:bg-destructive/10"
        title="Delete Selected (Delete)"
      >
        <Trash2 className="h-4 w-4" />
      </Button>

      <div className="ml-auto" />

      {/* Status */}
      {isDirty && (
        <div className="text-xs text-muted-foreground pr-2">
          <span className="inline-block w-2 h-2 bg-yellow-500 rounded-full mr-2" />
          Unsaved changes
        </div>
      )}

      {/* Menu */}
      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <Button size="icon" variant="ghost">
            <Menu className="h-4 w-4" />
          </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="end">
          <DropdownMenuItem onClick={onExport}>
            <Download className="mr-2 h-4 w-4" />
            Export as JSON
          </DropdownMenuItem>
          <DropdownMenuItem onClick={onImport}>
            <Upload className="mr-2 h-4 w-4" />
            Import from JSON
          </DropdownMenuItem>
          <DropdownMenuSeparator />
          <DropdownMenuItem onClick={onDelete} disabled={!hasSelection}>
            <Trash2 className="mr-2 h-4 w-4" />
            Delete Selection
          </DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>
    </div>
  )
}
