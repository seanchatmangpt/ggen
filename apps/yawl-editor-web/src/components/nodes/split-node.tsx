import React from "react"
import { Handle, Position, NodeProps } from "reactflow"
import { cn } from "@/lib/utils"

export function SplitNode({ selected }: NodeProps) {
  return (
    <div
      className={cn(
        "flex h-12 w-24 items-center justify-center border-2 bg-cyan-100 rounded",
        selected ? "border-cyan-600" : "border-cyan-300"
      )}
    >
      <span className="text-xs font-semibold text-cyan-900">Split</span>
      <Handle type="target" position={Position.Top} />
      <Handle type="source" position={Position.Bottom} />
      <Handle type="source" position={Position.Right} id="right" />
    </div>
  )
}
