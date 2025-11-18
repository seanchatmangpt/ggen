import React from "react"
import { Handle, Position, NodeProps } from "reactflow"
import { cn } from "@/lib/utils"

export function JoinNode({ selected }: NodeProps) {
  return (
    <div
      className={cn(
        "flex h-12 w-24 items-center justify-center border-2 bg-purple-100 rounded",
        selected ? "border-purple-600" : "border-purple-300"
      )}
    >
      <span className="text-xs font-semibold text-purple-900">Join</span>
      <Handle type="target" position={Position.Top} />
      <Handle type="target" position={Position.Left} id="left" />
      <Handle type="source" position={Position.Bottom} />
    </div>
  )
}
