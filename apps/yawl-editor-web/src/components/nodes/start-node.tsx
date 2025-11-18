import React from "react"
import { Handle, Position, NodeProps } from "reactflow"
import { cn } from "@/lib/utils"

export function StartNode({ selected }: NodeProps) {
  return (
    <div
      className={cn(
        "flex h-12 w-12 items-center justify-center rounded-full border-2 bg-green-100",
        selected ? "border-green-600" : "border-green-300"
      )}
    >
      <span className="text-xs font-semibold text-green-900">Start</span>
      <Handle type="source" position={Position.Bottom} />
    </div>
  )
}
