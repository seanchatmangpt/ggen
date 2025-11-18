import React from "react"
import { Handle, Position, NodeProps } from "reactflow"
import { cn } from "@/lib/utils"

export function EndNode({ selected }: NodeProps) {
  return (
    <div
      className={cn(
        "flex h-12 w-12 items-center justify-center rounded-full border-2 bg-red-100",
        selected ? "border-red-600" : "border-red-300"
      )}
    >
      <span className="text-xs font-semibold text-red-900">End</span>
      <Handle type="target" position={Position.Top} />
    </div>
  )
}
