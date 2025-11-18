import React, { useState } from "react"
import { Handle, Position, NodeProps } from "reactflow"
import { cn } from "@/lib/utils"

export function ConditionNode({ data, selected }: NodeProps) {
  const [label, setLabel] = useState(data.label)
  const [isEditing, setIsEditing] = useState(false)

  return (
    <div
      className={cn(
        "relative w-24 h-24 flex items-center justify-center transform rotate-45",
        "border-2 bg-yellow-50",
        selected ? "border-yellow-600" : "border-yellow-300"
      )}
    >
      <Handle type="target" position={Position.Top} />
      <div className="transform -rotate-45 text-center">
        {isEditing ? (
          <input
            type="text"
            value={label}
            onChange={(e) => setLabel(e.target.value)}
            onBlur={() => setIsEditing(false)}
            onKeyDown={(e) => {
              if (e.key === "Enter") setIsEditing(false)
            }}
            autoFocus
            className="w-full border border-yellow-400 bg-white px-1 py-0 text-xs font-medium outline-none rounded"
          />
        ) : (
          <div
            onDoubleClick={() => setIsEditing(true)}
            className="cursor-pointer text-xs font-medium text-yellow-900 hover:text-yellow-700"
          >
            {label}
          </div>
        )}
      </div>
      <Handle type="source" position={Position.Bottom} />
      <Handle type="source" position={Position.Right} id="right" />
    </div>
  )
}
