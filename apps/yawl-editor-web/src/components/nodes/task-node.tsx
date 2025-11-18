import React, { useState } from "react"
import { Handle, Position, NodeProps } from "reactflow"
import { cn } from "@/lib/utils"

export function TaskNode({ data, selected }: NodeProps) {
  const [label, setLabel] = useState(data.label)
  const [isEditing, setIsEditing] = useState(false)

  return (
    <div
      className={cn(
        "rounded-lg border-2 bg-blue-50 px-4 py-2 min-w-[100px]",
        selected ? "border-blue-600" : "border-blue-200"
      )}
    >
      <Handle type="target" position={Position.Top} />
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
          className="w-full border border-blue-400 bg-white px-1 py-0 text-sm font-medium outline-none"
        />
      ) : (
        <div
          onDoubleClick={() => setIsEditing(true)}
          className="cursor-pointer text-center text-sm font-medium text-blue-900 hover:text-blue-700"
        >
          {label}
        </div>
      )}
      <Handle type="source" position={Position.Bottom} />
    </div>
  )
}
