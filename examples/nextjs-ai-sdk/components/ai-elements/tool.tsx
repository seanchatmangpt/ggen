"use client";

import type { HTMLAttributes, ReactNode } from "react";
import { cn } from "@/lib/utils";

export function Tool({ defaultOpen, className, ...props }: HTMLAttributes<HTMLDetailsElement> & { defaultOpen?: boolean }) { return <details defaultOpen={defaultOpen} className={cn("w-full rounded-lg border", className)} {...props} />; }
export function ToolHeader({ type, state }: { type: string; state: string }) { return <summary className="cursor-pointer px-3 py-2 text-xs font-medium">{type} · {state}</summary>; }
export function ToolContent({ className, ...props }: HTMLAttributes<HTMLDivElement>) { return <div className={cn("space-y-3 border-t p-3", className)} {...props} />; }
export function ToolInput({ input }: { input: unknown }) { return <pre className="overflow-auto rounded bg-muted p-2 text-xs">{JSON.stringify(input, null, 2)}</pre>; }
export function ToolOutput({ output, errorText }: { output?: ReactNode; errorText?: string }) { if (errorText) return <p role="alert" className="text-sm text-destructive">{errorText}</p>; return output ? <div className="rounded bg-muted p-2">{output}</div> : null; }
