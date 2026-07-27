"use client";

import type { HTMLAttributes } from "react";
import { Button, type ButtonProps } from "@/components/ui/button";
import { cn } from "@/lib/utils";

export function Confirmation({ className, approval: _approval, state: _state, ...props }: HTMLAttributes<HTMLDivElement> & { approval?: unknown; state?: string }) { return <div role="group" aria-label="Tool approval" className={cn("space-y-3 rounded-lg border border-amber-500/40 bg-amber-500/5 p-3", className)} {...props} />; }
export function ConfirmationTitle({ className, ...props }: HTMLAttributes<HTMLHeadingElement>) { return <h3 className={cn("font-medium", className)} {...props} />; }
export function ConfirmationRequest({ className, ...props }: HTMLAttributes<HTMLPreElement>) { return <pre className={cn("overflow-auto text-xs", className)} {...props} />; }
export function ConfirmationActions({ className, ...props }: HTMLAttributes<HTMLDivElement>) { return <div className={cn("flex justify-end gap-2", className)} {...props} />; }
export function ConfirmationAction(props: ButtonProps) { return <Button type="button" {...props} />; }
