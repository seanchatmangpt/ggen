"use client";

import { createContext, useCallback, useContext, useEffect, useMemo, useRef, useState, type HTMLAttributes } from "react";
import { ArrowDownIcon } from "lucide-react";
import { Button, type ButtonProps } from "@/components/ui/button";
import { cn } from "@/lib/utils";

type ConversationContextValue = { isAtBottom: boolean; scrollToBottom: () => void };
const ConversationContext = createContext<ConversationContextValue | null>(null);

export function Conversation({ className, children, ...props }: HTMLAttributes<HTMLDivElement>) {
  const ref = useRef<HTMLDivElement>(null);
  const [isAtBottom, setIsAtBottom] = useState(true);
  const scrollToBottom = useCallback(() => ref.current?.scrollTo({ top: ref.current.scrollHeight, behavior: "smooth" }), []);
  const value = useMemo(() => ({ isAtBottom, scrollToBottom }), [isAtBottom, scrollToBottom]);
  useEffect(() => { if (isAtBottom) scrollToBottom(); }, [children, isAtBottom, scrollToBottom]);
  return <ConversationContext.Provider value={value}><div ref={ref} role="log" className={cn("relative flex-1 overflow-y-auto", className)} onScroll={(event) => { const node=event.currentTarget; setIsAtBottom(node.scrollHeight-node.scrollTop-node.clientHeight < 16); }} {...props}>{children}</div></ConversationContext.Provider>;
}
export function ConversationContent({ className, ...props }: HTMLAttributes<HTMLDivElement>) { return <div className={cn("flex flex-col gap-8 p-4", className)} {...props} />; }
export function ConversationScrollButton(props: ButtonProps) {
  const context = useContext(ConversationContext);
  if (!context || context.isAtBottom) return null;
  return <Button aria-label="Scroll to latest message" className="sticky bottom-4 left-1/2 -translate-x-1/2 rounded-full" onClick={context.scrollToBottom} size="icon" type="button" variant="outline" {...props}><ArrowDownIcon className="size-4" /></Button>;
}
