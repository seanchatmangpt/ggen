"use client";

import { createContext, useContext, type FormEvent, type FormHTMLAttributes, type HTMLAttributes, type TextareaHTMLAttributes } from "react";
import { LoaderCircleIcon, SendIcon } from "lucide-react";
import { Button, type ButtonProps } from "@/components/ui/button";
import { cn } from "@/lib/utils";

export type PromptInputMessage = { text: string };
type PromptContext = { submit: (text: string) => void };
const Context = createContext<PromptContext | null>(null);
export type PromptInputProps = Omit<FormHTMLAttributes<HTMLFormElement>, "onSubmit"> & { onSubmit: (message: PromptInputMessage) => void };
export function PromptInput({ onSubmit, className, ...props }: PromptInputProps) {
  const submit = (text: string) => onSubmit({ text });
  const contextValue = { submit };
  const handleSubmit = (event: FormEvent<HTMLFormElement>) => { event.preventDefault(); const form=new FormData(event.currentTarget); submit(String(form.get("prompt") ?? "")); };
  return <Context.Provider value={contextValue}><form className={cn("rounded-lg border bg-background", className)} onSubmit={handleSubmit} {...props} /></Context.Provider>;
}
export function PromptInputBody(props: HTMLAttributes<HTMLDivElement>) { return <div {...props} />; }
export function PromptInputFooter({ className, ...props }: HTMLAttributes<HTMLDivElement>) { return <div className={cn("flex items-center justify-between gap-3 p-2", className)} {...props} />; }
export function PromptInputTextarea({ className, name="prompt", onKeyDown, ...props }: TextareaHTMLAttributes<HTMLTextAreaElement>) {
  const context=useContext(Context);
  return <textarea name={name} className={cn("min-h-24 w-full resize-none bg-transparent p-3 outline-none", className)} onKeyDown={(event) => { onKeyDown?.(event); if (event.key === "Enter" && !event.shiftKey) { event.preventDefault(); context?.submit(event.currentTarget.value); } }} {...props} />;
}
export type PromptInputSubmitProps = ButtonProps & { status?: string };
export function PromptInputSubmit({ status, children, ...props }: PromptInputSubmitProps) { const busy=status === "submitted" || status === "streaming"; return <Button type="submit" size="icon" {...props}>{children ?? (busy ? <LoaderCircleIcon className="size-4 animate-spin" /> : <SendIcon className="size-4" />)}</Button>; }
