"use client";

import { useMemo, useState } from "react";
import { useChat } from "@ai-sdk/react";
import {
  DefaultChatTransport,
  isToolUIPart,
  lastAssistantMessageIsCompleteWithApprovalResponses,
  type DynamicToolUIPart,
  type ToolUIPart,
} from "ai";
import { Conversation, ConversationContent, ConversationScrollButton } from "@/components/ai-elements/conversation";
import { Message, MessageContent, MessageResponse } from "@/components/ai-elements/message";
import { PromptInput, PromptInputBody, PromptInputFooter, PromptInputSubmit, PromptInputTextarea } from "@/components/ai-elements/prompt-input";
import { Tool, ToolContent, ToolHeader, ToolInput, ToolOutput } from "@/components/ai-elements/tool";
import {
  Confirmation,
  ConfirmationAction,
  ConfirmationActions,
  ConfirmationRequest,
  ConfirmationTitle,
} from "@/components/ai-elements/confirmation";
import type { AppUIMessage } from "@/lib/ai/types";

export function Chat() {
  const [text, setText] = useState("");
  const transport = useMemo(() => new DefaultChatTransport({ api: "/api/chat" }), []);
  const { messages, sendMessage, status, addToolApprovalResponse } = useChat<AppUIMessage>({
    transport,
    sendAutomaticallyWhen: lastAssistantMessageIsCompleteWithApprovalResponses,
  });

  return (
    <section className="flex min-h-0 flex-1 flex-col overflow-hidden rounded-xl border bg-card shadow-sm">
      <header className="border-b p-4">
        <h1 className="text-lg font-semibold">Admitted AI Workspace</h1>
        <p className="text-sm text-muted-foreground">Streaming agent with brokered tools and explicit mutation approval.</p>
      </header>

      <Conversation className="min-h-0 flex-1">
        <ConversationContent>
          {messages.map((message) => (
            <Message key={message.id} from={message.role}>
              <MessageContent>
                {message.parts.map((part, index) => {
                  if (part.type === "text") return <MessageResponse key={index}>{part.text}</MessageResponse>;
                  if (!isToolUIPart(part)) return null;
                  return (
                    <ToolPart
                      key={index}
                      part={part}
                      onApproval={(id, approved) => addToolApprovalResponse({ id, approved })}
                    />
                  );
                })}
              </MessageContent>
            </Message>
          ))}
        </ConversationContent>
        <ConversationScrollButton />
      </Conversation>

      <div className="border-t p-3">
        <PromptInput
          onSubmit={({ text: submitted }) => {
            const value = submitted.trim();
            if (!value) return;
            sendMessage({ text: value });
            setText("");
          }}
        >
          <PromptInputBody>
            <PromptInputTextarea
              value={text}
              onChange={(event) => setText(event.currentTarget.value)}
              placeholder="Ask or request an action"
            />
          </PromptInputBody>
          <PromptInputFooter>
            <span className="text-xs text-muted-foreground">Mutations require approval.</span>
            <PromptInputSubmit aria-label="Send message" disabled={!text.trim()} status={status} />
          </PromptInputFooter>
        </PromptInput>
      </div>
    </section>
  );
}

function ToolPart({
  part,
  onApproval,
}: {
  part: ToolUIPart | DynamicToolUIPart;
  onApproval: (id: string, approved: boolean) => void;
}) {
  return (
    <Tool defaultOpen>
      <ToolHeader type={part.type} state={part.state} />
      <ToolContent>
        <ToolInput input={part.input} />
        {part.state === "approval-requested" && part.approval ? (
          <Confirmation approval={part.approval} state={part.state}>
            <ConfirmationTitle>Approve this tool execution?</ConfirmationTitle>
            <ConfirmationRequest>{JSON.stringify(part.input, null, 2)}</ConfirmationRequest>
            <ConfirmationActions>
              <ConfirmationAction
                variant="outline"
                onClick={() => onApproval(part.approval!.id, false)}
              >
                Deny
              </ConfirmationAction>
              <ConfirmationAction onClick={() => onApproval(part.approval!.id, true)}>
                Approve
              </ConfirmationAction>
            </ConfirmationActions>
          </Confirmation>
        ) : null}
        <ToolOutput
          output={
            part.state === "output-available" ? (
              <MessageResponse>{JSON.stringify(part.output, null, 2)}</MessageResponse>
            ) : undefined
          }
          errorText={part.state === "output-error" ? part.errorText : undefined}
        />
      </ToolContent>
    </Tool>
  );
}
