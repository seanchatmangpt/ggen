import type { Metadata } from "next";

export const metadata: Metadata = {
  title: "Interview Sandbox",
  description: "Self-hosted reference coding-interview sandbox (Next.js + Monaco): browser editor, language selection, compile/execute, visible/hidden tests, console -- the HackerRank/CoderPad/CodeSignal-equivalent surface for InterviewAssist Phase 0.",
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}
