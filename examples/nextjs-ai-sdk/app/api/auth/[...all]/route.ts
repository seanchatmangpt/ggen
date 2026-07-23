import { toNextJsHandler } from "better-auth/next-js";
import { getAuth } from "@/lib/auth";

async function dispatch(request: Request): Promise<Response> {
  const handlers = toNextJsHandler(getAuth());
  return request.method === "GET" ? handlers.GET(request) : handlers.POST(request);
}

export { dispatch as GET, dispatch as POST };
