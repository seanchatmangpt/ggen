import { WebSocketServer } from "ws";
import net from "node:net";

const listenPort = Number(process.argv[2] ?? 5433);
const targetHost = process.argv[3] ?? "127.0.0.1";
const targetPort = Number(process.argv[4] ?? 5432);

const wss = new WebSocketServer({ port: listenPort });
wss.on("connection", (socket) => {
  const tcp = net.connect(targetPort, targetHost);
  tcp.on("data", (chunk) => socket.readyState === socket.OPEN && socket.send(chunk));
  tcp.on("close", () => socket.close());
  tcp.on("error", () => socket.close());
  socket.on("message", (data) => tcp.write(data));
  socket.on("close", () => tcp.end());
  socket.on("error", () => tcp.end());
});

console.log(`local-wsproxy listening on ${listenPort} -> ${targetHost}:${targetPort}`);
