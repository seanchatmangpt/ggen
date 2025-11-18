# 🔐 ggen IDP Portal

Enterprise Identity Provider Web Portal with Monaco Editor and Vercel AI integration. Manage BPMN-based authentication flows, ReBAC policies, OAuth2 clients, and 2028-forward identity features.

## 🚀 Features

### Core IDP Management
- **Authentication Flows** - BPMN 2.0 workflow editor with visual steps
- **RBAC Management** - Relation-Based Access Control with CEL policies
- **OAuth2/OIDC** - Client management and configuration
- **Users & Organizations** - Multi-tenant user management
- **Audit Logs** - Comprehensive activity tracking

### AI-Powered Development
- **Monaco Editor Integration** - Professional code editor with syntax highlighting
- **AI Code Suggestions** - Vercel AI-powered code completion
- **Code Validation** - Real-time YAML, JSON, CEL validation
- **Flow Templates** - Pre-built auth flow templates
- **Smart Refactoring** - AI-assisted code optimization

### 2028 Features (Future-Ready)
- **Decentralized Identity (DIDs)** - W3C DID standard support
- **Zero-Knowledge Proofs** - Privacy-preserving credential verification
- **Autonomous Agents** - AI agents managing identity operations
- **Quantum-Safe Cryptography** - Post-quantum resistant algorithms

## 📋 Tech Stack

- **Frontend**: Next.js 14, React 18, TypeScript
- **Editor**: Monaco Editor with React integration
- **State Management**: Zustand
- **Styling**: Tailwind CSS
- **AI**: Vercel AI SDK
- **Code Validation**: js-yaml, jsonschema
- **UI Components**: Lucide Icons, Framer Motion
- **Notifications**: React Hot Toast

## 🛠️ Installation

```bash
cd apps/ggen-idp-portal
npm install
```

## 🚀 Getting Started

### Development Server

```bash
npm run dev
```

Navigate to http://localhost:3000

### Environment Variables

Create `.env.local`:

```env
NEXT_PUBLIC_API_URL=http://localhost:8000
NEXT_PUBLIC_VERCEL_AI_ENDPOINT=/api/ai
```

### Configuration

- `next.config.js` - Next.js configuration
- `tailwind.config.ts` - Tailwind CSS configuration
- `tsconfig.json` - TypeScript configuration

## 📁 Project Structure

```
src/
├── app/                          # Next.js 13+ App Router
│   ├── layout.tsx               # Root layout
│   ├── globals.css              # Global styles
│   ├── dashboard/               # Dashboard page
│   ├── auth-flows/              # Auth flow editor
│   ├── roles/                   # RBAC management
│   ├── 2028-features/           # Forward-looking features
│   └── api/
│       └── ai/                  # AI endpoints
│           ├── suggest/         # Code suggestions
│           └── validate/        # Code validation
├── components/
│   ├── providers/               # React providers
│   ├── editor/                  # CodeEditor component
│   └── ...                      # UI components
├── api/
│   └── client.ts               # IDP API client
├── lib/
│   ├── ai/
│   │   ├── suggestions.ts      # AI suggestions engine
│   │   └── validation.ts       # Code validation
│   └── templates/              # Flow templates
├── stores/
│   ├── authStore.ts            # Auth state (Zustand)
│   └── editorStore.ts          # Editor state (Zustand)
├── types/
│   └── index.ts                # TypeScript types
└── hooks/                       # Custom React hooks
```

## 🎨 Key Components

### CodeEditor
Monaco-powered code editor with AI suggestions and validation.

```tsx
<CodeEditor
  value={code}
  language="yaml"
  onChange={setCode}
  height="500px"
  showSuggestions={true}
/>
```

### Dashboard
Overview of organization metrics and quick actions.

```tsx
<Dashboard />
```

### Auth Flows
Visual BPMN workflow editor for authentication flows.

```tsx
<AuthFlowEditor flowId="login-basic" />
```

## 🤖 AI Integration

### Vercel AI Endpoints

#### /api/ai/suggest
Generate code suggestions using Vercel AI SDK.

```bash
curl -X POST http://localhost:3000/api/ai/suggest \
  -H "Content-Type: application/json" \
  -d '{
    "code": "id: login\nsteps:",
    "language": "yaml",
    "type": "code-completion"
  }'
```

#### /api/ai/validate
Validate code syntax and structure.

```bash
curl -X POST http://localhost:3000/api/ai/validate \
  -H "Content-Type: application/json" \
  -d '{
    "code": "id: login\nname: Login",
    "language": "yaml"
  }'
```

## 📝 Editors

### YAML Editor
Edit BPMN auth flows and configurations.

**Supported:**
- Auth flows (login, oauth, mfa, password reset)
- RBAC policies
- OAuth2 client config

### JSON Editor
Configure structured data.

**Supported:**
- API schemas
- Metadata
- Resource definitions

### CEL Editor
Write relation-based access control policies.

**Supported:**
- Boolean expressions
- Function calls
- Constraint validation

## 🔄 API Integration

### Authentication Flow

```tsx
// Login
await idpClient.login(orgId, { username, password })

// Refresh token
await idpClient.refreshToken(refreshToken)

// Logout
await idpClient.logout(sessionId)
```

### Auth Flows

```tsx
// List flows
const flows = await idpClient.listAuthFlows(orgId)

// Create flow
const flow = await idpClient.createAuthFlow(orgId, {...})

// Execute flow
const result = await idpClient.executeAuthFlow(flowId, context)
```

### RBAC

```tsx
// Check permission
const allowed = await idpClient.checkPermission(userId, 'pack', 'publish')

// Assign role
await idpClient.assignRole(userId, orgId, roleId)
```

## 🧪 Testing

```bash
npm run test
npm run test:watch
```

## 📦 Build & Deployment

### Development Build

```bash
npm run dev
```

### Production Build

```bash
npm run build
npm start
```

### Docker

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY . .
RUN npm ci && npm run build
EXPOSE 3000
CMD ["npm", "start"]
```

## 🎯 2028 Features Roadmap

### Phase 1: Foundation (Current)
- ✅ Monaco editor integration
- ✅ AI code suggestions
- ✅ YAML/JSON/CEL editors
- ✅ Dashboard and metrics

### Phase 2: Decentralized Identity
- [ ] DID creation and management
- [ ] DID Document generation
- [ ] Verifiable Credentials (VCs)
- [ ] VC presentation

### Phase 3: Zero-Knowledge Proofs
- [ ] ZK proof generation (Age, Membership, Balance)
- [ ] Circuit management
- [ ] Proof verification
- [ ] Privacy-preserving policies

### Phase 4: Autonomous Agents
- [ ] Agent creation and deployment
- [ ] LLM integration (GPT-4, Claude 3)
- [ ] Agent capabilities framework
- [ ] Real-time monitoring

### Phase 5: Quantum-Safe Crypto
- [ ] CRYSTALS-Kyber implementation
- [ ] CRYSTALS-Dilithium signatures
- [ ] Hybrid mode (classical + quantum)
- [ ] Post-quantum key migration

## 🔐 Security Considerations

- ✅ Environment-based configuration
- ✅ JWT token management
- ✅ Secure API communication (HTTPS)
- ✅ Input validation and sanitization
- ✅ CORS and CSRF protection
- ✅ Rate limiting support
- ✅ Content Security Policy

## 📊 Performance

- Monaco Editor lazy loading
- Code splitting per route
- Image optimization
- Efficient state management (Zustand)
- API request caching
- WebSocket support for real-time updates

## 🤝 Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new features
4. Submit a pull request

## 📄 License

MIT License - See LICENSE file for details

## 🆘 Support

- **Issues**: GitHub Issues
- **Documentation**: Full API docs at `/docs`
- **Email**: support@ggen.dev

## 🚀 Advanced Usage

### Custom AI Providers

To use a different AI provider:

```typescript
// src/lib/ai/custom-provider.ts
export async function generateWithCustomProvider(
  code: string,
  language: string
): Promise<AICodeSuggestion[]> {
  // Your implementation
}
```

### Extending Validation

Add custom validation rules:

```typescript
// src/lib/ai/validation.ts
export function validateCustomLanguage(code: string): ValidationResult {
  // Your validation logic
}
```

### Custom Themes

Edit `tailwind.config.ts` to customize:

```typescript
theme: {
  extend: {
    colors: {
      'custom-primary': '#...',
    },
  },
}
```

## 🎓 Learning Resources

- [Next.js Documentation](https://nextjs.org/docs)
- [Monaco Editor Guide](https://microsoft.github.io/monaco-editor/)
- [Tailwind CSS](https://tailwindcss.com/docs)
- [Zustand](https://github.com/pmndrs/zustand)
- [Vercel AI SDK](https://sdk.vercel.ai)

---

**Built with ❤️ for the ggen BPMN.js Marketplace ecosystem**
