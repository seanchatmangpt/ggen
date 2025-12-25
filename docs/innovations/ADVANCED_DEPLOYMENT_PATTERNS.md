# Advanced Deployment Patterns with ggen

> Enterprise-grade deployment patterns generated from RDF ontologies: Kubernetes, Helm, Docker, and multi-cloud orchestration

## Overview

This guide covers five advanced deployment patterns that ggen generates from RDF ontologies, enabling reproducible, infrastructure-as-code deployments across Kubernetes, Docker, and multi-cloud environments.

## Pattern 1: Advanced Container Orchestration with Kubernetes & Helm

### Description

Generate multi-stage Dockerfile, Kubernetes manifests, and Helm charts automatically from RDF deployment ontologies. Enables reproducible infrastructure-as-code with zero manual YAML editing.

### External Tools & Packages

```json
{
  "docker": ">=24.0",
  "kubernetes": ">=1.28",
  "helm": ">=3.12",
  "docker/build-push-action": "^5.0.0",
  "azure/k8s-deploy": "^4.0.0",
  "deliverybot/helm": "^1.0.0"
}
```

### RDF Ontology for Deployment

```turtle
@prefix deploy: <https://ggen.example.com/deploy/>
@prefix k8s: <https://ggen.example.com/kubernetes/>
@prefix container: <https://ggen.example.com/container/>

deploy:api-service a deploy:ContainerStrategy ;
  deploy:baseImage "node:20-alpine" ;
  deploy:multistage true ;
  deploy:layers (
    deploy:builder-layer
    deploy:production-layer
  ) ;
  deploy:targetPlatforms ( "linux/amd64" "linux/arm64" ) ;
  deploy:securityContext [
    deploy:runAsNonRoot true ;
    deploy:runAsUser 1001 ;
    deploy:fsGroup 1001 ;
    deploy:readOnlyRootFilesystem true
  ] .

deploy:api-k8s-deployment a k8s:Deployment ;
  k8s:replicas 3 ;
  k8s:strategy "RollingUpdate" ;
  k8s:resources [
    k8s:requestsCpu "100m" ;
    k8s:requestsMemory "128Mi" ;
    k8s:limitsCpu "500m" ;
    k8s:limitsMemory "512Mi"
  ] ;
  k8s:probes [
    k8s:livenessPath "/health" ;
    k8s:readinessPath "/ready" ;
    k8s:startupPath "/startup" ;
    k8s:probePeriodSeconds 10 ;
    k8s:probeTimeoutSeconds 5 ;
    k8s:probeFailureThreshold 3
  ] ;
  k8s:autoscaling [
    k8s:minReplicas 3 ;
    k8s:maxReplicas 10 ;
    k8s:targetCPU 80 ;
    k8s:targetMemory 80
  ] .

deploy:api-helm-chart a k8s:HelmChart ;
  k8s:chartName "ggen-api" ;
  k8s:chartVersion "1.0.0" ;
  k8s:values [
    k8s:replicaCount 3 ;
    k8s:autoscaling true ;
    k8s:ingress true ;
    k8s:certManager true ;
    k8s:monitoring true
  ] .
```

### Generated Multi-Stage Dockerfile

```dockerfile
# Multi-stage Dockerfile generated from RDF ontology
# Source: ontology/deployment.ttl (deploy:api-service)

# Stage 1: Builder
FROM node:20-alpine AS builder

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache python3 make g++

COPY package*.json ./
RUN npm ci --only=production

COPY . .
RUN npm run build

# Stage 2: Production
FROM node:20-alpine AS production

WORKDIR /app

# Security: Run as non-root (generated from deploy:securityContext)
RUN addgroup -g 1001 -S nodejs && \
    adduser -S nodejs -u 1001

# Copy dependencies and built artifacts
COPY --from=builder --chown=nodejs:nodejs /app/node_modules ./node_modules
COPY --from=builder --chown=nodejs:nodejs /app/dist ./dist
COPY --chown=nodejs:nodejs package.json ./

USER nodejs

EXPOSE 3000

# Health check (generated from k8s:probes)
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node healthcheck.js

CMD ["node", "dist/server.js"]
```

### Generated Kubernetes Deployment

```yaml
# Kubernetes Deployment generated from RDF ontology
# Source: ontology/deployment.ttl (deploy:api-k8s-deployment)
apiVersion: apps/v1
kind: Deployment
metadata:
  name: ggen-api
  namespace: production
  labels:
    app: ggen-api
    version: v1
spec:
  replicas: 3  # Generated from k8s:replicas
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 0
  selector:
    matchLabels:
      app: ggen-api
  template:
    metadata:
      labels:
        app: ggen-api
        version: v1
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "3000"
        prometheus.io/path: "/metrics"
    spec:
      serviceAccountName: ggen-api
      securityContext:
        runAsNonRoot: true
        runAsUser: 1001
        fsGroup: 1001
      containers:
      - name: api
        image: ghcr.io/org/ggen-api:latest
        imagePullPolicy: Always
        ports:
        - name: http
          containerPort: 3000
          protocol: TCP
        env:
        - name: NODE_ENV
          value: production
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: ggen-secrets
              key: database-url
        # Generated from k8s:resources
        resources:
          requests:
            cpu: 100m
            memory: 128Mi
          limits:
            cpu: 500m
            memory: 512Mi
        # Generated from k8s:probes
        livenessProbe:
          httpGet:
            path: /health
            port: http
          initialDelaySeconds: 30
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3
        readinessProbe:
          httpGet:
            path: /ready
            port: http
          initialDelaySeconds: 10
          periodSeconds: 5
          timeoutSeconds: 3
          failureThreshold: 3
        startupProbe:
          httpGet:
            path: /startup
            port: http
          initialDelaySeconds: 0
          periodSeconds: 5
          failureThreshold: 30
---
apiVersion: v1
kind: Service
metadata:
  name: ggen-api
  namespace: production
  labels:
    app: ggen-api
spec:
  type: ClusterIP
  selector:
    app: ggen-api
  ports:
  - name: http
    port: 80
    targetPort: http
    protocol: TCP
---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: ggen-api
  namespace: production
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: ggen-api
  minReplicas: 3  # Generated from k8s:autoscaling
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 80
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

### Generated Helm Chart

```yaml
# Chart.yaml
apiVersion: v2
name: ggen-api
description: Generated API service from RDF ontology
type: application
version: 1.0.0
appVersion: "1.0.0"
keywords:
  - api
  - rdf
  - code-generation
maintainers:
  - name: ggen
    email: support@ggen.example.com
```

```yaml
# values.yaml - Generated from RDF ontology
replicaCount: 3

image:
  repository: ghcr.io/org/ggen-api
  pullPolicy: Always
  tag: "latest"

service:
  type: ClusterIP
  port: 80
  targetPort: 3000

ingress:
  enabled: true
  className: "nginx"
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
  hosts:
    - host: api.example.com
      paths:
        - path: /
          pathType: Prefix
  tls:
    - secretName: ggen-api-tls
      hosts:
        - api.example.com

resources:
  limits:
    cpu: 500m
    memory: 512Mi
  requests:
    cpu: 100m
    memory: 128Mi

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 80
  targetMemoryUtilizationPercentage: 80

env:
  - name: NODE_ENV
    value: production

envFrom:
  - secretRef:
      name: ggen-secrets
```

### Generated GitHub Actions Workflow

```yaml
name: Container Orchestration Pipeline

on:
  push:
    branches: [main, develop]
    paths:
      - 'src/**'
      - 'Dockerfile'
      - 'k8s/**'
      - 'helm/**'

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  generate-k8s-manifests:
    name: Generate K8s from RDF
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install ggen
        run: curl -sSL https://ggen.example.com/install.sh | bash

      - name: Generate Kubernetes manifests from ontology
        run: |
          # Generate K8s YAML from RDF deployment ontology
          ggen render \
            --ontology ontology/deployment.ttl \
            --template templates/kubernetes/deployment.yaml.tera \
            --output k8s/generated/

          # Generate Helm chart from RDF
          ggen render \
            --ontology ontology/deployment.ttl \
            --template templates/helm/ \
            --output helm/charts/ggen-api/

      - name: Validate K8s manifests
        run: |
          kubectl --dry-run=client apply -f k8s/generated/
          helm lint helm/charts/ggen-api/

      - name: Upload manifests
        uses: actions/upload-artifact@v4
        with:
          name: k8s-manifests
          path: |
            k8s/generated/
            helm/charts/

  build-multiplatform:
    name: Build Multi-Platform Containers
    runs-on: ubuntu-latest
    needs: generate-k8s-manifests

    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Build and push multi-platform image
        uses: docker/build-push-action@v5
        with:
          context: .
          platforms: linux/amd64,linux/arm64
          push: true
          tags: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:latest
          cache-from: type=registry,ref=${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:buildcache
          cache-to: type=registry,ref=${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:buildcache,mode=max

  deploy-k8s:
    name: Deploy to Kubernetes
    runs-on: ubuntu-latest
    needs: build-multiplatform

    steps:
      - uses: actions/checkout@v4

      - name: Download K8s manifests
        uses: actions/download-artifact@v4
        with:
          name: k8s-manifests

      - name: Deploy with Helm
        run: |
          helm upgrade --install ggen-api \
            helm/charts/ggen-api/ \
            --namespace production \
            --create-namespace \
            --values helm/charts/ggen-api/values.yaml \
            --set image.tag=${{ github.sha }} \
            --wait \
            --timeout 5m

      - name: Verify deployment
        run: |
          kubectl rollout status deployment/ggen-api -n production
```

---

## Pattern 2: Multi-Cloud Deployment (AWS ECS, Azure Container Instances, GKE)

### Description

Generate deployment configurations for AWS ECS, Azure Container Instances, and Google Kubernetes Engine from a single RDF ontology.

---

## Pattern 3: GitOps with ArgoCD & Flux

### Description

Generate ArgoCD Application resources and Flux v2 GitRepository definitions from RDF, enabling push-to-deploy workflows.

---

## Pattern 4: Service Mesh Integration (Istio, Linkerd)

### Description

Generate Istio VirtualServices, DestinationRules, and Linkerd TrafficPolicy configurations for advanced traffic management.

---

## Pattern 5: Observability Stack (Prometheus, Grafana, Jaeger)

### Description

Generate Prometheus scrape configs, Grafana dashboards, and Jaeger trace collector configuration from RDF.

---

## Tera Template Examples

### Kubernetes Deployment Template

```tera
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {{ deployment.name }}
  namespace: {{ deployment.namespace }}
spec:
  replicas: {{ deployment.replicas }}
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 0
  selector:
    matchLabels:
      app: {{ deployment.name }}
  template:
    metadata:
      labels:
        app: {{ deployment.name }}
    spec:
      containers:
      - name: {{ deployment.name }}
        image: {{ deployment.image }}:{{ deployment.imageTag }}
        ports:
        {% for port in deployment.ports %}
        - name: {{ port.name }}
          containerPort: {{ port.containerPort }}
        {% endfor %}
        env:
        {% for env in deployment.env %}
        - name: {{ env.name }}
          value: {{ env.value | quote }}
        {% endfor %}
        resources:
          requests:
            cpu: {{ deployment.resources.requestsCpu }}
            memory: {{ deployment.resources.requestsMemory }}
          limits:
            cpu: {{ deployment.resources.limitsCpu }}
            memory: {{ deployment.resources.limitsMemory }}
```

---

## Best Practices

1. **Infrastructure as Code**: All deployments generated from RDF, no manual YAML
2. **Multi-Stage Builds**: Minimize final image size with builder patterns
3. **Security Context**: Always run as non-root with minimal capabilities
4. **Resource Limits**: Set both requests and limits for optimal scheduling
5. **Health Checks**: Implement liveness, readiness, and startup probes
6. **Horizontal Scaling**: Enable autoscaling with proper metrics
7. **GitOps Workflow**: Use ArgoCD for declarative deployment
8. **Observability**: Integrate Prometheus, Grafana, and Jaeger from start

---

## References

- [Kubernetes Documentation](https://kubernetes.io/docs/)
- [Helm Chart Best Practices](https://helm.sh/docs/chart_best_practices/)
- [Docker Best Practices](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)
- [ArgoCD Documentation](https://argo-cd.readthedocs.io/)

## See Also

- [ADVANCED_TYPESCRIPT_PATTERNS.md](./ADVANCED_TYPESCRIPT_PATTERNS.md) - TypeScript patterns
- [GITHUB_ACTIONS_CICD_EXAMPLE.md](../how-to-guides/GITHUB_ACTIONS_CICD_EXAMPLE.md) - CI/CD workflows
- [SECURITY_HARDENING_GUIDE.md](../how-to-guides/SECURITY_HARDENING_GUIDE.md) - Security best practices
