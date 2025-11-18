/**
 * 3D Spatial Visualization Engine
 * AR/VR Support with Three.js
 *
 * 2028 Innovation Phase: Spatial Computing
 */

/**
 * 3D Node representation
 */
export interface Node3D {
  id: string
  position: [number, number, number]
  size: number
  color: string
  label: string
  type: 'task' | 'gateway' | 'event' | 'subprocess' | 'code-block'
  metadata?: Record<string, any>
}

/**
 * 3D Connection/Edge representation
 */
export interface Edge3D {
  id: string
  source: string
  target: string
  color?: string
  width?: number
  curved?: boolean
  metadata?: Record<string, any>
}

/**
 * 3D Scene configuration
 */
export interface Scene3DConfig {
  width: number
  height: number
  backgroundColor?: string
  cameraDistance?: number
  autoRotate?: boolean
  vrEnabled?: boolean
  gridEnabled?: boolean
  lightingPreset?: 'default' | 'dark' | 'bright' | 'studio'
}

/**
 * BPMN element colors
 */
const BPMN_COLORS = {
  TASK: '#4CAF50',
  GATEWAY: '#FF9800',
  EVENT: '#2196F3',
  SUBPROCESS: '#9C27B0',
  ANNOTATION: '#757575',
  SEQUENCE_FLOW: '#212121',
}

/**
 * Code structure colors
 */
const CODE_COLORS = {
  MODULE: '#1E88E5',
  CLASS: '#43A047',
  FUNCTION: '#FB8C00',
  VARIABLE: '#5E35B1',
  IMPORT: '#00ACC1',
}

/**
 * 3D Visualization Engine
 * Main class for managing 3D scene, objects, and interactions
 */
export class Visualization3D {
  private canvasId: string
  private width: number
  private height: number
  private scene: any
  private camera: any
  private renderer: any
  private nodes: Map<string, Node3D> = new Map()
  private edges: Map<string, Edge3D> = new Map()
  private meshes: Map<string, any> = new Map()
  private vrSession: any
  private isVrSupported: boolean = false
  private animationFrameId: number | null = null
  private raycaster: any
  private mouse: any

  constructor(canvasId: string, config: Scene3DConfig) {
    this.canvasId = canvasId
    this.width = config.width
    this.height = config.height

    // Initialize Three.js components (would require npm install three)
    this.initScene(config)
    this.setupLighting(config.lightingPreset || 'default')
    this.setupInteraction()

    // Check WebXR support for VR
    if (config.vrEnabled && navigator.xr) {
      navigator.xr?.isSessionSupported('immersive-vr').then((supported) => {
        this.isVrSupported = supported
      })
    }
  }

  /**
   * Initialize Three.js scene
   */
  private initScene(config: Scene3DConfig): void {
    // Scene setup (stubbed for demonstration)
    const bgColor = config.backgroundColor || '#f0f0f0'

    // In production, this would be:
    // this.scene = new THREE.Scene()
    // this.camera = new THREE.PerspectiveCamera(75, this.width/this.height, 0.1, 1000)
    // this.renderer = new THREE.WebGLRenderer({ canvas: document.getElementById(this.canvasId) })

    console.log(`3D Scene initialized: ${this.width}x${this.height}, bg: ${bgColor}`)
  }

  /**
   * Setup lighting for the scene
   */
  private setupLighting(preset: string): void {
    // Lighting configuration
    const lightingConfigs: Record<string, { ambient: number; directional: [number, number, number]; intensity: number }> = {
      default: { ambient: 0.6, directional: [5, 5, 5], intensity: 0.8 },
      dark: { ambient: 0.3, directional: [10, 10, 10], intensity: 1.0 },
      bright: { ambient: 0.9, directional: [0, 0, 0], intensity: 0.5 },
      studio: { ambient: 0.7, directional: [8, 5, 3], intensity: 0.9 },
    }

    const config = lightingConfigs[preset] || lightingConfigs.default

    // Add ambient light
    // const ambientLight = new THREE.AmbientLight(0xffffff, config.ambient)
    // this.scene.add(ambientLight)

    // Add directional light
    // const directionalLight = new THREE.DirectionalLight(0xffffff, config.intensity)
    // directionalLight.position.set(...config.directional)
    // this.scene.add(directionalLight)

    console.log(`Lighting preset applied: ${preset}`)
  }

  /**
   * Setup mouse/touch interaction
   */
  private setupInteraction(): void {
    // Raycasting for object selection
    // this.raycaster = new THREE.Raycaster()
    // this.mouse = new THREE.Vector2()

    // document.getElementById(this.canvasId)?.addEventListener('mousemove', (e) => {
    //   this.mouse.x = (e.clientX / this.width) * 2 - 1
    //   this.mouse.y = -(e.clientY / this.height) * 2 + 1
    // })

    console.log('Interaction system initialized')
  }

  /**
   * Add BPMN process to 3D scene
   */
  addBpmnProcess(process: any): void {
    // Extract elements from process
    const elements = process.elements || []
    let xOffset = 0
    let yOffset = 0

    elements.forEach((element: any, index: number) => {
      const color = this.getBpmnColor(element.type)

      const node: Node3D = {
        id: element.id,
        position: [xOffset, yOffset, 0],
        size: 1.0,
        color,
        label: element.name || element.id,
        type: this.mapBpmnType(element.type),
        metadata: { bpmnType: element.type },
      }

      this.addNode(node)

      // Layout nodes in a grid
      xOffset += 2.5
      if (index % 5 === 4) {
        xOffset = 0
        yOffset -= 2.5
      }
    })

    // Add connections
    elements.forEach((element: any) => {
      if (element.outgoing) {
        element.outgoing.forEach((outgoing: any) => {
          const edge: Edge3D = {
            id: `${element.id}->${outgoing.targetRef}`,
            source: element.id,
            target: outgoing.targetRef,
            color: BPMN_COLORS.SEQUENCE_FLOW,
            width: 0.05,
          }

          this.addEdge(edge)
        })
      }
    })

    console.log(`BPMN process added: ${elements.length} elements`)
  }

  /**
   * Add code structure to 3D scene
   */
  addCodeStructure(codeAST: any): void {
    // Visualize code as 3D tree structure
    const layers: any[] = []
    let currentLayer = [codeAST]
    let depth = 0

    while (currentLayer.length > 0 && depth < 5) {
      const nextLayer: any[] = []

      currentLayer.forEach((node, index) => {
        const angle = (index / currentLayer.length) * Math.PI * 2
        const radius = depth * 3 + 2

        const x = Math.cos(angle) * radius
        const z = Math.sin(angle) * radius
        const y = -depth * 1.5

        const colorKey = node.type?.toUpperCase() || 'MODULE'
        const color = CODE_COLORS[colorKey as keyof typeof CODE_COLORS] || '#CCCCCC'

        const node3d: Node3D = {
          id: `code-${depth}-${index}`,
          position: [x, y, z],
          size: Math.max(0.5, 2 - depth * 0.3),
          color,
          label: node.name || node.type || 'node',
          type: 'code-block',
          metadata: { depth, nodeType: node.type },
        }

        this.addNode(node3d)

        if (node.children && node.children.length > 0) {
          nextLayer.push(...node.children)
        }
      })

      layers.push(currentLayer)
      currentLayer = nextLayer
      depth++
    }

    // Connect nodes by layer
    for (let d = 0; d < layers.length - 1; d++) {
      layers[d].forEach((parent, pIndex) => {
        if (parent.children) {
          parent.children.forEach((child: any, cIndex: number) => {
            const edge: Edge3D = {
              id: `code-${d}-${pIndex}->${d + 1}-${cIndex}`,
              source: `code-${d}-${pIndex}`,
              target: `code-${d + 1}-${cIndex}`,
              color: CODE_COLORS.IMPORT,
              curved: true,
            }

            this.addEdge(edge)
          })
        }
      })
    }

    console.log(`Code structure added: ${depth} layers`)
  }

  /**
   * Add node to scene
   */
  addNode(node: Node3D): void {
    this.nodes.set(node.id, node)

    // Create geometry and material
    // const geometry = new THREE.SphereGeometry(node.size, 32, 32)
    // const material = new THREE.MeshStandardMaterial({ color: node.color })
    // const mesh = new THREE.Mesh(geometry, material)
    // mesh.position.set(...node.position)
    // mesh.userData = { id: node.id, label: node.label, ...node.metadata }
    // this.scene.add(mesh)
    // this.meshes.set(node.id, mesh)

    console.log(`Node added: ${node.id} (${node.type})`)
  }

  /**
   * Add edge to scene
   */
  addEdge(edge: Edge3D): void {
    this.edges.set(edge.id, edge)

    const sourceNode = this.nodes.get(edge.source)
    const targetNode = this.nodes.get(edge.target)

    if (!sourceNode || !targetNode) return

    // Create line geometry
    // const geometry = new THREE.BufferGeometry()
    // const positions = new Float32Array([
    //   ...sourceNode.position,
    //   ...targetNode.position,
    // ])
    // geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3))

    // const material = new THREE.LineBasicMaterial({
    //   color: edge.color || 0x000000,
    //   linewidth: (edge.width || 0.1) * 100,
    // })
    // const line = new THREE.Line(geometry, material)
    // this.scene.add(line)

    console.log(`Edge added: ${edge.source} -> ${edge.target}`)
  }

  /**
   * Get color for BPMN element type
   */
  private getBpmnColor(type: string): string {
    const typeUpper = type.toUpperCase()
    for (const [key, color] of Object.entries(BPMN_COLORS)) {
      if (typeUpper.includes(key)) {
        return color
      }
    }
    return '#999999'
  }

  /**
   * Map BPMN type to 3D node type
   */
  private mapBpmnType(type: string): Node3D['type'] {
    const typeUpper = type.toUpperCase()

    if (typeUpper.includes('GATEWAY')) return 'gateway'
    if (typeUpper.includes('EVENT')) return 'event'
    if (typeUpper.includes('PROCESS')) return 'subprocess'
    return 'task'
  }

  /**
   * Enable VR mode
   */
  async enableVrMode(): Promise<void> {
    if (!this.isVrSupported) {
      console.warn('WebXR VR not supported on this device')
      return
    }

    try {
      // In production:
      // const session = await navigator.xr?.requestSession('immersive-vr', {
      //   requiredFeatures: ['local-floor'],
      //   optionalFeatures: ['bounded-floor', 'hand-tracking']
      // })

      this.vrSession = {}
      console.log('VR mode enabled')
    } catch (error) {
      console.error('Failed to enable VR:', error)
    }
  }

  /**
   * Exit VR mode
   */
  async exitVrMode(): Promise<void> {
    if (this.vrSession) {
      // await this.vrSession.end()
      this.vrSession = null
      console.log('VR mode exited')
    }
  }

  /**
   * Animate scene
   */
  animate(callback?: () => void): void {
    const animate = () => {
      this.animationFrameId = requestAnimationFrame(animate)

      // Update mesh rotations
      this.meshes.forEach((mesh) => {
        if (mesh.userData?.type === 'code-block') {
          mesh.rotation.x += 0.001
          mesh.rotation.y += 0.002
        }
      })

      // Render scene
      // this.renderer.render(this.scene, this.camera)

      callback?.()
    }

    animate()
  }

  /**
   * Stop animation
   */
  stopAnimation(): void {
    if (this.animationFrameId !== null) {
      cancelAnimationFrame(this.animationFrameId)
      this.animationFrameId = null
    }
  }

  /**
   * Reset view to default camera position
   */
  resetView(): void {
    // this.camera.position.set(0, 0, 10)
    // this.camera.lookAt(0, 0, 0)
    console.log('View reset to default')
  }

  /**
   * Zoom to fit all nodes
   */
  zoomToFit(): void {
    // Calculate bounding box of all nodes
    let minX = Infinity,
      maxX = -Infinity
    let minY = Infinity,
      maxY = -Infinity
    let minZ = Infinity,
      maxZ = -Infinity

    this.nodes.forEach((node) => {
      const [x, y, z] = node.position
      minX = Math.min(minX, x)
      maxX = Math.max(maxX, x)
      minY = Math.min(minY, y)
      maxY = Math.max(maxY, y)
      minZ = Math.min(minZ, z)
      maxZ = Math.max(maxZ, z)
    })

    // Adjust camera to view all nodes
    const centerX = (minX + maxX) / 2
    const centerY = (minY + maxY) / 2
    const centerZ = (minZ + maxZ) / 2

    const sizeX = maxX - minX
    const sizeY = maxY - minY
    const sizeZ = maxZ - minZ
    const maxSize = Math.max(sizeX, sizeY, sizeZ)

    console.log(`Zooming to fit: ${this.nodes.size} nodes`)
  }

  /**
   * Export scene as JSON
   */
  exportAsJson(): string {
    const data = {
      nodes: Array.from(this.nodes.values()),
      edges: Array.from(this.edges.values()),
      timestamp: new Date().toISOString(),
    }

    return JSON.stringify(data, null, 2)
  }

  /**
   * Export scene as GLTF (3D format)
   */
  async exportAsGltf(): Promise<ArrayBuffer> {
    // In production, use THREE.GLTFExporter
    // const exporter = new GLTFExporter()
    // return new Promise((resolve) => {
    //   exporter.parse(this.scene, resolve)
    // })

    const mockBuffer = new ArrayBuffer(1024)
    return mockBuffer
  }

  /**
   * Get scene statistics
   */
  getStatistics() {
    return {
      nodeCount: this.nodes.size,
      edgeCount: this.edges.size,
      meshCount: this.meshes.size,
      vrSupported: this.isVrSupported,
      vrActive: this.vrSession !== null,
    }
  }

  /**
   * Dispose resources
   */
  dispose(): void {
    this.stopAnimation()

    // Dispose Three.js resources
    this.meshes.forEach((mesh) => {
      // mesh.geometry.dispose()
      // mesh.material.dispose()
    })

    // this.renderer.dispose()

    this.nodes.clear()
    this.edges.clear()
    this.meshes.clear()

    console.log('3D visualization disposed')
  }
}

/**
 * Visualization Presets for common scenarios
 */
export const Visualization3DPresets = {
  /**
   * Process Flow visualization - optimized for BPMN
   */
  processFlow: (config: Scene3DConfig): Scene3DConfig => ({
    ...config,
    lightingPreset: 'studio',
    gridEnabled: true,
    cameraDistance: 15,
  }),

  /**
   * Code Structure visualization - optimized for code
   */
  codeStructure: (config: Scene3DConfig): Scene3DConfig => ({
    ...config,
    lightingPreset: 'dark',
    gridEnabled: false,
    cameraDistance: 12,
  }),

  /**
   * VR Experience visualization - optimized for immersive
   */
  vrExperience: (config: Scene3DConfig): Scene3DConfig => ({
    ...config,
    vrEnabled: true,
    lightingPreset: 'bright',
    autoRotate: false,
    cameraDistance: 8,
  }),

  /**
   * Presentation visualization - optimized for demos
   */
  presentation: (config: Scene3DConfig): Scene3DConfig => ({
    ...config,
    lightingPreset: 'default',
    autoRotate: true,
    cameraDistance: 10,
  }),
}
