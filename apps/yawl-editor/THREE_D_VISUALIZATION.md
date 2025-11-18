# 3D Spatial Visualization with AR/VR Support

## Overview

This is a **2028 Innovation Phase** implementation of spatial 3D visualization for YAWL Editor, enabling interactive exploration of business processes and code structures in three-dimensional space with WebXR virtual reality support.

## Features

### 1. BPMN Process Visualization
- **Interactive 3D rendering** of BPMN process diagrams
- **Automatic layout** with node positioning algorithm
- **Color-coded elements**: Tasks (green), Gateways (orange), Events (blue), Subprocesses (purple)
- **Sequence flow connections** with 3D lines
- **Zoom to fit** functionality for exploring large processes
- **Auto-rotation** for presentation mode

### 2. Code Structure Visualization
- **Tree-based visualization** of code architecture
- **Hierarchical node positioning** with radial layout
- **Color coding**: Modules (blue), Classes (green), Functions (orange), Variables (purple), Imports (cyan)
- **Dependency graph** visualization
- **Layer-based rendering** for depth perception
- **Interactive node selection** and highlighting

### 3. AR/VR Integration
- **WebXR VR support** for immersive experiences
- **Hand-tracking ready** for gesture-based interaction
- **Multiple VR presets** (room-scale, sitting, standing)
- **VR controller support** for navigation
- **Spatial audio integration** ready
- **Haptic feedback** configuration

### 4. Real-Time Rendering
- **Three.js 3D engine** with 60 FPS target
- **Multiple lighting presets**: Default, Dark, Bright, Studio
- **Camera controls**: Orbit, Pan, Zoom
- **Smooth animations** and transitions
- **Performance optimization** for mobile VR

## Architecture

```
┌─────────────────────────────────────────────┐
│     3D Visualization UI Components          │
├─────────────────────────────────────────────┤
│  ThreeDVisualizer (React)                   │
│  CompactThreeDVisualizer                    │
│  3D Viewer Page (/three-d-viewer)           │
└────────────────────────┬────────────────────┘
                         │
┌────────────────────────▼────────────────────┐
│     Visualization3D Engine                  │
├─────────────────────────────────────────────┤
│  • Scene Management                         │
│  • Node/Edge Management                     │
│  • BPMN/Code Parsing                        │
│  • VR Session Management                    │
│  • Export Functionality                     │
└────────────────────────┬────────────────────┘
                         │
┌────────────────────────▼────────────────────┐
│     Three.js 3D Engine (npm: three)         │
├─────────────────────────────────────────────┤
│  • WebGL Rendering                          │
│  • Mesh/Geometry Management                 │
│  • Camera Control                           │
│  • Lighting & Materials                     │
│  • Animation Loop                           │
└─────────────────────────────────────────────┘
```

## Core Components

### Visualization3D Service (lib/three-d-visualization.ts)

Main engine providing scene management and visualization:

```typescript
// Initialize visualizer
const viz = new Visualization3D('canvas-id', {
  width: 1200,
  height: 800,
  backgroundColor: '#f5f5f5',
  lightingPreset: 'studio',
  vrEnabled: true,
})

// Add BPMN process
viz.addBpmnProcess(bpmnData)

// Add code structure
viz.addCodeStructure(codeAST)

// Enable VR
await viz.enableVrMode()

// Export
const json = viz.exportAsJson()
const gltf = await viz.exportAsGltf()
```

### ThreeDVisualizer Component (components/visualization/three-d-visualizer.tsx)

React wrapper with UI controls:

```typescript
<ThreeDVisualizer
  mode="process"      // 'process', 'code', 'vr', 'presentation'
  processData={data}
  height="600px"
  showStats={true}
  onExport={(json) => console.log(json)}
/>
```

### Scene Configuration

```typescript
interface Scene3DConfig {
  width: number
  height: number
  backgroundColor?: string
  cameraDistance?: number
  autoRotate?: boolean
  vrEnabled?: boolean
  gridEnabled?: boolean
  lightingPreset?: 'default' | 'dark' | 'bright' | 'studio'
}
```

## Visualization Modes

### 1. Process Mode
- **Purpose**: Visualize BPMN workflows
- **Layout**: Grid-based automatic positioning
- **Colors**: By element type
- **Interaction**: Free orbit camera
- **Use Case**: Process design, optimization, training

### 2. Code Mode
- **Purpose**: Explore code architecture
- **Layout**: Radial tree layout
- **Colors**: By code structure type
- **Interaction**: Perspective view with zoom
- **Use Case**: Code review, refactoring, onboarding

### 3. VR Mode
- **Purpose**: Immersive exploration
- **Layout**: Large spatial distribution
- **Colors**: Enhanced contrast for VR
- **Interaction**: Controller-based
- **Use Case**: Training, collaboration, presentations

### 4. Presentation Mode
- **Purpose**: Automated demonstrations
- **Layout**: Optimized for viewing
- **Colors**: High contrast
- **Interaction**: Auto-rotating, clickable
- **Use Case**: Demos, presentations, walkthroughs

## API Reference

### Visualization3D Class

#### Constructor
```typescript
new Visualization3D(canvasId: string, config: Scene3DConfig)
```

#### Methods

**Adding Content:**
```typescript
addBpmnProcess(process: any): void
addCodeStructure(codeAST: any): void
addNode(node: Node3D): void
addEdge(edge: Edge3D): void
```

**Camera Control:**
```typescript
resetView(): void
zoomToFit(): void
```

**Animation:**
```typescript
animate(callback?: () => void): void
stopAnimation(): void
```

**VR:**
```typescript
async enableVrMode(): Promise<void>
async exitVrMode(): Promise<void>
```

**Export:**
```typescript
exportAsJson(): string
async exportAsGltf(): Promise<ArrayBuffer>
```

**Management:**
```typescript
getStatistics(): { nodeCount, edgeCount, meshCount, vrSupported, vrActive }
dispose(): void
```

## Data Formats

### Node3D
```typescript
interface Node3D {
  id: string
  position: [x: number, y: number, z: number]
  size: number          // 0.5 - 2.0
  color: string         // hex color
  label: string
  type: 'task' | 'gateway' | 'event' | 'subprocess' | 'code-block'
  metadata?: Record<string, any>
}
```

### Edge3D
```typescript
interface Edge3D {
  id: string
  source: string        // node id
  target: string        // node id
  color?: string
  width?: number
  curved?: boolean
  metadata?: Record<string, any>
}
```

### BPMN Process Format
```typescript
{
  elements: [
    {
      id: 'task1',
      name: 'Task Name',
      type: 'TASK',
      outgoing: [{ targetRef: 'task2' }]
    },
    // more elements...
  ]
}
```

### Code AST Format
```typescript
{
  name: 'Module',
  type: 'module',
  children: [
    {
      name: 'MyClass',
      type: 'class',
      children: [
        { name: 'method1', type: 'function' }
      ]
    }
  ]
}
```

## Interaction Model

### Mouse Controls
- **Left Click + Drag**: Rotate camera around center
- **Right Click + Drag**: Pan camera
- **Scroll Wheel**: Zoom in/out
- **Double Click**: Focus on nearest node

### Touch Controls
- **Single Finger Drag**: Rotate camera
- **Two Finger Pinch**: Zoom
- **Two Finger Drag**: Pan camera

### VR Controls
- **Trigger Button**: Select/interact with objects
- **Grip Button**: Pan camera
- **Thumbstick**: Move/teleport
- **Menu Button**: Show VR menu

## Visualization Presets

### Process Flow
```typescript
Visualization3DPresets.processFlow(config)
// Lighting: Studio
// Grid: Enabled
// Camera Distance: 15
// Best for: BPMN diagrams, workflows
```

### Code Structure
```typescript
Visualization3DPresets.codeStructure(config)
// Lighting: Dark
// Grid: Disabled
// Camera Distance: 12
// Best for: Code architecture, dependencies
```

### VR Experience
```typescript
Visualization3DPresets.vrExperience(config)
// Lighting: Bright
// VR: Enabled
// Camera Distance: 8
// Best for: Immersive exploration, training
```

### Presentation
```typescript
Visualization3DPresets.presentation(config)
// Lighting: Default
// Auto-rotate: Enabled
// Camera Distance: 10
// Best for: Demos, presentations, walkthroughs
```

## Performance Optimization

### Rendering
- **LOD (Level of Detail)**: Auto-reduce detail at distance
- **Frustum Culling**: Skip off-screen objects
- **Instancing**: Render multiple objects as one
- **Shader Optimization**: Efficient materials

### Memory
- **Geometry Pooling**: Reuse geometries
- **Material Caching**: Share materials
- **Texture Compression**: WebP/ASTC
- **Memory Limits**: Remove objects at distance

### Mobile VR
- **Target**: 6 DOF headsets (Quest 2/3)
- **Resolution**: Optimized for headset display
- **Frame Rate**: 72-90 FPS target
- **Pass-through**: Optional for AR

## Browser & Device Support

### Desktop
- Chrome 90+ (WebGL 2.0)
- Firefox 88+
- Safari 15+ (limited WebXR)
- Edge 90+

### Mobile Web
- Chrome Android
- Firefox Android
- Samsung Internet
- Opera Mobile

### VR Headsets
- Meta Quest 2/3 (standalone + PC link)
- HTC Vive Focus 3
- PlayStation VR
- Windows Mixed Reality

## Integration Examples

### With BPMN Designer
```typescript
// In designer component
const viz = new Visualization3D('canvas')
viz.addBpmnProcess(diagram.asBpmn())
```

### With Code Editor
```typescript
// In code editor
const ast = parser.parse(code)
viz.addCodeStructure(ast)
```

### With Collaboration
```typescript
// Real-time visualization updates
collab.on('diagram:updated', (data) => {
  viz.addBpmnProcess(data)
})
```

## Export Formats

### JSON Export
```json
{
  "nodes": [
    {
      "id": "task1",
      "position": [0, 0, 0],
      "color": "#4CAF50",
      "label": "Task 1",
      "type": "task"
    }
  ],
  "edges": [
    {
      "id": "edge1",
      "source": "task1",
      "target": "task2"
    }
  ],
  "timestamp": "2024-01-01T00:00:00Z"
}
```

### GLTF Export
- Standard 3D format
- Includes geometries, materials, animations
- Compatible with AR/VR engines
- Portable across platforms

## Future Enhancements

### Q1 2028
- [ ] Real-time collaboration visualization
- [ ] 3D annotation system
- [ ] Measurement tools
- [ ] Performance profiling view

### Q2 2028
- [ ] AR (Augmented Reality) support
- [ ] Spatial audio integration
- [ ] Hand-tracking gestures
- [ ] Avatar support for multiplayer

### Q3 2028
- [ ] Physics simulation
- [ ] Particle effects
- [ ] Advanced lighting (ray-tracing ready)
- [ ] Cloud rendering option

### Q4 2028
- [ ] ML-based layout optimization
- [ ] Automated visual explanations
- [ ] Neural rendering
- [ ] Full-body IK tracking

## Troubleshooting

### WebGL Not Supported
```
Error: WebGL not supported
Solution: Use desktop browser or update to latest device OS
```

### VR Not Available
```
Error: VR not supported on this device
Solution: Use compatible WebXR device or desktop browser
```

### Performance Issues
```
Symptoms: Low FPS, stuttering
Solutions:
1. Reduce node/edge count
2. Enable LOD rendering
3. Use simpler lighting preset
4. Check browser console for errors
```

### Canvas Not Rendering
```
Symptom: Black/blank canvas
Solutions:
1. Check canvas element exists with correct ID
2. Verify Three.js is loaded
3. Check browser console for JavaScript errors
4. Ensure WebGL is enabled
```

## Best Practices

### Performance
1. **Limit nodes**: Keep below 5000 for good performance
2. **Simplify layout**: Use grid layout for processes
3. **Batch updates**: Group scene modifications
4. **Manage memory**: Dispose unused visualizations

### Usability
1. **Provide context**: Label important nodes
2. **Use color consistently**: Map to semantic meaning
3. **Responsive layout**: Adapt to container size
4. **Clear interaction**: Provide visual feedback

### VR Design
1. **Large objects**: Make grabbable objects sizeable
2. **Clear paths**: Ensure unobstructed navigation
3. **Visual queues**: Use color/lighting for guidance
4. **Safe fallback**: Provide 2D alternative

## References

- [Three.js Documentation](https://threejs.org/docs)
- [WebXR Device API](https://immersive-web.github.io/webxr/)
- [GLTF Specification](https://www.khronos.org/gltf/)
- [BPMN 2.0 Standard](https://www.omg.org/bpmn/)

---

**Status**: ✅ Production Ready (2028 Innovation Phase)
**Version**: 1.0.0
**Last Updated**: 2024
**Technology**: Three.js, WebXR, TypeScript
