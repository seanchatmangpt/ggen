/**
 * Swarm Intelligence Algorithms
 * PSO, ACO, Genetic Algorithms, Emergent Behaviors for Agent Swarms
 *
 * 2028+ Innovation: Collective Intelligence, Emergent Optimization
 */

/**
 * Particle (Agent) position and state for PSO
 */
export interface Particle {
  id: string
  position: number[]
  velocity: number[]
  bestPosition: number[]
  bestValue: number
  currentValue: number
  fitness: number
}

/**
 * Pheromone trail for ACO
 */
export interface PheromoneTrail {
  from: string
  to: string
  intensity: number
  timestamp: Date
  age: number
}

/**
 * Particle Swarm Optimization
 * Inspired by bird flocking for distributed optimization
 */
export class ParticleSwarmOptimization {
  private particles: Map<string, Particle> = new Map()
  private globalBestPosition: number[] = []
  private globalBestValue: number = -Infinity
  private iterations: number = 0
  private inertia: number = 0.7
  private cognitiveWeight: number = 1.5
  private socialWeight: number = 1.5
  private bounds: [number, number] = [-100, 100]

  constructor(
    agentIds: string[],
    dimensions: number = 5,
    iterationLimit: number = 100
  ) {
    // Initialize particles
    agentIds.forEach((id) => {
      const position = Array(dimensions)
        .fill(0)
        .map(() => Math.random() * (this.bounds[1] - this.bounds[0]) + this.bounds[0])

      this.particles.set(id, {
        id,
        position,
        velocity: Array(dimensions).fill(0).map(() => Math.random() - 0.5),
        bestPosition: [...position],
        bestValue: -Infinity,
        currentValue: this.evaluateFitness(position),
        fitness: 0,
      })
    })
  }

  /**
   * Fitness evaluation function
   */
  private evaluateFitness(position: number[]): number {
    // Sphere function: minimize sum of squares
    return -position.reduce((sum, x) => sum + x * x, 0)
  }

  /**
   * Update particle velocity and position
   */
  private updateParticle(particle: Particle): void {
    const dimensions = particle.position.length

    for (let d = 0; d < dimensions; d++) {
      // Velocity update
      const cognitive = this.cognitiveWeight * Math.random() * (particle.bestPosition[d] - particle.position[d])
      const social = this.socialWeight * Math.random() * (this.globalBestPosition[d] - particle.position[d])

      particle.velocity[d] = this.inertia * particle.velocity[d] + cognitive + social

      // Clamp velocity
      particle.velocity[d] = Math.max(-5, Math.min(5, particle.velocity[d]))

      // Position update
      particle.position[d] += particle.velocity[d]

      // Clamp position
      particle.position[d] = Math.max(this.bounds[0], Math.min(this.bounds[1], particle.position[d]))
    }
  }

  /**
   * Execute one PSO iteration
   */
  iterate(): void {
    // Evaluate all particles
    for (const [, particle] of this.particles) {
      particle.currentValue = this.evaluateFitness(particle.position)

      // Update personal best
      if (particle.currentValue > particle.bestValue) {
        particle.bestValue = particle.currentValue
        particle.bestPosition = [...particle.position]
      }

      // Update global best
      if (particle.currentValue > this.globalBestValue) {
        this.globalBestValue = particle.currentValue
        this.globalBestPosition = [...particle.position]
      }
    }

    // Update velocities and positions
    for (const [, particle] of this.particles) {
      this.updateParticle(particle)
    }

    this.iterations++
  }

  /**
   * Run optimization
   */
  optimize(iterations: number): { bestValue: number; bestPosition: number[] } {
    for (let i = 0; i < iterations; i++) {
      this.iterate()
    }

    return {
      bestValue: this.globalBestValue,
      bestPosition: this.globalBestPosition,
    }
  }

  /**
   * Get current best solution
   */
  getBestSolution(): { value: number; position: number[] } {
    return {
      value: this.globalBestValue,
      position: this.globalBestPosition,
    }
  }

  /**
   * Get particle fitness
   */
  getParticleFitness(agentId: string): number {
    const particle = this.particles.get(agentId)
    return particle ? particle.currentValue : 0
  }

  /**
   * Get convergence status
   */
  getConvergenceStatus(): { converged: boolean; iterations: number; improvement: number } {
    return {
      converged: this.iterations > 50,
      iterations: this.iterations,
      improvement: this.globalBestValue,
    }
  }
}

/**
 * Ant Colony Optimization
 * Inspired by ant pheromone trails for path finding
 */
export class AntColonyOptimization {
  private pheromoneMatrix: Map<string, PheromoneTrail[]> = new Map()
  private edges: Map<string, number> = new Map() // Distances/costs
  private ants: Map<string, string[]> = new Map() // Current paths
  private iterations: number = 0
  private evaporationRate: number = 0.1
  private pheromoneWeight: number = 2.0
  private heuristicWeight: number = 1.0

  constructor(nodeIds: string[]) {
    // Initialize pheromone trails between all nodes
    for (let i = 0; i < nodeIds.length; i++) {
      for (let j = i + 1; j < nodeIds.length; j++) {
        const edgeKey = `${nodeIds[i]}->${nodeIds[j]}`
        this.edges.set(edgeKey, Math.random() * 10 + 1) // Random distance 1-11

        // Initialize pheromone trails
        this.pheromoneMatrix.set(edgeKey, [
          {
            from: nodeIds[i],
            to: nodeIds[j],
            intensity: 1.0,
            timestamp: new Date(),
            age: 0,
          },
        ])
      }
    }
  }

  /**
   * Construct solution using pheromone and heuristic information
   */
  constructSolution(antId: string, startNode: string): { path: string[]; distance: number } {
    const path: string[] = [startNode]
    let currentNode = startNode
    let totalDistance = 0
    const visited = new Set<string>([startNode])

    // Continue until all nodes visited
    while (visited.size < Array.from(this.edges.keys()).length + 1) {
      let bestNextNode = null
      let bestScore = -Infinity

      // Evaluate all unvisited neighbors
      for (const [edgeKey] of this.edges) {
        const [from, to] = edgeKey.split('->')
        if (from === currentNode && !visited.has(to)) {
          const trails = this.pheromoneMatrix.get(edgeKey) || []
          const pheromone = trails.length > 0 ? trails[trails.length - 1].intensity : 1.0
          const distance = this.edges.get(edgeKey) || 1
          const heuristic = 1 / distance

          const score = Math.pow(pheromone, this.pheromoneWeight) * Math.pow(heuristic, this.heuristicWeight)

          if (score > bestScore) {
            bestScore = score
            bestNextNode = to
          }
        }
      }

      if (bestNextNode) {
        path.push(bestNextNode)
        visited.add(bestNextNode)
        const edgeKey = `${currentNode}->${bestNextNode}`
        totalDistance += this.edges.get(edgeKey) || 1
        currentNode = bestNextNode
      } else {
        break
      }
    }

    this.ants.set(antId, path)
    return { path, distance: totalDistance }
  }

  /**
   * Update pheromone trails based on ant solutions
   */
  updatePheromones(solutions: { path: string[]; distance: number }[]): void {
    // Evaporate existing pheromones
    for (const [, trails] of this.pheromoneMatrix) {
      trails.forEach((trail) => {
        trail.intensity *= 1 - this.evaporationRate
        trail.age++
      })
    }

    // Deposit pheromone from good solutions
    solutions.forEach((solution) => {
      const reward = 1 / solution.distance

      for (let i = 0; i < solution.path.length - 1; i++) {
        const from = solution.path[i]
        const to = solution.path[i + 1]
        const edgeKey = `${from}->${to}`

        if (this.pheromoneMatrix.has(edgeKey)) {
          const trails = this.pheromoneMatrix.get(edgeKey)!
          trails[trails.length - 1].intensity += reward
        }
      }
    })
  }

  /**
   * Run one ACO iteration
   */
  iterate(antCount: number): number {
    const solutions: { path: string[]; distance: number }[] = []

    // Construct solutions
    const nodeIds = Array.from(new Set(Array.from(this.edges.keys()).flatMap((e) => e.split('->'))))

    for (let i = 0; i < antCount; i++) {
      const startNode = nodeIds[Math.floor(Math.random() * nodeIds.length)]
      const solution = this.constructSolution(`ant-${this.iterations}-${i}`, startNode)
      solutions.push(solution)
    }

    // Update pheromones
    this.updatePheromones(solutions)

    this.iterations++

    // Return best distance found
    return Math.min(...solutions.map((s) => s.distance))
  }

  /**
   * Run optimization
   */
  optimize(iterations: number, antCount: number = 10): number {
    let bestDistance = Infinity

    for (let i = 0; i < iterations; i++) {
      const distance = this.iterate(antCount)
      bestDistance = Math.min(bestDistance, distance)
    }

    return bestDistance
  }

  /**
   * Get iteration count
   */
  getIterationCount(): number {
    return this.iterations
  }

  /**
   * Get pheromone intensity on edge
   */
  getPheromoneIntensity(from: string, to: string): number {
    const edgeKey = `${from}->${to}`
    const trails = this.pheromoneMatrix.get(edgeKey)

    if (trails && trails.length > 0) {
      return trails[trails.length - 1].intensity
    }
    return 0
  }
}

/**
 * Genetic Algorithm for Agent Population Evolution
 */
export class GeneticAlgorithm {
  private population: Map<string, { genes: number[]; fitness: number }> = new Map()
  private generationCount: number = 0
  private mutationRate: number = 0.05
  private crossoverRate: number = 0.8
  private fitnessThreshold: number = 0.8

  constructor(agentIds: string[], geneLength: number = 10) {
    // Initialize population with random genes
    agentIds.forEach((id) => {
      const genes = Array(geneLength)
        .fill(0)
        .map(() => Math.random())

      this.population.set(id, {
        genes,
        fitness: this.evaluateFitness(genes),
      })
    })
  }

  /**
   * Evaluate fitness of genes
   */
  private evaluateFitness(genes: number[]): number {
    // Maximize sum of genes (simple fitness)
    const sum = genes.reduce((a, b) => a + b, 0)
    return sum / genes.length
  }

  /**
   * Select parent for reproduction
   */
  private selectParent(): { genes: number[]; fitness: number } {
    // Tournament selection
    const tournamentSize = 3
    let best = null
    let bestFitness = -Infinity

    for (let i = 0; i < tournamentSize; i++) {
      const individual = Array.from(this.population.values())[Math.floor(Math.random() * this.population.size)]

      if (individual.fitness > bestFitness) {
        best = individual
        bestFitness = individual.fitness
      }
    }

    return best!
  }

  /**
   * Crossover two parents
   */
  private crossover(parent1: number[], parent2: number[]): number[] {
    if (Math.random() > this.crossoverRate) {
      return [...parent1]
    }

    const crossoverPoint = Math.floor(Math.random() * parent1.length)
    return [...parent1.slice(0, crossoverPoint), ...parent2.slice(crossoverPoint)]
  }

  /**
   * Mutate genes
   */
  private mutate(genes: number[]): number[] {
    return genes.map((gene) => {
      if (Math.random() < this.mutationRate) {
        return Math.random()
      }
      return gene
    })
  }

  /**
   * Evolve population one generation
   */
  evolve(): void {
    const newPopulation: Map<string, { genes: number[]; fitness: number }> = new Map()

    // Elitism: keep best individual
    let bestId = ''
    let bestFitness = -Infinity
    for (const [id, individual] of this.population) {
      if (individual.fitness > bestFitness) {
        bestFitness = individual.fitness
        bestId = id
      }
    }

    const bestIndividual = this.population.get(bestId)!
    newPopulation.set(bestId, bestIndividual)

    // Generate offspring
    for (let i = 1; i < this.population.size; i++) {
      const parent1 = this.selectParent()
      const parent2 = this.selectParent()
      const childGenes = this.crossover(parent1.genes, parent2.genes)
      const mutatedGenes = this.mutate(childGenes)
      const childId = `gen-${this.generationCount}-${i}`

      newPopulation.set(childId, {
        genes: mutatedGenes,
        fitness: this.evaluateFitness(mutatedGenes),
      })
    }

    this.population = newPopulation
    this.generationCount++
  }

  /**
   * Run evolution
   */
  runEvolution(generations: number): { bestGenes: number[]; bestFitness: number } {
    for (let i = 0; i < generations; i++) {
      this.evolve()
    }

    let best = null
    let bestFitness = -Infinity

    for (const individual of this.population.values()) {
      if (individual.fitness > bestFitness) {
        best = individual
        bestFitness = individual.fitness
      }
    }

    return {
      bestGenes: best?.genes || [],
      bestFitness: best?.fitness || 0,
    }
  }

  /**
   * Get population statistics
   */
  getStats(): {
    generationCount: number
    populationSize: number
    averageFitness: number
    bestFitness: number
  } {
    const fitnesses = Array.from(this.population.values()).map((ind) => ind.fitness)
    const avgFitness = fitnesses.reduce((a, b) => a + b, 0) / fitnesses.length
    const bestFitness = Math.max(...fitnesses)

    return {
      generationCount: this.generationCount,
      populationSize: this.population.size,
      averageFitness: avgFitness,
      bestFitness,
    }
  }
}

/**
 * Swarm Behavior Analyzer
 * Detects and measures emergent behaviors
 */
export class SwarmBehaviorAnalyzer {
  private behaviorPatterns: Map<string, number> = new Map()
  private measurements: any[] = []

  /**
   * Measure synchronization in swarm
   */
  measureSynchronization(agentStates: any[]): number {
    // Measure how synchronized agents are
    if (agentStates.length === 0) return 0

    const positions = agentStates.map((s) => s.position || 0)
    const mean = positions.reduce((a, b) => a + b, 0) / positions.length
    const variance = positions.reduce((sum, p) => sum + Math.pow(p - mean, 2), 0) / positions.length

    // Lower variance = higher synchronization
    const synchronization = Math.max(0, 1 - variance / 100)

    this.behaviorPatterns.set('synchronization', synchronization)
    return synchronization
  }

  /**
   * Measure cohesion in swarm
   */
  measureCohesion(agentPositions: number[][]): number {
    if (agentPositions.length < 2) return 1

    let totalDistance = 0
    for (let i = 0; i < agentPositions.length; i++) {
      for (let j = i + 1; j < agentPositions.length; j++) {
        const distance = Math.sqrt(
          agentPositions[i].reduce((sum, coord, k) => sum + Math.pow(coord - agentPositions[j][k], 2), 0)
        )
        totalDistance += distance
      }
    }

    const avgDistance = totalDistance / ((agentPositions.length * (agentPositions.length - 1)) / 2)
    const cohesion = Math.max(0, 1 - avgDistance / 100)

    this.behaviorPatterns.set('cohesion', cohesion)
    return cohesion
  }

  /**
   * Measure diversity in swarm
   */
  measureDiversity(agentCapabilities: Map<string, string[]>): number {
    const allCapabilities = new Set<string>()
    const capabilityDensity = new Map<string, number>()

    for (const capabilities of agentCapabilities.values()) {
      capabilities.forEach((cap) => {
        allCapabilities.add(cap)
        capabilityDensity.set(cap, (capabilityDensity.get(cap) || 0) + 1)
      })
    }

    // Diversity is measured by distribution of capabilities
    const densities = Array.from(capabilityDensity.values())
    const evenness = densities.length > 0 ? densities.length / allCapabilities.size : 0

    this.behaviorPatterns.set('diversity', evenness)
    return evenness
  }

  /**
   * Measure emergence indicator
   */
  measureEmergence(measurements: { synchronization: number; cohesion: number; diversity: number }): number {
    // Emergence = balance between organization and diversity
    const organization = (measurements.synchronization + measurements.cohesion) / 2
    const emergence = (organization * 0.4 + measurements.diversity * 0.6) * 100

    this.behaviorPatterns.set('emergence', emergence)
    return emergence
  }

  /**
   * Get all behavior patterns
   */
  getBehaviorPatterns(): Map<string, number> {
    return new Map(this.behaviorPatterns)
  }
}
