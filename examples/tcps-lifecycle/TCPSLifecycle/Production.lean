import Std
import TCPSLifecycle.Autonomics

namespace TCPSLifecycle.Production

structure Measurement where
  value : Nat
  unitId : Nat
  windowSeconds : Nat
  windowPositive : windowSeconds > 0

structure StateVector where
  demand : Measurement
  errorBasisPoints : Nat
  latencyMicros : Nat
  receiptGap : Nat

structure RateVector where
  demandRateMilli : Int
  errorRateMilli : Int
  latencyRateMilli : Int

structure SafeRegion where
  maxErrorBasisPoints : Nat
  maxLatencyMicros : Nat
  maxReceiptGap : Nat
  maxErrorRateMilli : Int
  maxLatencyRateMilli : Int


def InRegion (region : SafeRegion) (state : StateVector) (rate : RateVector) : Prop :=
  state.errorBasisPoints ≤ region.maxErrorBasisPoints ∧
  state.latencyMicros ≤ region.maxLatencyMicros ∧
  state.receiptGap ≤ region.maxReceiptGap ∧
  rate.errorRateMilli ≤ region.maxErrorRateMilli ∧
  rate.latencyRateMilli ≤ region.maxLatencyRateMilli

inductive Action where
  | partitionDemand
  | enablePullShards
  | requireIndependentOracle
  | requireProofCarryingReceipts
  | stageCanaryRollback
  deriving DecidableEq, Repr

structure Topology where
  generation : Nat
  shards : Nat
  pullEnabled : Bool
  oracleRequired : Bool
  proofReceiptsRequired : Bool
  canaryPercent : Nat
  rollbackArmed : Bool
  deriving DecidableEq, Repr


def initialTopology : Topology :=
  { generation := 0
    shards := 1
    pullEnabled := false
    oracleRequired := false
    proofReceiptsRequired := false
    canaryPercent := 0
    rollbackArmed := false }


def phaseActions : List Action :=
  [ .partitionDemand
  , .enablePullShards
  , .requireIndependentOracle
  , .requireProofCarryingReceipts
  , .stageCanaryRollback
  ]


def phaseTopology (current : Topology) (observedDemand : Nat) : Topology :=
  { generation := current.generation + 1
    shards := max 2 ((observedDemand + 999) / 1000)
    pullEnabled := true
    oracleRequired := true
    proofReceiptsRequired := true
    canaryPercent := 5
    rollbackArmed := true }


def IsPhaseShift (baseline observed : Measurement) : Prop :=
  baseline.value > 0 ∧
  baseline.unitId = observed.unitId ∧
  baseline.windowSeconds = observed.windowSeconds ∧
  observed.value ≥ baseline.value * 1000

structure Capability where
  planDigest : String
  selector : String
  authorizer : String
  executor : String
  nonce : Nat
  epoch : Nat
  expiresAt : Nat


def DistinctAuthorities (cap : Capability) : Prop :=
  cap.selector ≠ cap.authorizer ∧
  cap.selector ≠ cap.executor ∧
  cap.authorizer ≠ cap.executor

structure Receipt where
  planDigest : String
  previousReceipt : String
  topologyBefore : String
  topologyAfter : String
  nonce : Nat
  receiptDigest : String


def Extends (previous : String) (receipt : Receipt) : Prop :=
  receipt.previousReceipt = previous


theorem phase_actions_are_exactly_bounded : phaseActions.length = 5 := by
  decide


theorem phase_topology_enables_all_safety_controls
    (current : Topology) (demand : Nat) :
    let next := phaseTopology current demand
    next.shards ≥ 2 ∧
    next.pullEnabled = true ∧
    next.oracleRequired = true ∧
    next.proofReceiptsRequired = true ∧
    next.canaryPercent = 5 ∧
    next.rollbackArmed = true := by
  simp [phaseTopology]


theorem exact_boundary_is_phase_shift
    (window : Nat) (hwindow : window > 0) :
    IsPhaseShift
      { value := 10, unitId := 1, windowSeconds := window, windowPositive := hwindow }
      { value := 10000, unitId := 1, windowSeconds := window, windowPositive := hwindow } := by
  simp [IsPhaseShift]


theorem below_boundary_is_not_phase_shift
    (window : Nat) (hwindow : window > 0) :
    ¬ IsPhaseShift
      { value := 10, unitId := 1, windowSeconds := window, windowPositive := hwindow }
      { value := 9999, unitId := 1, windowSeconds := window, windowPositive := hwindow } := by
  simp [IsPhaseShift]


theorem incompatible_units_refuse_phase_shift
    (window : Nat) (hwindow : window > 0) :
    ¬ IsPhaseShift
      { value := 1, unitId := 1, windowSeconds := window, windowPositive := hwindow }
      { value := 1000, unitId := 2, windowSeconds := window, windowPositive := hwindow } := by
  simp [IsPhaseShift]


theorem receipt_chain_is_not_reflexively_fabricated
    (prior : String) (receipt : Receipt)
    (h : Extends prior receipt) : receipt.previousReceipt = prior := by
  exact h

end TCPSLifecycle.Production
