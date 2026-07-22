import CrownVerify

#check MFW.Crown.traceEquiv_length_eq
#check MFW.Crown.BehaviorSwapEquiv.events
#check MFW.Crown.SemanticIndependence.lawful_iff
#check MFW.Crown.crown_forward
#check MFW.Crown.crown_reverse
#check MFW.Crown.crown_kernel_iff
#check MFW.Crown.canonicalTau_surjective
#check MFW.Crown.eventOnlyTraceTransport_false
#check MFW.Crown.arbitraryTransformationKernel_false

def main : IO Unit :=
  IO.println "independent MFW Crown verifier loaded"
