; ggen v26.8.1 planning projection covering numbered research documents 1-10
(define (problem ggen-v2681-governance-1-10)
 (:domain ggen-v2681-core)
 (:objects v2681 - release
  governance authority scope standing claims loss-law precedence generated-authority change-control release-decision risk-register documents-1-10 - subsystem
  crown - verifier r-governance - receipt)
 (:init
  (release-candidate v2681) (= (unresolved-count v2681) 10)
  (declared governance) (declared authority) (declared scope) (declared standing)
  (declared claims) (declared loss-law) (declared precedence) (declared generated-authority)
  (declared change-control) (declared release-decision) (declared risk-register)
  (declared documents-1-10)
  (= (coverage governance) 0) (= (coverage authority) 0) (= (coverage scope) 0)
  (= (coverage standing) 0) (= (coverage claims) 0) (= (coverage loss-law) 0)
  (= (coverage precedence) 0) (= (coverage generated-authority) 0)
  (= (coverage change-control) 0) (= (coverage release-decision) 0)
  (= (coverage risk-register) 0) (= (coverage documents-1-10) 0) (= (total-cost) 0))
 (:goal (and (admitted governance) (admitted authority) (admitted scope)
  (admitted standing) (admitted claims) (admitted loss-law) (admitted precedence)
  (admitted generated-authority) (admitted change-control) (admitted release-decision)
  (admitted risk-register) (admitted documents-1-10)))
 (:metric minimize (total-cost)))
