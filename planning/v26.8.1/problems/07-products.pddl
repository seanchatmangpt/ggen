(define (problem ggen-v2681-products)
 (:domain ggen-v2681-core)
 (:objects cli defaults config lsp diagnostics marketplace pack-kernel bblock lockfile protocols - subsystem
  v-products - verifier r-products - receipt)
 (:init (declared cli) (declared defaults) (declared config) (declared lsp)
  (declared diagnostics) (declared marketplace) (declared pack-kernel)
  (declared bblock) (declared lockfile) (declared protocols) (= (total-cost) 0))
 (:goal (forall (?s - subsystem) (admitted ?s)))
 (:metric minimize (total-cost)))
