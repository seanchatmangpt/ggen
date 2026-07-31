(define (problem ggen-v2681-graph)
 (:domain ggen-v2681-core)
 (:objects rdf-authority oxigraph graphlaw sparql n3-datalog shacl shex imports graph-hash graph-delta - subsystem
  v-graph - verifier r-graph - receipt)
 (:init (declared rdf-authority) (declared oxigraph) (declared graphlaw) (declared sparql)
  (declared n3-datalog) (declared shacl) (declared shex) (declared imports)
  (declared graph-hash) (declared graph-delta) (= (total-cost) 0))
 (:goal (forall (?s - subsystem) (admitted ?s)))
 (:metric minimize (total-cost)))
