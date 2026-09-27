"""Store fixtures authored but not executed."""
from runtime.model import ConsumerProjection,Provenance
from runtime.store import ProjectionStore
def p(i=0): return ConsumerProjection("https://chatman.ai/cs2#RFC-CS2-001","CONSTRUCT",f"c{i}",f"w{i}","e",Provenance("x","0"*64))
def test_round_trip(tmp_path):
    store=ProjectionStore(tmp_path); x=p(); path=store.put(x); assert path.exists(); assert store.get(x.digest())==x
def test_idempotent_put(tmp_path):
    store=ProjectionStore(tmp_path); x=p(); assert store.put(x)==store.put(x)
def test_manifest_sorted(tmp_path):
    store=ProjectionStore(tmp_path); [store.put(p(i)) for i in range(4)]
    m=store.manifest(); assert m["count"]==4; assert m["digests"]==sorted(m["digests"])
