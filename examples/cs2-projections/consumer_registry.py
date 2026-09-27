"""Registry over generated CS2 consumer projections."""
from dataclasses import dataclass

@dataclass(frozen=True)
class Consumer:
    work_id: str
    iri: str
    projection: str

class Registry:
    def __init__(self, consumers=()):
        self._items = {c.work_id: c for c in consumers}

    def add(self, consumer):
        if consumer.work_id in self._items and self._items[consumer.work_id] != consumer:
            raise ValueError("work id collision")
        self._items[consumer.work_id] = consumer
        return self

    def get(self, work_id):
        return self._items[work_id]

    def by_iri(self, iri):
        return tuple(sorted((c for c in self._items.values() if c.iri == iri), key=lambda c: c.work_id))

    def all(self):
        return tuple(sorted(self._items.values(), key=lambda c: (c.work_id, c.iri)))
