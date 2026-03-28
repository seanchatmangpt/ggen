# VIRTUAL THREAD DAO — Java 26 Pattern Language #15

> "Give each database call its own virtual thread and let the JVM scheduler
> handle the blocking — no pool sizing, no back-pressure tuning, no exhaustion."

---

## Context

You are building a Java 26 service that executes JDBC queries against a
relational database.  You have configured a connection pool (HikariCP, c3p0, or
similar) and wrapped it in a `DataSource`.  Traditional DAO classes call
`DataSource.getConnection()` on a platform thread.  Under moderate concurrency
each blocked JDBC call holds a carrier thread, and the platform-thread pool
becomes the bottleneck long before the database does.

---

## Problem

**Thread pool exhaustion under load causes cascading failures.**

- A conventional thread pool of N threads can service at most N simultaneous
  blocking JDBC calls.  Once all N threads are waiting on the network, new
  requests queue, latency climbs, and the service appears unresponsive.
- Increasing N moves the bottleneck to memory and OS scheduling overhead.
- Connection pool sizing becomes a delicate, environment-specific knob that is
  easy to misconfigure and impossible to auto-tune without careful profiling.
- The root cause is the 1-to-1 mapping between a blocking call and an OS thread:
  a thread that blocks on I/O contributes nothing while consuming ~1 MB of stack.

---

## Solution

**Use `Executors.newVirtualThreadPerTaskExecutor()` for every DAO operation.**

Virtual threads (JEP 444, shipped in Java 21, fully stabilised in Java 26) are
JVM-managed, heap-allocated continuations.  When a virtual thread blocks on a
socket read (i.e., waiting for the database to respond), the JVM unmounts it
from its carrier thread and parks it on the heap.  The carrier thread is
immediately free to run another virtual thread.  A service with 10 000
concurrent requests needs only as many carrier threads as the hardware has cores
— not 10 000 OS threads.

The pattern wraps each DAO method body in a `Future` submitted to the virtual
thread executor and synchronously awaits the result with `Future.get()`.  This
keeps the public API synchronous (no reactive streams, no `CompletableFuture`
propagation through the call stack) while gaining all the throughput benefits of
non-blocking I/O.

---

## Use This Pattern

**Step 1 — Declare the aggregate root in your ontology.**

```turtle
my:Order a java26:AggregateRoot ;
    java26:name "Order" ;
    java26:javaPackage "com.example.orders" ;
    java26:dataAccess java26:VirtualThreadDAO ;
    java26:field my:orderId, my:customerId, my:totalAmount .
```

**Step 2 — Run the generator.**

```bash
ggen sync
```

ggen queries `ontology/domain.ttl` via `queries/extract-daos.rq`, renders
`templates/VirtualThreadDAO.java.tera`, and writes the DAO to
`src/main/java/com/example/orders/OrderDAO.java`.

**Step 3 — Fill in the SQL stubs.**

The generated `insert()`, `update()`, and `map()` methods contain `TODO`
markers.  Add the column bindings that match your schema, e.g.:

```java
private Order insert(Order entity) throws SQLException {
    try (Connection conn = dataSource.getConnection();
         PreparedStatement ps = conn.prepareStatement(
                 "INSERT INTO order (id, customer_id, total_amount) VALUES (?,?,?)")) {
        ps.setObject(1, entity.getId());
        ps.setObject(2, entity.getCustomerId());
        ps.setBigDecimal(3, entity.getTotalAmount());
        ps.executeUpdate();
        return entity;
    }
}
```

**Step 4 — Inject the DAO and call it.**

```java
@Bean
OrderDAO orderDAO(DataSource ds) { return new OrderDAO(ds); }

// In a service or controller:
Optional<Order> order = orderDAO.findById(orderId);
```

No thread-pool tuning required.  Virtual threads scale with the number of
concurrent callers, bounded only by heap memory and the connection pool size.

---

## Consequences

**Benefits**

- No thread pool exhaustion: thousands of concurrent JDBC calls block only on
  heap-allocated continuations, not OS threads.
- Connection pool remains the only back-pressure point — size it to match
  database capacity, not platform thread count.
- Synchronous, readable code — no reactive operators, no `async`/`await` style
  chains, no callback hell.
- `@PreDestroy close()` shuts the executor down cleanly, draining in-flight
  tasks before the container stops.

**Liabilities**

- Requires a virtual-thread-friendly JDBC driver.  Drivers that use
  `synchronized` blocks internally (some older versions of Oracle JDBC, MySQL
  Connector/J < 9) may pin carrier threads, negating the throughput benefit.
  Verify with `-Djdk.tracePinnedThreads=full`.
- The `Future.get()` call re-throws `ExecutionException`; callers must handle
  (or wrap) checked `SQLException`s that surface as `RuntimeException`.
- `findById` inside `save` adds a round-trip; batch-save workloads should use a
  dedicated `saveAll` method with a single virtual thread for the entire batch.
- This pattern is not a substitute for reactive I/O when true back-pressure
  propagation is required (e.g., streaming large result sets to a slow client).

---

## Related Patterns

| Pattern | Number | Relationship |
|---------|--------|--------------|
| JPA ENTITY MAPPING | #13 | Alternative persistence strategy; use when you want ORM lifecycle management instead of raw JDBC. |
| SPRING DATA REPOSITORY | #14 | Higher-level abstraction over this pattern; auto-generates queries from method names. |
| STRUCTURED CONCURRENCY SCOPE | #24 | Composable sibling of this pattern; use `StructuredTaskScope` when you need fan-out with failure propagation across multiple concurrent DAO calls. |

---

*Generated by ggen v6.0.0 — edit `ontology/domain.ttl` to regenerate.*
