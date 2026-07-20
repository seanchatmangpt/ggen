# ABI Contract

The C ABI is versioned independently from Rust's internal ABI.

```c
uint32_t tcps_version_v1(void);
tcps_outcome_v1 tcps_select_v1(tcps_request_v1 request);
```

All records use `#[repr(C)]` and fixed-width integers. No pointers cross the v1 boundary. This avoids ownership, allocator, lifetime, alignment-of-borrowed-data, and callback reentrancy ambiguity.

Outcome kind values:

- `1`: selected
- `2`: refused

The C header is canonical at `include/tcps.h`. ABI compatibility must be checked on every platform package before promotion.

