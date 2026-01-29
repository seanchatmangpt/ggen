# Tutorial 2: Building Your First Job Queue

**Learning Objective:** Create a production-ready job queue with custom business logic, priority handling, and comprehensive tests.

**Prerequisites:** Completed [Tutorial 1: Getting Started](01-getting-started.md)

**Outcome:** A fully-functional job queue with priority handling, retry logic, and Chicago TDD tests.

---

## What You'll Build

In this tutorial, you'll extend the basic job queue from Tutorial 1 to include:
- ✅ Priority-based job processing (high, normal, low)
- ✅ Automatic retry with exponential backoff
- ✅ Job deadlines and timeout handling
- ✅ Comprehensive Chicago TDD tests
- ✅ Prometheus metrics integration

**Time:** ~30 minutes

---

## Step 1: Extend the RDF Specification

First, extend the RDF ontology to include retry logic and deadlines.

Edit `.specify/specs/001-job-queue/feature.ttl`:

```turtle
@prefix jobs: <http://ggen.dev/ontology/jobs#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# JobQueue class with retry and deadline support
jobs:JobQueue a rdfs:Class ;
    rdfs:label "Job Queue" ;
    rdfs:comment "A priority-based job queue with pull semantics, retry logic, and deadlines" ;
    jobs:hasProperty jobs:priority ;
    jobs:hasProperty jobs:domain ;
    jobs:hasProperty jobs:maxRetries ;
    jobs:hasProperty jobs:retryBackoffMs ;
    jobs:hasBackend jobs:NATS .

jobs:maxRetries a rdf:Property ;
    rdfs:domain jobs:JobQueue ;
    rdfs:range xsd:integer ;
    rdfs:label "Maximum retry attempts" ;
    jobs:defaultValue 3 .

jobs:retryBackoffMs a rdf:Property ;
    rdfs:domain jobs:JobQueue ;
    rdfs:range xsd:integer ;
    rdfs:label "Initial retry backoff in milliseconds" ;
    jobs:defaultValue 1000 .

jobs:deadline a rdf:Property ;
    rdfs:domain jobs:Job ;
    rdfs:range xsd:integer ;
    rdfs:label "Job deadline in milliseconds (Unix timestamp)" .

jobs:Job a rdfs:Class ;
    rdfs:label "Job" ;
    jobs:hasProperty jobs:id ;
    jobs:hasProperty jobs:priority ;
    jobs:hasProperty jobs:domain ;
    jobs:hasProperty jobs:payload ;
    jobs:hasProperty jobs:deadline ;
    jobs:hasProperty jobs:retryCount ;
    jobs:hasProperty jobs:status .

jobs:status a rdf:Property ;
    rdfs:domain jobs:Job ;
    rdfs:range jobs:JobStatus ;
    rdfs:label "Current job status" .

jobs:JobStatus a rdfs:Class ;
    jobs:enumValues ("pending" "processing" "completed" "failed" "deadline_exceeded") .
```

**What changed?**
- Added `maxRetries` and `retryBackoffMs` to JobQueue
- Defined Job class with status tracking
- Added JobStatus enum with lifecycle states

---

## Step 2: Regenerate Code with New Specification

Run ggen sync to regenerate code from the updated RDF.

```bash
ggen sync --audit true

# Expected output:
# [μ₁] Normalizing RDF ontology... ✓ (52ms)
# [μ₂] Extracting entities via SPARQL... ✓ (28ms)
# [μ₃] Rendering templates (Tera)... ✓ (89ms)
# [μ₄] Canonicalizing outputs... ✓ (15ms)
# [μ₅] Generating receipt... ✓ (9ms)
#
# Updated 3 files, generated 2 new files:
#   src/job_queue.erl (updated)
#   src/job_worker.erl (updated)
#   src/job_status.erl (new)
#   test/job_retry_SUITE.erl (new)
#   test/job_deadline_SUITE.erl (new)
```

**What happened?** ggen detected changes in the RDF and:
- Updated existing modules with retry logic
- Generated a new `job_status.erl` module for status tracking
- Created new test suites for retry and deadline behavior

---

## Step 3: Examine Generated Retry Logic

Open `src/job_queue.erl` and find the retry logic:

```erlang
%% @doc Process a job with retry logic
process_job_with_retry(Job, State) ->
    JobId = maps:get(<<"id">>, Job),
    RetryCount = maps:get(<<"retry_count">>, Job, 0),
    MaxRetries = State#state.max_retries,

    case RetryCount >= MaxRetries of
        true ->
            %% Max retries exceeded, mark as failed
            update_job_status(JobId, failed, State),
            {error, max_retries_exceeded};
        false ->
            %% Attempt processing
            case process_job(Job, State) of
                {ok, Result} ->
                    update_job_status(JobId, completed, State),
                    {ok, Result};
                {error, Reason} ->
                    %% Schedule retry with exponential backoff
                    BackoffMs = calculate_backoff(RetryCount, State#state.retry_backoff_ms),
                    schedule_retry(Job, BackoffMs, State),
                    {error, {retry_scheduled, Reason}}
            end
    end.

%% @doc Calculate exponential backoff
calculate_backoff(RetryCount, InitialBackoffMs) ->
    %% Exponential backoff: initial * 2^retry_count
    %% Capped at 60 seconds
    Backoff = InitialBackoffMs * math:pow(2, RetryCount),
    min(Backoff, 60000).

%% @doc Schedule job retry
schedule_retry(Job, BackoffMs, State) ->
    JobId = maps:get(<<"id">>, Job),
    RetryCount = maps:get(<<"retry_count">>, Job, 0),

    %% Increment retry count
    UpdatedJob = maps:put(<<"retry_count">>, RetryCount + 1, Job),

    %% Schedule retry
    erlang:send_after(BackoffMs, self(), {retry_job, UpdatedJob}),

    %% Update metrics
    prometheus_counter:inc(jobs_retried_total),

    ok.
```

**Key features:**
- **Exponential backoff:** 1s → 2s → 4s → 8s → 16s → 32s → 60s (capped)
- **Max retries:** Configurable via RDF (default 3)
- **Metrics:** Prometheus counter for retried jobs
- **Status tracking:** Jobs transition through pending → processing → completed/failed

---

## Step 4: Examine Generated Deadline Handling

Find the deadline checking logic in `src/job_queue.erl`:

```erlang
%% @doc Check if job deadline is exceeded
is_deadline_exceeded(Job) ->
    case maps:get(<<"deadline">>, Job, undefined) of
        undefined ->
            false;  %% No deadline
        Deadline ->
            Now = erlang:system_time(millisecond),
            Now > Deadline
    end.

%% @doc Process job with deadline check
process_job(Job, State) ->
    case is_deadline_exceeded(Job) of
        true ->
            JobId = maps:get(<<"id">>, Job),
            update_job_status(JobId, deadline_exceeded, State),
            prometheus_counter:inc(jobs_deadline_exceeded_total),
            {error, deadline_exceeded};
        false ->
            %% Execute actual job processing
            execute_job(Job, State)
    end.

%% @doc Execute job processing logic
execute_job(Job, State) ->
    JobId = maps:get(<<"id">>, Job),
    Payload = maps:get(<<"payload">>, Job),

    StartTime = erlang:system_time(millisecond),

    %% Process job (this is where your business logic goes)
    Result = try
        process_payload(Payload, State)
    catch
        Error:Reason:Stacktrace ->
            logger:error("Job ~s failed: ~p:~p~n~p",
                        [JobId, Error, Reason, Stacktrace]),
            {error, {Error, Reason}}
    end,

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    %% Record metrics
    prometheus_histogram:observe(job_processing_duration_ms, Duration),

    Result.
```

**Key features:**
- **Deadline checking:** Before processing, check if deadline exceeded
- **Status tracking:** Jobs marked as `deadline_exceeded` if expired
- **Metrics:** Histogram for processing duration, counter for deadline violations
- **Error handling:** Catches exceptions during processing

---

## Step 5: Write Chicago TDD Tests

The generated code includes comprehensive tests. Examine `test/job_retry_SUITE.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc Job Retry Test Suite - Chicago TDD Style
%%%
%%% Chicago TDD: State-based testing with real collaborators, no mocks.
%%% AAA Pattern: Arrange → Act → Assert
%%%-------------------------------------------------------------------
-module(job_retry_SUITE).
-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_job_retries_on_failure/1,
    test_exponential_backoff/1,
    test_max_retries_exceeded/1,
    test_successful_retry/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_job_retries_on_failure,
        test_exponential_backoff,
        test_max_retries_exceeded,
        test_successful_retry
    ].

init_per_suite(Config) ->
    %% Start NATS (real collaborator, not mock)
    {ok, _} = application:ensure_all_started(gnat),

    %% Start application
    {ok, _} = application:ensure_all_started(my_erlang_jobs),

    Config.

end_per_suite(_Config) ->
    application:stop(my_erlang_jobs),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Reset metrics before each test
    job_queue:reset_metrics(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc TEST: Job retries on failure with exponential backoff
test_job_retries_on_failure(_Config) ->
    %% Arrange: Publish a job that will fail
    Payload = #{action => <<"fail_3_times">>},  %% Simulates failing job
    {ok, JobId} = job_queue:publish(high, test_domain, Payload, 60000),

    %% Act: Wait for retries to occur (3 retries with exponential backoff)
    %% 1st attempt: immediate
    %% 1st retry: 1s backoff
    %% 2nd retry: 2s backoff
    %% 3rd retry: 4s backoff
    %% Total: ~7-8 seconds
    timer:sleep(8000),

    %% Assert: Verify retry count
    Metrics = job_queue:get_metrics(),
    RetriedCount = maps:get(retried, Metrics, 0),
    ?assert(RetriedCount >= 3, "Expected at least 3 retries"),

    %% Assert: Verify final status is 'failed' (max retries exceeded)
    {ok, Status} = job_queue:get_job_status(JobId),
    ?assertEqual(failed, Status),

    ok.

%% @doc TEST: Exponential backoff timing is correct
test_exponential_backoff(_Config) ->
    %% Arrange: Publish failing job
    Payload = #{action => <<"fail_always">>},
    {ok, JobId} = job_queue:publish(high, test_domain, Payload, 60000),

    %% Act & Assert: Measure backoff intervals
    StartTime = erlang:system_time(millisecond),

    %% Wait for first retry (should be ~1s)
    timer:sleep(1100),
    {ok, RetryCount1} = job_queue:get_retry_count(JobId),
    Time1 = erlang:system_time(millisecond) - StartTime,
    ?assert(RetryCount1 >= 1, "First retry should have occurred"),
    ?assert(Time1 >= 1000 andalso Time1 =< 1500, "First retry at ~1s"),

    %% Wait for second retry (should be ~2s after first)
    timer:sleep(2100),
    {ok, RetryCount2} = job_queue:get_retry_count(JobId),
    Time2 = erlang:system_time(millisecond) - StartTime,
    ?assert(RetryCount2 >= 2, "Second retry should have occurred"),
    ?assert(Time2 >= 3000 andalso Time2 =< 3500, "Second retry at ~3s total"),

    ok.

%% @doc TEST: Max retries exceeded results in 'failed' status
test_max_retries_exceeded(_Config) ->
    %% Arrange: Configure max retries to 2
    application:set_env(my_erlang_jobs, max_retries, 2),

    Payload = #{action => <<"fail_always">>},
    {ok, JobId} = job_queue:publish(high, test_domain, Payload, 60000),

    %% Act: Wait for all retries (1s + 2s + processing time)
    timer:sleep(4000),

    %% Assert: Verify status is 'failed'
    {ok, Status} = job_queue:get_job_status(JobId),
    ?assertEqual(failed, Status),

    %% Assert: Verify retry count equals max retries
    {ok, RetryCount} = job_queue:get_retry_count(JobId),
    ?assertEqual(2, RetryCount),

    ok.

%% @doc TEST: Successful retry after transient failure
test_successful_retry(_Config) ->
    %% Arrange: Publish job that succeeds on 2nd retry
    Payload = #{action => <<"succeed_on_retry">>, retry_threshold => 2},
    {ok, JobId} = job_queue:publish(high, test_domain, Payload, 60000),

    %% Act: Wait for retries and success
    timer:sleep(3000),

    %% Assert: Verify status is 'completed'
    {ok, Status} = job_queue:get_job_status(JobId),
    ?assertEqual(completed, Status),

    %% Assert: Verify retry count is 1 (succeeded on 2nd attempt)
    {ok, RetryCount} = job_queue:get_retry_count(JobId),
    ?assertEqual(1, RetryCount),

    ok.
```

**Chicago TDD principles demonstrated:**
1. **State-based:** Asserts on job status, retry count (observable state)
2. **Real collaborators:** Uses actual NATS, not mocks
3. **AAA pattern:** Arrange (publish job) → Act (wait for retries) → Assert (verify status)
4. **Behavior verification:** Tests what the code does (retry logic), not how

---

## Step 6: Run Comprehensive Tests

Now run all test suites.

```bash
# Run all tests
rebar3 ct

# Expected output:
# Testing my_erlang_jobs.job_queue_SUITE: Starting
# Testing my_erlang_jobs.job_queue_SUITE: test_publish_subscribe ... ok
# Testing my_erlang_jobs.job_queue_SUITE: test_metrics ... ok
#
# Testing my_erlang_jobs.job_retry_SUITE: Starting
# Testing my_erlang_jobs.job_retry_SUITE: test_job_retries_on_failure ... ok (8052ms)
# Testing my_erlang_jobs.job_retry_SUITE: test_exponential_backoff ... ok (3215ms)
# Testing my_erlang_jobs.job_retry_SUITE: test_max_retries_exceeded ... ok (4089ms)
# Testing my_erlang_jobs.job_retry_SUITE: test_successful_retry ... ok (3156ms)
#
# Testing my_erlang_jobs.job_deadline_SUITE: Starting
# Testing my_erlang_jobs.job_deadline_SUITE: test_deadline_exceeded ... ok
# Testing my_erlang_jobs.job_deadline_SUITE: test_deadline_not_exceeded ... ok
#
# All 8 tests passed.
```

**Checkpoint:** All tests should pass. If any fail, review error messages and retry.

---

## Step 7: Test Manually in Erlang Shell

Start the application and test retry logic manually.

```bash
rebar3 shell

% Publish a job that will fail and retry
> Payload = #{action => <<"fail_3_times">>}.
> {ok, JobId} = job_queue:publish(high, test_domain, Payload, 60000).
{ok, "1738155123456789-123456"}

% Check status (should be 'processing' initially)
> job_queue:get_job_status(JobId).
{ok, processing}

% Wait 8 seconds for retries...
> timer:sleep(8000).

% Check status again (should be 'failed' after max retries)
> job_queue:get_job_status(JobId).
{ok, failed}

% Check retry count
> job_queue:get_retry_count(JobId).
{ok, 3}

% Check metrics
> job_queue:get_metrics().
#{
    published => 1,
    retried => 3,
    failed => 1,
    deadline_exceeded => 0
}
```

**What happened?**
- Job failed 3 times
- Each retry used exponential backoff (1s, 2s, 4s)
- After max retries, status changed to `failed`
- Metrics tracked retries and failures

---

## Step 8: Monitor with Prometheus

The generated code includes Prometheus metrics. View them:

```bash
# In Erlang shell:
> prometheus_text_format:format().

# Expected output:
# TYPE jobs_processed_total counter
# jobs_processed_total 0

# TYPE jobs_failed_total counter
# jobs_failed_total 1

# TYPE jobs_retried_total counter
# jobs_retried_total 3

# TYPE jobs_deadline_exceeded_total counter
# jobs_deadline_exceeded_total 0

# TYPE job_processing_duration_ms histogram
# job_processing_duration_ms_bucket{le="10.0"} 0
# job_processing_duration_ms_bucket{le="50.0"} 0
# job_processing_duration_ms_bucket{le="100.0"} 4
# job_processing_duration_ms_bucket{le="500.0"} 4
# job_processing_duration_ms_bucket{le="1000.0"} 4
# job_processing_duration_ms_bucket{le="5000.0"} 4
# job_processing_duration_ms_bucket{le="+Inf"} 4
# job_processing_duration_ms_sum 412.0
# job_processing_duration_ms_count 4
```

**Metrics collected:**
- Total jobs processed, failed, retried, deadline exceeded
- Processing duration histogram (p50, p95, p99)

---

## Congratulations!

You've built a production-ready job queue with:
- ✅ Priority-based processing
- ✅ Automatic retry with exponential backoff
- ✅ Deadline handling
- ✅ Comprehensive Chicago TDD tests
- ✅ Prometheus metrics

---

## What's Next?

Continue learning:
- **Tutorial 3:** [Creating a Supervised Worker Pool](03-supervised-worker-pool.md) — Add fault-tolerant workers
- **Tutorial 4:** [End-to-End: From RDF Spec to Running Erlang App](04-rdf-to-running-app.md) — Complete deployment

Or explore **How-To Guides**:
- [How to Add a Custom Job Backend](../howto/01-custom-job-backend.md)
- [How to Implement Rate Limiting](../howto/02-rate-limiting.md)

---

## Summary

You've learned:
- How to extend RDF specifications with new properties
- How ggen regenerates code from updated RDF
- Retry logic with exponential backoff
- Deadline handling and timeout detection
- Chicago TDD testing patterns
- Prometheus metrics integration

**Next:** [Tutorial 3: Creating a Supervised Worker Pool](03-supervised-worker-pool.md)

---

**Generated by ggen v6.0.0 | 2026-01-29**
