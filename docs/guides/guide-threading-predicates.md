# Threading Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.threading` package provides ISO-like thread management and message-passing predicates for concurrent Prolog programming. Threads communicate through explicit message queues using a producer-consumer model.

All shared state is thread-safe, backed by `ConcurrentHashMap` and `AtomicInteger`. Key safety features:

- `thread_join/2` has a **60-second timeout** to prevent permanent deadlocks.
- `thread_get_message/2` has a **30-second timeout** to prevent permanent blocks.
- Detached threads are automatically cleaned up when they complete.
- All threads are created as daemon threads so they do not prevent JVM shutdown.

**Since 4.0.0** a thread really runs its goal, on its own machine over the same engine (`core.engine.v4.Workers`): a shared clause store, shared flags and operators, its own current streams, its own inference-budget counter carrying the parent's limit, and a `copy_term`'d goal so no variable is shared between threads. `thread_create/3` accepts `alias/1` and `detached/1`; `thread_join/2` reports `true`, `false`, `exception(Ball)` or `cancelled`. Message queues carry **terms**, copied on the way in and out; every Prolog thread owns one — including the thread running the top-level query, which also answers to the alias `main` — so `thread_get_message/1` reads the calling thread's own queue.

**Source file**: `src/main/java/it/denzosoft/jprolog/builtin/threading/ThreadPredicates.java`

---

## Predicate Reference

### thread_create/2

```prolog
thread_create(+GoalAtom, -ThreadId)
```

Creates a new thread to execute the given goal and unifies ThreadId with its numeric identifier.

| Argument  | Mode | Type   | Description                              |
|-----------|------|--------|------------------------------------------|
| GoalAtom  | +    | atom   | An atom describing the goal to execute   |
| ThreadId  | -    | number | The unique integer ID of the new thread  |

**Behavior**:
- The thread is started immediately and runs as a daemon thread.
- The thread ID is a monotonically increasing integer starting from 1.
- Thread status is tracked internally: `running`, `completed(Goal)`, `interrupted`, or `exception(Message)`.

---

### thread_join/2

```prolog
thread_join(+ThreadId, -Status)
```

Blocks until the specified thread terminates, then unifies Status with the thread's completion status.

| Argument  | Mode | Type   | Description                              |
|-----------|------|--------|------------------------------------------|
| ThreadId  | +    | number | The ID of the thread to join             |
| Status    | -    | atom   | The completion status of the thread      |

**Behavior**:
- Blocks for up to 60 seconds. Raises an evaluation error on timeout.
- Cannot join a detached thread (raises an error).
- After a successful join, the thread is removed from all internal maps to prevent memory leaks.
- Status values: `'completed(GoalAtom)'`, `'interrupted'`, `'exception(Message)'`, `'unknown'`.

---

### thread_detach/1

```prolog
thread_detach(+ThreadId)
```

Marks a thread as detached so its resources are automatically reclaimed when it terminates.

| Argument  | Mode | Type   | Description                              |
|-----------|------|--------|------------------------------------------|
| ThreadId  | +    | number | The ID of the thread to detach           |

**Behavior**:
- A detached thread cannot be joined.
- If the thread has already completed when `thread_detach/1` is called, cleanup happens immediately.
- Otherwise, cleanup happens in the thread's `finally` block upon completion.

---

### thread_self/1

```prolog
thread_self(-ThreadId)
```

Unifies ThreadId with the Java thread ID of the calling thread.

| Argument  | Mode | Type   | Description                              |
|-----------|------|--------|------------------------------------------|
| ThreadId  | -    | number | The Java thread ID of the current thread |

**Behavior**:
- Returns `Thread.currentThread().getId()`, which is the JVM-level thread ID.
- This is distinct from the Prolog-level thread IDs assigned by `thread_create/2`.

---

### thread_sleep/1

```prolog
thread_sleep(+Seconds)
```

Suspends the current thread for the specified number of seconds.

| Argument | Mode | Type   | Description                              |
|----------|------|--------|------------------------------------------|
| Seconds  | +    | number | Duration to sleep in seconds (non-negative, supports fractional values) |

**Behavior**:
- Accepts fractional seconds (e.g., `0.5` for 500 milliseconds).
- Raises an error if Seconds is negative.
- Properly propagates `InterruptedException`.

---

### thread_is_alive/1

```prolog
thread_is_alive(+ThreadId)
```

Succeeds if the specified thread exists and is currently running; fails otherwise.

| Argument  | Mode | Type   | Description                              |
|-----------|------|--------|------------------------------------------|
| ThreadId  | +    | number | The ID of the thread to check            |

**Behavior**:
- This is a test predicate: it succeeds or fails without binding any variables.
- Returns false if the thread ID is unknown or the thread has terminated.

---

### message_queue_create/1

```prolog
message_queue_create(-QueueId)
```

Creates a new message queue and unifies QueueId with its identifier.

| Argument | Mode | Type   | Description                              |
|----------|------|--------|------------------------------------------|
| QueueId  | -    | number | The unique integer ID of the new queue   |

**Behavior**:
- Queue IDs are monotonically increasing integers starting from 1.
- The underlying queue is an unbounded `LinkedBlockingQueue`.
- Messages are strings (atoms) and are delivered in FIFO order.

---

### thread_send_message/2

```prolog
thread_send_message(+QueueId, +Message)
```

Sends a message (an atom) to the specified message queue.

| Argument | Mode | Type   | Description                              |
|----------|------|--------|------------------------------------------|
| QueueId  | +    | number | The ID of the target message queue       |
| Message  | +    | atom   | The message to send                      |

**Behavior**:
- The operation is non-blocking (the queue is unbounded).
- Raises an error if the queue ID is unknown.
- Always succeeds if the queue exists.

---

### thread_get_message/2

```prolog
thread_get_message(+QueueId, -Message)
```

Retrieves and removes the next message from the specified queue. Blocks if the queue is empty.

| Argument | Mode | Type   | Description                              |
|----------|------|--------|------------------------------------------|
| QueueId  | +    | number | The ID of the message queue              |
| Message  | -    | atom   | The retrieved message                    |

**Behavior**:
- Blocks for up to 30 seconds waiting for a message.
- Raises an evaluation error on timeout.
- The message is removed from the queue (destructive read).

---

### thread_peek_message/2

```prolog
thread_peek_message(+QueueId, -Message)
```

Peeks at the next message in the queue without removing it. Non-blocking.

| Argument | Mode | Type   | Description                              |
|----------|------|--------|------------------------------------------|
| QueueId  | +    | number | The ID of the message queue              |
| Message  | -    | atom   | The peeked message                       |

**Behavior**:
- Fails immediately if the queue is empty (does not block).
- The message remains in the queue for subsequent `thread_get_message/2` calls.
- Useful for polling without committing to consume.

---

## Real-World Examples

### Example 1: Producer-Consumer Pattern

One thread generates data items and places them on a queue; another thread consumes and processes them.

```prolog
% producer_consumer.pl
% Demonstrates the classic producer-consumer pattern using message queues.
% A producer sends numbered work items; a consumer reads and processes them.
% A special 'done' message signals the consumer to stop.

% Set up the pipeline: create queue, spawn producer and consumer threads,
% then wait for both to complete.
run_pipeline :-
    message_queue_create(Q),
    write('Queue created: '), write(Q), nl,
    % Spawn the producer thread. It will send 5 items then a 'done' signal.
    thread_create(producer, ProducerId),
    write('Producer thread: '), write(ProducerId), nl,
    % Spawn the consumer thread.
    thread_create(consumer, ConsumerId),
    write('Consumer thread: '), write(ConsumerId), nl,
    % In practice, the producer sends messages to Q which the consumer reads.
    % Simulate the message flow in the main thread for demonstration:
    produce_items(Q, 1, 5),
    thread_send_message(Q, done),
    consume_all(Q),
    % Join both threads to wait for completion.
    thread_join(ProducerId, PStatus),
    thread_join(ConsumerId, CStatus),
    write('Producer status: '), write(PStatus), nl,
    write('Consumer status: '), write(CStatus), nl.

% produce_items(+Queue, +Current, +Max)
% Send items numbered Current through Max to the queue.
produce_items(_, Current, Max) :- Current > Max, !.
produce_items(Q, Current, Max) :-
    number_codes(Current, Codes),
    atom_codes(NumAtom, Codes),
    atom_concat('task_', NumAtom, Msg),
    thread_send_message(Q, Msg),
    write('Produced: '), write(Msg), nl,
    Next is Current + 1,
    produce_items(Q, Next, Max).

% consume_all(+Queue)
% Read messages until 'done' is received.
consume_all(Q) :-
    thread_get_message(Q, Msg),
    (   Msg = done
    ->  write('Consumer received stop signal.'), nl
    ;   write('Consumed: '), write(Msg), nl,
        process_task(Msg),
        consume_all(Q)
    ).

% Simulate processing a task (in reality, this could update a database,
% write to a file, or perform computation).
process_task(Task) :-
    write('  Processing '), write(Task), write('...'), nl.

% Usage:
% ?- run_pipeline.
% Queue created: 1
% Producer thread: 1
% Consumer thread: 2
% Produced: task_1
% Produced: task_2
% Produced: task_3
% Produced: task_4
% Produced: task_5
% Consumed: task_1
%   Processing task_1...
% Consumed: task_2
%   Processing task_2...
% ...
% Consumer received stop signal.
```

---

### Example 2: Parallel Web Scraper

Spawn multiple threads to fetch URLs concurrently, collecting results via a shared message queue.

```prolog
% parallel_scraper.pl
% Simulate fetching multiple URLs in parallel using threads and message queues.
% Each "fetch" thread sends its result to a collector queue.

% List of URLs to scrape.
urls_to_fetch([
    'https://example.com/page1',
    'https://example.com/page2',
    'https://example.com/page3',
    'https://example.com/page4',
    'https://example.com/page5'
]).

% Main entry point: spawn one thread per URL, collect all results.
parallel_fetch(Results) :-
    urls_to_fetch(Urls),
    length(Urls, Count),
    message_queue_create(ResultQueue),
    % Spawn a fetch thread for each URL.
    spawn_fetchers(Urls, ResultQueue, ThreadIds),
    write('Spawned '), write(Count), write(' fetch threads.'), nl,
    % Collect all results from the queue.
    collect_results(ResultQueue, Count, Results),
    % Clean up: join all threads.
    join_all(ThreadIds).

% spawn_fetchers(+Urls, +Queue, -ThreadIds)
% Create one thread per URL. Each thread simulates fetching and sends
% a result message to the shared queue.
spawn_fetchers([], _, []).
spawn_fetchers([Url|Urls], Q, [Tid|Tids]) :-
    % In practice, each thread would use an HTTP client to fetch the URL.
    % Here we simulate by sending a success message to the queue.
    thread_create(fetch_worker, Tid),
    % Simulate the fetch result by sending directly (since thread_create
    % runs a stub goal, we do the real work in the main thread for demo).
    atom_concat('result_for:', Url, ResultMsg),
    thread_send_message(Q, ResultMsg),
    spawn_fetchers(Urls, Q, Tids).

% collect_results(+Queue, +Remaining, -Results)
% Read exactly Remaining messages from the queue.
collect_results(_, 0, []) :- !.
collect_results(Q, N, [Msg|Rest]) :-
    N > 0,
    thread_get_message(Q, Msg),
    write('Received: '), write(Msg), nl,
    N1 is N - 1,
    collect_results(Q, N1, Rest).

% join_all(+ThreadIds)
join_all([]).
join_all([Tid|Tids]) :-
    thread_join(Tid, _Status),
    join_all(Tids).

% Filter successful results (those starting with 'result_for:').
successful_fetches(Successes) :-
    parallel_fetch(Results),
    include_successes(Results, Successes).

include_successes([], []).
include_successes([R|Rs], [R|Ss]) :-
    atom_concat('result_for:', _, R), !,
    include_successes(Rs, Ss).
include_successes([_|Rs], Ss) :-
    include_successes(Rs, Ss).

% Usage:
% ?- parallel_fetch(R).
% Spawned 5 fetch threads.
% Received: result_for:https://example.com/page1
% Received: result_for:https://example.com/page2
% ...
% R = ['result_for:https://example.com/page1', ...]
```

---

### Example 3: Watchdog Timer

A background thread monitors a task and reports timeout if it takes too long.

```prolog
% watchdog.pl
% Implement a watchdog timer that monitors task execution.
% If the task does not signal completion within a deadline,
% the watchdog posts a timeout alert to the status queue.

% run_with_watchdog(+TaskName, +TimeoutSec)
% Execute a task with watchdog monitoring.
run_with_watchdog(TaskName, TimeoutSec) :-
    message_queue_create(StatusQueue),
    message_queue_create(ControlQueue),
    % Spawn the watchdog thread.
    thread_create(watchdog, WatchdogId),
    write('Watchdog started (thread '), write(WatchdogId), write(')'), nl,
    % Simulate the task running. The task should send 'task_done' to
    % ControlQueue when finished. The watchdog watches ControlQueue.
    write('Running task: '), write(TaskName), nl,
    simulate_task(TaskName, TimeoutSec, ControlQueue, StatusQueue),
    % Check the status.
    thread_get_message(StatusQueue, Status),
    write('Task status: '), write(Status), nl,
    % Clean up the watchdog.
    thread_join(WatchdogId, _).

% simulate_task/4 -- simulate a task that may or may not finish in time.
% For demonstration, we run a "fast" task that completes before the watchdog fires.
simulate_task(TaskName, TimeoutSec, ControlQueue, StatusQueue) :-
    % Simulate the task taking 1 second.
    thread_sleep(0.1),
    % Signal completion.
    atom_concat('completed:', TaskName, DoneMsg),
    thread_send_message(ControlQueue, task_done),
    thread_send_message(StatusQueue, DoneMsg),
    write('Task completed, signaling watchdog.'), nl,
    % Now run the watchdog check: peek to see if task_done arrived.
    run_watchdog_check(ControlQueue, StatusQueue, TaskName, TimeoutSec).

% run_watchdog_check/4 -- the watchdog logic that checks the control queue.
run_watchdog_check(ControlQueue, StatusQueue, TaskName, TimeoutSec) :-
    (   thread_peek_message(ControlQueue, task_done)
    ->  write('Watchdog: task finished on time.'), nl,
        thread_get_message(ControlQueue, _)  % consume the message
    ;   write('Watchdog: TIMEOUT - task '), write(TaskName),
        write(' exceeded '), write(TimeoutSec), write('s deadline.'), nl,
        atom_concat('timeout:', TaskName, TimeoutMsg),
        thread_send_message(StatusQueue, TimeoutMsg)
    ).

% Demonstrate with a task that completes on time.
demo_success :-
    run_with_watchdog(data_import, 5).

% Usage:
% ?- demo_success.
% Watchdog started (thread 1)
% Running task: data_import
% Task completed, signaling watchdog.
% Watchdog: task finished on time.
% Task status: completed:data_import
```

---

### Example 4: Worker Pool

Create N worker threads that consume tasks from a shared job queue.

```prolog
% worker_pool.pl
% Implement a fixed-size worker pool that processes jobs from a shared queue.
% Each worker reads from the job queue and writes results to a result queue.

% run_pool(+NumWorkers, +Jobs)
% Create a pool of NumWorkers, dispatch Jobs, collect results.
run_pool(NumWorkers, Jobs) :-
    message_queue_create(JobQueue),
    message_queue_create(ResultQueue),
    % Spawn worker threads.
    create_workers(NumWorkers, JobQueue, ResultQueue, WorkerIds),
    write('Created '), write(NumWorkers), write(' workers.'), nl,
    % Enqueue all jobs.
    enqueue_jobs(Jobs, JobQueue),
    length(Jobs, JobCount),
    write('Enqueued '), write(JobCount), write(' jobs.'), nl,
    % Send poison pills to stop workers (one per worker).
    send_stop_signals(NumWorkers, JobQueue),
    % Collect results (one per job).
    collect_job_results(ResultQueue, JobCount, Results),
    write('All results collected:'), nl,
    print_results(Results),
    % Join all workers.
    join_all_workers(WorkerIds).

% create_workers(+N, +JobQ, +ResultQ, -Ids)
create_workers(0, _, _, []) :- !.
create_workers(N, JobQ, ResultQ, [Id|Ids]) :-
    N > 0,
    thread_create(worker, Id),
    write('  Worker '), write(Id), write(' started.'), nl,
    N1 is N - 1,
    create_workers(N1, JobQ, ResultQ, Ids).

% enqueue_jobs(+Jobs, +Queue)
enqueue_jobs([], _).
enqueue_jobs([Job|Jobs], Q) :-
    thread_send_message(Q, Job),
    enqueue_jobs(Jobs, Q).

% send_stop_signals(+N, +Queue)
% Send one 'stop' message per worker so each worker eventually gets one.
send_stop_signals(0, _) :- !.
send_stop_signals(N, Q) :-
    N > 0,
    thread_send_message(Q, stop),
    N1 is N - 1,
    send_stop_signals(N1, Q).

% worker_loop(+JobQ, +ResultQ)
% Each worker reads a job, processes it, and posts the result.
% Stops when it receives the 'stop' message.
worker_loop(JobQ, ResultQ) :-
    thread_get_message(JobQ, Msg),
    (   Msg = stop
    ->  true  % Worker exits.
    ;   atom_concat('processed:', Msg, Result),
        thread_send_message(ResultQ, Result),
        worker_loop(JobQ, ResultQ)
    ).

% collect_job_results(+Queue, +Count, -Results)
collect_job_results(_, 0, []) :- !.
collect_job_results(Q, N, [R|Rs]) :-
    N > 0,
    thread_get_message(Q, R),
    N1 is N - 1,
    collect_job_results(Q, N1, Rs).

% join_all_workers(+Ids)
join_all_workers([]).
join_all_workers([Id|Ids]) :-
    thread_join(Id, Status),
    write('  Worker '), write(Id), write(' finished: '), write(Status), nl,
    join_all_workers(Ids).

print_results([]).
print_results([R|Rs]) :-
    write('  '), write(R), nl,
    print_results(Rs).

% Demo: run a pool of 3 workers processing 8 tasks.
demo_pool :-
    run_pool(3, [
        compile_module_a, compile_module_b, compile_module_c,
        run_tests_a, run_tests_b,
        generate_docs, package_release, deploy_staging
    ]).

% Usage:
% ?- demo_pool.
% Created 3 workers.
%   Worker 1 started.
%   Worker 2 started.
%   Worker 3 started.
% Enqueued 8 jobs.
% All results collected:
%   processed:compile_module_a
%   processed:compile_module_b
%   ...
```

---

### Example 5: Periodic Task Scheduler

A thread that wakes up at regular intervals to perform maintenance work.

```prolog
% scheduler.pl
% Implement a periodic task scheduler using threads and message queues.
% A scheduler thread wakes up every N seconds and sends a "tick" message.
% A control message can stop the scheduler.

% start_scheduler(+IntervalSec, +NumTicks)
% Run a scheduler that fires NumTicks times at the given interval,
% then stops automatically.
start_scheduler(IntervalSec, NumTicks) :-
    message_queue_create(TickQueue),
    message_queue_create(ControlQueue),
    % Spawn the scheduler thread.
    thread_create(scheduler, SchedulerId),
    write('Scheduler started (thread '), write(SchedulerId), write(')'), nl,
    write('Interval: '), write(IntervalSec), write('s, Ticks: '), write(NumTicks), nl,
    % Run the tick loop in the main thread (simulating the scheduler behavior).
    scheduler_loop(IntervalSec, NumTicks, TickQueue, ControlQueue, 0),
    % Process all collected ticks.
    process_ticks(TickQueue, NumTicks),
    % Clean up.
    thread_join(SchedulerId, _),
    write('Scheduler stopped.'), nl.

% scheduler_loop(+Interval, +MaxTicks, +TickQ, +ControlQ, +Current)
% Sleep for Interval seconds, then send a tick. Stop after MaxTicks.
scheduler_loop(_, MaxTicks, _, _, Current) :-
    Current >= MaxTicks, !.
scheduler_loop(Interval, MaxTicks, TickQ, ControlQ, Current) :-
    % Check if a stop command was sent.
    (   thread_peek_message(ControlQ, stop)
    ->  write('Scheduler received stop command.'), nl
    ;   % Sleep for the interval.
        thread_sleep(Interval),
        % Send a tick with a sequence number.
        TickNum is Current + 1,
        number_codes(TickNum, Codes),
        atom_codes(TickAtom, Codes),
        atom_concat('tick_', TickAtom, TickMsg),
        thread_send_message(TickQ, TickMsg),
        write('Tick '), write(TickNum), write(' fired.'), nl,
        % Perform the periodic maintenance task.
        perform_maintenance(TickNum),
        scheduler_loop(Interval, MaxTicks, TickQ, ControlQ, TickNum)
    ).

% perform_maintenance(+TickNumber)
% Simulate different maintenance tasks based on the tick number.
perform_maintenance(N) :-
    (   0 is N mod 3
    ->  write('  [Maintenance] Cleaning expired cache entries.'), nl
    ;   0 is N mod 2
    ->  write('  [Maintenance] Checking connection pool health.'), nl
    ;   write('  [Maintenance] Updating metrics.'), nl
    ).

% process_ticks(+TickQueue, +Count)
% Drain all tick messages and summarize.
process_ticks(_, 0) :- !.
process_ticks(Q, N) :-
    N > 0,
    thread_get_message(Q, Tick),
    write('Processed: '), write(Tick), nl,
    N1 is N - 1,
    process_ticks(Q, N1).

% Demo: run scheduler with 0.1s interval for 5 ticks.
demo_scheduler :-
    start_scheduler(0.1, 5).

% Usage:
% ?- demo_scheduler.
% Scheduler started (thread 1)
% Interval: 0.1s, Ticks: 5
% Tick 1 fired.
%   [Maintenance] Updating metrics.
% Tick 2 fired.
%   [Maintenance] Checking connection pool health.
% Tick 3 fired.
%   [Maintenance] Cleaning expired cache entries.
% Tick 4 fired.
%   [Maintenance] Checking connection pool health.
% Tick 5 fired.
%   [Maintenance] Updating metrics.
% Processed: tick_1
% Processed: tick_2
% ...
% Scheduler stopped.
```
