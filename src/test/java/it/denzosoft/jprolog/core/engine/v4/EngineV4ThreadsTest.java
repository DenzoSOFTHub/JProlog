package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0479 / ISS-2025-0480 - engine v4 wave W8, design B.13: threads run on
// their own Machine over the same Engine (limit LIM-024).
/**
 * Threads and the concurrency predicates on v4 machines.
 *
 * <p>Before this wave {@code thread_create/2} did not run its goal at all (it slept 10 ms and
 * recorded {@code completed(<atom>)}), and {@code concurrent/3},
 * {@code concurrent_maplist/N} and {@code first_solution/3} handed their sub-goals to the shared
 * recursive {@code QuerySolver} from a worker thread — the last routine path from a v4 query into
 * {@code QuerySolver.solveInternal} and the whole of LIM-024.
 *
 * <p>The contract this class pins:
 * <ul>
 *   <li>a worker really runs the goal, on its own {@link Machine} over the shared {@link Engine};</li>
 *   <li>the clause store is shared and thread-safe: {@code assertz} from two threads is visible to
 *       both and to the parent;</li>
 *   <li>no {@code Variable} cell crosses machines — the goal is copied in, the answer copied out,
 *       and a worker binding never leaks into the parent's query;</li>
 *   <li>message queues carry terms, copied on the way in and out;</li>
 *   <li>each worker gets its own {@link ResourceGuard} with the parent's budget, and interrupting
 *       the parent cancels the workers;</li>
 *   <li>{@code Machine.assertOwnerThread} really is an assertion now.</li>
 * </ul>
 */
public class EngineV4ThreadsTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
        prolog.consult(
            ":- dynamic(res/1).\n"
          + "work(N) :- N1 is N * 2, assertz(res(N1)).\n"
          + "dbl(X, Y) :- Y is X * 2.\n"
          + "spin(N) :- N > 0, N1 is N - 1, spin(N1).\n"
          + "spin(0).\n");
    }

    @After
    public void tearDown() {
    }

    private void ok(String query) {
        assertFalse("must succeed: " + query, prolog.solve(query).isEmpty());
    }

    // ================================================================ ISS-2025-0479

    /** The headline change: {@code thread_create/2} runs the goal instead of faking a status. */
    @Test(timeout = 30000)
    public void testISS0479_ThreadCreateReallyRunsTheGoal() {
        ok("thread_create(work(21), Id), thread_join(Id, Status), Status == true, res(42)");
    }

    /** {@code thread_join/2} reports true / false / exception(Ball), as SWI does. */
    @Test(timeout = 30000)
    public void testISS0479_JoinStatusIsTheGoalsOutcome() {
        ok("thread_create(true, A), thread_join(A, true)");
        ok("thread_create(fail, B), thread_join(B, false)");
        ok("thread_create(throw(boom), C), thread_join(C, exception(boom))");
    }

    /** {@code thread_create/3}: {@code alias/1} names the thread, {@code detached/1} detaches it. */
    @Test(timeout = 30000)
    public void testISS0479_ThreadCreate3Options() {
        ok("thread_create(work(5), _, [alias(w1)]), thread_join(w1, true), res(10)");
        ok("thread_create(true, D, [detached(true)]), thread_detach(D)");
    }

    /** A worker reports its own Prolog thread id, not the creator's. */
    @Test(timeout = 30000)
    public void testISS0479_ThreadSelfInsideAWorker() {
        prolog.consult(":- dynamic(seen/1).\n");
        ok("thread_self(Mine), thread_create((thread_self(T), assertz(seen(T))), Id), "
         + "thread_join(Id, true), seen(Theirs), Theirs \\== Mine");
    }

    /** The clause store is shared and thread-safe by generations (design B.7). */
    @Test(timeout = 30000)
    public void testISS0479_AssertAndRetractFromTwoThreads() {
        ok("thread_create(work(1), A), thread_create(work(2), B), "
         + "thread_join(A, true), thread_join(B, true), res(2), res(4)");
        // and a retract done in a worker is visible in the parent
        ok("thread_create(retract(res(2)), C), thread_join(C, true), \\+ res(2)");
    }

    /**
     * No cell is shared across machines: the worker's goal is a {@code copy_term}, so a binding it
     * makes cannot appear in the parent's query (SWI behaves the same way).
     */
    @Test(timeout = 30000)
    public void testISS0479_WorkerBindingsDoNotLeakIntoTheParent() {
        List<Map<String, Term>> s = prolog.solve("thread_create(X = 1, Id), thread_join(Id, true), var(X).");
        assertEquals("X must still be unbound in the parent", 1, s.size());
    }

    /** Message queues carry TERMS, copied on the way in and on the way out. */
    @Test(timeout = 30000)
    public void testISS0479_MessageQueuesCarryTerms() {
        ok("message_queue_create(Q), thread_send_message(Q, f(1, [a,b])), "
         + "thread_get_message(Q, M), M == f(1,[a,b])");
        ok("message_queue_create(Q), thread_send_message(Q, one), thread_send_message(Q, two), "
         + "thread_get_message(Q, A), thread_get_message(Q, B), A == one, B == two");
        ok("message_queue_create(Q), thread_send_message(Q, peeked), "
         + "thread_peek_message(Q, P1), thread_peek_message(Q, P2), P1 == peeked, P2 == peeked");
    }

    /** Every Prolog thread owns a queue, so an alias can be sent to and read with /1 inside it. */
    @Test(timeout = 30000)
    public void testISS0479_ThreadOwnedQueueAndAlias() {
        ok("thread_create((thread_get_message(M), assertz(res(M))), Id, [alias(worker2)]), "
         + "thread_send_message(worker2, hello(world)), thread_join(Id, true), res(hello(world))");
    }

    /** A worker gets its OWN guard carrying the parent's budget (design B.13). */
    @Test(timeout = 30000)
    public void testISS0479_WorkerInheritsTheInferenceBudget() {
        Prolog p = new Prolog();
        p.consult("spin(N) :- N > 0, N1 is N - 1, spin(N1).\nspin(0).\n");
        p.setInferenceBudget(20000);
        List<Map<String, Term>> s =
            p.solve("thread_create(spin(1000000), Id), thread_join(Id, Status).");
        assertEquals(1, s.size());
        Term status = s.get(0).get("Status");
        assertNotNull(status);
        assertTrue("the worker must hit the budget, got " + status,
                   status.toString().contains("inference_limit_exceeded"));
    }

    // ================================================================ ISS-2025-0480

    /** {@code concurrent_maplist/2,3,4} are one registry entry that dispatches on arity. */
    @Test(timeout = 30000)
    public void testISS0480_ConcurrentMaplistAllArities() {
        ok("concurrent_maplist(work, [7, 8]), res(14), res(16)");
        ok("concurrent_maplist(dbl, [1,2,3], L), L == [2,4,6]");
        ok("concurrent_maplist(plus, [1,2], [10,20], L), L == [11,22]");
    }

    /** The rest of the family runs on worker machines too. */
    @Test(timeout = 30000)
    public void testISS0480_ConcurrentFamily() {
        ok("concurrent(2, [work(100), work(200)], []), res(200), res(400)");
        ok("concurrent_and([true, true], [])");
        ok("concurrent_or([fail, true], N), integer(N)");
        ok("first_solution(X, [(X = a)], []), X == a");
    }

    /**
     * The acceptance oracle of wave W3, extended by W8 and settled by W9: the recursive
     * {@code QuerySolver} a concurrency built-in could fall back to <b>no longer exists</b>
     * (ISS-2025-0484), so the counter probe is replaced by the structural fact plus the same
     * queries, which must still all succeed on their own worker machines.
     */
    @Test(timeout = 60000)
    public void testISS0480_NoBuiltinReachesTheRecursiveSolver() {
        try {
            Class.forName("it.denzosoft.jprolog.core.engine.QuerySolver");
            fail("the recursive QuerySolver must be deleted (wave W9, ISS-2025-0484)");
        } catch (ClassNotFoundException expected) {
            // the only correct outcome
        }
        ok("thread_create(work(3), Id), thread_join(Id, true), res(6)");
        ok("concurrent_maplist(dbl, [1,2,3], L), L == [2,4,6]");
        ok("concurrent_maplist(work, [9])");
        ok("first_solution(X, [(X = fast)], []), X == fast");
        ok("concurrent(2, [work(11), work(12)], [])");
        ok("concurrent_and([true, true], [])");
    }

    /**
     * {@code Machine.onOwnerThread()} became an assertion: a {@code Machine} handed to another
     * thread is a programming error, not a case to fall back from. It is an
     * {@code IllegalStateException} on purpose — untrusted {@code catch/3} must not see it.
     */
    @Test(timeout = 30000)
    public void testISS0480_MachineRefusesToRunOffItsOwnerThread() throws Exception {
        Engine engine = prolog.getV4Engine();
        final Machine m = new Machine(engine, new ResourceGuard(0));
        final SolverFacade facade = m.facade();
        final AtomicReference<Throwable> caught = new AtomicReference<Throwable>();
        Thread t = new Thread(new Runnable() {
            @Override public void run() {
                try {
                    facade.solveMeta(new Atom("true"), new HashMap<String, Term>(),
                                     new ArrayList<Map<String, Term>>());
                } catch (Throwable e) {
                    caught.set(e);
                }
            }
        });
        t.start();
        t.join(10000);
        assertNotNull("an off-thread facade call must be refused", caught.get());
        assertTrue("expected IllegalStateException, got " + caught.get(),
                   caught.get() instanceof IllegalStateException);
        // ... and the same call on the owner thread is fine
        List<Map<String, Term>> out = new ArrayList<Map<String, Term>>();
        assertTrue(facade.solveMeta(new Atom("true"), new HashMap<String, Term>(), out));
    }

    /**
     * The sandbox still holds. A JVM thread is a host resource, and wave W8 turned
     * {@code thread_create/2,3} from a no-op into something that really spawns one, so
     * {@code builtin.threading} joined the safe-mode deny list (ISS-2025-0479).
     */
    @Test(timeout = 30000)
    public void testISS0479_SafeModeRemovesTheThreadingPredicates() {
        Prolog sandboxed = new Prolog();
        int removed = sandboxed.enableSafeMode();
        assertTrue("safe mode must still remove the host-touching built-ins", removed > 40);
        assertTrue(sandboxed.isSafeMode());
        assertEquals("thread_create must be gone under safe mode",
                     null, sandboxed.getBuiltInRegistry().getBuiltIn("thread_create"));
        assertEquals("message_queue_create must be gone under safe mode",
                     null, sandboxed.getBuiltInRegistry().getBuiltIn("message_queue_create"));
        assertEquals("concurrent_maplist must be gone under safe mode (same package)",
                     null, sandboxed.getBuiltInRegistry().getBuiltIn("concurrent_maplist"));
        // ... while ordinary logic keeps working
        assertFalse("pure logic must still work in safe mode",
                    sandboxed.solve("append([1], [2], L).").isEmpty());
    }

    /** Interrupting the parent cancels the workers instead of leaving them running. */
    @Test(timeout = 60000)
    public void testISS0480_InterruptingTheParentCancelsTheWorkers() throws Exception {
        final Prolog p = new Prolog();
        p.consult("spin(N) :- N > 0, N1 is N - 1, spin(N1).\nspin(0).\n");
        final AtomicReference<Throwable> caught = new AtomicReference<Throwable>();
        Thread solver = new Thread(new Runnable() {
            @Override public void run() {
                try {
                    p.solve("concurrent_maplist(spin, [200000000, 200000000]).");
                } catch (Throwable e) {
                    caught.set(e);
                }
            }
        });
        solver.start();
        Thread.sleep(600);
        solver.interrupt();
        solver.join(30000);
        assertFalse("the parent thread must have stopped", solver.isAlive());
        assertNotNull("the parent must report the cancellation", caught.get());
        assertTrue("expected QueryCancelledException, got " + caught.get(),
                   caught.get() instanceof it.denzosoft.jprolog.core.engine.QueryCancelledException);
    }
}
// END_CHANGE: ISS-2025-0479
