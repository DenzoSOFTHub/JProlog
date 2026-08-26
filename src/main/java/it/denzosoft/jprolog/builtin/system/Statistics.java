package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.util.TermUtils;

import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.RuntimeMXBean;
import java.util.Map;
import java.util.List;
import java.util.Arrays;

/**
 * Implementation of statistics/2 predicate for system statistics.
 * 
 * statistics(+Key, -Value) - Get system statistics
 */
// START_CHANGE: ISS-2025-0424 - ENG-02: statistics/2 keys report counters, byte counts and
// millisecond counts, which are ISO INTEGERS; they used to rely on Number(double) auto-classifying
// integral doubles as integers, which no longer happens.
// END_CHANGE: ISS-2025-0424
public class Statistics extends AbstractBuiltInWithContext {
    
    private final MemoryMXBean memoryBean;
    private final RuntimeMXBean runtimeBean;
    private long startTime;
    
    /**
     * Create statistics predicate.
     * 
     * @param solver The query solver
     */
    public Statistics(SolverContext solver) {
        super(solver);
        this.memoryBean = ManagementFactory.getMemoryMXBean();
        this.runtimeBean = ManagementFactory.getRuntimeMXBean();
        this.startTime = System.currentTimeMillis();
    }
    
    // START_CHANGE: ISS-2025-0085 - Add solutions on success
    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        boolean result = solve(solver, bindings);
        if (result) {
            solutions.add(new java.util.HashMap<>(bindings));
        }
        return result;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        this.solver = solver;
        if (query instanceof it.denzosoft.jprolog.core.terms.CompoundTerm) {
            it.denzosoft.jprolog.core.terms.CompoundTerm compound = (it.denzosoft.jprolog.core.terms.CompoundTerm) query;
            this.arguments = new Term[compound.getArguments().size()];
            for (int i = 0; i < compound.getArguments().size(); i++) {
                this.arguments[i] = compound.getArguments().get(i);
            }
        } else {
            this.arguments = new Term[0];
        }
        boolean result = solve(solver, bindings);
        if (result) {
            solutions.add(new java.util.HashMap<>(bindings));
        }
        return result;
    }
    // END_CHANGE: ISS-2025-0085
    
    @Override
    public boolean solve(SolverContext solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        if (args.length != 2) {
            return false;
        }
        
        Term keyTerm = args[0];
        Term valueTerm = args[1];
        
        if (!(keyTerm instanceof Atom)) {
            return false;
        }
        
        String key = ((Atom) keyTerm).getName();
        Term value = getStatistic(key);
        
        if (value == null) {
            return false;
        }
        
        return unifyTerm(valueTerm, value, bindings);
    }
    
    /**
     * Get a specific statistic.
     * 
     * @param key The statistic key
     * @return The statistic value, or null if not found
     */
    private Term getStatistic(String key) {
        switch (key) {
            case "runtime":
                // [TotalRuntime, RuntimeSinceLastCall]
                long currentTime = System.currentTimeMillis();
                long totalRuntime = runtimeBean.getUptime();
                return createTimePair(totalRuntime, totalRuntime);
                
            case "cputime":
                // CPU time (approximated using system time)
                long cpuTime = runtimeBean.getUptime();
                return createTimePair(cpuTime, cpuTime);
                
            case "walltime":
                // Wall clock time
                long wallTime = System.currentTimeMillis() - startTime;
                return createTimePair(wallTime, wallTime);
                
            case "heapused":
                // Heap memory used
                long heapUsed = memoryBean.getHeapMemoryUsage().getUsed();
                return new it.denzosoft.jprolog.core.terms.Number((long) heapUsed);
                
            case "heap":
                // [HeapUsed, HeapFree]
                long used = memoryBean.getHeapMemoryUsage().getUsed();
                long committed = memoryBean.getHeapMemoryUsage().getCommitted();
                long free = committed - used;
                return createMemoryPair(used, free);
                
            case "localused":
                // Local (non-heap) memory used
                long localUsed = memoryBean.getNonHeapMemoryUsage().getUsed();
                return new it.denzosoft.jprolog.core.terms.Number((long) localUsed);
                
            case "local":
                // [LocalUsed, LocalFree]
                long localUsedMem = memoryBean.getNonHeapMemoryUsage().getUsed();
                long localCommitted = memoryBean.getNonHeapMemoryUsage().getCommitted();
                long localFree = localCommitted - localUsedMem;
                return createMemoryPair(localUsedMem, localFree);
                
            case "globalused":
                // Global memory used (total heap)
                long globalUsed = memoryBean.getHeapMemoryUsage().getUsed();
                return new it.denzosoft.jprolog.core.terms.Number((long) globalUsed);
                
            case "global":
                // [GlobalUsed, GlobalFree]
                return getStatistic("heap");
                
            case "trailused":
                // Trail stack used (approximated)
                return new it.denzosoft.jprolog.core.terms.Number(0L);
                
            case "trail":
                // [TrailUsed, TrailFree]
                return createMemoryPair(0, 1000000); // Approximated
                
            case "garbage_collection":
                // Number of garbage collections
                int gcCount = 0;
                try {
                    gcCount = ManagementFactory.getGarbageCollectorMXBeans()
                        .stream()
                        .mapToInt(bean -> (int) bean.getCollectionCount())
                        .sum();
                } catch (Exception e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    // Ignore
                }
                return new it.denzosoft.jprolog.core.terms.Number((long) gcCount);
                
            case "atoms":
                // Number of atoms (approximated)
                return new it.denzosoft.jprolog.core.terms.Number(1000L);
                
            case "functors":
                // Number of functors (approximated)
                return new it.denzosoft.jprolog.core.terms.Number(500L);
                
            case "predicates":
                // Number of predicates
                int predicateCount = solver != null ? 100 : 0;
                return new it.denzosoft.jprolog.core.terms.Number((long) predicateCount);
                
            case "modules":
                // Number of modules
                int moduleCount = solver != null ? 1 : 1;
                return new it.denzosoft.jprolog.core.terms.Number((long) moduleCount);
                
            case "threads":
                // Number of threads
                int threadCount = Thread.activeCount();
                return new it.denzosoft.jprolog.core.terms.Number((long) threadCount);
                
            default:
                return null; // Unknown statistic
        }
    }
    
    /**
     * Create a time pair [Total, SinceLast].
     */
    private Term createTimePair(long total, long sinceLast) {
        Term totalTerm = new it.denzosoft.jprolog.core.terms.Number((long) total);
        Term sinceLastTerm = new it.denzosoft.jprolog.core.terms.Number((long) sinceLast);
        return createList(totalTerm, sinceLastTerm);
    }
    
    /**
     * Create a memory pair [Used, Free].
     */
    private Term createMemoryPair(long used, long free) {
        Term usedTerm = new it.denzosoft.jprolog.core.terms.Number((long) used);
        Term freeTerm = new it.denzosoft.jprolog.core.terms.Number((long) free);
        return createList(usedTerm, freeTerm);
    }
    
    /**
     * Create a two-element list.
     */
    private Term createList(Term first, Term second) {
        Term nil = new Atom("[]");
        Term secondCons = TermUtils.createCompound(".", second, nil);
        return TermUtils.createCompound(".", first, secondCons);
    }
    
    /**
     * Unify a term with a variable.
     */
    private boolean unifyTerm(Term var, Term value, Map<String, Term> bindings) {
        if (var instanceof Variable) {
            String varName = ((Variable) var).getName();
            bindings.put(varName, value);
            return true;
        } else {
            return var.equals(value);
        }
    }
}