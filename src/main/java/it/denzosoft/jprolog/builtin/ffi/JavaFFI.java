// START_CHANGE: ISS-2025-0160 - Java Foreign Function Interface (FFI) built-in predicates
package it.denzosoft.jprolog.builtin.ffi;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.terms.Number;

import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Logger;

/**
 * Java Foreign Function Interface (FFI) for JProlog.
 * Allows Prolog programs to call Java methods, create objects, and access fields
 * via reflection.
 *
 * Java objects are stored in a reference table and represented in Prolog as atoms
 * with a special prefix ($java_ref_N).
 *
 * Supported predicates:
 *   java_new/3, java_call/4, java_get_field/3, java_set_field/3,
 *   java_instanceof/2, java_class/2, java_array_new/3, java_array_get/3,
 *   java_array_set/3, java_array_length/2, java_to_term/2, java_from_term/2
 */
public class JavaFFI implements BuiltIn {

    // START_CHANGE: ISS-2025-0173 - Fix refTable unbounded growth with release/gc predicates and soft limit
    private static final Logger LOGGER = Logger.getLogger(JavaFFI.class.getName());
    private static final int REF_TABLE_SOFT_LIMIT = 10000;
    // END_CHANGE: ISS-2025-0173

    /** Enum identifying the specific FFI operation. */
    public enum OperationType {
        JAVA_NEW,
        JAVA_CALL,
        JAVA_GET_FIELD,
        JAVA_SET_FIELD,
        JAVA_INSTANCEOF,
        JAVA_CLASS,
        JAVA_ARRAY_NEW,
        JAVA_ARRAY_GET,
        JAVA_ARRAY_SET,
        JAVA_ARRAY_LENGTH,
        JAVA_TO_TERM,
        JAVA_FROM_TERM,
        // START_CHANGE: ISS-2025-0173 - Add java_release_ref/1 and java_gc/0 for reference management
        JAVA_RELEASE_REF,
        JAVA_GC
        // END_CHANGE: ISS-2025-0173
    }

    private static final String REF_PREFIX = "$java_ref_";
    private static final AtomicLong refCounter = new AtomicLong(0);
    private static final Map<String, Object> refTable = new ConcurrentHashMap<>();

    private final OperationType operationType;

    public JavaFFI(OperationType operationType) {
        this.operationType = operationType;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0173 - Handle zero-arity predicates (java_gc)
        List<Term> args = query.getArguments();
        if (args == null) {
            args = java.util.Collections.emptyList();
        }
        // END_CHANGE: ISS-2025-0173

        try {
            boolean result;
            switch (operationType) {
                case JAVA_NEW:
                    result = executeJavaNew(args, bindings);
                    break;
                case JAVA_CALL:
                    result = executeJavaCall(args, bindings);
                    break;
                case JAVA_GET_FIELD:
                    result = executeJavaGetField(args, bindings);
                    break;
                case JAVA_SET_FIELD:
                    result = executeJavaSetField(args, bindings);
                    break;
                case JAVA_INSTANCEOF:
                    result = executeJavaInstanceof(args, bindings);
                    break;
                case JAVA_CLASS:
                    result = executeJavaClass(args, bindings);
                    break;
                case JAVA_ARRAY_NEW:
                    result = executeJavaArrayNew(args, bindings);
                    break;
                case JAVA_ARRAY_GET:
                    result = executeJavaArrayGet(args, bindings);
                    break;
                case JAVA_ARRAY_SET:
                    result = executeJavaArraySet(args, bindings);
                    break;
                case JAVA_ARRAY_LENGTH:
                    result = executeJavaArrayLength(args, bindings);
                    break;
                case JAVA_TO_TERM:
                    result = executeJavaToTerm(args, bindings);
                    break;
                case JAVA_FROM_TERM:
                    result = executeJavaFromTerm(args, bindings);
                    break;
                // START_CHANGE: ISS-2025-0173 - Handle java_release_ref/1 and java_gc/0
                case JAVA_RELEASE_REF:
                    result = executeJavaReleaseRef(args, bindings);
                    break;
                case JAVA_GC:
                    result = executeJavaGc(args, bindings);
                    break;
                // END_CHANGE: ISS-2025-0173
                default:
                    return false;
            }
            if (result) {
                solutions.add(new HashMap<>(bindings));
            }
            return result;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            // START_CHANGE: ISS-2025-0796 - 4.6 wave Q7: an argument fault RAISES (invariant 65) and a
            // host failure maps to an error term; every exception used to become a silent failure.
            throw hostError(e);
            // END_CHANGE: ISS-2025-0796
        }
    }

    // START_CHANGE: ISS-2025-0796 - 4.6 wave Q7: the FFI's ISO argument checks. 22 probed goals
    // (unbound or f(x) arguments) FAILED silently; they now raise instantiation_error / type_error,
    // and an unknown class / method / field / constructor raises existence_error(class|method|
    // field|constructor, Name). An exception thrown by the invoked Java code is
    // error(java_exception(ExceptionClass), context(Name/Arity, Message)) — JPL's formal, with the
    // class name instead of a reference. All contexts are context(Name/Arity, Message).
    private String pname() { return operationType.name().toLowerCase(); }

    private int parity() {
        switch (operationType) {
            case JAVA_CALL: return 4;
            case JAVA_NEW: case JAVA_GET_FIELD: case JAVA_SET_FIELD:
            case JAVA_ARRAY_NEW: case JAVA_ARRAY_GET: case JAVA_ARRAY_SET: return 3;
            case JAVA_RELEASE_REF: return 1;
            case JAVA_GC: return 0;
            default: return 2;
        }
    }

    private it.denzosoft.jprolog.core.exceptions.PrologException hostError(Exception e) {
        if (e instanceof it.denzosoft.jprolog.core.exceptions.PrologException) {
            return (it.denzosoft.jprolog.core.exceptions.PrologException) e;
        }
        Throwable t = e instanceof InvocationTargetException && e.getCause() != null ? e.getCause() : e;
        String msg = t.getMessage() != null ? t.getMessage() : t.getClass().getName();
        return Errors.error(new CompoundTerm(new Atom("java_exception"),
            java.util.Collections.<Term>singletonList(new Atom(t.getClass().getName()))), pname(), parity(), msg);
    }

    /** An atom or string argument, as text. */
    private String needText(Term t, String what) {
        if (t instanceof Variable) throw Errors.instantiation(pname(), parity(), what + " must be bound");
        String s = termToString(t);
        if (s == null) throw Errors.type("atom", t, pname(), parity(), what + " must be an atom or a string");
        return s;
    }

    private Class<?> needClass(String className) {
        Class<?> c = resolveClass(className);
        if (c == null) throw Errors.existence("class", new Atom(className), pname(), parity(), "unknown Java class " + className);
        return c;
    }

    private List<Object> needList(Term t, Map<String, Term> bindings) {
        List<Object> l = prologListToJavaList(t, bindings);
        if (l != null) return l;
        Term tail = t;
        while (tail instanceof CompoundTerm && ".".equals(((CompoundTerm) tail).getName())
                && ((CompoundTerm) tail).getArguments().size() == 2) {
            tail = ((CompoundTerm) tail).getArguments().get(1).resolveBindings(bindings);
        }
        if (tail instanceof Variable) throw Errors.instantiation(pname(), parity(), "the argument list must be a proper list");
        throw Errors.type("list", t, pname(), parity(), "the argument list must be a list");
    }

    /** A Java object reference (an array when {@code array}). */
    private Object needRef(Term t, boolean array) {
        if (t instanceof Variable) throw Errors.instantiation(pname(), parity(), "the Java reference must be bound");
        Object o = resolveJavaObject(t);
        if (o == null || (array && !o.getClass().isArray())) {
            throw Errors.type(array ? "java_array" : "java_object", t, pname(), parity(),
                array ? "expected a reference to a Java array" : "expected a Java object reference");
        }
        return o;
    }

    private int needInt(Term t, String what) {
        if (t instanceof Variable) throw Errors.instantiation(pname(), parity(), what + " must be bound");
        if (!(t instanceof Number) || !((Number) t).isInteger()) {
            throw Errors.type("integer", t, pname(), parity(), what + " must be an integer");
        }
        return (int) ((Number) t).longValue();
    }

    /** The target of java_call/get_field/set_field: a reference, or a class name for a static access. */
    private void needTarget(Term t) {
        if (t instanceof Variable) throw Errors.instantiation(pname(), parity(), "the target must be bound");
        if (!(t instanceof Atom) && !(t instanceof PrologString)) {
            throw Errors.type("java_object", t, pname(), parity(), "the target must be a Java reference or a class name");
        }
    }
    // END_CHANGE: ISS-2025-0796

    // ---------------------------------------------------------------
    // java_new(+ClassName, +ArgList, -Instance)
    // ---------------------------------------------------------------
    private boolean executeJavaNew(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 3) return false;

        Term classNameTerm = args.get(0).resolveBindings(bindings);
        Term argListTerm = args.get(1).resolveBindings(bindings);
        Term instanceTerm = args.get(2).resolveBindings(bindings);

        String className = needText(classNameTerm, "the class name");              // ISS-2025-0796
        Class<?> clazz = needClass(className);
        List<Object> javaArgs = needList(argListTerm, bindings);

        Object[] argArray = javaArgs.toArray();
        Constructor<?> constructor = findConstructor(clazz, argArray);
        if (constructor == null) {
            throw Errors.existence("constructor", new Atom(className), pname(), parity(),
                "no constructor of " + className + " accepts " + argArray.length + " such arguments");
        }

        constructor.setAccessible(true);
        Object[] convertedArgs = convertArguments(constructor.getParameterTypes(), argArray);
        Object instance = constructor.newInstance(convertedArgs);

        Term refTerm = registerObject(instance);
        return unifyTerms(instanceTerm, refTerm, bindings);
    }

    // ---------------------------------------------------------------
    // java_call(+ObjectOrClass, +MethodName, +ArgList, -Result)
    // ---------------------------------------------------------------
    private boolean executeJavaCall(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 4) return false;

        Term targetTerm = args.get(0).resolveBindings(bindings);
        Term methodNameTerm = args.get(1).resolveBindings(bindings);
        Term argListTerm = args.get(2).resolveBindings(bindings);
        Term resultTerm = args.get(3).resolveBindings(bindings);

        needTarget(targetTerm);                                                    // ISS-2025-0796
        String methodName = needText(methodNameTerm, "the method name");
        List<Object> javaArgs = needList(argListTerm, bindings);

        Object[] argArray = javaArgs.toArray();

        // Determine whether this is a static call or instance call
        Object target = resolveJavaObject(targetTerm);
        Class<?> clazz;
        boolean isStatic;

        if (target instanceof Class) {
            // target is a Class object from java_class/2 or a class name atom
            clazz = (Class<?>) target;
            target = null;
            isStatic = true;
        } else if (target != null) {
            clazz = target.getClass();
            isStatic = false;
        } else {
            // Try interpreting targetTerm as a class name for static call
            String className = termToString(targetTerm);
            clazz = resolveClass(className);
            if (clazz != null) {
                isStatic = true;
            } else {
                // Not a known class - treat the atom/string value as a Java String instance
                target = className;
                clazz = String.class;
                isStatic = false;
            }
        }

        Method method = findMethod(clazz, methodName, argArray, isStatic);
        if (method == null) {
            throw Errors.existence("method", new Atom(methodName), pname(), parity(),   // ISS-2025-0796
                "no " + (isStatic ? "static " : "") + "method " + clazz.getName() + "." + methodName
                + " accepts " + argArray.length + " such arguments");
        }

        method.setAccessible(true);
        Object[] convertedArgs = convertArguments(method.getParameterTypes(), argArray);
        Object result = method.invoke(target, convertedArgs);

        Term resultPrologTerm = javaObjectToTerm(result);
        return unifyTerms(resultTerm, resultPrologTerm, bindings);
    }

    // ---------------------------------------------------------------
    // java_get_field(+ObjectOrClass, +FieldName, -Value)
    // ---------------------------------------------------------------
    private boolean executeJavaGetField(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 3) return false;

        Term targetTerm = args.get(0).resolveBindings(bindings);
        Term fieldNameTerm = args.get(1).resolveBindings(bindings);
        Term valueTerm = args.get(2).resolveBindings(bindings);

        needTarget(targetTerm);                                                    // ISS-2025-0796
        String fieldName = needText(fieldNameTerm, "the field name");

        Object target = resolveJavaObject(targetTerm);
        Class<?> clazz;
        boolean isStatic;

        if (target instanceof Class) {
            clazz = (Class<?>) target;
            target = null;
            isStatic = true;
        } else if (target != null) {
            clazz = target.getClass();
            isStatic = false;
        } else {
            clazz = needClass(termToString(targetTerm));                        // ISS-2025-0796
            isStatic = true;
        }

        Field field = findField(clazz, fieldName);
        if (field == null) {
            throw Errors.existence("field", new Atom(fieldName), pname(), parity(),       // ISS-2025-0796
                "no field " + clazz.getName() + "." + fieldName);
        }

        field.setAccessible(true);
        if (isStatic && !Modifier.isStatic(field.getModifiers())) {
            throw Errors.existence("static_field", new Atom(fieldName), pname(), parity(),   // ISS-2025-0796
                clazz.getName() + "." + fieldName + " is an instance field");
        }
        Object value = field.get(isStatic ? null : target);

        Term resultTerm = javaObjectToTerm(value);
        return unifyTerms(valueTerm, resultTerm, bindings);
    }

    // ---------------------------------------------------------------
    // java_set_field(+ObjectOrClass, +FieldName, +Value)
    // ---------------------------------------------------------------
    private boolean executeJavaSetField(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 3) return false;

        Term targetTerm = args.get(0).resolveBindings(bindings);
        Term fieldNameTerm = args.get(1).resolveBindings(bindings);
        Term valueTermArg = args.get(2).resolveBindings(bindings);

        needTarget(targetTerm);                                                    // ISS-2025-0796
        String fieldName = needText(fieldNameTerm, "the field name");

        Object target = resolveJavaObject(targetTerm);
        Class<?> clazz;
        boolean isStatic;

        if (target instanceof Class) {
            clazz = (Class<?>) target;
            target = null;
            isStatic = true;
        } else if (target != null) {
            clazz = target.getClass();
            isStatic = false;
        } else {
            clazz = needClass(termToString(targetTerm));                        // ISS-2025-0796
            isStatic = true;
        }

        Field field = findField(clazz, fieldName);
        if (field == null) {
            throw Errors.existence("field", new Atom(fieldName), pname(), parity(),       // ISS-2025-0796
                "no field " + clazz.getName() + "." + fieldName);
        }

        field.setAccessible(true);
        if (Modifier.isFinal(field.getModifiers())) {
            throw Errors.permission("modify", "final_field", new Atom(fieldName), pname(), parity(),   // ISS-2025-0796
                clazz.getName() + "." + fieldName + " is final");
        }
        if (isStatic && !Modifier.isStatic(field.getModifiers())) {
            throw Errors.existence("static_field", new Atom(fieldName), pname(), parity(),
                clazz.getName() + "." + fieldName + " is an instance field");
        }
        if (valueTermArg instanceof Variable) {
            throw Errors.instantiation(pname(), parity(), "the value must be bound");
        }

        Object javaValue = termToJavaObject(valueTermArg);
        field.set(isStatic ? null : target, javaValue);
        return true;
    }

    // ---------------------------------------------------------------
    // java_instanceof(+Object, +ClassName)
    // ---------------------------------------------------------------
    private boolean executeJavaInstanceof(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 2) return false;

        Term objTerm = args.get(0).resolveBindings(bindings);
        Term classNameTerm = args.get(1).resolveBindings(bindings);

        // ISS-2025-0796: null is an instance of nothing (fails); anything else must be a reference
        if (objTerm instanceof Atom && "null".equals(((Atom) objTerm).getName())) {
            needClass(needText(classNameTerm, "the class name"));
            return false;
        }
        Object obj = needRef(objTerm, false);
        Class<?> clazz = needClass(needText(classNameTerm, "the class name"));

        return clazz.isInstance(obj);
    }

    // ---------------------------------------------------------------
    // java_class(+ClassName, -ClassObject)
    // ---------------------------------------------------------------
    private boolean executeJavaClass(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 2) return false;

        Term classNameTerm = args.get(0).resolveBindings(bindings);
        Term classObjTerm = args.get(1).resolveBindings(bindings);

        Class<?> clazz = needClass(needText(classNameTerm, "the class name"));   // ISS-2025-0796

        Term refTerm = registerObject(clazz);
        return unifyTerms(classObjTerm, refTerm, bindings);
    }

    // ---------------------------------------------------------------
    // java_array_new(+ComponentType, +Length, -Array)
    // ---------------------------------------------------------------
    private boolean executeJavaArrayNew(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 3) return false;

        Term typeTerm = args.get(0).resolveBindings(bindings);
        Term lengthTerm = args.get(1).resolveBindings(bindings);
        Term arrayTerm = args.get(2).resolveBindings(bindings);

        String typeName = needText(typeTerm, "the component type");                // ISS-2025-0796
        int length = needInt(lengthTerm, "the length");
        if (length < 0) throw Errors.domain("not_less_than_zero", lengthTerm, pname(), parity(), "negative array length");
        Class<?> componentType = needClass(typeName);

        Object array = Array.newInstance(componentType, length);
        Term refTerm = registerObject(array);
        return unifyTerms(arrayTerm, refTerm, bindings);
    }

    // ---------------------------------------------------------------
    // java_array_get(+Array, +Index, -Element)
    // ---------------------------------------------------------------
    private boolean executeJavaArrayGet(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 3) return false;

        Term arrayTerm = args.get(0).resolveBindings(bindings);
        Term indexTerm = args.get(1).resolveBindings(bindings);
        Term elementTerm = args.get(2).resolveBindings(bindings);

        Object array = needRef(arrayTerm, true);                                   // ISS-2025-0796
        int index = needInt(indexTerm, "the index");
        // an index outside the array FAILS, as arg/3 does for a position outside the term
        if (index < 0 || index >= Array.getLength(array)) return false;

        Object element = Array.get(array, index);
        Term resultTerm = javaObjectToTerm(element);
        return unifyTerms(elementTerm, resultTerm, bindings);
    }

    // ---------------------------------------------------------------
    // java_array_set(+Array, +Index, +Value)
    // ---------------------------------------------------------------
    private boolean executeJavaArraySet(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 3) return false;

        Term arrayTerm = args.get(0).resolveBindings(bindings);
        Term indexTerm = args.get(1).resolveBindings(bindings);
        Term valueTerm = args.get(2).resolveBindings(bindings);

        Object array = needRef(arrayTerm, true);                                   // ISS-2025-0796
        int index = needInt(indexTerm, "the index");
        // an index outside the array FAILS, as arg/3 does for a position outside the term
        if (index < 0 || index >= Array.getLength(array)) return false;

        Object value = termToJavaObject(valueTerm);
        Array.set(array, index, value);
        return true;
    }

    // ---------------------------------------------------------------
    // java_array_length(+Array, -Length)
    // ---------------------------------------------------------------
    private boolean executeJavaArrayLength(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 2) return false;

        Term arrayTerm = args.get(0).resolveBindings(bindings);
        Term lengthTerm = args.get(1).resolveBindings(bindings);

        Object array = needRef(arrayTerm, true);                                   // ISS-2025-0796

        int length = Array.getLength(array);
        Term resultTerm = new Number((long) length)   /* ISS-2025-0424 */;
        return unifyTerms(lengthTerm, resultTerm, bindings);
    }

    // ---------------------------------------------------------------
    // java_to_term(+JavaObject, -Term)
    // ---------------------------------------------------------------
    private boolean executeJavaToTerm(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 2) return false;

        Term objTerm = args.get(0).resolveBindings(bindings);
        Term resultTerm = args.get(1).resolveBindings(bindings);

        if (objTerm instanceof Variable) throw Errors.instantiation(pname(), parity(), "the object must be bound");   // ISS-2025-0796
        // Check if it's a Java reference
        Object obj = resolveJavaObject(objTerm);
        if (obj != null) {
            Term converted = javaObjectToTerm(obj);
            return unifyTerms(resultTerm, converted, bindings);
        }

        // Not a Java reference - if it's already a Prolog term (Atom, Number, etc.),
        // just unify directly
        if (objTerm instanceof Atom && !"null".equals(((Atom) objTerm).getName())) {
            return unifyTerms(resultTerm, objTerm, bindings);
        }
        if (objTerm instanceof Number || objTerm instanceof PrologString || objTerm instanceof CompoundTerm) {
            return unifyTerms(resultTerm, objTerm, bindings);
        }

        // Null case
        Term converted = javaObjectToTerm(null);
        return unifyTerms(resultTerm, converted, bindings);
    }

    // ---------------------------------------------------------------
    // java_from_term(+Term, -JavaObject)
    // ---------------------------------------------------------------
    private boolean executeJavaFromTerm(List<Term> args, Map<String, Term> bindings) throws Exception {
        if (args.size() != 2) return false;

        Term inputTerm = args.get(0).resolveBindings(bindings);
        Term resultTerm = args.get(1).resolveBindings(bindings);

        if (inputTerm instanceof Variable) throw Errors.instantiation(pname(), parity(), "the term must be bound");   // ISS-2025-0796
        Object javaObj = termToJavaObject(inputTerm);
        Term refTerm = registerObject(javaObj);
        return unifyTerms(resultTerm, refTerm, bindings);
    }

    // START_CHANGE: ISS-2025-0173 - java_release_ref/1 removes a single reference from refTable
    // ---------------------------------------------------------------
    // java_release_ref(+RefKey)
    // ---------------------------------------------------------------
    private boolean executeJavaReleaseRef(List<Term> args, Map<String, Term> bindings) {
        if (args.size() != 1) return false;

        Term refKeyTerm = args.get(0).resolveBindings(bindings);
        if (refKeyTerm instanceof Variable) throw Errors.instantiation(pname(), parity(), "the reference must be bound");   // ISS-2025-0796
        if (!(refKeyTerm instanceof Atom)) throw Errors.type("atom", refKeyTerm, pname(), parity(), "the reference must be an atom");

        String refKey = ((Atom) refKeyTerm).getName();
        return refTable.remove(refKey) != null;
    }

    // ---------------------------------------------------------------
    // java_gc/0
    // ---------------------------------------------------------------
    private boolean executeJavaGc(List<Term> args, Map<String, Term> bindings) {
        if (args.size() != 0) return false;

        int size = refTable.size();
        refTable.clear();
        refCounter.set(0);
        LOGGER.info("java_gc: cleared " + size + " Java object references");
        return true;
    }
    // END_CHANGE: ISS-2025-0173

    // ===============================================================
    // Utility methods
    // ===============================================================

    /**
     * Register a Java object in the reference table and return an Atom referencing it.
     */
    static Term registerObject(Object obj) {
        if (obj == null) {
            return new Atom("null");
        }
        // START_CHANGE: ISS-2025-0173 - Warn when refTable exceeds soft limit
        if (refTable.size() >= REF_TABLE_SOFT_LIMIT) {
            LOGGER.warning("JavaFFI refTable has exceeded " + REF_TABLE_SOFT_LIMIT
                + " entries (" + refTable.size() + "). Consider calling java_release_ref/1 or java_gc/0.");
        }
        // END_CHANGE: ISS-2025-0173
        String ref = REF_PREFIX + refCounter.incrementAndGet();
        refTable.put(ref, obj);
        return new Atom(ref);
    }

    /**
     * Look up a Java object from a term. If the term is an Atom starting with $java_ref_,
     * look it up in the reference table. Otherwise return null.
     */
    static Object resolveJavaObject(Term term) {
        if (term instanceof Atom) {
            String name = ((Atom) term).getName();
            if (name.startsWith(REF_PREFIX)) {
                return refTable.get(name);
            }
            // Check if it's a class name for static operations
            if ("null".equals(name)) return null;
        }
        return null;
    }

    /**
     * Extract a String from a Term (Atom or PrologString).
     */
    private String termToString(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName();
        } else if (term instanceof PrologString) {
            return ((PrologString) term).getStringValue();
        }
        return null;
    }

    /**
     * Convert a Prolog list term to a Java List of Objects.
     */
    private List<Object> prologListToJavaList(Term listTerm, Map<String, Term> bindings) {
        List<Object> result = new ArrayList<>();
        Term current = listTerm;

        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (!".".equals(compound.getFunctor().getName()) || compound.getArguments().size() != 2) {
                break;
            }
            Term head = compound.getArguments().get(0).resolveBindings(bindings);
            result.add(termToJavaObject(head));
            current = compound.getArguments().get(1).resolveBindings(bindings);
        }

        // Must end with [] for a proper list
        if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
            return result;
        }

        // Also accept empty list directly
        if (listTerm instanceof Atom && "[]".equals(((Atom) listTerm).getName())) {
            return new ArrayList<>();
        }

        return null;
    }

    /**
     * Convert a Prolog Term to a Java Object.
     */
    private Object termToJavaObject(Term term) {
        if (term instanceof Number) {
            Number num = (Number) term;
            if (num.isInteger()) {
                long val = num.longValue();
                if (val >= Integer.MIN_VALUE && val <= Integer.MAX_VALUE) {
                    return (int) val;
                }
                return val;
            }
            return num.getValue();
        } else if (term instanceof Atom) {
            String name = ((Atom) term).getName();
            if ("true".equals(name)) return Boolean.TRUE;
            if ("false".equals(name)) return Boolean.FALSE;
            if ("null".equals(name)) return null;
            if (name.startsWith(REF_PREFIX)) {
                return refTable.get(name);
            }
            return name;
        } else if (term instanceof PrologString) {
            return ((PrologString) term).getStringValue();
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            // Check if it's a list
            if (".".equals(compound.getFunctor().getName()) && compound.getArguments().size() == 2) {
                List<Object> list = new ArrayList<>();
                Term current = term;
                while (current instanceof CompoundTerm) {
                    CompoundTerm c = (CompoundTerm) current;
                    if (!".".equals(c.getFunctor().getName()) || c.getArguments().size() != 2) break;
                    list.add(termToJavaObject(c.getArguments().get(0)));
                    current = c.getArguments().get(1);
                }
                if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
                    return list;
                }
            }
            return term.toString();
        }
        return term.toString();
    }

    /**
     * Convert a Java Object to a Prolog Term.
     */
    Term javaObjectToTerm(Object obj) {
        if (obj == null) {
            return new Atom("null");
        } else if (obj instanceof Boolean) {
            return new Atom(obj.toString());
        } else if (obj instanceof Integer) {
            return new Number((long) (int) (Integer) obj)   /* ISS-2025-0424 */;
        } else if (obj instanceof Long) {
            return new Number((long) (Long) obj)   /* ISS-2025-0424 */;
        } else if (obj instanceof Float) {
            return new Number((double) (float) obj, false);
        } else if (obj instanceof Double) {
            return new Number((double) obj, false);
        } else if (obj instanceof Short) {
            return new Number((long) (short) (Short) obj)   /* ISS-2025-0424 */;
        } else if (obj instanceof Byte) {
            return new Number((long) (byte) (Byte) obj)   /* ISS-2025-0424 */;
        } else if (obj instanceof Character) {
            return new Atom(obj.toString());
        } else if (obj instanceof String) {
            return new Atom((String) obj);
        } else if (obj instanceof java.util.List) {
            return javaListToPrologList((java.util.List<?>) obj);
        } else if (obj.getClass().isArray()) {
            // Return array as a reference
            return registerObject(obj);
        } else {
            // Wrap as a reference
            return registerObject(obj);
        }
    }

    /**
     * Convert a Java List to a Prolog list term.
     */
    private Term javaListToPrologList(java.util.List<?> list) {
        Term result = new Atom("[]");
        for (int i = list.size() - 1; i >= 0; i--) {
            Term head = javaObjectToTerm(list.get(i));
            result = new CompoundTerm(new Atom("."), Arrays.asList(head, result));
        }
        return result;
    }

    /**
     * Resolve a class name to a Class object, supporting common short names.
     */
    private Class<?> resolveClass(String className) {
        // Handle primitive types
        switch (className) {
            case "int": return int.class;
            case "long": return long.class;
            case "double": return double.class;
            case "float": return float.class;
            case "boolean": return boolean.class;
            case "byte": return byte.class;
            case "char": return char.class;
            case "short": return short.class;
            case "void": return void.class;
            // Common short names
            case "String": return String.class;
            case "Integer": return Integer.class;
            case "Long": return Long.class;
            case "Double": return Double.class;
            case "Float": return Float.class;
            case "Boolean": return Boolean.class;
            case "Object": return Object.class;
            case "ArrayList": return ArrayList.class;
            case "HashMap": return HashMap.class;
            case "LinkedList": return java.util.LinkedList.class;
            case "HashSet": return java.util.HashSet.class;
            case "StringBuilder": return StringBuilder.class;
            case "Math": return Math.class;
        }

        try {
            return Class.forName(className);
        } catch (ClassNotFoundException e) {
            // Try java.lang and java.util prefixes
            try {
                return Class.forName("java.lang." + className);
            } catch (ClassNotFoundException e2) {
                try {
                    return Class.forName("java.util." + className);
                } catch (ClassNotFoundException e3) {
                    return null;
                }
            }
        }
    }

    /**
     * Find a matching constructor for the given arguments.
     */
    private Constructor<?> findConstructor(Class<?> clazz, Object[] args) {
        Constructor<?>[] constructors = clazz.getDeclaredConstructors();
        for (Constructor<?> c : constructors) {
            if (c.getParameterCount() == args.length && isAssignable(c.getParameterTypes(), args)) {
                return c;
            }
        }
        // Try with auto-boxing/widening
        for (Constructor<?> c : constructors) {
            if (c.getParameterCount() == args.length && isLooselyAssignable(c.getParameterTypes(), args)) {
                return c;
            }
        }
        return null;
    }

    /**
     * Find a matching method for the given arguments.
     */
    private Method findMethod(Class<?> clazz, String name, Object[] args, boolean isStatic) {
        // Search the class hierarchy
        Class<?> current = clazz;
        while (current != null) {
            Method[] methods = current.getDeclaredMethods();
            for (Method m : methods) {
                if (m.getName().equals(name) && m.getParameterCount() == args.length) {
                    if (isStatic && !Modifier.isStatic(m.getModifiers())) continue;
                    if (isAssignable(m.getParameterTypes(), args) || isLooselyAssignable(m.getParameterTypes(), args)) {
                        return m;
                    }
                }
            }
            current = current.getSuperclass();
        }

        // Also check interfaces
        for (Class<?> iface : getAllInterfaces(clazz)) {
            Method[] methods = iface.getDeclaredMethods();
            for (Method m : methods) {
                if (m.getName().equals(name) && m.getParameterCount() == args.length) {
                    if (isAssignable(m.getParameterTypes(), args) || isLooselyAssignable(m.getParameterTypes(), args)) {
                        return m;
                    }
                }
            }
        }

        return null;
    }

    /**
     * Get all interfaces implemented by a class, including inherited ones.
     */
    private Set<Class<?>> getAllInterfaces(Class<?> clazz) {
        Set<Class<?>> interfaces = new LinkedHashSet<>();
        Class<?> current = clazz;
        while (current != null) {
            for (Class<?> iface : current.getInterfaces()) {
                interfaces.add(iface);
                interfaces.addAll(getAllInterfaces(iface));
            }
            current = current.getSuperclass();
        }
        return interfaces;
    }

    /**
     * Find a field by name, searching the class hierarchy.
     */
    private Field findField(Class<?> clazz, String name) {
        Class<?> current = clazz;
        while (current != null) {
            try {
                return current.getDeclaredField(name);
            } catch (NoSuchFieldException e) {
                current = current.getSuperclass();
            }
        }
        return null;
    }

    /**
     * Check if arguments are assignable to parameter types (strict).
     */
    private boolean isAssignable(Class<?>[] paramTypes, Object[] args) {
        for (int i = 0; i < paramTypes.length; i++) {
            if (args[i] == null) {
                if (paramTypes[i].isPrimitive()) return false;
                continue;
            }
            Class<?> argType = args[i].getClass();
            if (!wrap(paramTypes[i]).isAssignableFrom(argType)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Check if arguments are loosely assignable (with numeric widening).
     */
    private boolean isLooselyAssignable(Class<?>[] paramTypes, Object[] args) {
        for (int i = 0; i < paramTypes.length; i++) {
            if (args[i] == null) {
                if (paramTypes[i].isPrimitive()) return false;
                continue;
            }
            Class<?> pt = wrap(paramTypes[i]);
            Class<?> at = args[i].getClass();
            if (pt.isAssignableFrom(at)) continue;
            // Allow numeric widening
            if (isNumericType(pt) && isNumericType(at)) continue;
            // Allow CharSequence for String
            if (pt == CharSequence.class && at == String.class) continue;
            if (pt == Object.class) continue;
            return false;
        }
        return true;
    }

    /**
     * Convert arguments to match parameter types.
     */
    private Object[] convertArguments(Class<?>[] paramTypes, Object[] args) {
        Object[] result = new Object[args.length];
        for (int i = 0; i < args.length; i++) {
            result[i] = convertArg(paramTypes[i], args[i]);
        }
        return result;
    }

    /**
     * Convert a single argument to match the target parameter type.
     */
    private Object convertArg(Class<?> paramType, Object arg) {
        if (arg == null) return null;
        Class<?> wrapped = wrap(paramType);

        if (wrapped.isInstance(arg)) return arg;

        // Numeric conversions
        if (arg instanceof java.lang.Number) {
            java.lang.Number num = (java.lang.Number) arg;
            if (wrapped == Integer.class || paramType == int.class) return num.intValue();
            if (wrapped == Long.class || paramType == long.class) return num.longValue();
            if (wrapped == Double.class || paramType == double.class) return num.doubleValue();
            if (wrapped == Float.class || paramType == float.class) return num.floatValue();
            if (wrapped == Short.class || paramType == short.class) return num.shortValue();
            if (wrapped == Byte.class || paramType == byte.class) return num.byteValue();
        }

        // String to char
        if ((wrapped == Character.class || paramType == char.class) && arg instanceof String) {
            String s = (String) arg;
            if (s.length() == 1) return s.charAt(0);
        }

        return arg;
    }

    /**
     * Wrap primitive types to their boxed equivalents.
     */
    private Class<?> wrap(Class<?> type) {
        if (type == int.class) return Integer.class;
        if (type == long.class) return Long.class;
        if (type == double.class) return Double.class;
        if (type == float.class) return Float.class;
        if (type == boolean.class) return Boolean.class;
        if (type == byte.class) return Byte.class;
        if (type == char.class) return Character.class;
        if (type == short.class) return Short.class;
        return type;
    }

    /**
     * Check if a type is numeric.
     */
    private boolean isNumericType(Class<?> type) {
        return type == Integer.class || type == Long.class || type == Double.class
            || type == Float.class || type == Short.class || type == Byte.class
            || type == int.class || type == long.class || type == double.class
            || type == float.class || type == short.class || type == byte.class;
    }

    /**
     * Unify two terms using the standard Term.unify mechanism.
     */
    private boolean unifyTerms(Term t1, Term t2, Map<String, Term> bindings) {
        Term resolved1 = t1.resolveBindings(bindings);
        Term resolved2 = t2.resolveBindings(bindings);
        return resolved1.unify(resolved2, bindings);
    }

    /**
     * Get the reference table (for testing and cleanup purposes).
     */
    public static Map<String, Object> getReferenceTable() {
        return Collections.unmodifiableMap(refTable);
    }

    /**
     * Clear all references (for cleanup in tests).
     */
    public static void clearReferences() {
        refTable.clear();
        refCounter.set(0);
    }
}
// END_CHANGE: ISS-2025-0160
