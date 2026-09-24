package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0475 - engine v4 wave W7 (design B.12): the ISO term writer.
/**
 * The term writer behind {@code write/1,2}, {@code writeq/1,2}, {@code print/1,2},
 * {@code write_canonical/1,2}, {@code write_term/2,3}, {@code portray_clause/1,2},
 * {@code print_message/2} and the console's answer printing — on <b>both</b> engines.
 *
 * <p>Three properties the old {@code core.util.TermFormatter} did not have:
 * <ul>
 *   <li><b>Fully iterative.</b> Everything runs off an explicit work stack, so a 1 000 000-element
 *       list, a 1 000 000-deep right-nested operator chain and a 1 000 000-deep last-argument spine
 *       all print at the default JVM stack. {@code TermFormatter} was iterative on the last argument
 *       of a plain compound only; an operator chain still recursed.</li>
 *   <li><b>Cycle-safe.</b> A rational tree (which the v4 engine creates happily — design decision 2)
 *       terminates: with {@code cycles(false)} a back edge prints as {@code ...}; with
 *       {@code cycles(true)} the term is rewritten into the SWI {@code @(Template, Substitutions)}
 *       form first. List spines are checked with Brent's algorithm, so a 1 M-element acyclic list
 *       costs no extra memory.</li>
 *   <li><b>The complete ISO option set</b>: {@code quoted}, {@code ignore_ops}, {@code numbervars},
 *       {@code max_depth}, {@code portray}, {@code cycles}, {@code variable_names},
 *       {@code spacing(next_argument)}.</li>
 * </ul>
 *
 * <p>The rendering rules (quoting, operator spacing, token-adjacency guards) are the ones
 * {@code TermFormatter} established under ISS-2025-0387/0388/0389 and the suite pins; the one
 * difference is that the separator decisions are made against the characters already emitted
 * instead of against re-formatted substrings, which is exact and needs no lookahead buffer.
 */
public final class Writer {

    /** The hook {@code portray(true)} calls: returns the user's rendering of {@code t}, or null. */
    public interface Portray {
        String portray(Term t);
    }

    /** The {@code write_term/2,3} option set. */
    public static final class Options {
        public boolean quoted;
        public boolean ignoreOps;
        public boolean numbervars = true;
        public int maxDepth;                    // 0 = unlimited
        public boolean portray;
        public boolean cycles;
        public boolean spacingNextArgument;
        public OperatorTable ops;
        public Portray portrayHook;
        public IdentityHashMap<Variable, String> variableNames;

        public Options() { }

        /** {@code write/1}: unquoted, operators, numbervars. */
        public static Options write() { return new Options(); }

        /** {@code writeq/1} and {@code print/1}: quoted, operators, numbervars. */
        public static Options writeq() {
            Options o = new Options();
            o.quoted = true;
            return o;
        }

        /** {@code write_canonical/1}: quoted, no operators, no numbervars. */
        public static Options canonical() {
            Options o = new Options();
            o.quoted = true;
            o.ignoreOps = true;
            o.numbervars = false;
            return o;
        }

        OperatorTable table() {
            if (ops != null) return ops;
            return Ops.current().table();
        }
    }

    private Writer() { }

    // ------------------------------------------------------------------
    // entry points
    // ------------------------------------------------------------------

    /** Render {@code t} with {@code o} at the top-level priority 1200. */
    public static String format(Term t, Options o) { return format(t, o, 1200); }

    /** Render {@code t} with {@code o} inside a context of priority {@code maxPrec}. */
    public static String format(Term t, Options o, int maxPrec) {
        StringBuilder sb = new StringBuilder();
        write(sb, t, o, maxPrec);
        return sb.toString();
    }

    /** Append the rendering of {@code t} to {@code sb}. */
    public static void write(StringBuilder sb, Term t, Options o, int maxPrec) {
        if (t == null) { sb.append("<null>"); return; }
        Term root = t;
        if (o.cycles) root = breakCycles(root);
        OperatorTable ops = o.table();
        Deque<Object> stack = new ArrayDeque<Object>();
        IdentityHashMap<Term, Boolean> onPath = new IdentityHashMap<Term, Boolean>();
        stack.push(new W(root, maxPrec, 0));
        while (!stack.isEmpty()) {
            Object item = stack.pop();
            if (item instanceof String) { sb.append((String) item); continue; }
            if (item instanceof Pop) { onPath.remove(((Pop) item).t); continue; }
            if (item instanceof Lst) { emitListTail(sb, (Lst) item, stack, o, ops); continue; }
            if (item instanceof Sep) { emitSeparator(sb, (Sep) item, o); continue; }
            emit(sb, (W) item, stack, onPath, o, ops);
        }
    }

    // ------------------------------------------------------------------
    // work items
    // ------------------------------------------------------------------

    /** A term still to be written, with its context priority and nesting depth. */
    private static final class W {
        final Term t; final int prec; final int depth;
        /** ISS-2025-0562: an operand of an operator (an operator atom there is bracketed). */
        final boolean operand;
        W(Term t, int prec, int depth) { this(t, prec, depth, false); }
        W(Term t, int prec, int depth, boolean operand) {
            this.t = t; this.prec = prec; this.depth = depth; this.operand = operand;
        }
    }

    /** Drop {@code t} from the current path once its subtree is finished. */
    private static final class Pop {
        final Term t;
        Pop(Term t) { this.t = t; }
    }

    /** The rest of a list spine, carrying Brent's cycle-detection state. */
    private static final class Lst {
        Term tail; int depth; int count;
        Term slow; int power = 1; int lam;
        Lst(Term tail, int depth, int count, Term slow) {
            this.tail = tail; this.depth = depth; this.count = count; this.slow = slow;
        }
    }

    /** An operator token whose surrounding spacing depends on the characters around it. */
    private static final class Sep {
        final String token; final Term right; final int rightPrec; final boolean alpha;
        final boolean rightIsNumber; final boolean hasLeft;
        Sep(String token, Term right, int rightPrec, boolean alpha, boolean rightIsNumber, boolean hasLeft) {
            this.token = token; this.right = right; this.rightPrec = rightPrec;
            this.alpha = alpha; this.rightIsNumber = rightIsNumber; this.hasLeft = hasLeft;
        }
    }

    // ------------------------------------------------------------------
    // the writer proper
    // ------------------------------------------------------------------

    private static void emit(StringBuilder sb, W w, Deque<Object> stack,
                             IdentityHashMap<Term, Boolean> onPath, Options o, OperatorTable ops) {
        Term t = Unify.deref(w.t);

        if (o.portray && o.portrayHook != null) {
            String custom = o.portrayHook.portray(t);
            if (custom != null) { sb.append(custom); return; }
        }

        if (t instanceof Variable) { sb.append(variableName((Variable) t, o)); return; }
        if (t instanceof Number) { sb.append(numberText((Number) t, o.quoted)); return; }   // ISS-2025-0564
        if (t instanceof PrologString) {
            String s = ((PrologString) t).getStringValue();
            sb.append(o.quoted ? "\"" + escapeString(s) + "\"" : s);
            return;
        }
        if (t instanceof Atom) {
            // START_CHANGE: ISS-2025-0562 - an operator atom as an operand is bracketed: -(-,-)
            // is (-)-(-), 1-(-) is 1-(-) (ISO 6.3.4.2: such an operand has priority 1201).
            String name = ((Atom) t).getName();
            if (w.operand && !o.ignoreOps && isOperatorAtom(name, ops)) {
                sb.append('(').append(atomText(name, o.quoted)).append(')');
                return;
            }
            // END_CHANGE: ISS-2025-0562
            sb.append(atomText(name, o.quoted));
            return;
        }
        if (!(t instanceof CompoundTerm)) { sb.append(t.toString()); return; }

        CompoundTerm c = (CompoundTerm) t;
        List<Term> args = c.getArguments();
        if (args == null || args.isEmpty()) { sb.append(atomText(c.getName(), o.quoted)); return; }

        // max_depth counts the printed nesting levels: the root compound is level 1, so
        // f(f(f(f(a)))) with max_depth(3) prints f(f(...)), matching the list rule below.
        if (o.maxDepth > 0 && w.depth + 1 >= o.maxDepth) { sb.append("..."); return; }

        // '$VAR'(N) under numbervars(true)
        if (o.numbervars && "$VAR".equals(c.getName()) && args.size() == 1) {
            Term a = Unify.deref(args.get(0));
            if (a instanceof Number && ((Number) a).isInteger() && ((Number) a).longValue() >= 0) {
                long n = ((Number) a).longValue();
                char letter = (char) ('A' + (n % 26));
                long idx = n / 26;
                sb.append(idx == 0 ? String.valueOf(letter) : letter + Long.toString(idx));
                return;
            }
            if (a instanceof Atom) { sb.append(((Atom) a).getName()); return; }
        }

        if (onPath.containsKey(c)) { sb.append("..."); return; }

        String name = c.getName();

        // list notation
        if (!o.ignoreOps && ".".equals(name) && args.size() == 2) {
            sb.append('[');
            stack.push(new Lst(args.get(1), w.depth, 1, c));
            stack.push(new W(args.get(0), 999, w.depth + 1));
            return;
        }
        // curly braces
        if (!o.ignoreOps && "{}".equals(name) && args.size() == 1) {
            onPath.put(c, Boolean.TRUE);
            sb.append('{');
            stack.push(new Pop(c));
            stack.push("}");
            stack.push(new W(args.get(0), 1200, w.depth + 1));
            return;
        }

        if (!o.ignoreOps && args.size() == 2) {
            // ISS-2025-0734: '|'(A,B) is written canonically, as SWI does ('|'(a,b))
            Operator op = "|".equals(name) ? null : ops.getInfixOperator(name);
            if (op != null) {
                onPath.put(c, Boolean.TRUE);
                boolean paren = op.getPrecedence() > w.prec;
                if (paren) sb.append('(');
                stack.push(new Pop(c));
                if (paren) stack.push(")");
                stack.push(new W(args.get(1), op.getRightPrecedence(), w.depth + 1, true));
                if (",".equals(name)) {
                    stack.push(o.spacingNextArgument ? ", " : ",");
                } else {
                    Term right = Unify.deref(args.get(1));
                    stack.push(new Sep(atomText(name, o.quoted), right, op.getRightPrecedence(),
                        isAlphaOp(name), false, true));
                }
                stack.push(new W(args.get(0), op.getLeftPrecedence(), w.depth + 1, true));
                return;
            }
        }
        if (!o.ignoreOps && args.size() == 1) {
            Operator pre = ops.getPrefixOperator(name);
            // START_CHANGE: ISS-2025-0562 - +/-(Operand) whose operand starts with a number: write
            // the canonical form, or the output re-reads as a negative literal: -(1) is -(1)
            // (not - 1), -(2^2) is -(2^2) (not -2^2, which reads as (-2)^2). SWI does the same.
            if (pre != null && ("-".equals(name) || "+".equals(name))) {
                Term inner0 = Unify.deref(args.get(0));
                if (inner0 instanceof Number || Character.isDigit(firstChar(inner0, pre.getRightPrecedence(), o))) {
                    onPath.put(c, Boolean.TRUE);
                    sb.append(atomText(name, o.quoted)).append('(');
                    stack.push(new Pop(c));
                    stack.push(")");
                    stack.push(new W(args.get(0), 999, w.depth + 1));
                    return;
                }
            }
            // END_CHANGE: ISS-2025-0562
            if (pre != null) {
                onPath.put(c, Boolean.TRUE);
                boolean paren = pre.getPrecedence() > w.prec;
                if (paren) sb.append('(');
                stack.push(new Pop(c));
                if (paren) stack.push(")");
                stack.push(new W(args.get(0), pre.getRightPrecedence(), w.depth + 1, true));
                Term inner = Unify.deref(args.get(0));
                stack.push(new Sep(atomText(name, o.quoted), inner, pre.getRightPrecedence(),
                    isAlphaOp(name), inner instanceof Number, false));
                return;
            }
            Operator post = ops.getPostfixOperator(name);
            if (post != null) {
                onPath.put(c, Boolean.TRUE);
                boolean paren = post.getPrecedence() > w.prec;
                if (paren) sb.append('(');
                stack.push(new Pop(c));
                if (paren) stack.push(")");
                stack.push(new Sep(atomText(name, o.quoted), null, 0, isAlphaOp(name), false, true));
                stack.push(new W(args.get(0), post.getLeftPrecedence(), w.depth + 1, true));
                return;
            }
        }

        // functional notation f(A1, ..., An)
        onPath.put(c, Boolean.TRUE);
        // ISS-2025-0562: '[]'(a,b) and '{}'(a,b) must stay quoted to re-read as compounds
        sb.append(o.quoted && ("[]".equals(name) || "{}".equals(name)) ? quoteAtom(name) : atomText(name, o.quoted));
        sb.append('(');
        stack.push(new Pop(c));
        stack.push(")");
        String comma = o.spacingNextArgument ? ", " : ",";
        for (int i = args.size() - 1; i >= 0; i--) {
            stack.push(new W(args.get(i), 999, w.depth + 1));
            if (i > 0) stack.push(comma);
        }
    }

    /** Continue a list spine: {@code ,Elem}, {@code |Tail]} or {@code ]}. */
    private static void emitListTail(StringBuilder sb, Lst l, Deque<Object> stack,
                                     Options o, OperatorTable ops) {
        Term tail = Unify.deref(l.tail);
        if (tail instanceof Atom && "[]".equals(((Atom) tail).getName())) { sb.append(']'); return; }
        if (!(tail instanceof CompoundTerm) || !".".equals(tail.getName())
                || tail.getArguments() == null || tail.getArguments().size() != 2) {
            sb.append('|');
            stack.push("]");
            stack.push(new W(tail, 999, l.depth + 1));
            return;
        }
        // Brent's cycle detection along the spine: O(1) memory, so a 1 M-element list is free
        if (tail == l.slow) { sb.append("|...]"); return; }
        if (l.lam == l.power) { l.slow = tail; l.power <<= 1; l.lam = 0; }
        l.lam++;
        if (o.maxDepth > 0 && l.count >= o.maxDepth - 1) { sb.append("|...]"); return; }

        List<Term> as = tail.getArguments();
        sb.append(o.spacingNextArgument ? ", " : ",");
        Lst next = new Lst(as.get(1), l.depth, l.count + 1, l.slow);
        next.power = l.power;
        next.lam = l.lam;
        stack.push(next);
        stack.push(new W(as.get(0), 999, l.depth + 1));
    }

    /**
     * Emit an operator token with the spacing ISO re-readability needs: alphabetic operators are
     * always surrounded by spaces, and two symbolic runs that would be re-tokenized as one atom
     * under maximal munch get a separating space ({@code 1 - -1} must not print as {@code 1--1}).
     * The left side is exact — it is the character already in the buffer.
     */
    private static void emitSeparator(StringBuilder sb, Sep s, Options o) {
        char last = sb.length() == 0 ? 0 : sb.charAt(sb.length() - 1);
        if (s.alpha) {
            if (s.hasLeft) sb.append(' ');
            sb.append(s.token);
            if (s.right != null) sb.append(' ');
            return;
        }
        if (s.hasLeft && last != 0 && isSymbolic(last)
                && !s.token.isEmpty() && isSymbolic(s.token.charAt(0))) {
            sb.append(' ');
        }
        sb.append(s.token);
        if (s.right == null) return;                    // postfix: nothing follows
        char first = firstChar(s.right, s.rightPrec, o);
        boolean needSpace = (!s.token.isEmpty() && isSymbolic(s.token.charAt(s.token.length() - 1)))
            && ((first != 0 && isSymbolic(first)) || s.rightIsNumber);
        // ISS-2025-0562: a PREFIX operator directly followed by '(' would re-read as the functional
        // notation op(...) (- (a,b) is -/1, -(a,b) is -/2), and a solo/punctuation-named prefix
        // operator needs the same guard.
        if (!needSpace && !s.hasLeft && first == '(') needSpace = true;
        if (needSpace) sb.append(' ');
    }

    /**
     * The first character {@code t} will render as inside a context of priority {@code prec}.
     * Only the leftmost spine is walked, and the walk is bounded.
     */
    private static char firstChar(Term t, int prec, Options o) {
        OperatorTable ops = o.table();
        for (int guard = 0; guard < 4096; guard++) {
            t = Unify.deref(t);
            if (t instanceof Variable) {
                String n = variableName((Variable) t, o);
                return n.isEmpty() ? 0 : n.charAt(0);
            }
            if (t instanceof Number) {
                String n = numberText((Number) t, o.quoted);   // ISS-2025-0564
                return n.isEmpty() ? 0 : n.charAt(0);
            }
            if (t instanceof PrologString) return '"';
            if (t instanceof Atom) {
                // ISS-2025-0562: every atom reached here is an operand; operator atoms are bracketed
                if (!o.ignoreOps && isOperatorAtom(((Atom) t).getName(), ops)) return '(';
                String n = atomText(((Atom) t).getName(), o.quoted);
                return n.isEmpty() ? 0 : n.charAt(0);
            }
            if (!(t instanceof CompoundTerm)) return 0;
            CompoundTerm c = (CompoundTerm) t;
            List<Term> args = c.getArguments();
            if (args == null || args.isEmpty()) {
                String n = atomText(c.getName(), o.quoted);
                return n.isEmpty() ? 0 : n.charAt(0);
            }
            if (o.numbervars && "$VAR".equals(c.getName()) && args.size() == 1) {
                Term a = Unify.deref(args.get(0));
                if (a instanceof Number && ((Number) a).longValue() >= 0) {
                    return (char) ('A' + (((Number) a).longValue() % 26));
                }
            }
            if (!o.ignoreOps) {
                if (".".equals(c.getName()) && args.size() == 2) return '[';
                if ("{}".equals(c.getName()) && args.size() == 1) return '{';
                if (args.size() == 2) {
                    Operator op = "|".equals(c.getName()) ? null : ops.getInfixOperator(c.getName());   // ISS-2025-0734
                    if (op != null) {
                        if (op.getPrecedence() > prec) return '(';
                        prec = op.getLeftPrecedence();
                        t = args.get(0);
                        continue;
                    }
                }
                if (args.size() == 1) {
                    Operator pre = ops.getPrefixOperator(c.getName());
                    if (pre != null) {
                        if ("-".equals(c.getName()) || "+".equals(c.getName())) return c.getName().charAt(0);
                        if (pre.getPrecedence() > prec) return '(';
                        String n = atomText(c.getName(), o.quoted);
                        return n.isEmpty() ? 0 : n.charAt(0);
                    }
                    Operator post = ops.getPostfixOperator(c.getName());
                    if (post != null) {
                        if (post.getPrecedence() > prec) return '(';
                        prec = post.getLeftPrecedence();
                        t = args.get(0);
                        continue;
                    }
                }
            }
            String n = atomText(c.getName(), o.quoted);
            return n.isEmpty() ? 0 : n.charAt(0);
        }
        return 0;
    }

    // ------------------------------------------------------------------
    // cycles(true): the SWI @(Template, Substitutions) rewrite
    // ------------------------------------------------------------------

    /**
     * If {@code t} is a rational tree, return {@code @(Template, Substitutions)} with each cyclic
     * subterm replaced by a fresh variable that the substitution list binds to the (broken) subterm
     * — the form {@code write_term(X, [cycles(true)])} produces in SWI for {@code X = f(X)}:
     * {@code @(_S1, [_S1=f(_S1)])}. An acyclic term is returned unchanged.
     */
    public static Term breakCycles(Term t) {
        if (!Unify.isCyclic(t, null)) return t;
        IdentityHashMap<Term, Variable> vars = new IdentityHashMap<Term, Variable>();
        IdentityHashMap<Term, Boolean> path = new IdentityHashMap<Term, Boolean>();
        findBackEdges(t, path, vars, 0);
        List<Term> substs = new ArrayList<Term>();
        for (Map.Entry<Term, Variable> e : vars.entrySet()) {
            Term body = rebuild(e.getKey(), vars, true, 0);
            substs.add(new CompoundTerm(new Atom("="), Arrays.asList((Term) e.getValue(), body)));
        }
        // The template substitutes at the root too: SWI prints `X = f(X)` as @(A,[A=f(A)]),
        // i.e. the template IS the variable when the root itself is the cyclic node.
        Term template = rebuild(t, vars, false, 0);
        Term list = new Atom("[]");
        for (int i = substs.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), Arrays.asList(substs.get(i), list));
        }
        return new CompoundTerm(new Atom("@"), Arrays.asList(template, list));
    }

    private static void findBackEdges(Term t, IdentityHashMap<Term, Boolean> path,
                                      IdentityHashMap<Term, Variable> vars, int depth) {
        if (depth > 10000) return;
        t = Unify.deref(t);
        if (!(t instanceof CompoundTerm)) return;
        if (path.containsKey(t)) {
            if (!vars.containsKey(t)) vars.put(t, new Variable("_S" + (vars.size() + 1)));
            return;
        }
        path.put(t, Boolean.TRUE);
        List<Term> args = ((CompoundTerm) t).getArguments();
        if (args != null) for (int i = 0; i < args.size(); i++) findBackEdges(args.get(i), path, vars, depth + 1);
        path.remove(t);
    }

    private static Term rebuild(Term t, IdentityHashMap<Term, Variable> vars, boolean root, int depth) {
        t = Unify.deref(t);
        if (!(t instanceof CompoundTerm) || depth > 10000) return t;
        if (!root && vars.containsKey(t)) return vars.get(t);
        CompoundTerm c = (CompoundTerm) t;
        List<Term> args = c.getArguments();
        if (args == null) return t;
        List<Term> out = new ArrayList<Term>(args.size());
        for (int i = 0; i < args.size(); i++) out.add(rebuild(args.get(i), vars, false, depth + 1));
        return new CompoundTerm(new Atom(c.getName()), out);
    }

    // ------------------------------------------------------------------
    // portray_clause / print_message
    // ------------------------------------------------------------------

    /**
     * {@code portray_clause/1}: the clause, quoted, with numbered variables, one body goal per line
     * and a terminating full stop.
     */
    public static String portrayClause(Term clause, Options base) {
        // START_CHANGE: ISS-2025-0570 - 4.5 wave P3.7: SWI portray_clause/1 layout, re-readable:
        // quoted, operators, `A, B` argument spacing, variables A, B, ... and `_` for a singleton,
        // one body goal per line, and if-then-else / disjunction / negation-free control blocks as
        //     (   Cond
        //     ->  Then
        //     ;   Else
        //     )
        Options o = new Options();
        o.quoted = true;
        o.numbervars = true;
        o.spacingNextArgument = true;
        o.ops = (base == null) ? null : base.ops;
        Term t = numberTheVariables(clause);
        StringBuilder sb = new StringBuilder();
        Term head = t;
        Term body = null;
        if (t instanceof CompoundTerm && ":-".equals(t.getName())
                && t.getArguments() != null && t.getArguments().size() == 2) {
            head = t.getArguments().get(0);
            body = Unify.deref(t.getArguments().get(1));
            if (body instanceof Atom && "true".equals(((Atom) body).getName())) body = null;
        } else if (t instanceof CompoundTerm && ":-".equals(t.getName())
                && t.getArguments() != null && t.getArguments().size() == 1) {
            sb.append(":- ");                                   // a directive
            write(sb, t.getArguments().get(0), o, 1199);
            sb.append(".\n");
            return sb.toString();
        }
        write(sb, head, o, 1199);
        if (body != null) {
            sb.append(" :-");
            portrayBody(sb, body, 1, o, 0);
        }
        sb.append(".\n");
        return sb.toString();
    }

    private static void indent(StringBuilder sb, int level) {
        sb.append('\n');
        for (int i = 0; i < level; i++) sb.append("    ");
    }

    /** A conjunction, one goal per line at {@code level}, the first goal on a new line. */
    private static void portrayBody(StringBuilder sb, Term body, int level, Options o, int depth) {
        List<Term> goals = conjuncts(body);
        for (int i = 0; i < goals.size(); i++) {
            indent(sb, level);
            portrayGoal(sb, goals.get(i), level, o, depth);
            if (i < goals.size() - 1) sb.append(',');
        }
    }

    private static boolean isControl(Term g) {
        if (!(g instanceof CompoundTerm) || g.getArguments() == null || g.getArguments().size() != 2) return false;
        String n = g.getName();
        return ";".equals(n) || "->".equals(n) || "*->".equals(n);
    }

    /** One body goal at the current column; control constructs become an indented block. */
    private static void portrayGoal(StringBuilder sb, Term g, int level, Options o, int depth) {
        g = Unify.deref(g);
        if (!isControl(g) || depth > 64) {
            write(sb, g, o, 999);
            return;
        }
        // the alternatives of a right-nested ';' chain, each possibly Cond -> Then
        List<Term> alts = new ArrayList<Term>();
        Term cur = g;
        while (cur instanceof CompoundTerm && ";".equals(cur.getName())
                && cur.getArguments() != null && cur.getArguments().size() == 2) {
            alts.add(cur.getArguments().get(0));
            cur = Unify.deref(cur.getArguments().get(1));
        }
        alts.add(cur);
        sb.append("(   ");
        for (int i = 0; i < alts.size(); i++) {
            Term a = Unify.deref(alts.get(i));
            if (i > 0) {
                indent(sb, level);
                sb.append(";   ");
            }
            if (a instanceof CompoundTerm && ("->".equals(a.getName()) || "*->".equals(a.getName()))
                    && a.getArguments() != null && a.getArguments().size() == 2) {
                portrayInline(sb, a.getArguments().get(0), level, o, depth);
                indent(sb, level);
                sb.append("->".equals(a.getName()) ? "->  " : "*-> ");
                portrayInline(sb, a.getArguments().get(1), level, o, depth);
            } else {
                portrayInline(sb, a, level, o, depth);
            }
        }
        indent(sb, level);
        sb.append(')');
    }

    /** A conjunction inside a block: the first goal at the cursor, the rest aligned under it. */
    private static void portrayInline(StringBuilder sb, Term body, int level, Options o, int depth) {
        List<Term> goals = conjuncts(body);
        for (int i = 0; i < goals.size(); i++) {
            if (i > 0) {
                sb.append(',');
                indent(sb, level);
                sb.append("    ");
            }
            portrayGoal(sb, goals.get(i), level + 1, o, depth + 1);
        }
    }
    // END_CHANGE: ISS-2025-0570

    private static List<Term> conjuncts(Term body) {
        List<Term> out = new ArrayList<Term>();
        Term cur = Unify.deref(body);
        while (cur instanceof CompoundTerm && ",".equals(cur.getName())
                && cur.getArguments() != null && cur.getArguments().size() == 2) {
            out.add(cur.getArguments().get(0));
            cur = Unify.deref(cur.getArguments().get(1));
        }
        out.add(cur);
        return out;
    }

    /** Replace each distinct free variable by {@code '$VAR'(N)} so it prints as A, B, C, ... */
    // START_CHANGE: ISS-2025-0570 - iterative (the writer must never recurse on term depth), and
    // SWI's naming: variables in order of first occurrence, a singleton prints as `_`.
    private static Term numberTheVariables(Term t) {
        final IdentityHashMap<Variable, int[]> count = new IdentityHashMap<Variable, int[]>();
        ArrayDeque<Term> work = new ArrayDeque<Term>();
        List<Variable> order = new ArrayList<Variable>();
        work.push(t);
        long budget = 50_000_000L;                       // a rational tree must not hang listing
        while (!work.isEmpty()) {
            if (--budget < 0) return t;
            Term x = Unify.deref(work.pop());
            if (x instanceof Variable) {
                int[] n = count.get(x);
                if (n == null) { count.put((Variable) x, new int[] {1}); order.add((Variable) x); }
                else n[0]++;
            } else if (x instanceof CompoundTerm) {
                List<Term> as = ((CompoundTerm) x).getArguments();
                for (int i = as.size() - 1; i >= 0; i--) work.push(as.get(i));
            }
        }
        final IdentityHashMap<Variable, Term> names = new IdentityHashMap<Variable, Term>();
        long k = 0;
        for (Variable v : order) {
            names.put(v, count.get(v)[0] == 1
                ? new CompoundTerm(new Atom("$VAR"), Arrays.<Term>asList(new Atom("_")))
                : new CompoundTerm(new Atom("$VAR"), Arrays.<Term>asList(new Number(k++))));
        }
        return substitute(t, names);
    }

    /** Rebuild {@code t} with each variable replaced from {@code map}; iterative, shares ground parts. */
    private static Term substitute(Term t, IdentityHashMap<Variable, Term> map) {
        final class Frame {
            final CompoundTerm c; final Term[] out; int i; boolean changed;
            Frame(CompoundTerm c) { this.c = c; this.out = new Term[c.getArguments().size()]; }
        }
        Term root = Unify.deref(t);
        if (root instanceof Variable) return map.containsKey(root) ? map.get(root) : root;
        if (!(root instanceof CompoundTerm) || ((CompoundTerm) root).getArguments().isEmpty()) return root;
        ArrayDeque<Frame> stack = new ArrayDeque<Frame>();
        stack.push(new Frame((CompoundTerm) root));
        Term result = null;
        while (!stack.isEmpty()) {
            Frame f = stack.peek();
            List<Term> as = f.c.getArguments();
            if (f.i < as.size()) {
                Term a = as.get(f.i);
                Term d = Unify.deref(a);
                if (d instanceof Variable) {
                    Term r = map.get(d);
                    f.out[f.i] = r != null ? r : d;
                    f.changed |= (f.out[f.i] != a);
                    f.i++;
                } else if (d instanceof CompoundTerm && !((CompoundTerm) d).getArguments().isEmpty()) {
                    stack.push(new Frame((CompoundTerm) d));
                } else {
                    f.out[f.i] = d;
                    f.changed |= (d != a);
                    f.i++;
                }
                continue;
            }
            stack.pop();
            Term built = f.changed ? new CompoundTerm(new Atom(f.c.getName()), Arrays.asList(f.out)) : f.c;
            Frame parent = stack.peek();
            if (parent == null) { result = built; break; }
            parent.out[parent.i] = built;
            parent.changed |= (built != parent.c.getArguments().get(parent.i));
            parent.i++;
        }
        return result;
    }
    // END_CHANGE: ISS-2025-0570

    /**
     * {@code print_message(+Kind, +Message)} in its minimal, always-available form: an ISO
     * {@code error(Formal, Context)} ball is rendered readably, anything else is written with
     * {@code quoted(true)}.
     */
    public static String message(String kind, Term message) {
        Options o = Options.writeq();
        StringBuilder sb = new StringBuilder();
        Term m = Unify.deref(message);
        if ("error".equals(kind) || isErrorTerm(m)) sb.append("ERROR: "); else sb.append("% ");
        if (isErrorTerm(m)) {
            List<Term> as = ((CompoundTerm) m).getArguments();
            Term formal = Unify.deref(as.get(0));
            Term context = Unify.deref(as.get(1));
            sb.append(describe(formal, o));
            if (!(context instanceof Variable)) {
                sb.append(" (").append(format(context, o)).append(')');
            }
        } else {
            sb.append(format(m, o));
        }
        return sb.toString();
    }

    private static boolean isErrorTerm(Term t) {
        return t instanceof CompoundTerm && "error".equals(t.getName())
            && t.getArguments() != null && t.getArguments().size() == 2;
    }

    private static String describe(Term formal, Options o) {
        if (formal instanceof Atom) {
            String n = ((Atom) formal).getName();
            if ("instantiation_error".equals(n)) return "Arguments are not sufficiently instantiated";
            return n;
        }
        if (!(formal instanceof CompoundTerm)) return format(formal, o);
        CompoundTerm c = (CompoundTerm) formal;
        List<Term> as = c.getArguments();
        String n = c.getName();
        if ("type_error".equals(n) && as.size() == 2) {
            return "Type error: `" + format(as.get(0), o) + "' expected, found `" + format(as.get(1), o) + "'";
        }
        if ("domain_error".equals(n) && as.size() == 2) {
            return "Domain error: `" + format(as.get(0), o) + "' expected, found `" + format(as.get(1), o) + "'";
        }
        if ("existence_error".equals(n) && as.size() == 2) {
            return "Unknown " + format(as.get(0), o) + ": " + format(as.get(1), o);
        }
        if ("permission_error".equals(n) && as.size() == 3) {
            return "No permission to " + format(as.get(0), o) + " " + format(as.get(1), o)
                 + " `" + format(as.get(2), o) + "'";
        }
        if ("evaluation_error".equals(n) && as.size() == 1) {
            return "Arithmetic: evaluation error: `" + format(as.get(0), o) + "'";
        }
        if ("representation_error".equals(n) && as.size() == 1) {
            return "Cannot represent due to `" + format(as.get(0), o) + "'";
        }
        if ("resource_error".equals(n) && as.size() == 1) {
            return "Not enough resources: `" + format(as.get(0), o) + "'";
        }
        if ("syntax_error".equals(n) && as.size() == 1) {
            return "Syntax error: " + format(as.get(0), o);
        }
        return format(formal, o);
    }

    // ------------------------------------------------------------------
    // atoms, quoting, spacing (the ISS-2025-0387/0388 rules)
    // ------------------------------------------------------------------

    private static String variableName(Variable v, Options o) {
        if (o.variableNames != null) {
            String n = o.variableNames.get(v);
            if (n != null) return n;
        }
        String n = v.getName();
        return (n == null) ? "_" : n;
    }

    /** The atom as it must appear in the output: quoted if {@code quoted} and quoting is needed. */
    public static String atomText(String name, boolean quoted) {
        if (!quoted) return name;
        return needsQuoting(name) ? quoteAtom(name) : name;
    }

    // START_CHANGE: ISS-2025-0562
    /** True when {@code name} is currently a prefix, infix or postfix operator. */
    private static boolean isOperatorAtom(String name, OperatorTable ops) {
        if ("[]".equals(name) || "{}".equals(name)) return false;
        return ops.getPrefixOperator(name) != null || ops.getInfixOperator(name) != null
            || ops.getPostfixOperator(name) != null;
    }
    // END_CHANGE: ISS-2025-0562

    // START_CHANGE: ISS-2025-0564 - special floats: writeq/print give the SWI syntax the reader
    // accepts (1.0Inf, -1.0Inf, 1.5NaN); write/1 keeps inf, -inf, nan.
    public static String numberText(Number n, boolean quoted) {
        if (quoted && !n.isInteger()) {
            double d = n.doubleValue();
            if (Double.isNaN(d)) return "1.5NaN";
            if (Double.isInfinite(d)) return d > 0 ? "1.0Inf" : "-1.0Inf";
        }
        return n.toString();
    }
    // END_CHANGE: ISS-2025-0564

    /** Alphabetic operators ({@code is}, {@code mod}, {@code rem}, ...) are always spaced. */
    private static boolean isAlphaOp(String name) {
        if (name.isEmpty()) return false;
        char c = name.charAt(0);
        return Character.isLetter(c) || c == '_';
    }

    /** True when the atom cannot be written unquoted and re-read as the same atom. */
    public static boolean needsQuoting(String name) {
        if (name == null || name.isEmpty()) return true;
        if ("[]".equals(name) || "{}".equals(name) || ";".equals(name) || "!".equals(name)) return false;
        if (",".equals(name) || ".".equals(name) || "|".equals(name)) return true;
        char c = name.charAt(0);
        if (Character.isLowerCase(c)) {   // ISS-2025-0562: '_x' is a variable when unquoted
            for (int i = 1; i < name.length(); i++) {
                char ch = name.charAt(i);
                if (!Character.isLetterOrDigit(ch) && ch != '_') return true;
            }
            return false;
        }
        boolean allSym = true;
        for (int i = 0; i < name.length(); i++) {
            if (!isSymbolic(name.charAt(i))) { allSym = false; break; }
        }
        if (allSym) return name.contains("/*");
        return true;
    }

    /** Quote an atom, escaping what ISO 6.4.2 requires. */
    public static String quoteAtom(String name) {
        StringBuilder sb = new StringBuilder("'");
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (c == '\'') sb.append("''");
            else if (c == '\\') sb.append("\\\\");
            else if (c == '\n') sb.append("\\n");
            else if (c == '\r') sb.append("\\r");
            else if (c == '\t') sb.append("\\t");
            else if (c == '') sb.append("\\v");
            else if (c == '\f') sb.append("\\f");
            else if (c == '\b') sb.append("\\b");
            else if (c == 7) sb.append("\\a");
            else if (c == 0) sb.append("\\0\\");
            else if (c == 27) sb.append("\\e");                  // ISS-2025-0673: SWI writes '\e'
            // ISS-2025-0562: other control characters (DEL included) as octal escapes, as SWI
            else if (c < 0x20 || c == 0x7F) sb.append('\\').append(Integer.toOctalString(c)).append('\\');
            else sb.append(c);
        }
        sb.append('\'');
        return sb.toString();
    }

    private static String escapeString(String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '"') sb.append("\\\"");
            else if (c == '\\') sb.append("\\\\");
            else if (c == '\n') sb.append("\\n");
            else if (c == '\r') sb.append("\\r");
            else if (c == '\t') sb.append("\\t");
            else if (c == 27) sb.append("\\e");                  // ISS-2025-0673
            else if (c < 0x20 || c == 0x7F) sb.append('\\').append(Integer.toOctalString(c)).append('\\');   // ISS-2025-0562
            else sb.append(c);
        }
        return sb.toString();
    }

    private static boolean isSymbolic(char c) {
        return "+-*/\\^<>=~:.?@#$&".indexOf(c) >= 0;
    }
}
// END_CHANGE: ISS-2025-0475
