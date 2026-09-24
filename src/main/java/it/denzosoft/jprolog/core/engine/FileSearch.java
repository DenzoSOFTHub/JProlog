package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.v4.Unify;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

// START_CHANGE: ISS-2025-0737 - 4.6 wave Q3.3: the file search behind absolute_file_name/2,3 and
// `library(X)` / `Alias(Path)` source specifications (SWI).
/**
 * Resolves a file specification to absolute paths. A plain name ({@code 'dir/f'}, {@code "f"},
 * {@code a/b}) is taken relative to {@code relative_to} (default: the directory of the file being
 * loaded, else the working directory); {@code Alias(Path)} is expanded through the user's
 * {@code file_search_path/2} clauses (recursively, an alias may map to another {@code Alias2(Sub)})
 * and then through the built-in defaults:
 * <ul>
 *   <li>{@code swi} — the JProlog home: the {@code jprolog.home} system property, else
 *       {@code ~/.jprolog};</li>
 *   <li>{@code library} — {@code swi(library)} and the directory of the bundled prelude library
 *       when it is on disk ({@code target/classes/prelude} in a development tree);</li>
 *   <li>{@code foreign} — {@code swi(lib)}.</li>
 * </ul>
 * Each directory is combined with each extension ({@code extensions(L)}, or the ones
 * {@code file_type(T)} implies) and the candidates are filtered by {@code access(A)} and the file
 * type. With no access, type or extension condition and no existing candidate, the first
 * candidate is the answer (SWI).
 */
public final class FileSearch {

    /** One search request (the options of absolute_file_name/3). */
    public static final class Options {
        public List<String> extensions = new ArrayList<>(Collections.singletonList(""));
        public boolean extensionsGiven;
        public String fileType = "txt";
        public String access = "none";
        public String relativeTo;
        public boolean all;
        public boolean errors = true;
    }

    private static final int MAX_ALIAS_DEPTH = 16;

    private FileSearch() {}

    /** Parse an absolute_file_name/3 option list (ISO/SWI errors). */
    public static Options parseOptions(Term list0, String ctx) {
        Options o = new Options();
        Term l = Unify.deref(list0);
        while (l instanceof CompoundTerm && ".".equals(((CompoundTerm) l).getName())
                && ((CompoundTerm) l).getArguments().size() == 2) {
            Term opt = Unify.deref(((CompoundTerm) l).getArguments().get(0));
            if (opt instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
            if (!(opt instanceof CompoundTerm) || ((CompoundTerm) opt).getArguments().size() != 1) {
                throw new PrologException(ISOErrorTerms.domainError("absolute_file_name_option", opt, ctx));
            }
            String on = ((CompoundTerm) opt).getName();
            Term v = Unify.deref(((CompoundTerm) opt).getArguments().get(0));
            if (v instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
            switch (on) {
                case "extensions": {
                    o.extensions = new ArrayList<>();
                    Term e = v;
                    while (e instanceof CompoundTerm && ".".equals(((CompoundTerm) e).getName())
                            && ((CompoundTerm) e).getArguments().size() == 2) {
                        String x = text(Unify.deref(((CompoundTerm) e).getArguments().get(0)));
                        if (x == null) throw new PrologException(ISOErrorTerms.typeError("atom", opt, ctx));
                        o.extensions.add(x.isEmpty() || x.startsWith(".") ? x : "." + x);
                        e = Unify.deref(((CompoundTerm) e).getArguments().get(1));
                    }
                    if (!(e instanceof Atom && "[]".equals(((Atom) e).getName()))) {
                        throw new PrologException(ISOErrorTerms.typeError("list", v, ctx));
                    }
                    o.extensionsGiven = true;
                    break;
                }
                case "file_type": {
                    String t = atom(v, opt, ctx);
                    if (!Arrays.asList("txt", "prolog", "source", "executable", "qlf", "directory", "regular")
                            .contains(t)) {
                        throw new PrologException(ISOErrorTerms.domainError("file_type", v, ctx));
                    }
                    o.fileType = t;
                    break;
                }
                case "access": {
                    String a = atom(v, opt, ctx);
                    if (!Arrays.asList("read", "write", "append", "execute", "exist", "none").contains(a)) {
                        throw new PrologException(ISOErrorTerms.domainError("io_mode", v, ctx));
                    }
                    o.access = a;
                    break;
                }
                case "relative_to": {
                    String r = text(v);
                    if (r == null) throw new PrologException(ISOErrorTerms.typeError("atom", v, ctx));
                    File rf = it.denzosoft.jprolog.core.engine.v4.EngineState.file(r);   // ISS-2025-0745
                    o.relativeTo = rf.isFile() ? rf.getAbsoluteFile().getParent() : rf.getAbsolutePath();
                    break;
                }
                case "solutions": {
                    String s = atom(v, opt, ctx);
                    if (!"first".equals(s) && !"all".equals(s)) {
                        throw new PrologException(ISOErrorTerms.domainError("solutions", v, ctx));
                    }
                    o.all = "all".equals(s);
                    break;
                }
                case "file_errors": {
                    String s = atom(v, opt, ctx);
                    if (!"error".equals(s) && !"fail".equals(s)) {
                        throw new PrologException(ISOErrorTerms.domainError("file_errors", v, ctx));
                    }
                    o.errors = "error".equals(s);
                    break;
                }
                default:
                    break;                          // expand/1 and the rest: accepted, ignored
            }
            l = Unify.deref(((CompoundTerm) l).getArguments().get(1));
        }
        if (l instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
        if (!(l instanceof Atom && "[]".equals(((Atom) l).getName()))) {
            throw new PrologException(ISOErrorTerms.typeError("list", list0, ctx));
        }
        if (!o.extensionsGiven && ("prolog".equals(o.fileType) || "source".equals(o.fileType))) {
            o.extensions = new ArrayList<>(Arrays.asList(".pl", ".prolog", ""));
        }
        return o;
    }

    private static String atom(Term v, Term opt, String ctx) {
        if (!(v instanceof Atom)) throw new PrologException(ISOErrorTerms.typeError("atom", v, ctx));
        return ((Atom) v).getName();
    }

    /** The text of an atom or a string, a/b segments joined with '/'; null otherwise. */
    public static String text(Term t0) {
        Term t = Unify.deref(t0);
        if (t instanceof Atom) return ((Atom) t).getName();
        if (t instanceof PrologString) return ((PrologString) t).getStringValue();
        if (t instanceof CompoundTerm && "/".equals(((CompoundTerm) t).getName())
                && ((CompoundTerm) t).getArguments().size() == 2) {
            String a = text(((CompoundTerm) t).getArguments().get(0));
            String b = text(((CompoundTerm) t).getArguments().get(1));
            return (a == null || b == null) ? null : a + "/" + b;
        }
        return null;
    }

    /**
     * The answers for {@code spec} (absolute, normalised paths). An empty list means "no match"
     * (the caller raises or fails per {@code file_errors}).
     *
     * @param solver runs a goal and returns its solutions (file_search_path/2 lookups)
     * @param baseDir the default directory for relative names
     */
    public static List<String> resolve(Term spec0, Options o, Function<Term, List<Map<String, Term>>> solver,
                                       String baseDir, String ctx) {
        Term spec = Unify.deref(spec0);
        if (spec instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
        List<String> bases = new ArrayList<>();
        String plain = text(spec);
        if (plain != null) {
            if (plain.startsWith("~/")) plain = System.getProperty("user.home") + plain.substring(1);
            File f = new File(plain);
            if (!f.isAbsolute()) f = new File(o.relativeTo != null ? o.relativeTo : baseDir, plain);
            bases.add(f.getPath());
        } else if (spec instanceof CompoundTerm && ((CompoundTerm) spec).getArguments().size() == 1) {
            String sub = text(((CompoundTerm) spec).getArguments().get(0));
            if (sub == null) throw new PrologException(ISOErrorTerms.domainError("file_path", spec, ctx));
            for (String dir : aliasDirs(((CompoundTerm) spec).getName(), solver, 0)) {
                bases.add(new File(dir, sub).getPath());
            }
        } else {
            throw new PrologException(ISOErrorTerms.domainError("file_path", spec, ctx));
        }
        LinkedHashSet<String> out = new LinkedHashSet<>();
        String firstCandidate = null;
        boolean conditional = !"none".equals(o.access) || o.extensionsGiven
            || !"txt".equals(o.fileType) || plain == null;
        for (String b : bases) {
            List<String> exts = o.extensions;
            String name = new File(b).getName();
            if (("prolog".equals(o.fileType) || "source".equals(o.fileType)) && !o.extensionsGiven
                    && name.lastIndexOf('.') > 0) {
                exts = Arrays.asList("", ".pl");       // an explicit extension is tried first
            }
            for (String ext : exts) {
                File f = new File(b + ext);
                String abs = normalise(f);
                if (firstCandidate == null) firstCandidate = abs;
                if (!conditional || matches(f, o)) {
                    out.add(abs);
                    if (!o.all) return new ArrayList<>(out);
                }
            }
        }
        if (out.isEmpty() && "none".equals(o.access) && !"directory".equals(o.fileType)
                && !o.extensionsGiven && plain != null && firstCandidate != null) {
            out.add(firstCandidate);
        }
        return new ArrayList<>(out);
    }

    private static boolean matches(File f, Options o) {
        if ("directory".equals(o.fileType)) {
            if (!f.isDirectory()) return false;
        } else if (!"none".equals(o.access) || o.extensionsGiven || !"txt".equals(o.fileType)) {
            if ("write".equals(o.access) || "append".equals(o.access)) {
                if (f.exists()) return f.isFile() && f.canWrite();
                File p = f.getAbsoluteFile().getParentFile();
                return p != null && p.isDirectory() && p.canWrite();
            }
            if (!f.isFile()) return false;
        }
        switch (o.access) {
            case "read": return f.canRead();
            case "execute": return f.canExecute();
            case "exist": return f.exists();
            default: return true;
        }
    }

    private static String normalise(File f) {
        try {
            return f.getAbsoluteFile().toPath().normalize().toString();
        } catch (RuntimeException e) {
            return f.getAbsolutePath();
        }
    }

    /** The directories alias {@code a} maps to: the user's clauses first, then the defaults. */
    static List<String> aliasDirs(String alias, Function<Term, List<Map<String, Term>>> solver, int depth) {
        List<String> dirs = new ArrayList<>();
        if (depth > MAX_ALIAS_DEPTH) return dirs;
        if (solver != null) {
            Variable d = new Variable("FileSearchDir__");
            Term goal = new CompoundTerm(new Atom("file_search_path"), Arrays.asList((Term) new Atom(alias), d));
            List<Map<String, Term>> sols;
            try {
                sols = solver.apply(goal);
            } catch (PrologException e) {
                if (e.isHalt()) throw e;
                sols = Collections.emptyList();
            }
            for (Map<String, Term> s : sols) {
                Term v = Unify.deref(s.get("FileSearchDir__"));
                if (v == null) continue;
                addTarget(v, dirs, solver, depth);
            }
        }
        for (Term def : defaults(alias)) addTarget(def, dirs, solver, depth);
        return dirs;
    }

    private static void addTarget(Term v, List<String> dirs, Function<Term, List<Map<String, Term>>> solver, int depth) {
        String t = text(v);
        if (t != null) {
            if (t.startsWith("~/")) t = System.getProperty("user.home") + t.substring(1);
            String p = normalise(it.denzosoft.jprolog.core.engine.v4.EngineState.file(t));   // ISS-2025-0745
            if (!dirs.contains(p)) dirs.add(p);
            return;
        }
        if (v instanceof CompoundTerm && ((CompoundTerm) v).getArguments().size() == 1) {
            String sub = text(((CompoundTerm) v).getArguments().get(0));
            if (sub == null) return;
            for (String d : aliasDirs(((CompoundTerm) v).getName(), solver, depth + 1)) {
                String p = normalise(new File(d, sub));
                if (!dirs.contains(p)) dirs.add(p);
            }
        }
    }

    /** The built-in file_search_path/2 defaults for {@code alias}. */
    static List<Term> defaults(String alias) {
        switch (alias) {
            case "swi":
                return Collections.<Term>singletonList(new Atom(home()));
            case "library": {
                List<Term> l = new ArrayList<>();
                l.add(new CompoundTerm(new Atom("swi"), Collections.<Term>singletonList(new Atom("library"))));
                String pre = preludeDirectory();
                if (pre != null) l.add(new Atom(pre));
                return l;
            }
            case "foreign":
                return Collections.<Term>singletonList(
                    new CompoundTerm(new Atom("swi"), Collections.<Term>singletonList(new Atom("lib"))));
            default:
                return Collections.emptyList();
        }
    }

    /** The JProlog home: {@code -Djprolog.home}, else {@code ~/.jprolog}. */
    public static String home() {
        String h = System.getProperty("jprolog.home");
        if (h == null || h.isEmpty()) h = System.getProperty("user.home") + File.separator + ".jprolog";
        return normalise(new File(h));
    }

    /** The directory of the bundled prelude when it is a directory on disk, else null. */
    static String preludeDirectory() {
        try {
            java.net.URL u = FileSearch.class.getResource("/prelude/lists.pl");
            if (u == null || !"file".equals(u.getProtocol())) return null;
            return new File(u.toURI()).getParent();
        } catch (Exception e) {
            ControlFlow.rethrowIfControl(e);
            return null;
        }
    }
}
// END_CHANGE: ISS-2025-0737
