package it.denzosoft.jprolog.builtin.filesystem;

// START_CHANGE: ISS-2025-0115 - File system built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.util.*;

/**
 * File system predicates:
 *   file_exists/1        - file_exists(+Path)
 *   directory_exists/1   - directory_exists(+Path)
 *   make_directory/1     - make_directory(+Path)
 *   make_directory_path/1- make_directory_path(+Path) (recursive)
 *   delete_file/1        - delete_file(+Path)
 *   delete_directory/1   - delete_directory(+Path)
 *   rename_file/2        - rename_file(+Old, +New)
 *   copy_file/2          - copy_file(+Src, +Dest)
 *   file_size/2          - file_size(+Path, -Size)
 *   file_modified/2      - file_modified(+Path, -Timestamp)
 *   directory_files/2    - directory_files(+Dir, -Files)
 *   working_directory/2  - working_directory(-Old, +New)
 *   (absolute_file_name/2,3 is builtin.filesystem.AbsoluteFileName since 4.6.0, ISS-2025-0795)
 *   read_file_to_atom/2  - read_file_to_atom(+Path, -Content)
 *   write_atom_to_file/2 - write_atom_to_file(+Path, +Content)
 */
public class FileSystemPredicates implements BuiltIn {

    public enum Mode {
        FILE_EXISTS, DIR_EXISTS, MAKE_DIR, MAKE_DIR_PATH,
        DELETE_FILE, DELETE_DIR, RENAME, COPY,
        FILE_SIZE, FILE_MODIFIED, DIR_FILES,
        // ISS-2025-0795: ABS_FILE_NAME deleted (dead since ISS-2025-0737 routed the name to AbsoluteFileName)
        WORKING_DIR,
        READ_FILE, WRITE_FILE
    }

    private final Mode mode;

    public FileSystemPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case FILE_EXISTS:    return doFileExists(query, bindings, solutions);
                case DIR_EXISTS:     return doDirExists(query, bindings, solutions);
                case MAKE_DIR:       return doMakeDir(query, bindings, solutions, false);
                case MAKE_DIR_PATH:  return doMakeDir(query, bindings, solutions, true);
                case DELETE_FILE:    return doDelete(query, bindings, solutions);
                case DELETE_DIR:     return doDelete(query, bindings, solutions);
                case RENAME:         return doRename(query, bindings, solutions);
                case COPY:           return doCopy(query, bindings, solutions);
                case FILE_SIZE:      return doFileSize(query, bindings, solutions);
                case FILE_MODIFIED:  return doFileModified(query, bindings, solutions);
                case DIR_FILES:      return doDirFiles(query, bindings, solutions);
                case WORKING_DIR:    return doWorkingDir(query, bindings, solutions);
                case READ_FILE:      return doReadFile(query, bindings, solutions);
                case WRITE_FILE:     return doWriteFile(query, bindings, solutions);
                default: return false;
            }
        } catch (IOException e) {
            // START_CHANGE: ISS-2025-0690 - a host failure maps to SWI's existence/permission/io
            // formals, with the path as the culprit (never a message atom)
            Term culprit = arityOf(query) > 0 ? query.getArguments().get(0).resolveBindings(bindings) : null;
            String op, kind = "file";
            switch (mode) {
                case MAKE_DIR: case MAKE_DIR_PATH: op = "create"; kind = "directory"; break;
                case DELETE_FILE: op = "delete"; break;
                case DELETE_DIR:  op = "delete"; kind = "directory"; break;
                case RENAME:      op = "rename"; break;
                case COPY:        op = "copy"; break;
                case WRITE_FILE:  op = "write"; break;
                case DIR_FILES:   op = "open"; kind = "directory"; break;
                default:          op = "read"; break;
            }
            throw Errors.host(e, op, kind, culprit, modeName(), arityOf(query));
            // END_CHANGE: ISS-2025-0690
        }
    }

    private boolean doFileExists(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        File f = new File(cwdPath(resolveAtom(query, 0, bindings)));
        if (f.isFile()) { solutions.add(bindings); return true; }
        return false;
    }

    private boolean doDirExists(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        File f = new File(cwdPath(resolveAtom(query, 0, bindings)));
        if (f.isDirectory()) { solutions.add(bindings); return true; }
        return false;
    }

    private boolean doMakeDir(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions, boolean recursive) throws IOException {
        checkArity(query, 1);
        Path p = Paths.get(cwdPath(resolveAtom(query, 0, bindings)));
        if (recursive) Files.createDirectories(p); else Files.createDirectory(p);
        solutions.add(bindings);
        return true;
    }

    private boolean doDelete(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 1);
        Files.delete(Paths.get(cwdPath(resolveAtom(query, 0, bindings))));
        solutions.add(bindings);
        return true;
    }

    private boolean doRename(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        Path src = Paths.get(cwdPath(resolveAtom(query, 0, bindings)));
        Path dst = Paths.get(cwdPath(resolveAtom(query, 1, bindings)));
        Files.move(src, dst, StandardCopyOption.REPLACE_EXISTING);
        solutions.add(bindings);
        return true;
    }

    private boolean doCopy(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        Path src = Paths.get(cwdPath(resolveAtom(query, 0, bindings)));
        Path dst = Paths.get(cwdPath(resolveAtom(query, 1, bindings)));
        Files.copy(src, dst, StandardCopyOption.REPLACE_EXISTING);
        solutions.add(bindings);
        return true;
    }

    private boolean doFileSize(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        long size = Files.size(Paths.get(cwdPath(resolveAtom(query, 0, bindings))));
        return unify(query.getArguments().get(1), new Number(size), bindings, solutions);
    }

    private boolean doFileModified(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        long ts = Files.getLastModifiedTime(Paths.get(cwdPath(resolveAtom(query, 0, bindings)))).toMillis();
        return unify(query.getArguments().get(1), new Number(ts), bindings, solutions);
    }

    private boolean doDirFiles(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        File dir = new File(cwdPath(resolveAtom(query, 0, bindings)));
        String[] names = dir.list();
        if (names == null) {                                           // ISS-2025-0690
            throw Errors.existence("directory", query.getArguments().get(0).resolveBindings(bindings),
                                   "directory_files", 2, "not a directory");
        }
        List<Term> files = new ArrayList<>();
        for (String n : names) files.add(new Atom(n));
        return unify(query.getArguments().get(1), CollectionUtils.createListTerm(files), bindings, solutions);
    }

    private boolean doWorkingDir(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        // START_CHANGE: ISS-2025-0745 - the working directory is the ENGINE's, never `user.dir`.
        // SWI: working_directory(-Old, +New) unifies Old with the current directory (with a
        // trailing `/`), then changes to New (relative to the old one) when New is not unbound or
        // equal to Old; a New that is not an existing directory raises existence_error(directory, New).
        it.denzosoft.jprolog.core.engine.v4.EngineState es = it.denzosoft.jprolog.core.engine.v4.EngineState.current();
        String cwd = es.workingDirectory();
        String shown = cwd.endsWith(File.separator) ? cwd : cwd + File.separator;
        Map<String, Term> nb = new HashMap<>(bindings);
        if (!query.getArguments().get(0).resolveBindings(bindings).unify(new Atom(shown), nb)) return false;
        Term newDir = query.getArguments().get(1).resolveBindings(nb);
        if (!(newDir instanceof it.denzosoft.jprolog.core.terms.Variable)) {
            String target;
            if (newDir instanceof Atom) target = ((Atom) newDir).getName();
            else if (newDir instanceof it.denzosoft.jprolog.core.terms.PrologString) {
                target = ((it.denzosoft.jprolog.core.terms.PrologString) newDir).getStringValue();
            } else throw Errors.type("atom", newDir, "working_directory", 2, "argument 2 must be an atom");
            File d = it.denzosoft.jprolog.core.engine.v4.EngineState.file(target);
            if (!d.isDirectory()) {
                throw Errors.existence("directory", newDir, "working_directory", 2, "no such directory");
            }
            String abs = d.toPath().toAbsolutePath().normalize().toString();
            if (abs.length() > 1 && abs.endsWith(File.separator)) abs = abs.substring(0, abs.length() - 1);
            es.setWorkingDirectory(abs);
        } else {
            if (!newDir.unify(new Atom(shown), nb)) return false;
        }
        solutions.add(nb);
        return true;
        // END_CHANGE: ISS-2025-0745
    }

    private boolean doReadFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        String content = new String(Files.readAllBytes(
            Paths.get(cwdPath(resolveAtom(query, 0, bindings)))));
        return unify(query.getArguments().get(1), new Atom(content), bindings, solutions);
    }

    private boolean doWriteFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        String path = resolveAtom(query, 0, bindings);
        String content = resolveAtom(query, 1, bindings);
        Files.write(Paths.get(cwdPath(path)), content.getBytes());
        solutions.add(bindings);
        return true;
    }


    // START_CHANGE: ISS-2025-0690 - wave Q1.1: ISO error terms error(Formal, context(Name/Arity, Msg)),
    // not message atoms (LIM-038)
    private static int arityOf(Term query) {
        return query.getArguments() == null ? 0 : query.getArguments().size();
    }

    /** Unreachable through the registry since ISS-2025-0685 (exact arities); kept for direct calls. */
    private void checkArity(Term query, int expected) {
        int n = arityOf(query);
        if (n != expected) throw Errors.existence("procedure", Errors.pi(modeName(), n), modeName(), n, null);
    }

    /** Argument {@code i} as text: an atom (or a string); unbound is an instantiation error. */
    /** ISS-2025-0745: a relative path resolved against the current engine's working directory. */
    private static String cwdPath(String p) {
        return it.denzosoft.jprolog.core.engine.v4.EngineState.path(p);
    }

    private String resolveAtom(Term query, int i, Map<String, Term> bindings) {
        int n = arityOf(query);
        Term resolved = query.getArguments().get(i).resolveBindings(bindings);
        if (resolved instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw Errors.instantiation(modeName(), n, "argument " + (i + 1) + " must be bound");
        }
        if (resolved instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            return ((it.denzosoft.jprolog.core.terms.PrologString) resolved).getStringValue();
        }
        if (!(resolved instanceof Atom)) {
            throw Errors.type("atom", resolved, modeName(), n, "argument " + (i + 1) + " must be an atom");
        }
        return ((Atom) resolved).getName();
    }
    // END_CHANGE: ISS-2025-0690

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    // START_CHANGE: ISS-2025-0690 - the REGISTERED name (it is the Name of the error context)
    private String modeName() {
        switch (mode) {
            case DIR_EXISTS:    return "directory_exists";
            case MAKE_DIR:      return "make_directory";
            case MAKE_DIR_PATH: return "make_directory_path";
            case DELETE_DIR:    return "delete_directory";
            case RENAME:        return "rename_file";
            case COPY:          return "copy_file";
            case DIR_FILES:     return "directory_files";
            case WORKING_DIR:   return "working_directory";
            case READ_FILE:     return "read_file_to_atom";
            case WRITE_FILE:    return "write_atom_to_file";
            default:            return mode.name().toLowerCase();   // file_exists, delete_file, file_size, file_modified
        }
    }
    // END_CHANGE: ISS-2025-0690
}
// END_CHANGE: ISS-2025-0115
