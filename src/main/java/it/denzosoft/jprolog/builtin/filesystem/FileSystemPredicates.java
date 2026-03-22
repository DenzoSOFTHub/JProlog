package it.denzosoft.jprolog.builtin.filesystem;

// START_CHANGE: ISS-2025-0115 - File system built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
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
 *   absolute_file_name/2 - absolute_file_name(+Rel, -Abs)
 *   read_file_to_atom/2  - read_file_to_atom(+Path, -Content)
 *   write_atom_to_file/2 - write_atom_to_file(+Path, +Content)
 */
public class FileSystemPredicates implements BuiltIn {

    public enum Mode {
        FILE_EXISTS, DIR_EXISTS, MAKE_DIR, MAKE_DIR_PATH,
        DELETE_FILE, DELETE_DIR, RENAME, COPY,
        FILE_SIZE, FILE_MODIFIED, DIR_FILES,
        WORKING_DIR, ABS_FILE_NAME,
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
                case ABS_FILE_NAME:  return doAbsFileName(query, bindings, solutions);
                case READ_FILE:      return doReadFile(query, bindings, solutions);
                case WRITE_FILE:     return doWriteFile(query, bindings, solutions);
                default: return false;
            }
        } catch (IOException e) {
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    private boolean doFileExists(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        File f = new File(resolveAtom(query.getArguments().get(0), bindings));
        if (f.isFile()) { solutions.add(bindings); return true; }
        return false;
    }

    private boolean doDirExists(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        File f = new File(resolveAtom(query.getArguments().get(0), bindings));
        if (f.isDirectory()) { solutions.add(bindings); return true; }
        return false;
    }

    private boolean doMakeDir(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions, boolean recursive) throws IOException {
        checkArity(query, 1);
        Path p = Paths.get(resolveAtom(query.getArguments().get(0), bindings));
        if (recursive) Files.createDirectories(p); else Files.createDirectory(p);
        solutions.add(bindings);
        return true;
    }

    private boolean doDelete(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 1);
        Files.delete(Paths.get(resolveAtom(query.getArguments().get(0), bindings)));
        solutions.add(bindings);
        return true;
    }

    private boolean doRename(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        Path src = Paths.get(resolveAtom(query.getArguments().get(0), bindings));
        Path dst = Paths.get(resolveAtom(query.getArguments().get(1), bindings));
        Files.move(src, dst, StandardCopyOption.REPLACE_EXISTING);
        solutions.add(bindings);
        return true;
    }

    private boolean doCopy(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        Path src = Paths.get(resolveAtom(query.getArguments().get(0), bindings));
        Path dst = Paths.get(resolveAtom(query.getArguments().get(1), bindings));
        Files.copy(src, dst, StandardCopyOption.REPLACE_EXISTING);
        solutions.add(bindings);
        return true;
    }

    private boolean doFileSize(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        long size = Files.size(Paths.get(resolveAtom(query.getArguments().get(0), bindings)));
        return unify(query.getArguments().get(1), new Number(size), bindings, solutions);
    }

    private boolean doFileModified(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        long ts = Files.getLastModifiedTime(Paths.get(resolveAtom(query.getArguments().get(0), bindings))).toMillis();
        return unify(query.getArguments().get(1), new Number(ts), bindings, solutions);
    }

    private boolean doDirFiles(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        File dir = new File(resolveAtom(query.getArguments().get(0), bindings));
        String[] names = dir.list();
        if (names == null) throw new PrologEvaluationException("directory_files: Not a directory.");
        List<Term> files = new ArrayList<>();
        for (String n : names) files.add(new Atom(n));
        return unify(query.getArguments().get(1), CollectionUtils.createListTerm(files), bindings, solutions);
    }

    private boolean doWorkingDir(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        String cwd = System.getProperty("user.dir");
        Map<String, Term> nb = new HashMap<>(bindings);
        if (query.getArguments().get(0).resolveBindings(bindings).unify(new Atom(cwd), nb)) {
            Term newDir = query.getArguments().get(1).resolveBindings(nb);
            if (newDir instanceof Atom) {
                System.setProperty("user.dir", ((Atom) newDir).getName());
            }
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private boolean doAbsFileName(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        String rel = resolveAtom(query.getArguments().get(0), bindings);
        String abs = Paths.get(rel).toAbsolutePath().normalize().toString();
        return unify(query.getArguments().get(1), new Atom(abs), bindings, solutions);
    }

    private boolean doReadFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        String content = new String(Files.readAllBytes(
            Paths.get(resolveAtom(query.getArguments().get(0), bindings))));
        return unify(query.getArguments().get(1), new Atom(content), bindings, solutions);
    }

    private boolean doWriteFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        String path = resolveAtom(query.getArguments().get(0), bindings);
        String content = resolveAtom(query.getArguments().get(1), bindings);
        Files.write(Paths.get(path), content.getBytes());
        solutions.add(bindings);
        return true;
    }

    private void checkArity(Term query, int expected) {
        if (query.getArguments().size() != expected)
            throw new PrologEvaluationException(modeName() + " requires " + expected + " arguments.");
    }

    private String resolveAtom(Term term, Map<String, Term> bindings) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Atom)) throw new PrologEvaluationException(modeName() + ": argument must be an atom.");
        return ((Atom) resolved).getName();
    }

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0115
