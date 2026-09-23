package it.denzosoft.jprolog.core.engine;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

// START_CHANGE: ISS-2025-0625 - wave P6.1: what an embedder may still allow in safe mode.
/**
 * Options for {@link Prolog#enableSafeMode(SafeModeOptions)}.
 *
 * <p>By default safe mode denies every host access. {@link #allowFileRead(String)} whitelists a
 * directory tree for READING: {@code open/3,4} in {@code read} mode and the loaders
 * ({@code consult/1}, {@code ensure_loaded/1}, {@code load_files/1,2}, {@code [F]},
 * {@code :- include(F)}, {@code use_module(File)}) are then kept, but refuse — with
 * {@code permission_error(open, source_sink, F)} — any file whose canonical path is not inside one
 * of the allowed directories. Writing, appending and every other host-touching predicate stay
 * denied.
 */
public final class SafeModeOptions {

    private final List<File> readDirs = new ArrayList<File>();

    // START_CHANGE: ISS-2025-0672 - halt/0,1 in safe mode. The engine never calls System.exit
    // (halt surfaces to the embedder as a PrologException with isHalt()), but an untrusted
    // program must not be able to end the embedder's query — or, in a host that honours the
    // halt signal, its process — so safe mode denies it by default with
    // permission_error(call, sandboxed, halt/N), as SWI's sandbox does. A host that wants the
    // signal (the CLI's --safe mode: the process belongs to the user) opts back in.
    private boolean haltAllowed;

    /** Keep halt/0,1 working in safe mode (the embedder then handles {@code isHalt()}). */
    public SafeModeOptions allowHalt() {
        haltAllowed = true;
        return this;
    }

    public boolean haltAllowed() { return haltAllowed; }
    // END_CHANGE: ISS-2025-0672

    /** Allow reading files below {@code dir} (canonicalised, so {@code ..} and links cannot escape). */
    public SafeModeOptions allowFileRead(String dir) {
        try {
            readDirs.add(new File(dir).getCanonicalFile());
        } catch (IOException e) {
            throw new IllegalArgumentException("cannot resolve " + dir, e);
        }
        return this;
    }

    /** The whitelisted directories (canonical). */
    public List<File> readDirs() { return Collections.unmodifiableList(readDirs); }

    /** Is {@code f} inside one of the whitelisted directories? */
    public boolean allowsRead(File f) {
        if (readDirs.isEmpty() || f == null) return false;
        File c;
        try {
            c = f.getCanonicalFile();
        } catch (IOException e) {
            return false;
        }
        for (File d : readDirs) {
            for (File p = c; p != null; p = p.getParentFile()) {
                if (p.equals(d)) return true;
            }
        }
        return false;
    }
}
// END_CHANGE: ISS-2025-0625
