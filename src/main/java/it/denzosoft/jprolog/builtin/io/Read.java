package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologParserException;
import it.denzosoft.jprolog.core.parser.TermParser;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;


public class Read implements BuiltInWithContext {

    // START_CHANGE: ISS-2025-0203 - per-alias BufferedReader cache for read/2
    private static final Map<String, BufferedReader> READERS = new HashMap<>();
    // END_CHANGE: ISS-2025-0203

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("read/1 or read/2 expected.");
        }

        // START_CHANGE: ISS-2025-0203 - read/2 reads from given stream
        Term termVar;
        String streamAlias = null;
        if (arity == 1) {
            termVar = query.getArguments().get(0);
        } else {
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            termVar = query.getArguments().get(1);
            if (streamTerm instanceof Atom) {
                streamAlias = ((Atom) streamTerm).getName();
            } else if (streamTerm instanceof CompoundTerm && "stream".equals(streamTerm.getName())) {
                Term inner = streamTerm.getArguments().get(0);
                if (inner instanceof Atom) streamAlias = ((Atom) inner).getName();
            } else {
                throw new PrologEvaluationException("read/2: invalid stream argument");
            }
        }

        String inputLine = readLineFromStream(streamAlias);
        if (inputLine == null) {
            // EOF
            Map<String, Term> nb = new HashMap<>(bindings);
            if (termVar.unify(new Atom("end_of_file"), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        }
        // END_CHANGE: ISS-2025-0203

        inputLine = inputLine.trim();
        if (inputLine.isEmpty()) {
            throw new PrologEvaluationException("read: No input provided.");
        }
        if (inputLine.endsWith(".")) {
            inputLine = inputLine.substring(0, inputLine.length() - 1);
        }

        Prolog prolog = solver.getPrologContext();
        if (prolog == null) {
            throw new PrologEvaluationException("Prolog context not available for read");
        }
        try {
            TermParser termParser = prolog.getTermParser();
            Term parsedTerm = termParser.parseTerm(inputLine);
            if (parsedTerm != null) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (termVar.unify(parsedTerm, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            }
            return false;
        } catch (PrologParserException e) {
            throw new PrologEvaluationException("Error parsing term in read: " + e.getMessage(), e);
        }
    }

    // START_CHANGE: ISS-2025-0203 - read a line from named stream or stdin
    private String readLineFromStream(String alias) {
        if (alias == null || "current_input".equals(alias) || "user_input".equals(alias)) {
            System.out.print("?- ");
            Scanner scanner = new Scanner(System.in);
            try {
                return scanner.nextLine();
            } catch (java.util.NoSuchElementException e) {
                return null;
            }
        }
        BufferedReader br = READERS.get(alias);
        if (br == null) {
            InputStream is = StreamManager.getInputStream(alias);
            if (is == null) {
                throw new PrologEvaluationException("existence_error(stream, " + alias + ")");
            }
            br = new BufferedReader(new InputStreamReader(is));
            READERS.put(alias, br);
        }
        try {
            return br.readLine();
        } catch (java.io.IOException e) {
            throw new PrologEvaluationException("io_error(read, " + alias + "): " + e.getMessage());
        }
    }
    // END_CHANGE: ISS-2025-0203

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'read' must be invoked with context");
    }
}
