package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * jdbc_connect/2 - jdbc_connect(+URL, -Connection)
 * jdbc_connect/4 - jdbc_connect(+URL, +User, +Password, -Connection)
 * Opens a JDBC connection and returns a handle.
 */
public class JdbcConnect implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        int arity = args.size();

        if (arity != 2 && arity != 4) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0692
        }

        Term urlTerm = args.get(0).resolveBindings(bindings);
        if (!(urlTerm instanceof Atom)) {
            throw LibArgs.notA("atom", urlTerm, "jdbc_connect", args.size(), "the URL");   // ISS-2025-0692
        }
        String url = ((Atom) urlTerm).getName();

        try {
            String handle;
            Term connTerm;

            if (arity == 2) {
                handle = JdbcConnectionManager.getInstance().openConnection(url);
                connTerm = args.get(1);
            } else {
                Term userTerm = args.get(1).resolveBindings(bindings);
                Term passTerm = args.get(2).resolveBindings(bindings);
                if (!(userTerm instanceof Atom) || !(passTerm instanceof Atom)) {   // ISS-2025-0692
                    Term bad = !(userTerm instanceof Atom) ? userTerm : passTerm;
                    throw LibArgs.notA("atom", bad, "jdbc_connect", 4, "User and Password");
                }
                handle = JdbcConnectionManager.getInstance().openConnection(
                    url, ((Atom) userTerm).getName(), ((Atom) passTerm).getName());
                connTerm = args.get(3);
            }

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (connTerm.resolveBindings(bindings).unify(new Atom(handle), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            JdbcConnectionManager.getInstance().closeConnection(handle);
            return false;
        } catch (SQLException e) {
            throw Errors.host(e, "execute", "sql", null, "jdbc_connect", LibArgs.arity(query));   // ISS-2025-0692
        }
    }
}
// END_CHANGE: ISS-2025-0108
