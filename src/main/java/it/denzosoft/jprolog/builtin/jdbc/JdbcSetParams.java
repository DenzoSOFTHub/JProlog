package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0110 - Prepared statements with parameters and stored procedures
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * jdbc_set_params/2 - jdbc_set_params(+Statement, +ParamList)
 * Sets all parameters on a prepared statement from a Prolog list.
 *
 * ParamList is a standard Prolog list of values, assigned to positions 1, 2, ...
 *
 * Example:
 *   jdbc_set_params(Stmt, [25, 'Milan', null])
 *   % equivalent to: jdbc_set_param(Stmt,1,25), jdbc_set_param(Stmt,2,'Milan'), jdbc_set_param(Stmt,3,null)
 */
public class JdbcSetParams implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0692
        }

        Term stmtTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term paramsTerm = query.getArguments().get(1).resolveBindings(bindings);

        if (!(stmtTerm instanceof Atom)) {
            throw LibArgs.notA("atom", stmtTerm, "jdbc_set_params", 2, "Statement must be an atom handle");   // ISS-2025-0692
        }

        String handle = ((Atom) stmtTerm).getName();
        List<Term> params = termToList(paramsTerm);

        try {
            PreparedStatement ps = JdbcConnectionManager.getInstance().getStatement(handle);
            for (int i = 0; i < params.size(); i++) {
                JdbcSetParam.setParameter(ps, i + 1, params.get(i));
            }
            solutions.add(bindings);
            return true;
        } catch (SQLException e) {
            throw Errors.host(e, "execute", "sql", null, "jdbc_set_params", LibArgs.arity(query));   // ISS-2025-0692
        }
    }

    private List<Term> termToList(Term term) {
        List<Term> result = new ArrayList<>();
        Term current = term;
        while (current instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) current;
            if (".".equals(ct.getName()) && ct.getArguments().size() == 2) {
                result.add(ct.getArguments().get(0));
                current = ct.getArguments().get(1);
            } else {
                break;
            }
        }
        return result;
    }
}
// END_CHANGE: ISS-2025-0110
