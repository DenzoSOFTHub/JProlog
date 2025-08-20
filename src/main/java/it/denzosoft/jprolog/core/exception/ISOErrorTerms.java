package it.denzosoft.jprolog.core.exception;

import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.util.TermUtils;
import java.util.Arrays;

/**
 * Factory for creating ISO Prolog error terms.
 * Implements the standard error term structure: error(Error, Context).
 */
public class ISOErrorTerms {
    
    /**
     * Create an instantiation error.
     * 
     * @param context The error context
     * @return The error term
     */
    public static Term instantiationError(Term context) {
        return createErrorTerm(new Atom("instantiation_error"), context);
    }
    
    /**
     * Create an uninstantiation error.
     * 
     * @param culprit The culprit term
     * @param context The error context
     * @return The error term
     */
    public static Term uninstantiationError(Term culprit, Term context) {
        Term errorDetail = TermUtils.createCompound("uninstantiation_error", culprit);
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create a type error.
     * 
     * @param validType The expected type
     * @param culprit The culprit term
     * @param context The error context
     * @return The error term
     */
    public static Term typeError(String validType, Term culprit, Term context) {
        Term errorDetail = TermUtils.createCompound("type_error", 
            new Atom(validType), culprit);
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create a domain error.
     * 
     * @param validDomain The valid domain
     * @param culprit The culprit term
     * @param context The error context
     * @return The error term
     */
    public static Term domainError(String validDomain, Term culprit, Term context) {
        Term errorDetail = TermUtils.createCompound("domain_error", 
            new Atom(validDomain), culprit);
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create an existence error.
     * 
     * @param objectType The object type
     * @param culprit The culprit term
     * @param context The error context
     * @return The error term
     */
    public static Term existenceError(String objectType, Term culprit, Term context) {
        Term errorDetail = TermUtils.createCompound("existence_error", 
            new Atom(objectType), culprit);
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create a permission error.
     * 
     * @param operation The operation
     * @param permissionType The permission type
     * @param culprit The culprit term
     * @param context The error context
     * @return The error term
     */
    public static Term permissionError(String operation, String permissionType, 
                                     Term culprit, Term context) {
        Term errorDetail = TermUtils.createCompound("permission_error", 
            new Atom(operation), new Atom(permissionType), culprit);
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create a representation error.
     * 
     * @param flag The representation flag
     * @param context The error context
     * @return The error term
     */
    public static Term representationError(String flag, Term context) {
        Term errorDetail = TermUtils.createCompound("representation_error", new Atom(flag));
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create an evaluation error.
     * 
     * @param error The evaluation error type
     * @param context The error context
     * @return The error term
     */
    public static Term evaluationError(String error, Term context) {
        Term errorDetail = TermUtils.createCompound("evaluation_error", new Atom(error));
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create a resource error.
     * 
     * @param resource The resource type
     * @param context The error context
     * @return The error term
     */
    public static Term resourceError(String resource, Term context) {
        Term errorDetail = TermUtils.createCompound("resource_error", new Atom(resource));
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create a syntax error.
     * 
     * @param message The syntax error message
     * @param context The error context
     * @return The error term
     */
    public static Term syntaxError(String message, Term context) {
        Term errorDetail = TermUtils.createCompound("syntax_error", new Atom(message));
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create a system error.
     * 
     * @param message The system error message
     * @param context The error context
     * @return The error term
     */
    public static Term systemError(String message, Term context) {
        Term errorDetail = new Atom(message);
        return createErrorTerm(errorDetail, context);
    }
    
    /**
     * Create the standard error term structure.
     * 
     * @param errorDetail The error detail term
     * @param context The error context
     * @return The complete error term
     */
    private static Term createErrorTerm(Term errorDetail, Term context) {
        if (context == null) {
            context = new Atom("unknown");
        }
        return TermUtils.createCompound("error", errorDetail, context);
    }
    
    /**
     * Create a context term for predicate/arity.
     * 
     * @param predicate The predicate name
     * @param arity The predicate arity
     * @return The context term
     */
    public static Term predicateContext(String predicate, int arity) {
        return TermUtils.createCompound("/", new Atom(predicate), 
            new it.denzosoft.jprolog.core.terms.Number((double) arity));
    }
    
    /**
     * Create a context term for clause.
     * 
     * @param clause The clause term
     * @return The context term
     */
    public static Term clauseContext(Term clause) {
        return TermUtils.createCompound("clause", clause);
    }
    
    /**
     * Create a context term for goal.
     * 
     * @param goal The goal term
     * @return The context term
     */
    public static Term goalContext(Term goal) {
        return TermUtils.createCompound("goal", goal);
    }
}