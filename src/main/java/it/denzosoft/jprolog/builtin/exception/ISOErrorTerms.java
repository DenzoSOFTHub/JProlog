package it.denzosoft.jprolog.builtin.exception;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.Arrays;
import java.util.List;

/**
 * Factory for creating ISO standard error terms.
 * 
 * ISO 13211-1 defines a hierarchy of standard error terms that must be used
 * for proper exception handling. This class provides factory methods for
 * creating these standard terms.
 */
public class ISOErrorTerms {
    
    /**
     * Create the main error/2 term structure.
     * 
     * @param errorClass The error class term (e.g., instantiation_error)
     * @param context Additional context information
     * @return error(ErrorClass, Context) term
     */
    public static Term error(Term errorClass, Term context) {
        return new CompoundTerm(new Atom("error"), 
                               Arrays.asList(errorClass, context));
    }
    
    /**
     * Create an instantiation error term.
     * ISO: error(instantiation_error, ImplementationDefined)
     * 
     * @param context Context information
     * @return instantiation_error term wrapped in error/2
     */
    public static Term instantiationError(String context) {
        return error(new Atom("instantiation_error"), 
                    new Atom(context));
    }
    
    /**
     * Create a type error term.
     * ISO: error(type_error(ValidType, Culprit), ImplementationDefined)
     * 
     * @param validType The expected type
     * @param culprit The term that caused the error
     * @param context Context information
     * @return type_error term wrapped in error/2
     */
    public static Term typeError(String validType, Term culprit, String context) {
        Term typeErrorTerm = new CompoundTerm(new Atom("type_error"),
                                            Arrays.asList(new Atom(validType), culprit));
        return error(typeErrorTerm, new Atom(context));
    }
    
    /**
     * Create a domain error term.
     * ISO: error(domain_error(ValidDomain, Culprit), ImplementationDefined)
     * 
     * @param validDomain The valid domain
     * @param culprit The term that caused the error
     * @param context Context information
     * @return domain_error term wrapped in error/2
     */
    public static Term domainError(String validDomain, Term culprit, String context) {
        Term domainErrorTerm = new CompoundTerm(new Atom("domain_error"),
                                              Arrays.asList(new Atom(validDomain), culprit));
        return error(domainErrorTerm, new Atom(context));
    }
    
    /**
     * Create an existence error term.
     * ISO: error(existence_error(ObjectType, Culprit), ImplementationDefined)
     * 
     * @param objectType The type of object that should exist
     * @param culprit The term that doesn't exist
     * @param context Context information
     * @return existence_error term wrapped in error/2
     */
    public static Term existenceError(String objectType, Term culprit, String context) {
        Term existenceErrorTerm = new CompoundTerm(new Atom("existence_error"),
                                                 Arrays.asList(new Atom(objectType), culprit));
        return error(existenceErrorTerm, new Atom(context));
    }
    
    /**
     * Create a permission error term.
     * ISO: error(permission_error(Operation, PermissionType, Culprit), ImplementationDefined)
     * 
     * @param operation The operation that was attempted
     * @param permissionType The type of permission required
     * @param culprit The term involved
     * @param context Context information
     * @return permission_error term wrapped in error/2
     */
    public static Term permissionError(String operation, String permissionType, 
                                     Term culprit, String context) {
        Term permissionErrorTerm = new CompoundTerm(new Atom("permission_error"),
                                                  Arrays.asList(new Atom(operation),
                                                              new Atom(permissionType),
                                                              culprit));
        return error(permissionErrorTerm, new Atom(context));
    }
    
    /**
     * Create a representation error term.
     * ISO: error(representation_error(Flag), ImplementationDefined)
     * 
     * @param flag The representation flag
     * @param context Context information
     * @return representation_error term wrapped in error/2
     */
    public static Term representationError(String flag, String context) {
        Term representationErrorTerm = new CompoundTerm(new Atom("representation_error"),
                                                      Arrays.asList(new Atom(flag)));
        return error(representationErrorTerm, new Atom(context));
    }
    
    /**
     * Create an evaluation error term.
     * ISO: error(evaluation_error(Error), ImplementationDefined)
     * 
     * @param errorType The type of evaluation error
     * @param context Context information
     * @return evaluation_error term wrapped in error/2
     */
    public static Term evaluationError(String errorType, String context) {
        Term evaluationErrorTerm = new CompoundTerm(new Atom("evaluation_error"),
                                                  Arrays.asList(new Atom(errorType)));
        return error(evaluationErrorTerm, new Atom(context));
    }
    
    /**
     * Create a resource error term.
     * ISO: error(resource_error(Resource), ImplementationDefined)
     * 
     * @param resource The resource that caused the error
     * @param context Context information
     * @return resource_error term wrapped in error/2
     */
    public static Term resourceError(String resource, String context) {
        Term resourceErrorTerm = new CompoundTerm(new Atom("resource_error"),
                                                Arrays.asList(new Atom(resource)));
        return error(resourceErrorTerm, new Atom(context));
    }
    
    /**
     * Create a syntax error term.
     * ISO: error(syntax_error(ImplDepAtom), ImplementationDefined)
     * 
     * @param description Description of the syntax error
     * @param context Context information
     * @return syntax_error term wrapped in error/2
     */
    public static Term syntaxError(String description, String context) {
        Term syntaxErrorTerm = new CompoundTerm(new Atom("syntax_error"),
                                              Arrays.asList(new Atom(description)));
        return error(syntaxErrorTerm, new Atom(context));
    }
    
    /**
     * Create a system error term (non-ISO, but commonly used).
     * 
     * @param description Description of the system error
     * @param context Context information
     * @return system_error term wrapped in error/2
     */
    public static Term systemError(String description, String context) {
        Term systemErrorTerm = new CompoundTerm(new Atom("system_error"),
                                              Arrays.asList(new Atom(description)));
        return error(systemErrorTerm, new Atom(context));
    }
    
    // Common specific error instances
    
    /**
     * Zero divisor evaluation error.
     */
    public static Term zeroDivisorError(String context) {
        return evaluationError("zero_divisor", context);
    }
    
    /**
     * Undefined function evaluation error.
     */
    public static Term undefinedFunctionError(String context) {
        return evaluationError("undefined", context);
    }
    
    /**
     * Float overflow evaluation error.
     */
    public static Term floatOverflowError(String context) {
        return evaluationError("float_overflow", context);
    }
    
    /**
     * Integer overflow evaluation error.
     */
    public static Term integerOverflowError(String context) {
        return evaluationError("int_overflow", context);
    }
    
    /**
     * Underflow evaluation error.
     */
    public static Term underflowError(String context) {
        return evaluationError("underflow", context);
    }
    
    /**
     * Max arity representation error.
     */
    public static Term maxArityError(String context) {
        return representationError("max_arity", context);
    }
    
    /**
     * Character representation error.
     */
    public static Term characterError(String context) {
        return representationError("character", context);
    }
}