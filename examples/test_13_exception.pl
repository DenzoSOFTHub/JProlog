% ===================================================================
% TEST 13: Exception Handling
% ===================================================================
% Tests: catch/3, throw/1, error handling

% Exception throwing predicates
divide_safe(X, Y, Result) :-
    (Y =:= 0 ->
        throw(division_by_zero(X, Y))
    ;   Result is X / Y
    ).

sqrt_safe(X, Result) :-
    (X < 0 ->
        throw(negative_sqrt(X))
    ;   Result is sqrt(X)
    ).

list_nth_safe(List, Index, Element) :-
    length(List, Len),
    (Index < 1 ; Index > Len ->
        throw(index_out_of_bounds(Index, Len))
    ;   nth1(Index, List, Element)
    ).

% Exception handling wrappers
safe_operation(Operation, Result, Error) :-
    catch(Operation, Exception, (Error = Exception, fail)),
    Result = success.

safe_divide(X, Y, Result) :-
    catch(
        divide_safe(X, Y, Result),
        Error,
        (writeln(error(Error)), fail)
    ).

try_multiple_operations(Operations, Results) :-
    try_ops(Operations, [], Results).

try_ops([], Acc, Results) :-
    reverse(Acc, Results).
try_ops([Op|Rest], Acc, Results) :-
    catch(
        (call(Op), NewAcc = [success(Op)|Acc]),
        Error,
        NewAcc = [error(Op, Error)|Acc]
    ),
    try_ops(Rest, NewAcc, Results).

% Error recovery
robust_calculation(Expr, Result) :-
    catch(
        (Result is Expr),
        Error,
        (writeln('Calculation failed'), 
         writeln(Error),
         Result = undefined)
    ).

% Exception types
validate_input(Input) :-
    (var(Input) ->
        throw(instantiation_error)
    ; \+ number(Input) ->
        throw(type_error(number, Input))  
    ; Input < 0 ->
        throw(domain_error(positive_number, Input))
    ; true
    ).

% Complex exception handling
process_with_cleanup(Goal, Cleanup) :-
    catch(
        Goal,
        Exception,
        (call(Cleanup), throw(Exception))
    ),
    call(Cleanup).

% Resource management
with_resource(Resource, Goal) :-
    setup_resource(Resource),
    catch(
        Goal,
        Exception,
        (cleanup_resource(Resource), throw(Exception))
    ),
    cleanup_resource(Resource).

setup_resource(Resource) :-
    writeln(setting_up(Resource)).

cleanup_resource(Resource) :-
    writeln(cleaning_up(Resource)).

% Test queries:
% ?- divide_safe(10, 2, R).       % Should succeed
% ?- divide_safe(10, 0, R).       % Should throw exception
% ?- safe_divide(10, 0, R).       % Should handle exception
% ?- sqrt_safe(-4, R).            % Should throw exception
% ?- validate_input(5).           % Should succeed
% ?- validate_input(-1).          % Should throw domain_error
% ?- robust_calculation(5/0, R).  % Should handle error gracefully