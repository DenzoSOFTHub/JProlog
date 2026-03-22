% Test 78: Java FFI (Foreign Function Interface)
% Tests for java_new, java_call, java_get_field, java_set_field,
% java_instanceof, java_class, java_array_*, java_to_term, java_from_term

% Helper to print test results
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

% ---------------------------------------------------------------
% 1. java_new - Creating Java objects
% ---------------------------------------------------------------

% Test 1: Create a String object
test_new_string :-
    java_new('java.lang.String', ['hello'], Obj),
    Obj \= null.

% Test 2: Create an ArrayList
test_new_arraylist :-
    java_new('java.util.ArrayList', [], Obj),
    Obj \= null.

% Test 3: Create a HashMap
test_new_hashmap :-
    java_new('java.util.HashMap', [], Obj),
    Obj \= null.

% Test 4: Create a StringBuilder
test_new_stringbuilder :-
    java_new('java.lang.StringBuilder', ['initial'], Obj),
    Obj \= null.

% Test 5: Create StringBuilder using short name
test_new_shortname :-
    java_new('StringBuilder', [], Obj),
    Obj \= null.

% ---------------------------------------------------------------
% 2. java_call - Calling methods
% ---------------------------------------------------------------

% Test 6: Call String.length()
test_string_length :-
    java_new('java.lang.String', ['hello world'], Str),
    java_call(Str, length, [], Len),
    Len =:= 11.

% Test 7: Call String.toUpperCase()
test_string_uppercase :-
    java_new('java.lang.String', ['hello'], Str),
    java_call(Str, toUpperCase, [], Upper),
    Upper = 'HELLO'.

% Test 8: Call ArrayList.add and size
test_arraylist_add :-
    java_new('java.util.ArrayList', [], List),
    java_call(List, add, ['item1'], _),
    java_call(List, add, ['item2'], _),
    java_call(List, size, [], Size),
    Size =:= 2.

% Test 9: Call ArrayList.get
test_arraylist_get :-
    java_new('java.util.ArrayList', [], List),
    java_call(List, add, ['first'], _),
    java_call(List, add, ['second'], _),
    java_call(List, get, [0], First),
    First = first.

% Test 10: Call HashMap.put and get
test_hashmap_put_get :-
    java_new('java.util.HashMap', [], Map),
    java_call(Map, put, [key1, value1], _),
    java_call(Map, get, [key1], Val),
    Val = value1.

% Test 11: Call HashMap.size
test_hashmap_size :-
    java_new('java.util.HashMap', [], Map),
    java_call(Map, put, [a, 1], _),
    java_call(Map, put, [b, 2], _),
    java_call(Map, size, [], Size),
    Size =:= 2.

% Test 12: Call StringBuilder.append and toString
test_stringbuilder_ops :-
    java_new('java.lang.StringBuilder', [], SB),
    java_call(SB, append, ['hello'], _),
    java_call(SB, append, [' '], _),
    java_call(SB, append, ['world'], _),
    java_call(SB, toString, [], Result),
    Result = 'hello world'.

% ---------------------------------------------------------------
% 3. Static method calls
% ---------------------------------------------------------------

% Test 13: Call Integer.parseInt (static)
test_integer_parseint :-
    java_call('java.lang.Integer', parseInt, ['42'], Result),
    Result =:= 42.

% Test 14: Call Math.max (static)
test_math_max :-
    java_call('java.lang.Math', max, [10, 20], Result),
    Result =:= 20.

% Test 15: Call Math.min (static)
test_math_min :-
    java_call('java.lang.Math', min, [5, 3], Result),
    Result =:= 3.

% Test 16: Call Math.abs (static)
test_math_abs :-
    java_call('java.lang.Math', abs, [-42], Result),
    Result =:= 42.

% Test 17: Call String.valueOf (static)
test_string_valueof :-
    java_call('java.lang.String', valueOf, [123], Result),
    Result = '123'.

% ---------------------------------------------------------------
% 4. java_get_field - Field access
% ---------------------------------------------------------------

% Test 18: Get Integer.MAX_VALUE
test_integer_max_value :-
    java_get_field('java.lang.Integer', 'MAX_VALUE', Val),
    Val =:= 2147483647.

% Test 19: Get Integer.MIN_VALUE
test_integer_min_value :-
    java_get_field('java.lang.Integer', 'MIN_VALUE', Val),
    Val =:= -2147483648.

% Test 20: Get Boolean.TRUE
test_boolean_true :-
    java_get_field('java.lang.Boolean', 'TRUE', Val),
    Val \= null.

% ---------------------------------------------------------------
% 5. java_instanceof
% ---------------------------------------------------------------

% Test 21: Check instanceof String
test_instanceof_string :-
    java_new('java.lang.String', ['test'], Obj),
    java_instanceof(Obj, 'java.lang.String').

% Test 22: Check instanceof Object (everything is an Object)
test_instanceof_object :-
    java_new('java.util.ArrayList', [], Obj),
    java_instanceof(Obj, 'java.lang.Object').

% Test 23: Check instanceof fails for wrong type
test_instanceof_fail :-
    java_new('java.lang.String', ['test'], Obj),
    \+ java_instanceof(Obj, 'java.util.List').

% ---------------------------------------------------------------
% 6. java_class
% ---------------------------------------------------------------

% Test 24: Load a class
test_java_class :-
    java_class('java.lang.String', Class),
    Class \= null.

% Test 25: Use loaded class for static call
test_class_static_call :-
    java_class('java.lang.Integer', Class),
    java_call(Class, parseInt, ['100'], Result),
    Result =:= 100.

% ---------------------------------------------------------------
% 7. Array operations
% ---------------------------------------------------------------

% Test 26: Create an int array
test_array_new :-
    java_array_new(int, 5, Arr),
    Arr \= null.

% Test 27: Set and get array elements
test_array_set_get :-
    java_array_new(int, 3, Arr),
    java_array_set(Arr, 0, 10),
    java_array_set(Arr, 1, 20),
    java_array_set(Arr, 2, 30),
    java_array_get(Arr, 1, Val),
    Val =:= 20.

% Test 28: Get array length
test_array_length :-
    java_array_new(int, 7, Arr),
    java_array_length(Arr, Len),
    Len =:= 7.

% Test 29: String array operations
test_string_array :-
    java_array_new('java.lang.String', 2, Arr),
    java_array_set(Arr, 0, hello),
    java_array_set(Arr, 1, world),
    java_array_get(Arr, 0, V0),
    java_array_get(Arr, 1, V1),
    V0 = hello,
    V1 = world.

% ---------------------------------------------------------------
% 8. Type conversions
% ---------------------------------------------------------------

% Test 30: java_to_term with String object
test_to_term_string :-
    java_new('java.lang.String', ['converted'], Obj),
    java_to_term(Obj, T),
    T = converted.

% Test 31: java_from_term converts atom to Java
test_from_term_atom :-
    java_from_term(hello, Ref),
    Ref \= null.

% Test 32: Boolean true conversion
test_boolean_conversion :-
    java_call('java.lang.Boolean', valueOf, ['true'], Obj),
    java_to_term(Obj, Val),
    Val = true.

% ---------------------------------------------------------------
% 9. Error cases
% ---------------------------------------------------------------

% Test 33: java_new with non-existent class fails
test_new_bad_class :-
    \+ java_new('com.nonexistent.Foo', [], _).

% Test 34: java_call with non-existent method fails
test_call_bad_method :-
    java_new('java.lang.String', ['test'], Obj),
    \+ java_call(Obj, nonExistentMethod, [], _).

% Test 35: java_get_field with non-existent field fails
test_field_bad_name :-
    \+ java_get_field('java.lang.Integer', 'NONEXISTENT', _).

% Test 36: java_array_get with out of bounds fails
test_array_oob :-
    java_array_new(int, 3, Arr),
    \+ java_array_get(Arr, 10, _).

% ---------------------------------------------------------------
% 10. Complex scenarios
% ---------------------------------------------------------------

% Test 37: Chain of method calls
test_method_chain :-
    java_new('java.util.ArrayList', [], List),
    java_call(List, add, ['a'], _),
    java_call(List, add, ['b'], _),
    java_call(List, add, ['c'], _),
    java_call(List, contains, ['b'], Contains),
    Contains = true.

% Test 38: HashMap with multiple entries and containsKey
test_hashmap_containskey :-
    java_new('java.util.HashMap', [], Map),
    java_call(Map, put, [name, 'Alice'], _),
    java_call(Map, put, [age, 30], _),
    java_call(Map, containsKey, [name], R),
    R = true.

% Test 39: String operations chain
test_string_chain :-
    java_new('java.lang.String', ['  Hello World  '], Str),
    java_call(Str, trim, [], Trimmed),
    java_call(Trimmed, toLowerCase, [], Lower),
    Lower = 'hello world'.

% Test 40: Create object and check instanceof after method call
test_complex_instanceof :-
    java_new('java.util.ArrayList', [], List),
    java_instanceof(List, 'java.util.List'),
    java_instanceof(List, 'java.util.Collection').

% ---------------------------------------------------------------
% Main test runner
% ---------------------------------------------------------------

:- write('=== Java FFI Tests ==='), nl.

:- run_test('Create String object', test_new_string).
:- run_test('Create ArrayList', test_new_arraylist).
:- run_test('Create HashMap', test_new_hashmap).
:- run_test('Create StringBuilder', test_new_stringbuilder).
:- run_test('Create with short name', test_new_shortname).
:- run_test('String.length()', test_string_length).
:- run_test('String.toUpperCase()', test_string_uppercase).
:- run_test('ArrayList add and size', test_arraylist_add).
:- run_test('ArrayList.get()', test_arraylist_get).
:- run_test('HashMap put and get', test_hashmap_put_get).
:- run_test('HashMap.size()', test_hashmap_size).
:- run_test('StringBuilder operations', test_stringbuilder_ops).
:- run_test('Integer.parseInt static', test_integer_parseint).
:- run_test('Math.max static', test_math_max).
:- run_test('Math.min static', test_math_min).
:- run_test('Math.abs static', test_math_abs).
:- run_test('String.valueOf static', test_string_valueof).
:- run_test('Integer.MAX_VALUE field', test_integer_max_value).
:- run_test('Integer.MIN_VALUE field', test_integer_min_value).
:- run_test('Boolean.TRUE field', test_boolean_true).
:- run_test('instanceof String', test_instanceof_string).
:- run_test('instanceof Object', test_instanceof_object).
:- run_test('instanceof fails for wrong type', test_instanceof_fail).
:- run_test('Load class', test_java_class).
:- run_test('Class static call', test_class_static_call).
:- run_test('Array new', test_array_new).
:- run_test('Array set and get', test_array_set_get).
:- run_test('Array length', test_array_length).
:- run_test('String array ops', test_string_array).
:- run_test('java_to_term String', test_to_term_string).
:- run_test('java_from_term atom', test_from_term_atom).
:- run_test('Boolean conversion', test_boolean_conversion).
:- run_test('New with bad class fails', test_new_bad_class).
:- run_test('Call bad method fails', test_call_bad_method).
:- run_test('Get bad field fails', test_field_bad_name).
:- run_test('Array out of bounds fails', test_array_oob).
:- run_test('Method call chain', test_method_chain).
:- run_test('HashMap containsKey', test_hashmap_containskey).
:- run_test('String operations chain', test_string_chain).
:- run_test('Complex instanceof', test_complex_instanceof).

:- write('=== Java FFI Tests Complete ==='), nl.
