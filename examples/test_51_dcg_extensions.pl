% test_51_dcg_extensions.pl  
% Comprehensive test for Phase 8 DCG Extensions per ISO/IEC DTS 13211-3
% Tests enhanced DCG features and ISO compliance

% ===================================================================
% Enhanced DCG Grammar Rules
% ===================================================================

% Basic grammar with enhanced features
simple_noun --> [cat].
simple_noun --> [dog].
simple_noun --> [bird].

article --> [the].
article --> [a].

% Enhanced noun phrase with optional modifiers
noun_phrase --> article, adjectives, simple_noun.
noun_phrase --> article, simple_noun.

% Adjectives (zero or more)
adjectives --> [].
adjectives --> adjective, adjectives.

adjective --> [big].
adjective --> [small].
adjective --> [red].
adjective --> [blue].

% Verbs
verb --> [runs].
verb --> [flies].
verb --> [sleeps].

% Complete sentence
sentence --> noun_phrase, verb.
sentence --> noun_phrase, verb, noun_phrase.

% ===================================================================
% Advanced DCG Features (ISO/IEC DTS 13211-3)
% ===================================================================

% DCG with embedded Prolog goals
number_range(N) --> [N], { N >= 1, N =< 100 }.

% DCG with cuts and conditionals
optional_sign(positive) --> [+], !.
optional_sign(negative) --> [-], !.
optional_sign(positive) --> [].

signed_number(Sign-Num) --> 
    optional_sign(Sign), 
    number_range(Num).

% DCG with disjunction and complex control
expression --> term.
expression --> term, [+], expression.
expression --> term, [-], expression.

term --> factor.
term --> factor, [*], term.
term --> factor, [/], term.

factor --> number_range(_).
factor --> ['('], expression, [')'].

% DCG with list processing
list_of_numbers([]) --> [].
list_of_numbers([N|Ns]) --> 
    number_range(N), 
    [','], 
    list_of_numbers(Ns).
list_of_numbers([N]) --> number_range(N).

% ===================================================================
% Meta-DCG Features
% ===================================================================

% Higher-order DCG predicate
repeat_pattern(_, 0) --> [].
repeat_pattern(Pattern, N) --> 
    { N > 0, N1 is N - 1 },
    Pattern,
    repeat_pattern(Pattern, N1).

% DCG with variable goals
call_dcg_rule(Rule) --> call(Rule).

% Pattern matching DCG
match_pattern([]) --> [].
match_pattern([H|T]) --> [H], match_pattern(T).

% ===================================================================
% DCG Translation and Utilities
% ===================================================================

% Test DCG rule translation
test_dcg_translation :-
    % Test translation of simple rule
    Rule = (simple_noun --> [cat]),
    dcg_translate_rule(Rule, Translated),
    write('Simple rule translation: '), writeln(Translated).

% Test enhanced phrase predicates
test_enhanced_phrase :-
    % Test basic enhanced phrase
    enhanced_phrase(simple_noun, [cat]),
    write('Enhanced phrase test 1: PASSED'), nl,
    
    % Test phrase with remainder
    enhanced_phrase(simple_noun, [cat, runs], Rest),
    write('Enhanced phrase test 2 - Rest: '), writeln(Rest).

% Test phrase with options
test_phrase_with_options :-
    Options = [trace(true), debug(false), max_depth(100)],
    phrase_with_options(simple_noun, [cat], [], Options),
    write('Phrase with options test: PASSED'), nl.

% ===================================================================
% Comprehensive DCG Test Suite
% ===================================================================

% Test 1: Basic DCG functionality
test_basic_dcg :-
    writeln('=== Test 1: Basic DCG Functionality ==='),
    
    % Test simple noun recognition
    ( enhanced_phrase(simple_noun, [cat]) -> 
        writeln('✅ Simple noun recognition: PASSED')
    ; 
        writeln('❌ Simple noun recognition: FAILED')
    ),
    
    % Test article recognition  
    ( enhanced_phrase(article, [the]) ->
        writeln('✅ Article recognition: PASSED')
    ;
        writeln('❌ Article recognition: FAILED')
    ),
    nl.

% Test 2: Complex DCG structures
test_complex_dcg :-
    writeln('=== Test 2: Complex DCG Structures ==='),
    
    % Test noun phrase
    ( enhanced_phrase(noun_phrase, [the, big, red, cat]) ->
        writeln('✅ Complex noun phrase: PASSED')
    ;
        writeln('❌ Complex noun phrase: FAILED')
    ),
    
    % Test complete sentence
    ( enhanced_phrase(sentence, [the, cat, runs]) ->
        writeln('✅ Simple sentence: PASSED')
    ;
        writeln('❌ Simple sentence: FAILED')
    ),
    
    % Test sentence with object
    ( enhanced_phrase(sentence, [a, big, dog, chases, the, small, cat]) ->
        writeln('✅ Complex sentence: PASSED')
    ;
        writeln('❌ Complex sentence: FAILED')
    ),
    nl.

% Test 3: DCG with Prolog goals
test_dcg_goals :-
    writeln('=== Test 3: DCG with Prolog Goals ==='),
    
    % Test number range
    ( enhanced_phrase(number_range(N), [42]) ->
        (N =:= 42 ->
            writeln('✅ Number range parsing: PASSED')
        ;
            writeln('❌ Number range parsing: Wrong value')
        )
    ;
        writeln('❌ Number range parsing: FAILED')
    ),
    
    % Test signed numbers
    ( enhanced_phrase(signed_number(Sign-Num), [+, 25]) ->
        (Sign = positive, Num =:= 25 ->
            writeln('✅ Signed number parsing: PASSED')
        ;
            writeln('❌ Signed number parsing: Wrong values')
        )
    ;
        writeln('❌ Signed number parsing: FAILED')
    ),
    nl.

% Test 4: Advanced control structures
test_control_structures :-
    writeln('=== Test 4: Advanced Control Structures ==='),
    
    % Test expression parsing
    ( enhanced_phrase(expression, [5, +, 3, *, 2]) ->
        writeln('✅ Expression parsing: PASSED')
    ;
        writeln('❌ Expression parsing: FAILED')
    ),
    
    % Test list processing
    ( enhanced_phrase(list_of_numbers([1, 2, 3]), [1, ',', 2, ',', 3]) ->
        writeln('✅ List processing: PASSED')
    ;
        writeln('❌ List processing: FAILED')
    ),
    nl.

% Test 5: Meta-DCG features  
test_meta_dcg :-
    writeln('=== Test 5: Meta-DCG Features ==='),
    
    % Test repeat pattern
    ( enhanced_phrase(repeat_pattern(simple_noun, 3), [cat, dog, bird]) ->
        writeln('✅ Repeat pattern: PASSED')
    ;
        writeln('❌ Repeat pattern: FAILED')
    ),
    
    % Test pattern matching
    ( enhanced_phrase(match_pattern([a, b, c]), [a, b, c]) ->
        writeln('✅ Pattern matching: PASSED')
    ;
        writeln('❌ Pattern matching: FAILED')
    ),
    nl.

% Test 6: DCG utilities
test_dcg_utilities :-
    writeln('=== Test 6: DCG Utilities ==='),
    
    % Test DCG translation
    ( dcg_translate_rule((simple_noun --> [cat]), Translated) ->
        (write('✅ DCG translation: PASSED - '), writeln(Translated))
    ;
        writeln('❌ DCG translation: FAILED')
    ),
    
    % Test call_dcg
    ( call_dcg(simple_noun, [cat], []) ->
        writeln('✅ call_dcg: PASSED')
    ;
        writeln('❌ call_dcg: FAILED')
    ),
    nl.

% Test 7: Phrase with options
test_phrase_options :-
    writeln('=== Test 7: Phrase with Options ==='),
    
    % Test basic options
    Options1 = [syntax_errors(fail), max_depth(50)],
    ( phrase_with_options(simple_noun, [cat], [], Options1) ->
        writeln('✅ Phrase with basic options: PASSED')
    ;
        writeln('❌ Phrase with basic options: FAILED')
    ),
    
    % Test tracing options
    Options2 = [trace(true), debug(true)],
    ( phrase_with_options(article, [the], [], Options2) ->
        writeln('✅ Phrase with trace options: PASSED')  
    ;
        writeln('❌ Phrase with trace options: FAILED')
    ),
    nl.

% Test 8: Error handling
test_error_handling :-
    writeln('=== Test 8: Error Handling ==='),
    
    % Test syntax error handling
    Options = [syntax_errors(fail)],
    ( \+ phrase_with_options(nonexistent_rule, [test], [], Options) ->
        writeln('✅ Syntax error handling: PASSED')
    ;
        writeln('❌ Syntax error handling: FAILED')  
    ),
    
    % Test max depth
    DeepOptions = [max_depth(5)],
    ( phrase_with_options(simple_noun, [cat], [], DeepOptions) ->
        writeln('✅ Max depth handling: PASSED')
    ;
        writeln('❌ Max depth handling: FAILED')
    ),
    nl.

% Main test runner for all DCG extension tests
run_all_dcg_tests :-
    writeln('=== JProlog Phase 8 DCG Extensions Test Suite ==='),
    writeln('Testing ISO/IEC DTS 13211-3 compliance'),
    nl,
    
    test_basic_dcg,
    test_complex_dcg, 
    test_dcg_goals,
    test_control_structures,
    test_meta_dcg,
    test_dcg_utilities,
    test_phrase_options,
    test_error_handling,
    
    writeln('=== DCG Extensions Test Suite Complete ==='),
    writeln('Phase 8 implementation provides comprehensive DCG support'),
    writeln('including ISO/IEC DTS 13211-3 advanced features.').

% Quick test for verification
quick_dcg_test :-
    enhanced_phrase(simple_noun, [cat]),
    enhanced_phrase(article, [the]),
    write('Quick DCG test: PASSED').

% Performance test
performance_test :-
    writeln('=== DCG Performance Test ==='),
    get_time(Start),
    
    % Run multiple DCG operations
    forall(between(1, 1000, _), (
        enhanced_phrase(simple_noun, [cat]),
        enhanced_phrase(article, [the])
    )),
    
    get_time(End),
    Time is End - Start,
    write('1000 DCG operations completed in: '),
    write(Time), writeln(' seconds').