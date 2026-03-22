% Test 37: XML predicates
% Tests: xml_parse, xml_serialize, xpath

% ============================================================
% Test framework
% ============================================================
:- dynamic(test_passed/1).
:- dynamic(test_failed/2).
:- dynamic(test_count/1).
:- assert(test_count(0)).

run_test(Name, Goal) :-
    retract(test_count(N)), N1 is N + 1, assert(test_count(N1)),
    copy_term(Goal, GoalCopy),
    ( catch(call(GoalCopy), E, (assert(test_failed(Name, E)), fail))
    -> assert(test_passed(Name)),
       write('  PASS: '), write(Name), nl
    ;  ( \+ test_failed(Name, _) -> assert(test_failed(Name, failed)) ; true ),
       write('  FAIL: '), write(Name), nl
    ).

run_all_tests :-
    write('=== Test 37: XML Predicates ==='), nl, nl,
    test_xml_parse,
    test_xml_serialize,
    test_xpath,
    nl, write('--- Results ---'), nl,
    aggregate_all(count, test_passed(_), Passed),
    aggregate_all(count, test_failed(_, _), Failed),
    test_count(Total),
    write('Passed: '), write(Passed), write('/'), write(Total), nl,
    write('Failed: '), write(Failed), nl,
    ( Failed > 0
    -> forall(test_failed(N, R), (write('  '), write(N), write(': '), write(R), nl))
    ; true
    ).

% ============================================================
% 1. xml_parse
% ============================================================
test_xml_parse :-
    write('--- xml_parse ---'), nl,
    run_test(parse_simple_element,
        (xml_parse('<root>hello</root>', T), nonvar(T))),
    run_test(parse_with_children,
        (xml_parse('<root><child>text</child></root>', T), nonvar(T))),
    run_test(parse_with_attributes,
        (xml_parse('<item id="1">test</item>', T), nonvar(T))),
    run_test(parse_nested,
        (xml_parse('<a><b><c>deep</c></b></a>', T), nonvar(T))),
    run_test(parse_empty_element,
        (xml_parse('<empty/>', T), nonvar(T))),
    run_test(parse_extracts_tag,
        (xml_parse('<root>hello</root>', element(Tag, _, _)), Tag == root)),
    run_test(parse_extracts_attributes,
        (xml_parse('<item id="42">test</item>', element(item, Attrs, _)),
         member(id='42', Attrs))).

% ============================================================
% 2. xml_serialize
% ============================================================
test_xml_serialize :-
    write('--- xml_serialize ---'), nl,
    run_test(serialize_roundtrip_atom,
        (xml_parse('<root>hello</root>', T),
         xml_serialize(T, S), atom(S))),
    run_test(serialize_with_children,
        (xml_parse('<root><child>text</child></root>', T),
         xml_serialize(T, S), atom(S))),
    run_test(serialize_element_term,
        (xml_serialize(element(test, [], ['content']), S),
         atom(S))).

% ============================================================
% 3. xpath
% ============================================================
test_xpath :-
    write('--- xpath ---'), nl,
    run_test(xpath_select_text,
        (xpath('<root><name>Alice</name></root>',
               '//name', Results),
         Results == ['Alice'])),
    run_test(xpath_multiple_results,
        (xpath('<root><x>1</x><x>2</x><x>3</x></root>',
               '//x', Results),
         length(Results, 3))),
    run_test(xpath_nested_elements,
        (xpath('<root><a><b>deep</b></a></root>',
               '//b', Results),
         Results == [deep])).

:- run_all_tests.
