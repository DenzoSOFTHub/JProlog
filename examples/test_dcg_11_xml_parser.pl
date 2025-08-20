% DCG Test 11: Simple XML Parser
% Tests: Tag parsing, attributes, nested elements

% XML character classes
letter --> [C], { (C >= 97, C =< 122); (C >= 65, C =< 90) }.
digit --> [C], { C >= 48, C =< 57 }.
name_char --> letter.
name_char --> digit.
name_char --> [45].  % hyphen
name_char --> [95].  % underscore

% XML names
xml_name([C|Cs]) --> letter, xml_name_rest(Cs).
xml_name([C]) --> letter.

xml_name_rest([]) --> [].
xml_name_rest([C|Cs]) --> name_char(C), xml_name_rest(Cs).

% Whitespace
ws --> [].
ws --> [32], ws.  % space
ws --> [9], ws.   % tab
ws --> [10], ws.  % newline
ws --> [13], ws.  % carriage return

% XML attributes (simplified)
attr_value([]) --> [34], [34].  % empty ""
attr_value([C|Cs]) --> [34], attr_chars([C|Cs]), [34].

attr_chars([]) --> [].
attr_chars([C|Cs]) --> [C], { C \= 34 }, attr_chars(Cs).

attribute(attr(Name, Value)) --> 
    xml_name(Name), ws, [61], ws, attr_value(Value).  % name = "value"

attributes([]) --> [].
attributes([A|As]) --> ws, attribute(A), attributes(As).

% XML elements
element(element(Name, Attrs, [])) -->
    [60], xml_name(Name), attributes(Attrs), ws, [47], [62].  % <name attrs />

element(element(Name, Attrs, Content)) -->
    [60], xml_name(Name), attributes(Attrs), [62],    % <name attrs>
    content(Content),
    [60], [47], xml_name(Name), [62].                 % </name>

% Content (text and nested elements)
content([]) --> [].
content([text(Text)|Rest]) --> text_content(Text), content(Rest).
content([Elem|Rest]) --> element(Elem), content(Rest).

text_content([C|Cs]) --> [C], { C \= 60 }, text_content(Cs).  % not <
text_content([C]) --> [C], { C \= 60 }.
text_content([]) --> [].

% Document
xml_document(doc(Root)) --> ws, element(Root), ws.

% Test queries:
% ?- phrase(xml_name(N), [116,97,103]).             % Expected: N = [116,97,103] ("tag")
% ?- phrase(element(E), [60,112,62,60,47,112,62]).  % Expected: E = element([112],[],[])
% ?- phrase(element(E), [60,112,47,62]).            % Expected: E = element([112],[],[])