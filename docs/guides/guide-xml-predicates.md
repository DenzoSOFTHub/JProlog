# XML Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.xml` package provides built-in predicates for parsing, serializing, and querying XML data directly from Prolog. It uses Java's built-in DOM parser and XPath engine under the hood.

XML documents are represented in Prolog as nested `element/3` terms:

```prolog
element(Tag, Attributes, Children)
```

- **Tag** -- an atom representing the element name (e.g., `item`, `'rss'`).
- **Attributes** -- a list of `Name = Value` pairs, both atoms (e.g., `[version = '2.0']`).
- **Children** -- a list of nested `element/3` terms or atoms (text content).

**Source file**: `src/main/java/it/denzosoft/jprolog/builtin/xml/XmlPredicates.java`

---

## Predicate Reference

### xml_parse/2

```prolog
xml_parse(+XmlString, -Term)
```

Parses an XML string into a Prolog term tree.

| Argument   | Mode | Type | Description                              |
|------------|------|------|------------------------------------------|
| XmlString  | +    | atom | A well-formed XML document as a string   |
| Term       | -    | term | The resulting `element/3` compound term   |

**Behavior**:
- The input must be a well-formed XML string. Malformed XML raises an evaluation error.
- Text nodes are trimmed; whitespace-only text nodes are discarded.
- Attributes are represented as `Name = Value` pairs in a list.
- DOCTYPE declarations are disallowed for security (prevents XXE attacks).

**Example**:
```prolog
?- xml_parse('<book lang="en"><title>Prolog</title></book>', T).
T = element(book, [lang = en], [element(title, [], ['Prolog'])])
```

---

### xml_serialize/2

```prolog
xml_serialize(+Term, -XmlString)
```

Converts a Prolog `element/3` term tree back into an XML string.

| Argument   | Mode | Type | Description                                |
|------------|------|------|--------------------------------------------|
| Term       | +    | term | An `element/3` compound term               |
| XmlString  | -    | atom | The resulting XML string                   |

**Behavior**:
- Elements with no children are serialized as self-closing tags (`<tag/>`).
- Attributes are included in the opening tag.
- Special characters (`&`, `<`, `>`, `"`, `'`) are escaped automatically.
- Plain atoms appearing as children are serialized as text content.

**Example**:
```prolog
?- xml_serialize(element(p, [class = intro], ['Hello']), Xml).
Xml = '<p class="intro">Hello</p>'
```

---

### xpath/3

```prolog
xpath(+XmlString, +XPathExpr, -Results)
```

Evaluates an XPath expression against an XML string and returns matching text content.

| Argument   | Mode | Type | Description                                 |
|------------|------|------|---------------------------------------------|
| XmlString  | +    | atom | A well-formed XML document as a string      |
| XPathExpr  | +    | atom | An XPath 1.0 expression                     |
| Results    | -    | list | A list of atoms with the text content of each matched node |

**Behavior**:
- Uses Java's XPath 1.0 engine.
- Returns the `textContent` of each matched node as an atom.
- If no nodes match, Results unifies with the empty list `[]`.
- The XPath is evaluated against the full document (not a pre-parsed term).

**Example**:
```prolog
?- xpath('<ul><li>A</li><li>B</li></ul>', '//li', R).
R = ['A', 'B']
```

---

## Real-World Examples

### Example 1: RSS Feed Parser

Parse an RSS feed, extract article titles, publication dates, and links.

```prolog
% rss_parser.pl
% Parse an RSS XML feed and extract structured article data.

% Sample RSS feed content (normally loaded from a file or HTTP response).
rss_feed('<rss version="2.0"><channel><title>Tech News</title><item><title>JProlog 2.5 Released</title><pubDate>2026-03-15</pubDate><link>https://example.com/jprolog-25</link></item><item><title>Prolog in Industry</title><pubDate>2026-03-10</pubDate><link>https://example.com/prolog-industry</link></item><item><title>Logic Programming Revival</title><pubDate>2026-03-05</pubDate><link>https://example.com/logic-revival</link></item></channel></rss>').

% Extract all article titles from the RSS feed.
% Uses XPath to select all <title> elements inside <item> elements.
article_titles(Titles) :-
    rss_feed(Xml),
    xpath(Xml, '//item/title', Titles).

% Extract all publication dates.
article_dates(Dates) :-
    rss_feed(Xml),
    xpath(Xml, '//item/pubDate', Dates).

% Extract all links.
article_links(Links) :-
    rss_feed(Xml),
    xpath(Xml, '//item/link', Links).

% Build a list of article(Title, Date, Link) structures by combining
% the three extracted lists element-wise.
article_records(Records) :-
    article_titles(Titles),
    article_dates(Dates),
    article_links(Links),
    zip_three(Titles, Dates, Links, Records).

% zip_three/4 -- combine three lists into a list of article/3 terms.
zip_three([], [], [], []).
zip_three([T|Ts], [D|Ds], [L|Ls], [article(T, D, L)|Rs]) :-
    zip_three(Ts, Ds, Ls, Rs).

% Extract the channel title (the feed-level title, not article titles).
feed_title(Title) :-
    rss_feed(Xml),
    xpath(Xml, '/rss/channel/title', [Title|_]).

% Find articles published after a given date (lexicographic comparison).
articles_after(DateThreshold, Matching) :-
    article_records(Records),
    include_after(Records, DateThreshold, Matching).

include_after([], _, []).
include_after([article(T, D, L)|Rs], Threshold, [article(T, D, L)|Ms]) :-
    D @> Threshold, !,
    include_after(Rs, Threshold, Ms).
include_after([_|Rs], Threshold, Ms) :-
    include_after(Rs, Threshold, Ms).

% Usage:
% ?- article_titles(T).
% T = ['JProlog 2.5 Released', 'Prolog in Industry', 'Logic Programming Revival']
%
% ?- article_records(R).
% R = [article('JProlog 2.5 Released','2026-03-15','https://example.com/jprolog-25'),
%      article('Prolog in Industry','2026-03-10','https://example.com/prolog-industry'),
%      article('Logic Programming Revival','2026-03-05','https://example.com/logic-revival')]
%
% ?- articles_after('2026-03-08', M).
% M = [article('JProlog 2.5 Released','2026-03-15',...),
%      article('Prolog in Industry','2026-03-10',...)]
```

---

### Example 2: Configuration File Processor

Read an XML configuration, extract settings by section, and validate required keys.

```prolog
% config_processor.pl
% Parse an XML configuration file, extract settings, and validate
% that all required keys are present.

% Sample application configuration.
app_config('<config>
  <database>
    <host>db.example.com</host>
    <port>5432</port>
    <name>production</name>
    <pool_size>10</pool_size>
  </database>
  <cache>
    <enabled>true</enabled>
    <ttl>3600</ttl>
    <max_entries>5000</max_entries>
  </cache>
  <logging>
    <level>info</level>
    <file>/var/log/app.log</file>
  </logging>
</config>').

% Extract all values under a given section.
% section_values(+Section, -Pairs) where Pairs is a list of key=value atoms.
section_values(Section, Pairs) :-
    app_config(Xml),
    xml_parse(Xml, Tree),
    Tree = element(config, _, Children),
    member(element(Section, _, SectionChildren), Children),
    extract_pairs(SectionChildren, Pairs).

% extract_pairs/2 -- convert child elements into key=value pairs.
extract_pairs([], []).
extract_pairs([element(Key, _, [Value])|Rest], [Key=Value|Pairs]) :-
    extract_pairs(Rest, Pairs).
extract_pairs([_|Rest], Pairs) :-
    extract_pairs(Rest, Pairs).

% Get a single configuration value by section and key.
config_value(Section, Key, Value) :-
    section_values(Section, Pairs),
    member(Key=Value, Pairs).

% Validate that a section contains all required keys.
% Returns missing keys if any are absent.
validate_section(Section, RequiredKeys, Status) :-
    section_values(Section, Pairs),
    findall(K, member(K=_, Pairs), PresentKeys),
    find_missing(RequiredKeys, PresentKeys, Missing),
    (   Missing = []
    ->  Status = ok
    ;   Status = missing(Missing)
    ).

find_missing([], _, []).
find_missing([K|Ks], Present, Missing) :-
    (   member(K, Present)
    ->  find_missing(Ks, Present, Missing)
    ;   Missing = [K|RestMissing],
        find_missing(Ks, Present, RestMissing)
    ).

% List all section names in the configuration.
config_sections(Sections) :-
    app_config(Xml),
    xml_parse(Xml, element(config, _, Children)),
    findall(Name, member(element(Name, _, _), Children), Sections).

% Usage:
% ?- config_value(database, host, V).
% V = 'db.example.com'
%
% ?- section_values(cache, Pairs).
% Pairs = [enabled=true, ttl='3600', max_entries='5000']
%
% ?- validate_section(database, [host, port, name, password], S).
% S = missing([password])
%
% ?- config_sections(S).
% S = [database, cache, logging]
```

---

### Example 3: HTML Report Generator

Build an HTML document from Prolog data and serialize it to a string.

```prolog
% report_generator.pl
% Build an HTML report from structured Prolog data using xml_serialize/2.

% Sample sales data: sales(Region, Q1, Q2, Q3, Q4).
sales(north, 12000, 15000, 13500, 18000).
sales(south, 9500,  11000, 10200, 14000).
sales(east,  8700,  9800,  11500, 12300).
sales(west,  14200, 16300, 15800, 19500).

% Build a complete HTML report with a table of sales data.
generate_report(Html) :-
    findall(
        sales(Region, Q1, Q2, Q3, Q4),
        sales(Region, Q1, Q2, Q3, Q4),
        AllSales
    ),
    build_table_rows(AllSales, RowElements),
    % Construct the header row.
    HeaderRow = element(tr, [], [
        element(th, [], ['Region']),
        element(th, [], ['Q1']),
        element(th, [], ['Q2']),
        element(th, [], ['Q3']),
        element(th, [], ['Q4']),
        element(th, [], ['Total'])
    ]),
    % Assemble the full HTML document.
    Document = element(html, [], [
        element(head, [], [
            element(title, [], ['Quarterly Sales Report']),
            element(style, [], ['table { border-collapse: collapse; } th, td { border: 1px solid black; padding: 8px; }'])
        ]),
        element(body, [], [
            element(h1, [], ['Quarterly Sales Report']),
            element(table, [], [HeaderRow | RowElements]),
            element(p, [], ['Report generated by JProlog.'])
        ])
    ]),
    xml_serialize(Document, Html).

% Build HTML table rows from sales data.
build_table_rows([], []).
build_table_rows([sales(Region, Q1, Q2, Q3, Q4)|Rest], [Row|Rows]) :-
    Total is Q1 + Q2 + Q3 + Q4,
    number_to_atom(Q1, A1), number_to_atom(Q2, A2),
    number_to_atom(Q3, A3), number_to_atom(Q4, A4),
    number_to_atom(Total, AT),
    Row = element(tr, [], [
        element(td, [], [Region]),
        element(td, [], [A1]),
        element(td, [], [A2]),
        element(td, [], [A3]),
        element(td, [], [A4]),
        element(td, [], [AT])
    ]),
    build_table_rows(Rest, Rows).

number_to_atom(N, A) :- number_codes(N, Codes), atom_codes(A, Codes).

% Usage:
% ?- generate_report(Html).
% Html = '<html><head><title>Quarterly Sales Report</title>...'
% The output is a complete, valid HTML document with a styled table.
```

---

### Example 4: SOAP Message Builder

Construct a SOAP XML envelope for a web service call.

```prolog
% soap_builder.pl
% Build SOAP 1.1 request envelopes for a hypothetical stock quote service.

% Build a SOAP envelope for a GetStockPrice request.
% soap_stock_request(+Symbol, -SoapXml)
soap_stock_request(Symbol, SoapXml) :-
    Body = element('soap:Body', [], [
        element('GetStockPrice', [xmlns = 'http://example.com/stocks'], [
            element('Symbol', [], [Symbol])
        ])
    ]),
    Envelope = element('soap:Envelope', [
        'xmlns:soap' = 'http://schemas.xmlsoap.org/soap/envelope/',
        'soap:encodingStyle' = 'http://schemas.xmlsoap.org/soap/encoding/'
    ], [
        element('soap:Header', [], [
            element('Authentication', [xmlns = 'http://example.com/auth'], [
                element('ApiKey', [], ['sk-demo-key-12345']),
                element('Timestamp', [], ['2026-03-21T10:00:00Z'])
            ])
        ]),
        Body
    ]),
    xml_serialize(Envelope, SoapXml).

% Build a batch request for multiple stock symbols.
% soap_batch_request(+Symbols, -SoapXml)
soap_batch_request(Symbols, SoapXml) :-
    build_symbol_elements(Symbols, SymbolElements),
    Body = element('soap:Body', [], [
        element('GetBatchStockPrices', [xmlns = 'http://example.com/stocks'], SymbolElements)
    ]),
    Envelope = element('soap:Envelope', [
        'xmlns:soap' = 'http://schemas.xmlsoap.org/soap/envelope/'
    ], [Body]),
    xml_serialize(Envelope, SoapXml).

build_symbol_elements([], []).
build_symbol_elements([S|Ss], [element('Symbol', [], [S])|Es]) :-
    build_symbol_elements(Ss, Es).

% Parse a SOAP response and extract the price.
% parse_stock_response(+ResponseXml, -Symbol, -Price)
parse_stock_response(ResponseXml, Symbol, Price) :-
    xpath(ResponseXml, '//Symbol', [Symbol|_]),
    xpath(ResponseXml, '//Price', [Price|_]).

% Usage:
% ?- soap_stock_request('AAPL', Xml).
% Xml = '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ...>
%          <soap:Header>...</soap:Header>
%          <soap:Body><GetStockPrice xmlns="http://example.com/stocks">
%            <Symbol>AAPL</Symbol></GetStockPrice></soap:Body>
%        </soap:Envelope>'
%
% ?- soap_batch_request(['AAPL', 'GOOG', 'MSFT'], Xml).
% Xml = '<soap:Envelope ...><soap:Body><GetBatchStockPrices ...>
%          <Symbol>AAPL</Symbol><Symbol>GOOG</Symbol><Symbol>MSFT</Symbol>
%        </GetBatchStockPrices></soap:Body></soap:Envelope>'
```

---

### Example 5: XML Data Transformation

Parse XML in one schema, restructure the data, and serialize to a different format.

```prolog
% xml_transform.pl
% Transform a flat employee list XML into a department-grouped XML structure.

% Input: flat list of employees with department attributes.
employee_xml('<employees>
  <employee dept="engineering"><name>Alice</name><role>Lead</role></employee>
  <employee dept="engineering"><name>Bob</name><role>Developer</role></employee>
  <employee dept="marketing"><name>Carol</name><role>Manager</role></employee>
  <employee dept="marketing"><name>Dave</name><role>Analyst</role></employee>
  <employee dept="engineering"><name>Eve</name><role>Developer</role></employee>
  <employee dept="sales"><name>Frank</name><role>Director</role></employee>
</employees>').

% Parse the XML and extract employee records as emp(Name, Role, Dept) terms.
extract_employees(Employees) :-
    employee_xml(Xml),
    xml_parse(Xml, element(employees, _, Children)),
    findall(
        emp(Name, Role, Dept),
        (   member(element(employee, Attrs, EChildren), Children),
            member(dept = Dept, Attrs),
            member(element(name, _, [Name]), EChildren),
            member(element(role, _, [Role]), EChildren)
        ),
        Employees
    ).

% Group employees by department.
% group_by_dept(-Groups) produces a list of dept(Name, [emp(...),...]) terms.
group_by_dept(Groups) :-
    extract_employees(Emps),
    collect_depts(Emps, DeptNames),
    sort(DeptNames, UniqueDepts),
    findall(
        dept(D, DeptEmps),
        (   member(D, UniqueDepts),
            findall(emp(N, R, D), member(emp(N, R, D), Emps), DeptEmps)
        ),
        Groups
    ).

collect_depts([], []).
collect_depts([emp(_, _, D)|Es], [D|Ds]) :- collect_depts(Es, Ds).

% Transform grouped data into a new XML structure organized by department.
transform_to_dept_xml(OutputXml) :-
    group_by_dept(Groups),
    build_dept_elements(Groups, DeptElements),
    OutputTree = element(organization, [], DeptElements),
    xml_serialize(OutputTree, OutputXml).

build_dept_elements([], []).
build_dept_elements([dept(DeptName, Emps)|Rest], [DeptElem|Elems]) :-
    build_member_elements(Emps, MemberElems),
    length(Emps, Count),
    number_codes(Count, CountCodes), atom_codes(CountAtom, CountCodes),
    DeptElem = element(department, [name = DeptName, headcount = CountAtom], MemberElems),
    build_dept_elements(Rest, Elems).

build_member_elements([], []).
build_member_elements([emp(Name, Role, _)|Es], [Elem|Elems]) :-
    Elem = element(member, [role = Role], [Name]),
    build_member_elements(Es, Elems).

% Verify transformation by querying the output with XPath.
verify_dept_count(Dept, Count) :-
    transform_to_dept_xml(Xml),
    atom_concat('//department[@name="', Dept, P1),
    atom_concat(P1, '"]/member', XPath),
    xpath(Xml, XPath, Members),
    length(Members, Count).

% Usage:
% ?- extract_employees(E).
% E = [emp('Alice','Lead',engineering), emp('Bob','Developer',engineering),
%      emp('Carol','Manager',marketing), emp('Dave','Analyst',marketing),
%      emp('Eve','Developer',engineering), emp('Frank','Director',sales)]
%
% ?- transform_to_dept_xml(Xml).
% Xml = '<organization>
%          <department name="engineering" headcount="3">
%            <member role="Lead">Alice</member>
%            <member role="Developer">Bob</member>
%            <member role="Developer">Eve</member>
%          </department>
%          <department name="marketing" headcount="2">
%            <member role="Manager">Carol</member>
%            <member role="Analyst">Dave</member>
%          </department>
%          <department name="sales" headcount="1">
%            <member role="Director">Frank</member>
%          </department>
%        </organization>'
%
% ?- verify_dept_count(engineering, N).
% N = 3
```
