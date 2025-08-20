# DCG Test Issues Report

**Date**: 2025-08-20  
**Test Suite**: 20 DCG Comprehensive Programs  
**Overall Success Rate**: 35% (7/20 passed)  

## Summary

Testing of 20 comprehensive DCG programs revealed significant parsing and syntax support issues in JProlog's DCG implementation. While basic DCG functionality works, complex constructs and certain syntax patterns cause failures.

## Issues Identified

### ISS-2025-0035: DCG Parser Limitations with Complex Character Lists
**Status**: TO_ANALYZE  
**Priority**: HIGH  
**Affected Programs**: 13/20

**Description**: DCG rules containing complex character codes in lists fail to parse correctly.

**Failing Examples**:
```prolog
% These DCG rules fail to load:
http --> [104,116,116,112].              % "http" character codes
email_char --> [C], { C >= 97, C =< 122 }. % Character range validation
```

**Expected Behavior**: DCG rules should accept character code lists and constraints
**Actual Behavior**: Parser error "Expected ']' at line X, column Y"

### ISS-2025-0036: DCG Constraint Handling in Goals
**Status**: TO_ANALYZE  
**Priority**: HIGH  
**Affected Programs**: 11/20

**Description**: DCG rules with Prolog constraints `{ Goal }` are not properly parsed or processed.

**Failing Examples**:
```prolog
digit(D) --> [C], { C >= 48, C =< 57, D is C - 48 }.
char_range(C) --> [C], { member(C, [97,98,99]) }.
```

**Expected Behavior**: Constraints should be evaluated during DCG parsing
**Actual Behavior**: Syntax error or constraint evaluation failure

### ISS-2025-0037: DCG Negation and Cut Support
**Status**: TO_ANALYZE  
**Priority**: MEDIUM  
**Affected Programs**: 5/20

**Description**: DCG rules using negation `\+` and cut operations are not supported.

**Failing Examples**:
```prolog
identifier_char --> [C], { \+ member(C, [32,9,10]) }.
keyword(if) --> [105,102], \+ identifier_char.
```

### ISS-2025-0038: DCG Variable Scope in Complex Rules
**Status**: TO_ANALYZE  
**Priority**: MEDIUM  
**Affected Programs**: 8/20

**Description**: Variables in complex DCG rules don't maintain proper scope across rule components.

**Failing Examples**:
```prolog
number(N) --> digits(Ds), { number_codes(N, Ds) }.
validate_date(date(M,D,Y)) --> date_parts(M,D,Y), { valid_date(date(M,D,Y)) }.
```

### ISS-2025-0039: DCG List Manipulation Integration
**Status**: TO_ANALYZE  
**Priority**: MEDIUM  
**Affected Programs**: 6/20

**Description**: DCG rules that need to manipulate input lists beyond simple consumption fail.

**Failing Examples**:
```prolog
exact_length(N, List) --> { length(List, N) }, phrase_list(List).
append_dcg(L1, L2, Result) --> L1, L2, { append(L1, L2, Result) }.
```

## Working DCG Features (Confirmed)

Based on successful tests, these DCG features work correctly:

1. **Basic Terminal Matching**: `a --> [a].`
2. **Simple Non-terminal Rules**: `abc --> a, b, c.`
3. **Basic Recursion**: `as --> []; [a], as.`
4. **Simple Variable Binding**: `single(X) --> [X].`
5. **Basic List Construction**: `collect([]) --> []; [H], collect(T).`
6. **Simple Choice**: `vowel --> [a]; [e]; [i].`
7. **Basic Sequence**: `word --> letter, letters.`

## Impact Analysis

- **Severity**: HIGH - 65% of comprehensive DCG programs fail
- **Scope**: DCG parsing limitations affect advanced language processing
- **User Impact**: Limited ability to create complex parsers and language processors
- **Development Impact**: Restricts DCG use to basic pattern matching only

## Recommendations

### Immediate Actions (Priority 1)
1. **Fix Character Code List Parsing**: Support `[104,116,116,112]` format
2. **Implement Constraint Goal Support**: Proper `{ Goal }` handling in DCG rules
3. **Enhanced DCG Parser**: Support complex syntax patterns

### Medium Term (Priority 2)
1. **Variable Scope Management**: Ensure proper variable binding across DCG components
2. **Negation Support**: Implement `\+` operator in DCG context
3. **List Integration**: Better integration with Prolog list predicates

### Long Term (Priority 3)
1. **Advanced DCG Features**: Support for pushback, parsing with context
2. **Performance Optimization**: Optimize DCG transformation and execution
3. **Extended Syntax**: Support additional DCG extensions and syntactic sugar

## Test Programs Status

### ✅ PASSED (7/20)
- `test_dcg_01_basic_parsing.pl` - Basic DCG functionality
- `test_dcg_02_variables.pl` - Simple variable binding  
- `test_dcg_03_arithmetic_expressions.pl` - Basic expression parsing
- `test_dcg_04_list_processing.pl` - List pattern matching
- `test_dcg_08_calculator.pl` - Simple calculator DCG
- `test_dcg_11_xml_parser.pl` - Basic XML parsing
- `test_dcg_14_state_machine.pl` - State machine simulation

### ❌ FAILED (13/20)
- `test_dcg_05_string_validation.pl` - Character validation patterns
- `test_dcg_06_json_parser.pl` - JSON parsing with constraints  
- `test_dcg_07_context_free_grammar.pl` - Complex grammar rules
- `test_dcg_09_balanced_parentheses.pl` - Bracket validation
- `test_dcg_10_csv_parser.pl` - CSV field parsing
- `test_dcg_12_regular_expressions.pl` - Regex pattern matching
- `test_dcg_13_prolog_parser.pl` - Prolog term parsing
- `test_dcg_15_lexer.pl` - Lexical analysis
- `test_dcg_16_date_parser.pl` - Date format parsing
- `test_dcg_17_sql_parser.pl` - SQL statement parsing
- `test_dcg_18_configuration_parser.pl` - Config file parsing
- `test_dcg_19_protocol_parser.pl` - Network protocol parsing
- `test_dcg_20_compiler_frontend.pl` - Complete language parsing

## Next Steps

1. **Create issues in tracking system** for each identified problem
2. **Prioritize DCG parser improvements** in development roadmap
3. **Create simplified versions** of failed tests as interim solutions
4. **Develop DCG feature roadmap** for comprehensive support

This analysis provides a clear path forward for improving JProlog's DCG capabilities to support advanced parsing and language processing tasks.