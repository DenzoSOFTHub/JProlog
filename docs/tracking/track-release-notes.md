# JProlog - Release Notes

## Release 2.3.0 - 2026-03-19

### 100% ISO 13211-1 Compliance & 25+ New Predicates

This release achieves full ISO Prolog compliance and adds comprehensive higher-order, list utility, term I/O, and conversion predicates.

### New Predicates (25+)

**Higher-order list predicates** (meta-predicates with QuerySolver access):
- `maplist/2,3,4` — apply goal to list elements
- `include/3`, `exclude/3` — filter lists by goal success/failure
- `foldl/4,5,6` — left fold with accumulator

**List utility predicates**:
- `last/2`, `flatten/2`, `numlist/3`
- `sum_list/2` / `sumlist/2`, `max_list/2`, `min_list/2`
- `delete/3`, `subtract/3`, `intersection/3`, `union/3`

**Term I/O predicates**:
- `term_to_atom/2` — bidirectional term/atom conversion
- `numbervars/3` — number variables with `$VAR(N)`
- `tab/1` — output N spaces
- `with_output_to/2` — capture output as atom

**Conversion predicates**:
- `string_to_atom/2`, `number_to_atom/2`, `atom_to_number/2`
- `string_code/3` — character code at index

### Module System (CR-0002 completed)
- Module-qualified calls `Module:Goal` via `solveInModuleContext`
- Module-isolated rule storage for non-user modules
- Import resolution: current module → global KB → imported modules

### Bug Fixes
- `atom_concat/3` modes (+,-,+) and (-,+,+) for suffix/prefix extraction
- `float/1` arithmetic function (ISO convert to float)
- 6 stale issues closed by triage

### Quality Metrics
- **100% ISO 13211-1 compliance** (111/111 core predicates)
- **320 unit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **0 active issues, 0 active limitations**
- **9/10 Change Requests completed**

### Repository Information
- **Tag**: v2.3.0
- **Release Date**: 2026-03-19
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 2.2.0 - 2026-03-18

### Parser Hardening & Binary Compiled Format

This release delivers a completely rewritten parser, a new binary compiled format (.jpc) for faster program loading, and fixes for 6+ previously failing tests.

### New Features
- **Binary Compiled Format (.jpc)**: Serialize Prolog programs to compact binary with string interning, varint encoding, and source hash validation. Load programs 5-10x faster on subsequent consults.
- **Smart Consult**: `consultSmart(file)` auto-compiles to `.jpc` on first load and uses the cache on subsequent loads, with hash-based invalidation.
- **CLI Commands**: `:compile <file>` compiles `.pl` to `.jpc`; `:consult_compiled <file>` loads `.jpc` directly.
- **Module-qualified calls**: `Module:Goal` syntax now works (dispatched by QuerySolver).
- **call/1 through call/8**: Higher-order `call/N` now recognized at all arities.

### Parser Improvements
- **Unified Operator Table**: Replaced three disconnected operator registries with single shared `OperatorTable` instance, eliminating custom operator parsing failures.
- **Incremental Directive Processing**: `op/3` directives take effect immediately during `consult()` and `asserta()`, before subsequent clauses are parsed.
- **Pratt Parser**: Proper operator-precedence parsing with correct XFX/XFY/YFX associativity handling.
- **ISO Operator Removal**: Precedence 0 in `op/3` removes the operator per ISO standard.

### Bug Fixes
- Custom operators (e.g., `means`) now work correctly after `:- op(...)` directives
- `statistics/2` now returns solutions properly
- `=..` operator tokenization fixed
- Negative number vs prefix minus disambiguation
- Quoted atom parsing in new parser

### DCG Enhancements (CR-0003 completed)
- All 3 DCG parser limitations resolved (ISS-0040, 0041, 0042) — compound operators in list heads, special characters in terminals, complex arithmetic in constraints all now work
- `phrase/2`, `phrase/3` confirmed working with disjunction, constraint goals, and complex grammars

### New I/O Predicates (CR-0005 completed)
- `at_end_of_stream/0`, `at_end_of_stream/1` — end-of-stream testing
- `get_byte/1`, `get_byte/2`, `put_byte/1`, `put_byte/2` — binary I/O
- `peek_byte/1`, `peek_byte/2` — non-consuming byte lookahead
- `write_canonical/1`, `write_canonical/2` — canonical term output
- `char_conversion/2`, `current_char_conversion/2` — character conversion table

### Module System (CR-0002 completed)
- `module/2` directive for module declarations with export lists
- `use_module/1` for importing modules
- Module-qualified calls `Module:Goal` via `solveInModuleContext`
- Module-isolated rule storage: non-user module rules stored in module's `localRules`, user module rules in global `KnowledgeBase`
- Unqualified call resolution: current module context → global KB → imported modules
- Full backward compatibility maintained

### List Operations (CR-0008 completed)
- `permutation/2` — generates list permutations
- `predsort/3` — sort with user-defined comparison predicate (merge sort, BuiltInWithContext)

### Issue Triage (10 issues closed)
- ISS-0040 through ISS-0049 resolved or confirmed already implemented
- `unify_with_occurs_check/2`, `clause/2`, `current_op/3` confirmed existing

### Quality Metrics
- **320 unit tests, 0 failures** (up from 311 with 6 failures)
- **20/20 example programs pass** (100%, up from 95%)
- **94/94 MegaPredicateTest** passes
- **0 active limitations** (down from 3)
- **9/10 Change Requests completed** (only CR-0009 Debugging Port Model remains)

### Repository Information
- **Tag**: v2.2.0
- **Release Date**: 2026-03-18
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 2.0.6 - 2025-08-20

### 🚀 Major Enhancements
- Fixed critical StackOverflowError in Variable.resolveBindings() with cycle detection
- Enhanced string handling in DCG parsing with string_codes/2 predicate  
- Comprehensive documentation reorganization with structured naming conventions
- Improved system stability and DCG processing reliability

### 🔧 Technical Fixes
- Fixed Variable.resolveBindings circular reference causing system crashes (ISS-2025-0008)
- Implemented string_codes/2 predicate for PrologString support (ISS-2025-0006)
- Enhanced to_codes/2 to handle both Atom and PrologString types
- Resolved StackOverflowError in Variable.occurs() method (ISS-2025-0012)
- Updated phrase/2 predicate for better DCG variable handling

### 📊 Quality Metrics
- Comprehensive tests: 19/20 programs passed (95% success rate)
- No regressions introduced by stability fixes
- System now processes DCG rules without crashes
- Enhanced error handling and variable resolution

### 🎯 Impact
- Eliminated critical system crashes in Variable processing
- Improved DCG system stability and reliability
- Better string/atom type handling in grammar parsing
- Structured documentation for improved maintainability

### 📝 Documentation Updates
- Enhanced CLAUDE.md with comprehensive pre-push preparation procedures
- Reorganized documentation with categorical naming conventions (docs/guides/, docs/references/, etc.)
- Updated all file references to new structured paths

---

## Release v2.0.5 - 2025-08-20

### 🚀 Major Enhancements
- Enhanced list representation with ISO-compliant formatting [a,b,c] instead of .(a, .(b, .(c, [])))
- Meta-predicates (findall/3, bagof/3, setof/3) verified fully functional
- Term manipulation predicates (functor/3, arg/3, =../2, copy_term/2) working correctly
- Advanced arithmetic operators (=:=, =\=, rem, xor, shift operators) operational
- Control structures (;, ->, \+, once/1) fully functional
- DCG (Definite Clause Grammar) system fully operational with phrase/2

### 🔧 Technical Fixes
- Fixed copy_term/2 predicate registration in BuiltInRegistry (ISS-2025-0025)
- Resolved list format issues for improved ISO compliance (ISS-2025-0019)
- Enhanced CompoundTerm.toString() with proper list formatting
- Updated comprehensive documentation and issue tracking

### 📊 Quality Metrics
- Comprehensive tests passed with 95% success rate (19/20 programs)
- ISO Prolog compliance significantly improved from 47.6% to 95%
- Built-in coverage increased from ~50% to ~90%
- Parser support enhanced from ~60% to ~85%

### 🎯 Impact
- Dramatically improved ISO Prolog standard compliance
- Enhanced developer experience with better list representation
- Robust meta-programming capabilities now available
- Comprehensive term manipulation for advanced Prolog programming

### 📝 Documentation Updates
- Updated README.md with complete project overview
- Enhanced CLAUDE.md with release procedures and mandatory release notes
- Updated issues.md with resolved issues and quality metrics
- Created comprehensive built-in predicates reference

### 🔗 Repository Information
- **Repository**: https://github.com/DenzoSOFTHub/JProlog
- **Tag**: v2.0.5
- **Release Date**: 2025-08-20
- **Compatibility**: Java 8+, Maven 3.6+

---

*For previous releases and detailed changelogs, see CHANGELOG.md*