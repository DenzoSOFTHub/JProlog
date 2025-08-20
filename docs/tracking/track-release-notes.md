# JProlog - Release Notes

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