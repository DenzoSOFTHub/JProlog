# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**JProlog** is a complete Prolog implementation in Java featuring a core Prolog engine, comprehensive built-in predicates, and an integrated development environment (IDE). The project provides both a command-line interface (CLI) and a full-featured GUI IDE with syntax highlighting, debugging capabilities, and project management.

## Build & Development Commands

### Essential Commands
```bash
# Compile the project
mvn compile

# Run tests
mvn test

# Launch the GUI IDE
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
# or use the convenience script:
./start-ide.sh

# Run the CLI interface
java -cp target/classes it.denzosoft.jprolog.PrologCLI

# Alternative Maven execution
mvn exec:java -Dexec.mainClass="it.denzosoft.jprolog.editor.PrologIDE"
mvn exec:java -Dexec.mainClass="it.denzosoft.jprolog.PrologCLI"
```

### Testing Scripts
```bash
# Comprehensive built-in predicates testing
./comprehensive_test.sh

# Test all 40 example programs (REQUIRED after any build)
./test_all_examples.sh

# Simple interactive testing
./simple_test.sh

# Debug features testing
./test-debug.sh

# Phase 1 ISO features testing
./test_phase1_features.sh

# Phase 2 exception handling testing
./test_phase2_features.sh

# IDE console testing
./test-ide-console.sh

# Terminal interface testing
./test-terminal.sh
```

### Utility Scripts
```bash
# Fix import statements across the project
./update_imports.sh

# Fix compilation errors automatically
./fix_compilation_errors.sh

# Clean syntax errors
./clean_syntax_errors.sh

# Fix built-in imports
./fix_builtin_imports.sh
```

## Code Architecture & Key Components

### Core Package Structure

**Primary Engine Components:**
- `it.denzosoft.jprolog.core.engine` - Core Prolog execution engine
  - `PrologEngine` - Main engine interface
  - `Prolog` - Primary implementation class  
  - `SimplePrologEngine` - Alternative lightweight engine
  - `QuerySolver` - Query resolution and backtracking
  - `KnowledgeBase` - Facts and rules storage
  - `BuiltIn`, `BuiltInWithContext` - Built-in predicate interfaces
  - `Clause`, `Rule`, `Predicate` - Knowledge representation
  - `ArithmeticEvaluator` - Arithmetic expression evaluation
  - `BuiltInRegistry`, `BuiltInFactory` - Built-in management

**Term System:**
- `it.denzosoft.jprolog.core.terms` - Prolog term representation
  - `Term` - Base term interface
  - `Atom`, `Number`, `Variable`, `CompoundTerm` - Concrete term types
  - `PrologString` - String term representation
- `it.denzosoft.jprolog.core.utils` - Term utilities
  - `ListTerm` - Prolog list representation  
  - `Substitution` - Variable substitution handling
  - `CollectionUtils` - Collection manipulation

**Parser & Language Processing:**
- `it.denzosoft.jprolog.core.parser` - Prolog syntax parsing
  - `Parser` - Main parser class
  - `PrologParser` - Core Prolog syntax parser
  - `TermParser` - Individual term parsing

**Built-in Predicates (Comprehensive):**
- `it.denzosoft.jprolog.builtin` - All built-in predicates organized by category:
  - `arithmetic/` - Arithmetic operations and comparisons
  - `atom/` - Atom manipulation predicates  
  - `character/` - Character I/O and manipulation
  - `control/` - Control structures (cut, if-then-else, etc.)
  - `conversion/` - Type conversion predicates
  - `database/` - Dynamic database manipulation (assert/retract)
  - `dcg/` - Definite Clause Grammar support
  - `debug/` - Debugging predicates
  - `exception/` - Exception handling
  - `io/` - Input/output predicates
  - `list/` - List processing predicates
  - `meta/` - Meta-predicates (call, forall, etc.)
  - `string/` - String manipulation predicates
  - `system/` - System predicates and flags
  - `term/` - Term manipulation predicates
  - `type/` - Type checking predicates
  - `unification/` - Unification predicates

**IDE Components:**
- `it.denzosoft.jprolog.editor` - Full-featured IDE
  - `PrologIDE` - Main IDE window
  - `FileEditor` - Text editor with syntax highlighting
  - `DebugPanel` - Interactive debugger
  - `ConsolePanel` - Query execution console

**CLI Interface:**
- `it.denzosoft.jprolog.PrologCLI` - Interactive command-line interface with:
  - Query execution with `:- query.` syntax
  - File consultation with `:consult filename.pl`
  - Built-in commands (`:help`, `:quit`, `:trace`, etc.)
  - History navigation and session logging

### Critical Engine Components

**Query Processing Flow:**
1. `Parser` - Converts Prolog text to Term objects
2. `QuerySolver` - Resolves queries against the knowledge base
3. `KnowledgeBase` - Stores and retrieves facts/rules
4. `BuiltIn` implementations - Handle built-in predicates
5. `Variable.unify()` - Core unification algorithm

**DCG (Definite Clause Grammar) Support:**
- `it.denzosoft.jprolog.core.dcg.DCGTransformer` - Transforms DCG rules to standard Prolog
- `it.denzosoft.jprolog.util.TermCopier` - Handles variable renaming for rule copying
- `it.denzosoft.jprolog.builtin.dcg.Phrase` - phrase/2 predicate implementation
- Full support for phrase/2 parsing

**Exception Handling System (Phase 2 ISO Compliance):**
- `it.denzosoft.jprolog.builtin.exception.ISOErrorTerms` - Factory for ISO standard error terms
- `it.denzosoft.jprolog.builtin.exception.Throw` - throw/1 predicate implementation  
- `it.denzosoft.jprolog.builtin.exception.Catch` - catch/3 predicate implementation
- Complete ISO 13211-1 standard compliance for exception handling
- Error term hierarchy: instantiation_error, type_error, domain_error, evaluation_error, etc.
- Built-in zero divisor protection in arithmetic operations
- Proper exception unification and recovery mechanisms
- Test coverage: ExceptionHandlingTest.java and examples/test_48_exception_handling.pl

**Arithmetic Functions System (Phase 3 ISO Compliance):**
- `it.denzosoft.jprolog.builtin.arithmetic.ISOArithmeticFunctions` - Extended arithmetic functions
- `it.denzosoft.jprolog.core.engine.ArithmeticEvaluator` - Enhanced with 20+ ISO functions
- Complete mathematical function suite: trigonometric, logarithmic, rounding, bitwise
- ISO naming compliance (`ceiling` instead of `ceil`)
- Domain/range error handling for all mathematical functions
- Binary operations: `max/2`, `min/2`, `atan2/2`, `**/2`
- Mathematical constants: `pi`, `e`
- Bitwise operations: `xor/2`, `/\/2`, `\\/2`, `<</2`, `>>/2`
- Test coverage: examples/test_49_arithmetic_functions.pl and test_phase3_features.sh

**Module System:**
- `it.denzosoft.jprolog.core.module` - Module support infrastructure
  - `Module` - Module representation
  - `ModuleManager` - Module management
  - `PredicateSignature` - Predicate identification
  - `ModuleQualifiedCall` - Cross-module calls

**Operator System:**
- `it.denzosoft.jprolog.core.operator` - Operator precedence and parsing
  - `Operator` - Operator definition
  - `OperatorTable` - Operator precedence management

**Key Algorithms:**
- **Unification**: `Variable.unify()` implements Robinson unification
- **Backtracking**: `QuerySolver` manages choice points and backtracking
- **Cut Semantics**: Proper cut (!) implementation in `Cut` built-in
- **Variable Scoping**: Correct variable handling in rule copying
- **Term Copying**: `TermCopier` provides safe variable renaming

## Development Workflow

### Automated Issue & Change Request Management

**CRITICAL PROCEDURE**: Every bug or feature request MUST be documented in tracking files before implementation:

**Issue Tracking (`docs/tracking/track-issues.md`)**:
- Format: `ISS-YYYY-NNNN` (e.g., ISS-2025-0001)
- Status flow: `TO_ANALYZE` → `IN_ANALYSIS` → `IN_PROGRESS` → `RESOLVED` → `CLOSED`
- Always register issues before fixing
- Create separate issues for related problems discovered during analysis

**Limitations Documentation (`docs/tracking/track-limitations.md`)**:
- **MANDATORY**: When an issue is identified, the `docs/tracking/track-limitations.md` file MUST be updated
- Document the limitation with concrete examples of failing Prolog code
- When an issue is resolved (status `RESOLVED`), remove the corresponding entry from `docs/tracking/track-limitations.md`
- Entry format:
  ```markdown
  ## ISS-YYYY-NNNN: [Limitation Title]
  
  **Description**: Brief description of the limitation
  
  **Failing Examples**:
  ```prolog
  % Example 1: Query that fails
  ?- functor(f(a,b), F, A).
  % Expected: F = f, A = 2
  % Actual: No solutions found
  
  % Example 2: Another failure case
  ?- arg(1, f(a,b,c), X).
  % Expected: X = a
  % Actual: No solutions found
  ```
  
  **Workaround** (if available): Alternative method to achieve the same result
  ```

**Built-in Documentation**:
- **docs/references/BUILTIN_PREDICATES_REFERENCE.md**: Comprehensive reference for all built-in predicates organized by functional categories
- **docs/references/BUILTIN_OPERATORS_REFERENCE.md**: Complete reference for operators with precedence rules and examples  
- **MANDATORY**: Every time a built-in predicate or operator is added or modified, both files MUST be updated with English descriptions and explained code examples
- Formato entry:
  ```markdown
  ## predicate_name/arity
  
  **Category**: [arithmetic|type_checking|list_operations|control|etc.]
  **ISO Compliance**: [yes|no|partial]
  
  **Description**: Brief description of what the predicate does
  
  **Syntax**: 
  ```prolog
  predicate_name(+Arg1, ?Arg2, -Result)
  ```
  
  **Arguments**:
  - `+Arg1`: Input argument description
  - `?Arg2`: Input/output argument description  
  - `-Result`: Output argument description
  
  **Examples**:
  ```prolog
  % Example 1: Basic usage
  ?- predicate_name(input, Output).
  Output = result.
  
  % Example 2: Advanced usage
  ?- predicate_name(complex_input, X).
  X = complex_result.
  ```
  
  **Behavior**: Detailed explanation of behavior, edge cases, and failure conditions
  
  **See Also**: Related predicates
  ```

**Change Request Tracking (`docs/tracking/track-change-requests.md`)**:
- Format: `CR-YYYY-NNNN` (e.g., CR-2025-0001)  
- Status flow: `RICHIESTA` → `IN_ANALYSIS` → `APPROVED` → `IN_DEVELOPMENT` → `COMPLETED`
- Include acceptance criteria and impact analysis

### Code Change Tagging

**MANDATORY**: All code modifications must be tagged with issue/CR references:

```java
// START_CHANGE: ISS-2025-0001 - Fix variable unification in DCG rules
private boolean unifyVariables(Variable v1, Variable v2) {
    // Implementation
}
// END_CHANGE: ISS-2025-0001

// For multiple related issues:
// START_CHANGE: ISS-2025-0003 | CR-2025-0002 - Add DCG support with conjunction handling
```

### Testing Requirements

**Built-in Testing**: When modifying built-ins, always run:
```bash
./comprehensive_test.sh  # Tests all built-in predicates
```

**Manual Testing Patterns**:
```prolog
% Test in CLI or IDE console:
?- atom(hello).        % Type checking
?- X is 2 + 3.         % Arithmetic
?- append([a], [b], L). % List operations
?- assertz(test(1)).   % Database operations
```

**Regression Testing**: Always verify existing functionality still works after changes.

## Project-Specific Conventions

### Java Code Standards
- Target Java 1.8 compatibility (configured in pom.xml)
- Package structure: `it.denzosoft.jprolog.*`
- Use descriptive names following Java conventions
- Implement proper exception handling with custom Prolog exceptions
- Current version: 2.0.3 (check pom.xml for updates)

### Prolog Implementation Standards
- ISO Prolog compliance where possible
- Proper cut semantics and backtracking
- Standard operator precedence (managed by OperatorTable)
- Support for both `%` and `/* */` comment styles
- DCG support with automatic transformation
- Module system for namespace management

### IDE Integration
- Always compile project before launching IDE: `mvn compile`
- Use `./start-ide.sh` for standard IDE launch
- Test major changes with both CLI and IDE interfaces
- Session logging in `sessions/` directory

### Maven Integration
```bash
# Standard Maven goals
mvn clean                    # Clean build directory
mvn compile                  # Compile source code
mvn test                     # Run JUnit tests
mvn exec:java               # Execute main class (configured as Main)

# Alternative execution profiles
mvn exec:java -Dexec.mainClass="it.denzosoft.jprolog.editor.PrologIDE"
mvn exec:java -Dexec.mainClass="it.denzosoft.jprolog.PrologCLI"
```

## Architecture Decision Records

### Core Design Decisions

**Term Representation**: Immutable Term objects with proper equals/hashCode for unification
**Built-in Architecture**: Category-based organization with consistent interfaces
**Variable Handling**: Unique variable instances with proper scoping in rule copying
**Parser Design**: Recursive descent parser with proper operator precedence
**IDE Architecture**: Swing-based with event-driven design for responsiveness

### Known Architectural Constraints
- Single-threaded execution model (Prolog semantics)
- No external dependencies beyond standard Java libraries (JUnit for tests only)
- Maven-based build system
- Desktop-only IDE (Swing-based)
- Java 1.8 compatibility requirement

## Example Programs & Testing

### Standard Test Suite (`examples/test_*.pl`)
The `examples/` directory contains 40 comprehensive Prolog test programs covering:

**Basic Features (test_01-test_06)**:
- Basic facts and queries (`test_01_basic_facts.pl`)
- Unification mechanisms (`test_02_unification.pl`) 
- Arithmetic operations (`test_03_arithmetic.pl`)
- List processing (`test_04_lists.pl`)
- Recursive predicates (`test_05_recursion.pl`)
- Cut and control structures (`test_06_cut_control.pl`)

**Standard Predicates (test_07-test_16)**:
- Type checking predicates (`test_07_type_checking.pl`)
- Term manipulation (`test_08_term_manipulation.pl`)
- Meta-predicates (`test_09_meta_predicates.pl`)
- String and atom operations (`test_10_string_atom.pl`)
- Database operations (`test_11_database.pl`)
- Basic I/O (`test_12_io_basic.pl`)
- Exception handling (`test_13_exception.pl`)
- DCG grammars (`test_14_dcg_simple.pl`)
- Operators (`test_15_operators.pl`)
- Sorting predicates (`test_16_sorting.pl`)

**Advanced Features (test_17-test_40)**:
- Constraint solving, higher-order predicates, advanced arithmetic
- Module systems, co-routining, tabling/memoization
- Concurrent programming, advanced I/O, global variables
- Debugging/profiling, attributed variables, probabilistic logic

### Test Execution
```bash
# REQUIRED: Run comprehensive test suite after any build
./test_all_examples.sh

# Success criteria:
# - Success rate >= 75% for maintenance work
# - Success rate >= 85% for new feature development
# - Success rate < 75% blocks the session (investigate failures)
```

### Additional Examples
- `examples/family_tree*.pl` - Family relationship examples
- `examples/nqueens*.pl` - N-Queens problem variants  
- `examples/*calculator*.pl` - Calculator implementations
- `examples/DGC*.pl` - DCG parsing examples

## Final Cleanup and Validation Procedure

**MANDATORY PROCEDURE**: At the end of every work session or significant implementation, always follow this cleanup and validation procedure:

### 1. Temporary Files Cleanup
```bash
# Remove all temporary test and debug files
rm -f *.class
rm -f *Test.java         # Only those in root, NOT under src/test/
rm -f Debug*.java
rm -f temp_*.txt
rm -f *_debug.*
rm -f test_*.txt         # Temporary files, NOT test programs examples/test_*.pl
```

**Files to Keep**:
- `examples/test_*.pl` - Official test programs
- `src/test/**/*.java` - Official unit tests
- `*.sh` - Automation scripts
- `*.md` - Documentation
- `docs/tracking/track-issues.md`, `docs/tracking/track-change-requests.md` - Formal tracking

**Files to Remove**:
- .class files in root
- Temporary Debug*.java files
- Test*.java files in root (not under src/test/)
- temp_*.txt, *_debug.*, test_input.txt, etc.

**Prolog Test Files Organization**:
- All Prolog files (*.pl) used for testing MUST be moved to the `examples/` directory
- Use descriptive names following the pattern: `examples/test_XX_description.pl`
- Remove any temporary *.pl files from the root directory
- Example cleanup:
```bash
# Move test files to examples directory
mv test_*.pl examples/ 2>/dev/null || true
mv *_test.pl examples/ 2>/dev/null || true
# Remove temporary prolog files from root
rm -f temp_*.pl debug_*.pl
```

### 2. Complete Recompilation
```bash
# Always recompile everything before concluding
mvn clean compile
```

### 3. Error Validation
```bash
# Verify there are no compilation errors
mvn compile -q
echo $?  # Must return 0 (success)
```

### 4. Comprehensive Testing on 40 Standard Prolog Programs
**MANDATORY PROCEDURE**: After each build, run comprehensive testing on all 40 test programs to verify that all standard functionalities operate correctly.

```bash
# Execute comprehensive testing on all Prolog example programs
./test_all_examples.sh

# Expected output: Summary with success percentage >= 75%
# If success rate < 75%, investigate failures before proceeding
```

**Programs Tested** (examples/test_*.pl):
- Basic facts, arithmetic, lists, recursion  
- Control structures, type checking, term manipulation
- Meta-predicates, database operations, I/O
- DCG grammars, exception handling, string/atom operations
- Advanced features, modules, performance tests

**Acceptance Criteria**:
- **Success Rate >= 75%**: Acceptable for maintenance sessions  
- **Success Rate >= 85%**: Required for new feature development sessions
- **Success Rate < 75%**: BLOCKS the session - identify and resolve regressions

**Failure Investigation**:
```bash
# To investigate individual failures:
java -cp target/classes it.denzosoft.jprolog.PrologCLI
:consult examples/test_XX_name.pl
[test queries manually]

# Debug logging for specific problems:
./test-debug.sh
```

**Notes**:
- Failures for **Parser Limitations** (advanced ISO syntax not supported) are acceptable
- Failures for **Missing Advanced Features** (e.g., constraint programming) are acceptable  
- Failures for **Core Features** (arithmetic, lists, control structures) are NOT acceptable

### 5. Smoke Test
```bash
# Quick test to verify basic system functionality
echo ":quit" | timeout 5 java -cp target/classes it.denzosoft.jprolog.PrologCLI
```

### 6. Version and Documentation Update
**MANDATORY PROCEDURE**: At the end of every code modification that compiles successfully AND has passed comprehensive tests:

#### Complete Versioning Sequence:
```bash
# 1. Check current version
grep "<version>" pom.xml | head -1

# 2. Complete Maven build
mvn clean compile

# 3. Maven tests (if present)
mvn test

# 4. Comprehensive test on 40 programs (MANDATORY)
./test_all_examples.sh
# Verify that success rate >= 75%

# 5. ONLY if all tests pass, increment patch version
# Example: if it was 2.0.3, change to 2.0.4 in pom.xml
```

#### Documentation Update (MANDATORY)
**After every version increment**, update immediately:

1. **File `docs/tracking/track-issues.md`**:
   - Update status of resolved issues from `IN_PROGRESS` → `RESOLVED`
   - Add resolution date: `**Resolution Date**: YYYY-MM-DD`
   - Document implemented solution in `#### Implemented Solution` section
   - Add references to modified files
   - Update test results and validation

2. **File `docs/tracking/track-change-requests.md`** (if applicable):
   - Update CR status from `IN_DEVELOPMENT` → `COMPLETED`
   - Document implementation and satisfied acceptance criteria
   - Update impact analysis with actual results

3. **File `docs/tracking/track-limitations.md`**:
   - **REMOVE** entries for resolved issues
   - Update workarounds if no longer needed
   - Document new limitations discovered during implementation

4. **Complete Documentation Review** (MANDATORY):
   - Verify that all file references follow the naming convention
   - **IMPORTANT**: Ensure that ALL documentation is in English
   - Update metrics and statistics in report files
   - Verify consistency of information between related documents
   - Update dates and versions in technical documents
   - Ensure files are in the correct directory structure

#### Documentation Structure and Naming Convention (MANDATORY)

**Documentation Naming Rules**: All documentation files must follow these strict naming conventions:

**File Categories and Prefixes**:
- **System Files** (uppercase, root only): `README.md`, `CHANGELOG.md`, `CLAUDE.md`
- **User Guides** (guide-): `docs/guides/guide-[descriptive-name].md`
- **Technical References** (ref-): `docs/references/ref-[descriptive-name].md`  
- **Reports and Analysis** (report-): `docs/reports/report-[descriptive-name].md`
- **Issue Tracking** (track-): `docs/tracking/track-[descriptive-name].md`
- **Examples** (example-): `examples/example-[descriptive-name].md`

**Naming Rules** (STRICT COMPLIANCE REQUIRED):
1. **Format**: `[category-]descriptive-name.md` (lowercase with hyphens)
2. **Characters**: Only lowercase letters, numbers, hyphens (NO underscores, NO spaces)
3. **Length**: Maximum 30 characters for filename
4. **Descriptive**: Name must describe content, not be generic
5. **No Versioning**: Version info goes in file content, not filename
6. **Language**: All documentation MUST be in English

**Directory Structure**:
```
/workspace/JProlog/
├── README.md                    # Project overview
├── CHANGELOG.md                 # Version change log  
├── CLAUDE.md                    # AI assistant instructions
├── docs/                        # All documentation
│   ├── guides/                  # User guides
│   │   ├── guide-quick-start.md
│   │   ├── guide-user-manual.md
│   │   ├── guide-cli-usage.md
│   │   ├── guide-ide-usage.md
│   │   ├── guide-prolog-intro.md
│   │   ├── guide-java-integration.md
│   │   ├── guide-debugging.md
│   │   └── guide-extension.md
│   ├── references/              # Technical references
│   │   ├── ref-builtins.md
│   │   ├── ref-iso-compliance.md
│   │   ├── ref-dcg-grammar.md
│   │   └── ref-limitations.md
│   ├── reports/                 # Analysis reports
│   │   ├── report-test-results.md
│   │   ├── report-package-reorg.md
│   │   └── report-resolution-summary.md
│   └── tracking/                # Project tracking
│       ├── track-issues.md
│       ├── track-change-requests.md
│       └── track-release-notes.md
└── examples/                    # Example files
    └── example-bug-workflow.md
```

**Special Cases**:
- **Built-in References**: `BUILTIN_PREDICATES_REFERENCE.md` and `BUILTIN_OPERATORS_REFERENCE.md` have been moved to `docs/references/` following naming conventions
- **Legacy Files**: Existing uppercase documentation files have been migrated to follow naming convention and moved to appropriate directories
- **Cross-References**: All internal documentation links use relative paths and are updated when files are moved

**Documentation Language** (MANDATORY):
- **ALL documentation files MUST be in English**
- This includes: guides, references, reports, tracking, examples, README, CHANGELOG
- English documentation ensures international accessibility of the project

**Key Files for Each Release**:
- `docs/tracking/track-issues.md` - Issue status and resolution
- `docs/tracking/track-change-requests.md` - Change request tracking
- `docs/tracking/track-release-notes.md` - Release notes
- `docs/tracking/track-limitations.md` - Current limitations
- `docs/references/ref-builtins.md` - Predicate documentation
- `docs/reports/report-test-results.md` - Complete test results

#### Versioning Criteria
**IMPORTANT**: The version is incremented ONLY if:
- Code compiles without errors (`mvn compile` success)
- Maven tests pass (`mvn test` success, if present)
- Comprehensive test passes with success rate >= 75% (`./test_all_examples.sh`)
- No critical regressions identified
- **Documentation updated** (docs/tracking/track-issues.md, docs/tracking/track-change-requests.md, docs/tracking/track-limitations.md)

### 7. GitHub Push and Tagging (AFTER Version Update)
**MANDATORY PROCEDURE**: After incrementing the version and updating documentation:

#### Pre-Push Cleanup and Validation
```bash
# 1. Remove ALL temporary test files from root
rm -f Test*.java
rm -f Debug*.java
rm -f temp_*.txt
rm -f test_*.txt  # Temporary files, NOT test programs examples/test_*.pl
rm -f *.class
rm -f *_debug.*

# 2. Update .gitignore to ensure unwanted files are excluded
echo "*.sh" >> .gitignore
echo "Test*.java" >> .gitignore  
echo "Debug*.java" >> .gitignore
echo "temp_*.txt" >> .gitignore
echo "*.class" >> .gitignore
echo "*_debug.*" >> .gitignore

# 3. CRITICAL: Verify that sensitive files are NOT tracked or staged
git status --ignored
if git ls-files | grep -E "(token|key|secret|credential)" >/dev/null; then
    echo "❌ ERROR: Sensitive files detected in repository!"
    echo "Remove sensitive files before proceeding."
    exit 1
fi

# 4. Verify only intended files will be included in the tag
echo "Files to be included in tag:"
git ls-files | sort
echo ""
echo "Untracked files (should be empty or only development files):"
git status --porcelain | grep "^??" || echo "None"
```

#### Pre-Push Preparation (MANDATORY)
**CRITICAL**: Before any push to GitHub, execute the complete preparation procedure to ensure code quality, documentation alignment, and system integrity.

**This procedure includes**:
1. **Complete file cleanup** (test files, debug files, temporary files)
2. **Prolog test file organization** (move all *.pl files to examples/)
3. **Documentation review and alignment** (verify all file paths and procedures)
4. **Release notes update** (mandatory before any release)
5. **Limitations tracking update** (review and update current system limitations)
6. **Comprehensive testing** (verify system integrity)
7. **Final compilation check** (ensure no build errors)

```bash
# 1. CLEANUP: Remove all test and debug files
rm -f *.class
rm -f *Test.java         # Only in root, NOT src/test/
rm -f Debug*.java
rm -f temp_*.txt
rm -f *_debug.*
rm -f test_input.txt

# Move test files to examples directory
mv test_*.pl examples/ 2>/dev/null || true
mv *_test.pl examples/ 2>/dev/null || true
# Remove temporary prolog files from root
rm -f temp_*.pl debug_*.pl

# 2. DOCUMENTATION REVIEW: Ensure all documentation is aligned and up-to-date
echo "📚 Reviewing documentation alignment..."

# Check documentation structure
ls -la docs/guides/ docs/references/ docs/reports/ docs/tracking/ examples/

# Verify README.md references correct file paths
echo "✅ Verify README.md file paths are updated"

# CRITICAL: Verify tracking documentation is current
echo "📋 Verifying tracking documentation completeness:"
echo "   - docs/tracking/track-issues.md (issue status tracking)"
echo "   - docs/tracking/track-change-requests.md (CR status tracking)"  
echo "   - docs/tracking/track-release-notes.md (release documentation)"
echo "   - docs/tracking/track-limitations.md (current system limitations)"

# Ensure CLAUDE.md reflects current procedures
echo "✅ Verify CLAUDE.md procedures are current"

# 3. UPDATE RELEASE NOTES: Mandatory before any release
CURRENT_VERSION=$(grep '<version>' pom.xml | head -1 | sed 's/.*<version>\(.*\)<\/version>.*/\1/')
RELEASE_DATE=$(date +"%Y-%m-%d")

# Update docs/tracking/track-release-notes.md with version details
echo "📝 Updating release notes for version ${CURRENT_VERSION}..."
echo "## Release ${CURRENT_VERSION} - ${RELEASE_DATE}" >> docs/tracking/track-release-notes.md
echo "### Changes in this release:" >> docs/tracking/track-release-notes.md
echo "- [Add release details here]" >> docs/tracking/track-release-notes.md
echo "" >> docs/tracking/track-release-notes.md

echo "⚠️  MANUAL ACTION REQUIRED: Update release notes in docs/tracking/track-release-notes.md"

# 3b. UPDATE LIMITATIONS: Mandatory review for every release
echo "📋 Reviewing and updating limitations tracking..."
echo "⚠️  MANUAL ACTION REQUIRED: Review docs/tracking/track-limitations.md and:"
echo "   - Remove entries for issues resolved in this release"
echo "   - Update workarounds that are no longer needed"
echo "   - Add any new limitations discovered during development"
echo "   - Verify all examples still reflect current behavior"

# 4. COMPREHENSIVE TESTING: Verify system integrity
echo "🧪 Running comprehensive tests..."
./test_all_examples.sh
if [ $? -ne 0 ]; then
    echo "❌ Tests failed! Fix issues before pushing."
    exit 1
fi

# 5. FINAL COMPILATION: Ensure everything compiles
mvn clean compile -q
if [ $? -ne 0 ]; then
    echo "❌ Compilation failed! Fix issues before pushing."
    exit 1
fi
```

#### Push and Tagging Sequence
```bash
# 6. Configure GitHub credentials (SECURITY: NEVER hardcode tokens in files)
# CRITICAL: Credentials MUST be set as environment variables OUTSIDE of any tracked files
export GITHUB_USER="DenzoSOFTHub"
export GITHUB_REPO="https://github.com/DenzoSOFTHub/JProlog"

# ⚠️ SECURITY WARNING: The token MUST be provided via environment variable
# NEVER hardcode tokens in scripts, files, or commit them to repository
if [ -z "$GITHUB_TOKEN" ]; then
    echo "❌ ERROR: GITHUB_TOKEN environment variable not set!"
    echo "Set it with: export GITHUB_TOKEN='your_personal_access_token'"
    echo "NEVER include tokens in tracked files or commits!"
    exit 1
fi

# Verify token format (basic validation)
if [[ ! "$GITHUB_TOKEN" =~ ^(ghp_|github_pat_) ]]; then
    echo "⚠️ WARNING: Token format may be incorrect. Expected format: ghp_* or github_pat_*"
fi

# 7. Check Git status and prepare commit
git status
git add .
git add -u  # Include deleted files

# 8. Verify Release Notes are Updated (already done in Pre-Push Preparation)
echo "✅ Release notes already updated in docs/tracking/track-release-notes.md"

# 9. Create commit with structured message
git commit -m "Release v${CURRENT_VERSION}

- Resolved critical issues and enhanced functionality
- Updated documentation and version bump
- Comprehensive tests passed (success rate >= 75%)
- Updated docs/tracking/track-release-notes.md for version ${CURRENT_VERSION}

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"

# 10. Push to remote repository
git push https://${GITHUB_USER}:${GITHUB_TOKEN}@github.com/DenzoSOFTHub/JProlog.git main

# 11. Create and push version tag
git tag -a "v${CURRENT_VERSION}" -m "Release version ${CURRENT_VERSION}

Features and fixes included in this release:
- Enhanced list representation with ISO-compliant formatting
- Meta-predicates (findall/3, bagof/3, setof/3) verified functional
- DCG (Definite Clause Grammar) system fully operational
- Comprehensive testing passed with high success rate

🤖 Generated with [Claude Code](https://claude.ai/code)"

git push https://${GITHUB_USER}:${GITHUB_TOKEN}@github.com/DenzoSOFTHub/JProlog.git "v${CURRENT_VERSION}"

# 12. Post-Push Validation (MANDATORY)
echo "Validating pushed content..."

# Verify tag contains only intended files
echo "Files in the new tag:"
git ls-tree -r "v${CURRENT_VERSION}" --name-only | sort

# Check for any sensitive or unwanted files in the tag
SENSITIVE_FILES=$(git ls-tree -r "v${CURRENT_VERSION}" --name-only | grep -E "(token|key|secret|credential|\.env|config\.properties)" || true)
if [ -n "$SENSITIVE_FILES" ]; then
    echo "❌ CRITICAL ERROR: Sensitive files found in tag!"
    echo "$SENSITIVE_FILES"
    echo "Delete tag and fix before proceeding: git tag -d v${CURRENT_VERSION} && git push --delete origin v${CURRENT_VERSION}"
    exit 1
fi

# Verify no temporary or debug files made it into the tag
TEMP_FILES=$(git ls-tree -r "v${CURRENT_VERSION}" --name-only | grep -E "(Test.*\.java|Debug.*\.java|temp_.*|.*_debug\.|\.class$)" || true)
if [ -n "$TEMP_FILES" ]; then
    echo "⚠️ WARNING: Temporary files found in tag!"
    echo "$TEMP_FILES"
    echo "Consider cleaning and re-tagging"
fi

echo "✅ Tag validation completed"
```

#### Importance of Release Notes
**⚠️ MANDATORY PROCEDURE**: The `docs/tracking/track-release-notes.md` file MUST be updated before every release for:

1. **Traceability**: Maintains complete history of all releases
2. **Communication**: Provides clear information to users about changes
3. **Marketing**: Highlights improvements and project evolution  
4. **Support**: Helps in debugging and resolving version-specific issues
5. **Compliance**: Standard practice for professional projects

**Standard Release Notes Format**:
- **Release Date**: Always in YYYY-MM-DD format
- **Structured Sections**: Major Enhancements, Technical Fixes, Quality Metrics, Impact, Documentation
- **Concrete Metrics**: Success rates, improvement percentages, specific numbers
- **Emoji Coding**: To improve readability and visual impact

**Note**: The file is overwritten with each release (do not append), keeping only the latest release for simplicity.

#### Post-Push Verification
```bash
# 13. Verify push and tag on GitHub
echo "✅ Push completed for version: ${CURRENT_VERSION}"
echo "🔗 Repository: ${GITHUB_REPO}"
echo "🏷️  Tag: v${CURRENT_VERSION}"
echo ""
echo "Verify on GitHub:"
echo "- Repository: ${GITHUB_REPO}"
echo "- Releases: ${GITHUB_REPO}/releases"
echo "- Tag: ${GITHUB_REPO}/releases/tag/v${CURRENT_VERSION}"
```

#### Security and Best Practices
**⚠️ CRITICAL - Credential Management**:
- **NEVER** commit access tokens to source code
- **NEVER** include credentials in tracked files, scripts, or documentation
- Use environment variables for sensitive credentials ONLY
- Consider using SSH keys instead of HTTPS with tokens for greater security
- **MANDATORY**: Always validate that no sensitive files are included in tags or commits
- Remove any hardcoded credentials immediately if found in repository

**Security Validation Requirements**:
- Pre-push validation MUST confirm no sensitive files are tracked
- Post-push validation MUST verify tag contains only intended files  
- Any tag containing sensitive information MUST be deleted immediately
- Use `git filter-branch` or `git filter-repo` to remove credentials from history if needed

**Repository Target**:
- **URL**: https://github.com/DenzoSOFTHub/JProlog  
- **User**: DenzoSOFTHub
- **Branch**: main (default)
- **Tag Format**: v{major}.{minor}.{patch} (e.g., v2.0.4)

#### Versioning Rationale:
- **Major version (x.0.0)**: Architectural changes or breaking changes (user request)
- **Minor version (x.y.0)**: Significant new features (user request)
- **Patch version (x.y.z)**: Bug fixes and improvements (automatic after each modification)

#### Issue Update Template (example):
```markdown
#### Implemented Solution
✅ **COMPLETED**: [Brief description of solution]

**Modified Files**:
- `path/file1.java` - [modification description]
- `path/file2.java` - [modification description]

**Test Results**:
- ✅ Maven build: SUCCESS
- ✅ Comprehensive test: XX/40 programs (XX% success rate)
- ✅ Target functionality: [Specific test results]

**Version**: Incremented from X.Y.Z-1 → X.Y.Z
```

**Rationale**: Keeping the workspace clean avoids:
- Confusion between temporary files and permanent code
- Accumulation of obsolete debug files
- Compilation errors from corrupted temporary files
- Difficulties in tracking real modifications

## Common Development Scenarios

### Adding New Built-in Predicates
1. Create class in appropriate `builtin/*` category package
2. Implement `BuiltIn` or `BuiltInWithContext` interface  
3. Register in `BuiltInRegistry` (through BuiltInFactory registration)
4. Add comprehensive test cases to `comprehensive_test.sh`
5. Update docs/references/BUILTIN_PREDICATES_REFERENCE.md and docs/references/BUILTIN_OPERATORS_REFERENCE.md documentation (MANDATORY)
6. Run `./test_all_examples.sh` to ensure no regressions

### Debugging Query Resolution Issues
1. Use `./test-debug.sh` to test debug features
2. Enable trace mode in IDE or CLI with `:trace` command
3. Check variable bindings and unification steps
4. Verify clause matching in `QuerySolver`
5. Use debug predicates: `spy/1`, `trace/0`, `notrace/0`

### Parser Extensions
1. Modify `Parser` or `PrologParser` for syntax changes
2. Update `TermParser` for new term types
3. Update `OperatorTable` for new operators
4. Ensure operator precedence is maintained
5. Test with complex expressions and existing examples

### IDE Component Development
1. All IDE components extend from appropriate Swing base classes
2. Use event-driven architecture with proper listeners
3. Update `PrologIDE` main window for new panels
4. Test with both keyboard shortcuts and menu actions
5. Ensure proper integration with existing debug/build/run systems

### Error Handling Best Practices
1. Use custom Prolog exceptions from `core.exceptions` package
2. Provide meaningful error messages with term context
3. Handle ISO standard error terms when applicable
4. Log errors appropriately for debugging
5. Ensure graceful degradation in IDE components

---

*This document should be updated after significant architectural changes or new feature implementations.*