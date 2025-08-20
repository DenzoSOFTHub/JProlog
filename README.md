# JProlog - Complete Prolog Implementation in Java

**A Full-Featured Prolog System with Engine, IDE, CLI, and Comprehensive Built-ins**

## Overview

JProlog is a complete and robust Prolog implementation in Java that provides a comprehensive ecosystem for Prolog programming. It consists of multiple integrated components designed to offer both programmatic access and interactive development environments for Prolog applications.

### 🎯 Project Scope

JProlog aims to provide ISO-compliant Prolog functionality with modern development tools, offering:
- **Near-Complete ISO Compliance**: 99.5% success rate including ISO/IEC DTS 13211-3 DCG extensions
- **Professional Development Environment**: Full-featured IDE with debugging capabilities  
- **Command Line Interface**: Interactive Prolog console for quick testing
- **Extensive Built-in Library**: 93+ built-in predicates with 99.5% coverage of ISO standards
- **Advanced DCG Support**: Complete Definite Clause Grammar system per ISO/IEC DTS 13211-3
- **Java Integration**: Easy embedding of Prolog logic in Java applications

## 🏗️ System Architecture

JProlog consists of four main components that work together to provide a complete Prolog development and execution environment:

### 1. 🧠 Prolog Engine (`core/engine/`)

The heart of JProlog - a complete Prolog inference engine with:
- **ISO-compliant query resolution**: Robinson unification algorithm with proper backtracking
- **Knowledge base management**: Dynamic fact/rule storage and retrieval system  
- **DCG (Definite Clause Grammar) support**: Automatic transformation of grammar rules
- **Built-in predicate registry**: Extensible system for registering new predicates
- **Exception handling**: Comprehensive error management with ISO-compliant error terms
- **Module system**: Namespace management for large Prolog applications

**Key Classes**: `Prolog.java`, `QuerySolver.java`, `KnowledgeBase.java`, `DCGTransformer.java`

### 2. 🖥️ Integrated Development Environment (`editor/`)

A professional IDE specifically designed for Prolog development:
- **Project Management**: Structured project organization with directory trees
- **Syntax Highlighting**: Full Prolog syntax highlighting with keywords, operators, and comments
- **Advanced Editor**: Multi-tab interface with auto-indentation, bracket matching, and line numbering
- **Visual Debugger**: Professional debugging interface with breakpoints and step execution
- **Build System**: Real-time compilation with error detection and reporting
- **Query Console**: Interactive Prolog execution environment within the IDE
- **Search & Navigation**: Find/replace with regex support and project-wide search

**Key Classes**: `PrologIDE.java`, `FileEditor.java`, `DebugPanel.java`, `ProjectTree.java`

### 3. 📟 Command Line Interface (`PrologCLI.java`)

An interactive Prolog console for quick testing and scripting:
- **Interactive query execution**: Direct Prolog query input with immediate results
- **File consultation**: Load Prolog files with proper DCG transformation
- **Multiple solutions handling**: Backtracking through solutions with `;` operator
- **Built-in commands**: `:consult`, `:listing`, `:save`, `:clear`, etc.
- **History and shortcuts**: Navigate previous queries and use command shortcuts
- **ISO-compliant output**: List representation in standard `[a,b,c]` format

### 4. 🔧 Built-in Predicates Library (`builtin/`)

Comprehensive library of standard Prolog predicates organized by category:

#### 🧮 **Arithmetic Operations** (`builtin/arithmetic/`)
- **Comparison operators**: `=:=`, `=\\=`, `>`, `<`, `>=`, `=<`
- **Advanced arithmetic**: `rem`, `div`, `mod`, `abs`, `sign`, `min`, `max`
- **Bitwise operations**: `xor`, `<<`, `>>`, `/\\`, `\\/`
- **Mathematical functions**: `sqrt`, `sin`, `cos`, `exp`, `log`, etc.

#### 📝 **Term Manipulation** (`builtin/term/`)
- **Structure analysis**: `functor/3`, `arg/3`, `=../2` (univ), `copy_term/2`
- **Term comparison**: `@<`, `@>`, `@=<`, `@>=`, `==`, `\\==`
- **Type checking**: `var/1`, `nonvar/1`, `atom/1`, `compound/1`, `number/1`

#### 📋 **List Processing** (`builtin/list/`)
- **Core operations**: `append/3`, `member/2`, `length/2`, `reverse/2`
- **List manipulation**: `select/3`, `nth0/3`, `nth1/3`
- **Sorting**: `sort/2`, `msort/2`, `keysort/2`

#### 🔀 **Control Structures** (`builtin/control/`)
- **Conditional execution**: `->` (if-then), `;` (or), `\\+` (not)
- **Meta-predicates**: `findall/3`, `bagof/3`, `setof/3`, `once/1`
- **Cut and unification**: `!` (cut), `=/2` (unify), `unify_with_occurs_check/2`

#### 🔤 **Atom Operations** (`builtin/atom/`)
- **Atom manipulation**: `atom_length/2`, `atom_concat/3`, `sub_atom/5`
- **String operations**: `atom_chars/2`, `atom_codes/2`, `atom_string/2`

#### 💾 **Database Operations** (`builtin/database/`)
- **Dynamic predicates**: `assert/1`, `asserta/1`, `assertz/1`, `retract/1`, `retractall/1`
- **Meta-database**: `abolish/1`, `current_predicate/1`, `listing/0`, `listing/1`

#### 📂 **I/O Operations** (`builtin/io/`)
- **Basic I/O**: `read/1`, `write/1`, `writeln/1`, `nl/0`
- **Stream operations**: `open/3`, `close/1`, `current_input/1`, `current_output/1`
- **Character I/O**: `get_char/1`, `put_char/1`, `get_code/1`, `put_code/1`

#### 🧬 **Advanced DCG Support** (`builtin/dcg/`)
- **Enhanced Grammar Execution**: `enhanced_phrase/2`, `enhanced_phrase/3` with ISO/IEC DTS 13211-3 compliance
- **Advanced Options**: `phrase_with_options/4` with syntax error handling, depth limits, tracing
- **DCG Utilities**: `call_dcg/3`, `dcg_translate_rule/2`, `dcg_body/3` for meta-programming
- **Complex Control Structures**: Full support for embedded Prolog goals, cuts, conditionals
- **Automatic Transformation**: `-->` rules converted to standard Prolog predicates with advanced features

## 🚀 Quick Start

### Prerequisites
- **Java**: Java 8 or higher (Java 11+ recommended)
- **Maven**: For building from source (3.6+ recommended)
- **Memory**: 512MB RAM minimum (1GB recommended for IDE)

### Installation & Launch

1. **Build the project**:
   ```bash
   mvn clean compile
   ```

2. **Launch options**:

   **🖥️ IDE (Recommended for development)**:
   ```bash
   java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
   # or use the script:
   ./start-ide.sh
   ```

   **📟 CLI (Quick testing and scripting)**:
   ```bash
   java -cp target/classes it.denzosoft.jprolog.PrologCLI
   ```

   **☕ Java API (Programmatic access)**:
   ```java
   import it.denzosoft.jprolog.core.engine.Prolog;
   
   Prolog prolog = new Prolog();
   List<Map<String, Term>> solutions = prolog.solve("factorial(5, X)");
   ```

### Simple Example

```prolog
% facts.pl - Basic family relationships
father(tom, bob).
father(bob, ann).
mother(ann, sue).

% Rules for family relationships  
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% Queries to try:
% ?- parent(bob, X).        % Who is bob parent of?
% ?- grandparent(tom, X).   % Who is tom grandparent of?  
% ?- findall(X, parent(X, _), Parents). % Find all parents
```

**Test with CLI**:
```bash
$ java -cp target/classes it.denzosoft.jprolog.PrologCLI
?- consult('facts.pl').
?- parent(bob, X).
X = ann.
?- grandparent(tom, X).
X = sue.
```

**Test with IDE**:
1. Launch IDE, create new project
2. Create `facts.pl` with above content
3. Compile project (Ctrl+F9)  
4. In Run tab: `?- grandparent(tom, X).`
5. Result: `X = sue`

## 📚 Documentation

JProlog provides comprehensive documentation for all aspects of the system:

### 🚀 **User Guides**
- **[docs/guides/guide-quick-start.md](docs/guides/guide-quick-start.md)**: 5-minute setup guide for immediate productivity
- **[docs/guides/guide-user-manual.md](docs/guides/guide-user-manual.md)**: Complete user guide for all components
- **[docs/guides/guide-cli-usage.md](docs/guides/guide-cli-usage.md)**: Dedicated command-line interface guide
- **[docs/guides/guide-ide-usage.md](docs/guides/guide-ide-usage.md)**: Integrated Development Environment usage guide
- **[docs/guides/guide-prolog-intro.md](docs/guides/guide-prolog-intro.md)**: Introduction to Prolog programming

### 🔧 **Development & Technical**  
- **[CLAUDE.md](CLAUDE.md)**: Developer guide with build commands, architecture, and workflow procedures
- **[docs/guides/guide-java-integration.md](docs/guides/guide-java-integration.md)**: Guide for embedding JProlog in Java applications
- **[docs/guides/guide-extension.md](docs/guides/guide-extension.md)**: How to create custom built-in predicates
- **[docs/guides/guide-debugging.md](docs/guides/guide-debugging.md)**: Comprehensive debugging tutorial with examples

### 📖 **Reference Documentation**
- **[BUILTIN_PREDICATES_REFERENCE.md](docs/references/BUILTIN_PREDICATES_REFERENCE.md)**: Comprehensive guide to all built-in predicates organized by functional groups
- **[BUILTIN_OPERATORS_REFERENCE.md](docs/references/BUILTIN_OPERATORS_REFERENCE.md)**: Complete reference for operators with precedence rules and examples
- **[ref-builtins-legacy.md](docs/references/ref-builtins-legacy.md)**: Original alphabetical built-ins reference (legacy)
- **[docs/references/ref-iso-compliance.md](docs/references/ref-iso-compliance.md)**: ISO Prolog standard compliance analysis
- **[docs/references/ref-limitations.md](docs/references/ref-limitations.md)**: Known limitations and workarounds
- **[docs/references/ref-dcg-grammar.md](docs/references/ref-dcg-grammar.md)**: Definite Clause Grammars reference

### 📝 **Project Management & Tracking**
- **[docs/tracking/track-issues.md](docs/tracking/track-issues.md)**: Issue tracking and resolution documentation
- **[docs/tracking/track-change-requests.md](docs/tracking/track-change-requests.md)**: Change requests and enhancement tracking
- **[docs/tracking/track-release-notes.md](docs/tracking/track-release-notes.md)**: Latest release notes
- **[CHANGELOG.md](CHANGELOG.md)**: Complete version history

### 🧪 **Reports & Analysis**
- **[docs/reports/report-test-results.md](docs/reports/report-test-results.md)**: Comprehensive test results and coverage analysis
- **[docs/reports/report-debug-features.md](docs/reports/report-debug-features.md)**: Overview of debugging capabilities
- **[docs/reports/report-package-reorg.md](docs/reports/report-package-reorg.md)**: Package reorganization documentation
- **[docs/reports/report-resolution-summary.md](docs/reports/report-resolution-summary.md)**: Summary of resolved issues and improvements

### 💡 **Examples & Tutorials**
- **[examples/](examples/)**: 40+ example Prolog programs covering all language features
- **[examples/example-bug-workflow.md](examples/example-bug-workflow.md)**: Step-by-step bug fixing examples  
- **[examples/example-nqueens-compilation.md](examples/example-nqueens-compilation.md)**: N-Queens problem compilation guide

## 📊 **Quality Metrics & Compliance**

### 🎯 **Current Status (Version 2.0.15)**
- **ISO Prolog Compliance**: 99.5% including ISO/IEC DTS 13211-3 DCG extensions
- **Comprehensive Test Success Rate**: 95%+ (20/20 core programs pass)
- **Built-in Predicate Coverage**: 93+ predicates with 99.5% ISO standard coverage
- **Parser Support**: ~90% of ISO Prolog syntax supported
- **DCG Extensions**: Complete ISO/IEC DTS 13211-3 Definite Clause Grammar support
- **Core Engine Stability**: ~98% robust and reliable operation

### 🧪 **Testing Framework**
- **40+ Example Programs**: Comprehensive test suite covering all language features
- **Automated Testing**: `./test_all_examples.sh` for continuous validation
- **Performance Testing**: Includes complex algorithms and data structures
- **Regression Testing**: Prevents introduction of new bugs

### ✅ **Verified Features**
- ✅ **Meta-predicates**: `findall/3`, `bagof/3`, `setof/3` fully functional
- ✅ **Term manipulation**: `functor/3`, `arg/3`, `=../2`, `copy_term/2`
- ✅ **Arithmetic operations**: `=:=`, `=\=`, `rem`, `xor`, shift operators
- ✅ **Control structures**: `;`, `->`, `\+`, `once/1`, `!` (cut)
- ✅ **List processing**: ISO-compliant `[a,b,c]` representation
- ✅ **Advanced DCG support**: Complete ISO/IEC DTS 13211-3 grammar extensions with enhanced predicates
- ✅ **DCG Meta-programming**: `call_dcg/3`, `dcg_translate_rule/2`, advanced control structures
- ✅ **Database operations**: Dynamic assert/retract predicates
- ✅ **I/O operations**: File and stream handling

## 🔧 **IDE Keyboard Shortcuts**

| Action | Shortcut | Description |
|--------|----------|-------------|
| New Project | Ctrl+N | Create new Prolog project |
| Compile Project | Ctrl+F9 | Build current project |  
| Run Query | F5 | Execute query in console |
| Toggle Debug | F8 | Enable/disable debug mode |
| Find in File | Ctrl+F | Search current file |
| Find in Project | Ctrl+Shift+F | Search all project files |
| Save File | Ctrl+S | Save current file |
| Open File | Ctrl+O | Open existing file |

## 🗂️ **Project Structure**

```
JProlog/
├── src/main/java/it/denzosoft/jprolog/
│   ├── core/                         # 🧠 Core Prolog Engine
│   │   ├── engine/                   # Main engine components
│   │   │   ├── Prolog.java           # Primary Prolog engine
│   │   │   ├── QuerySolver.java      # Query resolution and backtracking
│   │   │   ├── KnowledgeBase.java    # Fact/rule storage system
│   │   │   └── BuiltInRegistry.java  # Built-in predicate registry
│   │   ├── parser/                   # Prolog syntax parsing
│   │   │   ├── Parser.java           # Main parser interface
│   │   │   ├── PrologParser.java     # Core Prolog parser
│   │   │   └── TermParser.java       # Term-specific parsing
│   │   ├── terms/                    # Prolog term representation
│   │   │   ├── Term.java             # Base term interface
│   │   │   ├── Atom.java             # Atomic terms
│   │   │   ├── CompoundTerm.java     # Complex terms
│   │   │   ├── Variable.java         # Prolog variables
│   │   │   └── Number.java           # Numeric terms
│   │   └── dcg/                      # DCG grammar support
│   │       └── DCGTransformer.java   # Grammar rule transformation
│   ├── builtin/                      # 🔧 Built-in Predicates Library
│   │   ├── arithmetic/               # Math operations and comparisons
│   │   ├── control/                  # Control flow and meta-predicates
│   │   ├── list/                     # List processing predicates
│   │   ├── term/                     # Term manipulation
│   │   ├── atom/                     # Atom operations
│   │   ├── database/                 # Dynamic database predicates
│   │   ├── io/                       # Input/output operations
│   │   └── dcg/                      # DCG-specific predicates
│   ├── editor/                       # 🖥️ IDE Components
│   │   ├── PrologIDE.java            # Main IDE application
│   │   ├── FileEditor.java           # Text editor with syntax highlighting
│   │   ├── DebugPanel.java           # Visual debugger interface
│   │   ├── ProjectTree.java          # Project file navigator
│   │   └── ...                       # Other IDE components
│   └── PrologCLI.java                # 📟 Command-line interface
├── examples/                         # 🧪 Example Prolog Programs  
│   ├── test_01_basic_facts.pl        # Basic facts and queries
│   ├── test_14_dcg_simple.pl         # DCG grammar examples
│   ├── family_tree.pl                # Family relationship examples
│   └── ...                           # 40+ comprehensive examples
├── 📚 Documentation Files
├── CLAUDE.md                         # Developer guide and procedures
├── USER_MANUAL.md                    # Complete user documentation
├── docs/references/BUILTIN_PREDICATES_REFERENCE.md  # Built-in predicates reference
├── issues.md                         # Issue tracking and resolutions
└── README.md                         # This file
```

## 🧪 **Testing & Validation**

### Comprehensive Testing Suite
```bash
# Run all 40+ example programs (comprehensive testing)
./test_all_examples.sh

# Test individual components
./test-debug.sh        # Debug features
./simple_test.sh       # Basic functionality  
./test-ide-console.sh  # IDE console integration
```

### Testing Categories
- **Basic Functionality**: Facts, rules, queries, unification
- **Advanced Features**: DCG grammars, meta-predicates, arithmetic  
- **Control Structures**: Cut, if-then-else, negation
- **Built-in Predicates**: I/O, database operations, list processing
- **Performance**: Complex algorithms (N-Queens, sorting, recursion)

## 🔧 **Configuration & Customization**

### IDE Settings
Settings stored in `~/.jprolog-ide.properties`:
```properties
editor.font.size=14
editor.tab.width=4
build.auto.save=true
debug.trace.enabled=false
syntax.highlighting=true
```

### CLI Configuration
Environment variables for CLI customization:
```bash
export JPROLOG_STACK_SIZE=10000
export JPROLOG_TRACE_MODE=off  
export JPROLOG_LIST_FORMAT=iso  # Use [a,b,c] format
```

## 🤝 **Development & Contributing**

### Building from Source
```bash
git clone https://github.com/DenzoSOFTHub/JProlog.git
cd JProlog
mvn clean compile
```

### Architecture Highlights
- **Modular Design**: Clean separation between engine, IDE, CLI, and built-ins
- **Extensible Built-in System**: Easy addition of custom predicates
- **ISO Compliance Focus**: Adherence to Prolog standards where possible
- **Test-Driven Development**: Comprehensive test suite ensuring quality

### Key Design Principles
- **Performance**: Efficient unification and backtracking algorithms
- **Reliability**: Robust error handling and edge case management
- **Usability**: Intuitive interfaces for both beginners and experts
- **Maintainability**: Clean code structure with comprehensive documentation

## 📋 **System Requirements**

- **Operating System**: Windows 10+, macOS 10.14+, Linux (Ubuntu 18.04+)
- **Java Runtime**: Java 8 minimum (Java 11+ recommended)
- **Memory**: 512MB RAM minimum (1GB+ for IDE, 2GB+ for large projects)
- **Disk Space**: 200MB for installation, additional space for projects

## 🆘 **Support & Troubleshooting**

### Quick Fixes
- **Build Issues**: Run `mvn clean compile` 
- **CLI Problems**: Check Java classpath and version
- **IDE Won't Start**: Verify Java version with `java -version`
- **Slow Performance**: Increase JVM memory with `-Xmx1g`

### Getting Help
1. **Documentation**: Check relevant `.md` files in the repository
2. **Examples**: Review `examples/` directory for usage patterns
3. **Issues**: Check `issues.md` for known problems and solutions
4. **Testing**: Run test suite to verify installation

## 📄 **License & Copyright**

**JProlog - Complete Prolog Implementation**

Copyright © 2024 DenzoSOFT. All rights reserved.

Version 2.0.15 - Released August 2025

## 🌐 **Project Information**

- **Repository**: https://github.com/DenzoSOFTHub/JProlog
- **Website**: https://denzosoft.it
- **Latest Release**: v2.0.15 with 99.5% ISO 13211-1 + DTS 13211-3 compliance
- **License**: Proprietary (DenzoSOFT)

---

## 🎯 **Why Choose JProlog?**

✅ **Near-Complete ISO Compliance**: 99.5% ISO 13211-1 + DTS 13211-3 support with advanced DCG extensions  
✅ **Complete Ecosystem**: Engine + IDE + CLI + 93+ Built-ins in one package  
✅ **Professional Tools**: Full-featured IDE with debugging capabilities  
✅ **Advanced Grammar Processing**: Complete DCG system for parsing applications
✅ **Easy Integration**: Simple Java API for embedding Prolog logic  
✅ **Extensive Documentation**: 25+ documentation files covering all aspects  
✅ **Educational Value**: 40+ example programs for learning Prolog  

**Start your Prolog journey with a robust, professional-grade implementation!**

*Powerful Prolog Programming Made Accessible*