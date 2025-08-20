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

**Issue Tracking (`issues.md`)**:
- Format: `ISS-YYYY-NNNN` (e.g., ISS-2025-0001)
- Status flow: `TO_ANALYZE` → `IN_ANALYSIS` → `IN_PROGRESS` → `RESOLVED` → `CLOSED`
- Always register issues before fixing
- Create separate issues for related problems discovered during analysis

**Limitations Documentation (`limitations.md`)**:
- **OBBLIGATORIO**: Quando viene identificata una issue, DEVE essere aggiornato anche il file `limitations.md`
- Documentare la limitazione con esempi concreti di codice Prolog che fallisce
- Quando una issue viene risolta (status `RESOLVED`), rimuovere la corrispondente entry da `limitations.md`
- Formato entry:
  ```markdown
  ## ISS-YYYY-NNNN: [Titolo Limitazione]
  
  **Descrizione**: Breve descrizione della limitazione
  
  **Esempi che falliscono**:
  ```prolog
  % Esempio 1: Query che fallisce
  ?- functor(f(a,b), F, A).
  % Expected: F = f, A = 2
  % Actual: No solutions found
  
  % Esempio 2: Altro caso di fallimento
  ?- arg(1, f(a,b,c), X).
  % Expected: X = a
  % Actual: No solutions found
  ```
  
  **Workaround** (se disponibile): Metodo alternativo per ottenere lo stesso risultato
  ```

**Built-in Documentation (`builtins.md`)**:
- **OBBLIGATORIO**: Ogni volta che si aggiunge o modifica un predicato o operatore built-in, DEVE essere aggiornato il file `builtins.md` con una descrizione in inglese e esempi di codice spiegati
- **ORDINAMENTO**: I predicati devono essere organizzati in ordine alfabetico per nome
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

**Change Request Tracking (`ChangeRequest.md`)**:
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

## Procedura di Cleanup e Validazione Finale

**PROCEDURA OBBLIGATORIA**: Al termine di ogni sessione di lavoro o implementazione significativa, seguire sempre questa procedura di cleanup e validazione:

### 1. Cleanup File Temporanei
```bash
# Rimuovere tutti i file temporanei di test e debug
rm -f *.class
rm -f *Test.java         # Solo quelli nella root, NON sotto src/test/
rm -f Debug*.java
rm -f temp_*.txt
rm -f *_debug.*
rm -f test_*.txt         # File temporanei, NON i programmi test examples/test_*.pl
```

**File da mantenere**:
- `examples/test_*.pl` - Programmi di test ufficiali
- `src/test/**/*.java` - Test unit ufficiali
- `*.sh` - Script di automazione  
- `*.md` - Documentazione
- `issues.md`, `ChangeRequest.md` - Tracking formale

**File da rimuovere**:
- File .class nella root 
- File Debug*.java temporanei
- File Test*.java nella root (non sotto src/test/)
- File temp_*.txt, *_debug.*, test_input.txt, etc.

### 2. Ricompilazione Completa
```bash
# Sempre ricompilare tutto prima di concludere
mvn clean compile
```

### 3. Validazione Errori
```bash
# Verificare che non ci siano errori di compilazione
mvn compile -q
echo $?  # Deve restituire 0 (successo)
```

### 4. Test Comprehensive sui 40 Programmi Prolog Standard
**PROCEDURA OBBLIGATORIA**: Dopo ogni build, eseguire test completo sui 40 programmi di test per verificare che tutte le funzionalità standard operino correttamente.

```bash
# Eseguire testing completo su tutti i programmi Prolog di esempio
./test_all_examples.sh

# Output atteso: Summary con percentuale successo >= 75%
# Se success rate < 75%, investigare fallimenti prima di procedere
```

**Programmi Testati** (examples/test_*.pl):
- Basic facts, arithmetic, lists, recursion  
- Control structures, type checking, term manipulation
- Meta-predicates, database operations, I/O
- DCG grammars, exception handling, string/atom operations
- Advanced features, modules, performance tests

**Criteri di Accettazione**:
- **Success Rate >= 75%**: Acceptable per sessioni di maintenance  
- **Success Rate >= 85%**: Required per sessioni di sviluppo nuove feature
- **Success Rate < 75%**: BLOCCA la sessione - identificare e risolvere regressioni

**Investigazione Fallimenti**:
```bash
# Per investigare singoli fallimenti:
java -cp target/classes it.denzosoft.jprolog.PrologCLI
:consult examples/test_XX_nome.pl
[testare queries manualmente]

# Log di debug per problemi specifici:
./test-debug.sh
```

**Note**:
- I fallimenti per **Parser Limitations** (sintassi ISO avanzata non supportata) sono accettabili
- I fallimenti per **Missing Advanced Features** (es. constraint programming) sono accettabili  
- I fallimenti per **Core Features** (arithmetic, lists, control structures) NON sono accettabili

### 5. Test di Smoke
```bash
# Test rapido per verificare che il sistema funzioni base
echo ":quit" | timeout 5 java -cp target/classes it.denzosoft.jprolog.PrologCLI
```

### 6. Aggiornamento Versione e Documentazione
**PROCEDURA OBBLIGATORIA**: Al termine di ogni modifica del codice che compila con successo E ha passato i test comprehensive:

#### Sequenza Completa Versioning:
```bash
# 1. Verificare versione corrente
grep "<version>" pom.xml | head -1

# 2. Build completa Maven
mvn clean compile

# 3. Test Maven (se presenti)
mvn test

# 4. Test comprehensive sui 40 programmi (OBBLIGATORIO)
./test_all_examples.sh
# Verificare che success rate >= 75%

# 5. SOLO se tutti i test passano, incrementare patch version
# Esempio: se era 2.0.3, cambiare a 2.0.4 nel pom.xml
```

#### Aggiornamento Documentazione (OBBLIGATORIO)
**Dopo ogni incremento versione**, aggiornare immediatamente:

1. **File `issues.md`**:
   - Aggiornare status delle issue risolte da `IN_PROGRESS` → `RESOLVED`
   - Aggiungere data risoluzione: `**Data Risoluzione**: YYYY-MM-DD`
   - Documentare soluzione implementata nella sezione `#### Soluzione Implementata`
   - Aggiungere riferimenti ai file modificati
   - Aggiornare test results e validation

2. **File `ChangeRequest.md`** (se applicabile):
   - Aggiornare status CR da `IN_DEVELOPMENT` → `COMPLETED`
   - Documentare implementazione e acceptance criteria soddisfatti
   - Aggiornare impatto analysis con risultati effettivi

3. **File `limitations.md`**:
   - **RIMUOVERE** le entry per issue risolte
   - Aggiornare workarounds se non più necessari
   - Documentare nuove limitazioni scoperte durante l'implementazione

#### Criteri di Versioning
**IMPORTANTE**: La versione viene incrementata SOLO se:
- Codice compila senza errori (`mvn compile` success)
- Test Maven passano (`mvn test` success, se presenti)
- Test comprehensive passa con success rate >= 75% (`./test_all_examples.sh`)
- Nessuna regressione critica identificata
- **Documentazione aggiornata** (issues.md, ChangeRequest.md, limitations.md)

### 7. Push a GitHub e Tagging (DOPO Aggiornamento Versione)
**PROCEDURA OBBLIGATORIA**: Dopo aver incrementato la versione e aggiornato la documentazione:

#### Pre-Push Cleanup
```bash
# 1. Rimuovere TUTTI i file di test temporanei dalla root
rm -f Test*.java
rm -f Debug*.java
rm -f temp_*.txt
rm -f test_*.txt  # File temporanei, NON i programmi examples/test_*.pl
rm -f *.class
rm -f *_debug.*

# 2. Aggiornare .gitignore per ignorare i file .sh (se non già presente)
echo "*.sh" >> .gitignore
echo "Test*.java" >> .gitignore  
echo "Debug*.java" >> .gitignore
echo "temp_*.txt" >> .gitignore
echo "*.class" >> .gitignore
```

#### Push e Tagging Sequence
```bash
# 3. Configurare credenziali GitHub (SICUREZZA: usa variabili ambiente)
# NOTA: Le credenziali sono fornite come riferimento ma dovrebbero essere in variabili ambiente
export GITHUB_USER="DenzoSOFTHub"
export GITHUB_TOKEN="${GITHUB_TOKEN:-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}"
export GITHUB_REPO="https://github.com/DenzoSOFTHub/JProlog"

# 4. Verificare status Git e preparare commit
git status
git add .
git add -u  # Include file eliminati

# 5. Creare commit con messaggio strutturato
CURRENT_VERSION=$(grep '<version>' pom.xml | head -1 | sed 's/.*<version>\(.*\)<\/version>.*/\1/')
git commit -m "Release v${CURRENT_VERSION}

- Resolved critical issues and enhanced functionality
- Updated documentation and version bump
- Comprehensive tests passed (success rate >= 75%)

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"

# 6. Push al repository remoto
git push https://${GITHUB_USER}:${GITHUB_TOKEN}@github.com/DenzoSOFTHub/JProlog.git main

# 7. Creare e pushare tag con versione
git tag -a "v${CURRENT_VERSION}" -m "Release version ${CURRENT_VERSION}

Features and fixes included in this release:
- Enhanced list representation with ISO-compliant formatting
- Meta-predicates (findall/3, bagof/3, setof/3) verified functional
- DCG (Definite Clause Grammar) system fully operational
- Comprehensive testing passed with high success rate

🤖 Generated with [Claude Code](https://claude.ai/code)"

git push https://${GITHUB_USER}:${GITHUB_TOKEN}@github.com/DenzoSOFTHub/JProlog.git "v${CURRENT_VERSION}"
```

#### Post-Push Verification
```bash
# 8. Verificare push e tag su GitHub
echo "✅ Push completed for version: ${CURRENT_VERSION}"
echo "🔗 Repository: ${GITHUB_REPO}"
echo "🏷️  Tag: v${CURRENT_VERSION}"
echo ""
echo "Verify on GitHub:"
echo "- Repository: ${GITHUB_REPO}"
echo "- Releases: ${GITHUB_REPO}/releases"
echo "- Tag: ${GITHUB_REPO}/releases/tag/v${CURRENT_VERSION}"
```

#### Sicurezza e Best Practices
**⚠️ IMPORTANTE - Gestione Credenziali**:
- **MAI** committare token di accesso nel codice sorgente
- Usare variabili d'ambiente per le credenziali sensibili
- Il token mostrato sopra è solo di esempio - usare token personale valido
- Considerare l'uso di SSH keys invece di HTTPS con token per maggiore sicurezza

**Repository Target**:
- **URL**: https://github.com/DenzoSOFTHub/JProlog  
- **User**: DenzoSOFTHub
- **Branch**: main (default)
- **Tag Format**: v{major}.{minor}.{patch} (es. v2.0.4)

#### Razionale Versioning:
- **Major version (x.0.0)**: Cambi architetturali o breaking changes (su richiesta utente)
- **Minor version (x.y.0)**: Nuove funzionalità significative (su richiesta utente)
- **Patch version (x.y.z)**: Bug fix e miglioramenti (automatico dopo ogni modifica)

#### Template Aggiornamento Issue (esempio):
```markdown
#### Soluzione Implementata
✅ **COMPLETATA**: [Descrizione breve della soluzione]

**File Modificati**:
- `percorso/file1.java` - [descrizione modifica]
- `percorso/file2.java` - [descrizione modifica]

**Test Results**:
- ✅ Maven build: SUCCESS
- ✅ Test comprehensive: XX/40 programmi (XX% success rate)
- ✅ Funzionalità target: [Risultati specifici dei test]

**Versione**: Incrementata da X.Y.Z-1 → X.Y.Z
```

**Razionale**: Mantenere il workspace pulito evita:
- Confusione tra file temporanei e codice permanente
- Accumulo di file di debug obsoleti  
- Errori di compilazione da file temporanei corrotti
- Difficoltà nel tracking delle modifiche reali

## Common Development Scenarios

### Adding New Built-in Predicates
1. Create class in appropriate `builtin/*` category package
2. Implement `BuiltIn` or `BuiltInWithContext` interface  
3. Register in `BuiltInRegistry` (through BuiltInFactory registration)
4. Add comprehensive test cases to `comprehensive_test.sh`
5. Update `builtins.md` documentation (MANDATORY)
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