# JProlog IDE - Interactive Development Environment

## Table of Contents
1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [IDE Interface](#ide-interface)
4. [Project Management](#project-management)
5. [Editor Features](#editor-features)
6. [Query Execution](#query-execution)
7. [Debugging Features](#debugging-features)
8. [File Operations](#file-operations)
9. [Search and Navigation](#search-and-navigation)
10. [Advanced Features](#advanced-features)

---

## Introduction

The JProlog IDE is a comprehensive integrated development environment for Prolog programming that provides:

- **Project-based development** with file management
- **Advanced code editor** with syntax highlighting and line numbering
- **Interactive query execution** with multiple solution exploration
- **Integrated debugging system** with breakpoints and step execution
- **Real-time compilation** with error detection and reporting
- **Knowledge base management** with incremental loading

### Key Features
- Full Prolog syntax support with ISO compliance
- Interactive query console with backtracking support (`;`)
- Advanced debugging with variable monitoring and stack traces
- Project tree navigation and file management
- Search functionality across files and projects
- Comprehensive built-in predicate support

---

## Getting Started

### Prerequisites
- Java 8 or higher
- Maven (for building from source)

### Launching the IDE

#### Method 1: Using Maven
```bash
# Compile the project first
mvn compile

# Launch the IDE
mvn exec:java -Dexec.mainClass="it.denzosoft.jprolog.editor.PrologIDE"
```

#### Method 2: Direct Java execution
```bash
# Ensure project is compiled
mvn compile

# Launch from compiled classes
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
```

#### Method 3: Using the provided script
```bash
./start-ide.sh
```

### First Launch
When you first launch the IDE, you'll see:
- **Welcome screen**: Status showing "IDE started - Open or create a project to begin"
- **Empty workspace**: Ready for project creation or loading
- **Full interface**: All panels and tools available but requiring a project to activate

---

## IDE Interface

### Main Window Layout

The JProlog IDE is organized into several key areas:

#### **Top Section**
- **Menu Bar**: File, Edit, Prolog, Help menus for all IDE operations
- **Toolbar**: Quick access buttons for common operations (New, Open, Save, Compile, Run)

#### **Left Panel** (Vertical Split)
- **Project Tree** (Top): Hierarchical display of all project files and directories
- **Predicates Panel** (Bottom): Lists all predicates defined in the currently active file

#### **Right Panel** (Vertical Split)
- **Editor Area** (Top): Multi-tabbed text editor with syntax highlighting
- **Console Area** (Bottom): Tabbed panels for different console operations

#### **Bottom Tabs** (Console Area)
- **Output**: General IDE messages and status information
- **Build**: Compilation results, errors, and warnings
- **Run**: Interactive Prolog query console for query execution
- **Debug**: Advanced debugging interface with breakpoints and variable monitoring
- **Search**: Search results and navigation across project files

#### **Status Bar** (Bottom)
- Shows current project status, active file information, and system messages

---

## Project Management

### Creating a New Project

1. **Using Menu**: Go to **File → New Project...** (Ctrl+N)
2. **Select Directory**: Choose the directory that will become your project root
3. **Project Initialization**: The IDE will:
   - Use the selected directory as the project root
   - Set the directory name as the project name
   - Create `main.pl` with example code if the directory is empty
   - Initialize the project tree view

### Opening Existing Projects

1. **Using Menu**: Go to **File → Open Project...** (Ctrl+O)
2. **Select Directory**: Choose any directory containing Prolog files
3. **Project Loading**: The IDE will:
   - Scan the directory for `.pl` files
   - Build the project tree structure
   - Initialize the Prolog engine for the project

### Project Tree Operations

The project tree provides comprehensive file management:

#### **Navigation**
- **Double-click files**: Opens files in the editor
- **Expand/collapse directories**: Navigate complex project structures
- **Auto-refresh**: Tree updates automatically when files are added or removed

#### **Context Operations**
- **Right-click**: Access context menu with file operations
- **File creation**: Right-click empty space to create new files
- **File management**: Copy, move, rename, and delete operations

---

## Editor Features

### Syntax Highlighting

The advanced editor provides comprehensive syntax highlighting:

#### **Language Elements**
- **Keywords**: Prolog reserved words (if, then, else, is, etc.)
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`) and logical (`:-`, `->`, `;`) operators
- **Comments**: Both single-line (`%`) and multi-line (`/* */`) comments
- **Strings**: Quoted strings and atoms with distinct coloring
- **Variables**: Prolog variables (starting with uppercase) highlighted separately
- **Numbers**: Integer and floating-point literals

### Advanced Editing Features

#### **Line Management**
- **Line Numbers**: Always visible on the left margin
- **Line Navigation**: Click line numbers for quick positioning
- **Line Highlighting**: Current line highlighting for easy tracking

#### **Code Intelligence**
- **Auto-indentation**: Smart indentation based on Prolog syntax
- **Bracket Matching**: Automatic matching of parentheses, brackets, and braces
- **Code Folding**: Collapse and expand code sections (planned feature)

#### **Multi-Tab Interface**
- **Multiple Files**: Work with several files simultaneously
- **Tab Navigation**: Click tabs or use Ctrl+Tab for switching
- **Unsaved Indicators**: `*` symbol shows modified files
- **Tab Management**: Right-click tabs for close, close others, close all operations

---

## Query Execution

### Interactive Query Console

The Run tab provides a full-featured Prolog query interface:

#### **Basic Query Execution**
1. **Switch to Run tab**: Click on the "Run" tab in the bottom panel
2. **Enter query**: Type Prolog queries using standard syntax
3. **Execute**: Press Enter to execute the query
4. **View results**: Results appear with proper formatting and coloring

#### **Query Syntax**
```prolog
?- parent(tom, X).          % Find all children of tom
?- append([1,2], [3,4], L). % Append two lists
?- member(X, [a,b,c]).      % Find all members of list
?- factorial(5, F).         % Calculate factorial
```

### Multiple Solution Exploration

#### **Backtracking with `;`**
When queries have multiple solutions:

1. **First Solution**: Displayed with ` ;` prompt
2. **Continue**: Press `;` + Enter to see next solution
3. **Exploration**: Continue pressing `;` for all solutions
4. **Completion**: Last solution ends with `.` or shows `false.`

#### **Example Session**
```prolog
?- parent(X, Y).
X = tom, Y = bob ;      ← Press ; + Enter for next
X = tom, Y = liz ;      ← Press ; + Enter for next
X = bob, Y = ann ;      ← Press ; + Enter for next
X = bob, Y = pat.       ← Final solution
```

### Query Result Formatting

#### **Color-Coded Output**
- **Blue**: Query text that was executed
- **Black**: Normal query results and variable bindings
- **Red**: Error messages and compilation failures
- **Dark Gray**: System prompts and status messages
- **Green**: Success confirmations and completion messages

---

## Debugging Features

### Integrated Debug System

Access comprehensive debugging through the Debug tab:

#### **Debug Panel Components**
- **Toolbar**: Start/stop debug, step controls, trace toggle
- **Stack Trace**: Hierarchical view of execution stack
- **Variables Table**: Real-time variable bindings with type information
- **Breakpoints Manager**: Add, remove, and manage breakpoints
- **Debug Console**: Detailed trace information and debug messages

### Basic Debug Operations

#### **Starting Debug Session**
1. **Switch to Debug tab**: Click on the "Debug" tab
2. **Start Debug**: Click **🚀 Start Debug** button
3. **Set Breakpoints**: Use the breakpoints panel to add breakpoints
4. **Execute Queries**: Run queries in the Run tab to trigger breakpoints

#### **Breakpoint Management**
- **Add Breakpoint**: Click "Add" and enter predicate name (e.g., `factorial/2`)
- **Remove Breakpoint**: Select and click "Remove"
- **Clear All**: Remove all breakpoints at once

#### **Step Execution**
- **Step Into**: ⬇️ Enter the next predicate call
- **Step Over**: ➡️ Execute current goal without entering sub-goals
- **Step Out**: ⬆️ Exit current predicate to caller
- **Continue**: ▶️ Resume execution until next breakpoint

For detailed debugging information, see the [Debugging Guide](guide-debugging.md).

---

## File Operations

### File Management

#### **Creating Files**
1. **Menu Method**: **File → New File...** (Ctrl+Shift+N)
2. **Project Requirement**: Must have an active project
3. **Auto-naming**: Files automatically get `.pl` extension
4. **Auto-opening**: New files open immediately in editor

#### **Saving Files**
- **Save Current**: **File → Save** (Ctrl+S)
- **Save All**: **File → Save All** (Ctrl+Shift+S)
- **Auto-save**: Configurable in preferences
- **Backup Creation**: Automatic backup file generation

#### **Loading Programs**
- **Compilation**: **Prolog → Compile File** (F9) for single files
- **Project Compilation**: **Prolog → Compile Project** (Ctrl+F9) for entire project
- **Auto-compilation**: Triggered on file save (configurable)

### Knowledge Base Management

#### **Compilation Process**
1. **Parse**: Analyze Prolog syntax and structure
2. **Load**: Add clauses to the knowledge base
3. **Validate**: Check for syntax errors and warnings
4. **Update**: Refresh IDE with new predicate information

#### **Error Handling**
- **Syntax Errors**: Red underlines in editor
- **Build Messages**: Detailed error reporting in Build tab
- **Line Linking**: Click errors to jump to problematic code
- **Real-time Validation**: Immediate feedback during typing

---

## Search and Navigation

### Search Functionality

#### **File Search** (Ctrl+F)
- **Current File**: Search within the active editor tab
- **Incremental**: Real-time highlighting as you type
- **Case Options**: Case-sensitive and case-insensitive search
- **Navigation**: Next/Previous match navigation

#### **Project Search** (Ctrl+Shift+F)
- **Multi-file**: Search across entire project
- **Results Panel**: Dedicated search results in Search tab
- **File Navigation**: Click results to open files at match locations
- **Context Display**: Shows matching lines with surrounding context

### Navigation Features

#### **Quick Navigation**
- **Go to Line**: Ctrl+G for direct line jumping
- **File Navigation**: Use project tree for file switching
- **Tab Navigation**: Ctrl+Tab for recent file switching
- **Predicate Navigation**: Use predicates panel for quick jumping

---

## Advanced Features

### Prolog Engine Integration

#### **Real-time Compilation**
- **Automatic**: Files compile when saved or manually triggered
- **Incremental**: Only changed predicates reload when possible
- **Error Recovery**: IDE continues functioning despite compilation errors

#### **Knowledge Base Status**
- **Predicate Listing**: **Prolog → Show KB Status** for knowledge base overview
- **Clear KB**: **Prolog → Clear Knowledge Base** for fresh starts
- **Engine Reset**: Full engine reinitialization when needed

### Built-in Predicate Support

The IDE supports the full range of JProlog built-in predicates:

#### **Comprehensive Coverage**
- **Arithmetic**: Complete mathematical operations and comparisons
- **Lists**: All list manipulation predicates (append, member, length, etc.)
- **Type Checking**: Full type system support (atom, number, var, etc.)
- **Meta-predicates**: findall/3, bagof/3, setof/3 with full functionality
- **String Operations**: String and atom manipulation predicates
- **I/O Operations**: File and stream operations
- **Database Operations**: assert/retract family for dynamic predicates

#### **ISO Compliance**
JProlog IDE supports approximately **95% of ISO Prolog standard**, including:
- Core syntax and unification
- Control structures (if-then-else, cut, negation)
- Term manipulation (functor/3, arg/3, =../2, copy_term/2)
- DCG (Definite Clause Grammar) system
- Exception handling (basic support)

### Configuration and Customization

#### **IDE Preferences**
- **Editor Settings**: Font size, tab width, color schemes
- **Compilation Options**: Auto-compile, error handling behavior
- **Debug Configuration**: Default breakpoint behavior, trace options

---

## Version Information

This guide is current as of **JProlog v2.0.6**. The IDE features described are fully functional and tested with this version.

### Key Improvements in v2.0.6
- Enhanced list representation with ISO-compliant formatting
- Meta-predicates (findall/3, bagof/3, setof/3) fully operational
- Term manipulation predicates working correctly
- Advanced arithmetic operators functional
- DCG system fully operational
- Comprehensive built-in predicate coverage (~90%)
- 95% ISO Prolog compliance

### Launch Commands (Updated)
```bash
# Maven execution
mvn exec:java -Dexec.mainClass="it.denzosoft.jprolog.editor.PrologIDE"

# Direct execution
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE

# Script execution
./start-ide.sh
```

---

## Getting Help

### Documentation Resources
- **[User Manual](guide-user-manual.md)**: Complete IDE functionality reference
- **[Debugging Guide](guide-debugging.md)**: Detailed debugging system documentation
- **[Quick Start Guide](guide-quick-start.md)**: Fast introduction to core features
- **[Java Integration Guide](guide-java-integration.md)**: Extending JProlog with Java

### Troubleshooting
- **Project Issues**: Ensure project is properly created/opened before file operations
- **Compilation Errors**: Check Build tab for detailed error messages and line numbers
- **Query Failures**: Verify predicates are compiled and loaded into knowledge base
- **Debug Problems**: Ensure debug session is active and breakpoints are properly set

### Best Practices
1. **Regular Compilation**: Compile frequently to catch errors early
2. **Project Organization**: Use clear directory structures and meaningful file names
3. **Query Testing**: Test predicates individually before complex queries
4. **Debug Usage**: Use debugging features to understand execution flow
5. **File Management**: Keep related predicates in appropriately named files

---

**JProlog IDE from DenzoSOFT** - Complete Integrated Development Environment for Prolog Programming

*Version 2.0.6 | DenzoSOFT | https://denzosoft.it*

*For additional support and documentation, visit the DenzoSOFT website or consult the comprehensive user manual.*