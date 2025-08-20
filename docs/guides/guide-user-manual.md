# JProlog Editor from DenzoSOFT - User Manual

## Table of Contents
1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Project Management](#project-management)
4. [File Editor](#file-editor)
5. [Prolog Engine Integration](#prolog-engine-integration)
6. [Query Execution](#query-execution)
7. [Debugging System](#debugging-system)
8. [Search and Navigation](#search-and-navigation)
9. [Advanced Features](#advanced-features)
10. [Keyboard Shortcuts](#keyboard-shortcuts)
11. [Configuration](#configuration)
12. [Troubleshooting](#troubleshooting)

---

## Introduction

**JProlog Editor from DenzoSOFT** is a comprehensive Integrated Development Environment (IDE) specifically designed for Prolog programming. It provides a complete development workflow with project management, intelligent editing, compilation, debugging, and query execution capabilities.

### Key Features
- **Project Management**: Organize Prolog files in structured projects
- **Advanced Editor**: Syntax highlighting, line numbering, and intelligent editing
- **Compilation System**: Real-time parsing with error detection and reporting
- **Query Console**: Interactive Prolog query execution
- **Debug System**: Complete debugging with breakpoints, step execution, and variable monitoring
- **Search Tools**: Find and replace across files and projects
- **Knowledge Base Management**: Intelligent clause loading and management

### System Requirements
- Java 8 or higher
- 100MB available disk space
- 512MB RAM (1GB recommended)
- Operating System: Windows, macOS, or Linux

---

## Getting Started

### Installation and Launch

1. **Compile the Project**: 
   ```bash
   mvn compile
   ```

2. **Launch the IDE**:
   ```bash
   java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
   ```

3. **Alternative Launch Script**:
   ```bash
   ./start-ide.sh
   ```

### First Time Setup

When you first launch JProlog Editor, you'll see:
- **Main Window**: The IDE interface with project tree, editor area, and console
- **Welcome Screen**: Status bar showing "IDE started - Open or create a project to begin"
- **Empty Workspace**: Ready for you to create or open your first project

### IDE Interface Overview

The IDE is organized into several main areas:

#### **Top Section**
- **Menu Bar**: File, Edit, Prolog, Help menus
- **Toolbar**: Quick access to common operations (New, Open, Save, Compile, Run)

#### **Left Panel** (Split vertically)
- **Project Tree** (Top): Hierarchical view of project files
- **Predicates Panel** (Bottom): Shows predicates defined in the current file

#### **Right Panel** (Split vertically)
- **Editor Area** (Top): Multi-tab editor with syntax highlighting
- **Bottom Tabs** (Bottom): Output, Build, Run, Search, Debug consoles

#### **Bottom**
- **Status Bar**: Shows current status, messages, and project information

---

## Project Management

### Creating a New Project

1. **Menu Method**: Go to **File → New Project...** (Ctrl+N)
2. **Select Directory**: Choose the directory that will become your project root
3. **Automatic Setup**: The IDE uses the selected directory as the project root
4. **Project Structure**: The IDE creates:
   - Uses the selected directory as project root
   - Creates `main.pl` file with example code (if directory is empty)
   - Directory name becomes the project name

#### Example Project Structure
```
MyProject/
├── main.pl          # Main entry point
├── utils.pl         # Utility predicates
└── tests.pl         # Test cases
```

### Opening Existing Projects

1. **Menu Method**: Go to **File → Open Project...** (Ctrl+O)
2. **Select Directory**: Choose any directory to open as project root
3. **Project Loading**: The IDE will:
   - Use the selected directory as project root
   - Load the directory structure in the project tree
   - Scan for .pl files
   - Initialize the Prolog engine for the project

### Project Tree Operations

The project tree provides several operations:

#### **File Operations**
- **Double-click**: Open file in editor
- **Right-click**: Context menu with file operations
- **Drag & Drop**: Move files within the project

#### **Navigation**
- **Expand/Collapse**: Navigate directory structures
- **Auto-refresh**: Tree updates when files are added/removed
- **File Icons**: Visual indicators for different file types

### Creating New Files

1. **Menu Method**: **File → New File...** (Ctrl+Shift+N)
2. **Enter Name**: File name (without .pl extension)
3. **Auto-creation**: File is created with .pl extension
4. **Auto-open**: New file opens in the editor

**Requirements**: You must have a project open before creating new files.

---

## File Editor

### Editor Features

The JProlog Editor provides a sophisticated editing environment:

#### **Syntax Highlighting**
- **Keywords**: Prolog keywords highlighted in blue
- **Operators**: Arithmetic and logical operators
- **Comments**: Single-line (%) and multi-line (/* */) comments
- **Strings**: Quoted strings in green
- **Variables**: Prolog variables with special formatting

#### **Line Numbering**
- **Always Visible**: Line numbers displayed on the left
- **Scroll Sync**: Line numbers scroll with the code
- **Click to Navigate**: Click line numbers for quick navigation

#### **Advanced Editing**
- **Auto-indentation**: Smart indentation for Prolog clauses
- **Bracket Matching**: Automatic bracket and parenthesis matching
- **Multi-tab Support**: Work with multiple files simultaneously
- **Undo/Redo**: Full undo/redo support

### Multi-Tab Interface

#### **Tab Operations**
- **New Tab**: Opens when you open a new file
- **Tab Switching**: Click tabs or use Ctrl+Tab
- **Close Tab**: X button on each tab
- **Unsaved Indicator**: * symbol for modified files

#### **Tab Context Menu**
- **Close**: Close the current tab
- **Close Others**: Close all other tabs
- **Close All**: Close all tabs
- **Reload**: Reload file from disk

### File Operations

#### **Saving Files**
- **Save Current**: **File → Save** (Ctrl+S)
- **Save All**: **File → Save All** (Ctrl+Shift+S)
- **Auto-save**: Available in preferences
- **Backup**: Creates backup copies

#### **File Status Indicators**
- **Clean**: No indicator - file is saved
- **Modified**: * symbol in tab title
- **Error**: Red underline for syntax errors
- **Compiled**: Green indicator when successfully compiled

---

## Prolog Engine Integration

### Engine Architecture

JProlog Editor integrates seamlessly with the JProlog engine:

#### **Knowledge Base Management**
- **Automatic Loading**: Compiled predicates load into the knowledge base
- **Incremental Updates**: Changes are reflected in real-time
- **Conflict Resolution**: Handles predicate redefinition
- **Memory Management**: Efficient clause storage and retrieval

#### **Engine Lifecycle**
- **Initialization**: Engine starts with IDE launch
- **Project Switching**: Engine resets when changing projects
- **Cleanup**: Proper cleanup on IDE shutdown

### Compilation System

#### **Single File Compilation**
1. **Trigger**: **Prolog → Compile File** (F9) or toolbar button
2. **Process**:
   - Saves the file automatically
   - Parses the Prolog source code
   - Loads clauses into the knowledge base
   - Reports errors and warnings

#### **Project Compilation**
1. **Trigger**: **Prolog → Compile Project** (Ctrl+F9)
2. **Process**:
   - Saves all open files
   - Clears existing knowledge base
   - Compiles all .pl files in the project
   - Provides compilation summary

#### **Error Handling**
- **Syntax Errors**: Highlighted in the editor with red underlines
- **Parse Errors**: Detailed error messages in Build tab
- **Line Numbers**: Errors linked to specific lines
- **Error Navigation**: Click errors to jump to problematic code

### Knowledge Base Operations

#### **Manual Management**
- **Clear KB**: **Prolog → Clear Knowledge Base**
- **KB Status**: **Prolog → Show KB Status**
- **Reset Engine**: Reinitialize the Prolog engine

#### **Automatic Management**
- **Smart Loading**: Only loads changed files when possible
- **Dependency Tracking**: Handles file dependencies
- **Conflict Detection**: Warns about predicate conflicts

---

## Query Execution

### Interactive Query Console

The Run tab provides an interactive Prolog query environment:

#### **Query Interface**
- **Prompt**: Standard Prolog `?-` prompt
- **Input Area**: Type queries directly
- **Execute**: Press Enter to execute queries
- **History**: Up/down arrows to navigate query history

#### **Query Execution Process**
1. **Type Query**: Enter Prolog query (e.g., `factorial(5, X).`)
2. **Press Enter**: Query executes against the knowledge base
3. **View Results**: Solutions appear in the console
4. **Multiple Solutions**: Press `;` for additional solutions
5. **Completion**: Press Enter to finish or `.` to stop

### Query Examples

#### **Simple Queries**
```prolog
?- factorial(5, X).
X = 120.

?- member(X, [1, 2, 3]).
X = 1 ;
X = 2 ;
X = 3 ;
false.
```

#### **Complex Queries**
```prolog
?- findall(X, between(1, 5, X), L).
L = [1, 2, 3, 4, 5].

?- append(L1, L2, [1, 2, 3, 4]).
L1 = [], L2 = [1, 2, 3, 4] ;
L1 = [1], L2 = [2, 3, 4] ;
L1 = [1, 2], L2 = [3, 4] ;
...
```

### Query Features

#### **Built-in Support**
- **Arithmetic**: Full arithmetic evaluation
- **List Operations**: append/3, member/2, length/2, etc.
- **Control Structures**: Cut (!), if-then-else (->)
- **Meta-predicates**: findall/3, bagof/3, setof/3

#### **Error Handling**
- **Syntax Errors**: Immediate feedback for malformed queries
- **Runtime Errors**: Clear error messages during execution
- **Stack Overflow**: Protection against infinite recursion
- **Type Errors**: Helpful messages for type mismatches

---

## Debugging System

*For complete debugging documentation, see the [Debugging Guide](DEBUGGING_GUIDE.md).*

### Debug Panel Overview

The Debug tab provides comprehensive debugging capabilities:

#### **Debug Controls**
- **🚀 Start Debug**: Begin debugging session
- **⏹️ Stop**: End debugging session
- **⬇️ Step Into**: Enter next predicate
- **➡️ Step Over**: Skip over current predicate
- **⬆️ Step Out**: Exit current predicate
- **▶️ Continue**: Run to next breakpoint
- **🔍 Trace**: Toggle detailed tracing

### Breakpoint Management

#### **Setting Breakpoints**
1. Open Debug tab
2. Click "Add" in Breakpoints section
3. Enter predicate/arity (e.g., `factorial/2`)
4. Breakpoint appears in the list

#### **Breakpoint Types**
- **Predicate/Arity**: `factorial/2`, `append/3`
- **Predicate Name**: `member`, `sort`
- **Line Numbers**: `line:25`

### Variable Monitoring

#### **Variables Table**
- **Real-time Updates**: Shows current variable bindings
- **Type Detection**: Automatically identifies data types
- **Value Display**: Clear representation of complex terms

#### **Supported Types**
- **Integer**: Whole numbers
- **Float**: Decimal numbers
- **List**: [1,2,3] structures
- **Compound**: f(a,b) terms
- **Atom**: Simple identifiers
- **Var**: Unbound variables

### Stack Trace Viewer

#### **Call Stack Display**
- **Hierarchical View**: Tree structure of function calls
- **Current Position**: Highlights current execution point
- **Navigation**: Click stack frames to inspect context
- **Recursion Tracking**: Visual representation of recursive calls

---

## Search and Navigation

### Find and Replace

#### **Quick Find** (Ctrl+F)
1. **Activate**: Press Ctrl+F or use Edit menu
2. **Search Field**: Appears at top of editor
3. **Navigation**: Use Enter/Shift+Enter for next/previous
4. **Options**: Case sensitive, whole word, regex

#### **Advanced Features**
- **Incremental Search**: Results update as you type
- **Highlight Matches**: All occurrences highlighted
- **Replace Mode**: Find and replace functionality
- **Escape**: Press Escape to close search panel

### Project-Wide Search

#### **Find in Project** (Ctrl+Shift+F)
1. **Activate**: Use keyboard shortcut or Edit menu
2. **Search Scope**: Entire project directory
3. **File Types**: Filters for .pl files
4. **Results**: Appear in Search tab

#### **Search Results**
- **File Grouping**: Results organized by file
- **Line Numbers**: Exact location of matches
- **Context**: Shows surrounding code
- **Navigation**: Double-click to jump to result

### Navigation Features

#### **Go to Line**
- **Quick Navigation**: Jump to specific line numbers
- **Bookmarks**: Set bookmarks for important locations
- **Recent Files**: Quick access to recently opened files

#### **Symbol Navigation**
- **Predicate List**: Predicates panel shows all predicates
- **Quick Jump**: Double-click to navigate to definition
- **Filter**: Type to filter predicate list

---

## Advanced Features

### Predicate Panel

#### **Predicate Discovery**
- **Automatic Parsing**: Scans open files for predicate definitions
- **Real-time Updates**: Updates as you type
- **Sorting**: Alphabetical sorting of predicates
- **Filtering**: Quick filter to find specific predicates

#### **Information Display**
- **Predicate Name**: Full predicate identifier
- **Arity**: Number of arguments
- **File Location**: Source file reference
- **Documentation**: Comments and documentation

### Syntax Support

#### **ISO Prolog Compatibility**
- **Standard Operators**: Full operator precedence
- **If-Then-Else**: `(condition -> then ; else)` constructs
- **Cut Operator**: Proper ! (cut) semantics
- **Comments**: Single-line (%) and multi-line (/* */)

#### **JProlog Extensions**
- **Enhanced Parsing**: Robust comment handling
- **Error Recovery**: Continues parsing after errors
- **Unicode Support**: Full Unicode character support
- **Line Continuation**: Multi-line clause support

### Build System

#### **Build Tab Features**
- **Real-time Output**: Live compilation feedback
- **Error Highlighting**: Errors shown in red
- **Success Messages**: Green text for successful operations
- **Build Summary**: Statistics and summary information

#### **Output Organization**
- **Timestamping**: All messages timestamped
- **Color Coding**: Different colors for different message types
- **Auto-scroll**: Automatically scrolls to show latest output
- **Clear Function**: Clear output for fresh builds

---

## Keyboard Shortcuts

### File Operations
| Shortcut | Action |
|----------|---------|
| Ctrl+N | New Project |
| Ctrl+O | Open Project |
| Ctrl+Shift+N | New File |
| Ctrl+S | Save File |
| Ctrl+Shift+S | Save All Files |

### Editing
| Shortcut | Action |
|----------|---------|
| Ctrl+F | Find |
| Ctrl+Shift+F | Find in Project |
| Ctrl+Z | Undo |
| Ctrl+Y | Redo |
| Ctrl+A | Select All |

### Prolog Operations
| Shortcut | Action |
|----------|---------|
| F9 | Compile File |
| Ctrl+F9 | Compile Project |
| F5 | Run Query |
| F8 | Toggle Trace/Debug |

### Navigation
| Shortcut | Action |
|----------|---------|
| Ctrl+Tab | Switch Between Tabs |
| Escape | Close Search Panel |
| Alt+F4 | Exit Application |

---

## Configuration

### IDE Settings

Configuration is stored in `.jprolog-ide.properties` in your home directory.

#### **Available Settings**
- **Project Locations**: Recent project paths
- **Editor Preferences**: Font size, tab width, theme
- **Compilation Options**: Build preferences
- **Debug Settings**: Debug panel configuration

#### **Manual Configuration**
Edit the configuration file directly:
```properties
# JProlog Editor from DenzoSOFT Configuration
editor.font.size=12
editor.tab.width=4
build.auto.save=true
debug.trace.enabled=false
```

### Workspace Management

#### **Project History**
- **Recent Projects**: Quick access to recently opened projects
- **Workspace Persistence**: IDE remembers open files and layout
- **Session Restore**: Restore previous session on startup

#### **Customization Options**
- **Window Layout**: Adjustable split pane positions
- **Tab Preferences**: Tab placement and behavior
- **Color Themes**: Editor color schemes
- **Font Settings**: Customizable fonts and sizes

---

## Troubleshooting

### Common Issues

#### **IDE Won't Start**
**Problem**: JProlog Editor fails to launch
**Solutions**:
- Check Java version (requires Java 8+)
- Verify classpath: `java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE`
- Check console for error messages
- Try: `mvn clean compile` first

#### **Compilation Errors**
**Problem**: Files won't compile successfully
**Solutions**:
- Check syntax errors in the editor
- Review error messages in Build tab
- Ensure proper Prolog syntax
- Try compiling individual files first

#### **Debug Panel Not Working**
**Problem**: Debug features not responding
**Solutions**:
- Start debug session first (🚀 Start Debug)
- Ensure project is compiled successfully
- Check Debug tab is selected
- Try F8 to toggle trace mode

#### **Query Execution Fails**
**Problem**: Queries don't execute properly
**Solutions**:
- Compile project first (Ctrl+F9)
- Check knowledge base status
- Verify query syntax
- Try simple queries first (e.g., `?- true.`)

### Performance Issues

#### **Large Projects**
**Problem**: IDE slow with large projects
**Solutions**:
- Compile files individually instead of entire project
- Close unused tabs
- Clear knowledge base periodically
- Restart IDE for fresh memory state

#### **Memory Issues**
**Problem**: Out of memory errors
**Solutions**:
- Increase Java heap size: `java -Xmx1G -cp ...`
- Close unnecessary files
- Clear compilation output regularly
- Restart IDE periodically

### Error Messages

#### **"No project open"**
**Solution**: Create or open a project before creating files

#### **"Please compile the project first"**
**Solution**: Use Compile Project (Ctrl+F9) before running queries

#### **"Debug panel not ready"**
**Solution**: Wait for IDE to fully initialize, then try again

#### **"Knowledge base empty"**
**Solution**: Compile your Prolog files to load predicates

### Getting Help

#### **Documentation**
- **User Manual**: This document
- **Debug Guide**: [DEBUGGING_GUIDE.md](DEBUGGING_GUIDE.md)
- **Example Files**: Check `debug_test.pl` for examples

#### **Support Resources**
- **DenzoSOFT Website**: https://denzosoft.it
- **Issue Reporting**: Use IDE's error reporting features
- **Community Forums**: Check for community discussions
- **Contact Support**: For complex technical issues

---

## Quick Start Guide

### 5-Minute Setup

1. **Launch IDE**: 
   ```bash
   java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
   ```

2. **Create Project**:
   - File → New Project
   - Choose directory and name
   - IDE creates project with sample files

3. **Edit Code**:
   - Double-click `main.pl` in project tree
   - Modify or add Prolog predicates
   - Save with Ctrl+S

4. **Compile**:
   - Press Ctrl+F9 or use Compile Project
   - Check Build tab for results

5. **Run Queries**:
   - Go to Run tab
   - Type queries like `?- factorial(5, X).`
   - Press Enter to execute

6. **Debug** (Optional):
   - Press F8 to start debugging
   - Set breakpoints in Debug tab
   - Use step controls to trace execution

### Example Session

```prolog
% Create new project "MyFirstProject"
% Edit main.pl:

factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Compile (Ctrl+F9)
% Run query: ?- factorial(5, X).
% Result: X = 120.
```

---

**JProlog Editor from DenzoSOFT** - Complete Integrated Development Environment for Prolog Programming

*Version 2.0.6 | DenzoSOFT | https://denzosoft.it*

---

*This manual covers all features and functionality of JProlog Editor from DenzoSOFT. For debugging-specific information, refer to the [Debugging Guide](DEBUGGING_GUIDE.md). For additional support, visit the DenzoSOFT website.*