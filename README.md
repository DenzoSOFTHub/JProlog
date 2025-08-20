# JProlog Editor from DenzoSOFT

**A Complete Integrated Development Environment for Prolog Programming**

## Overview

JProlog Editor from DenzoSOFT is a comprehensive IDE specifically designed for Prolog development. It provides a complete development workflow with project management, intelligent editing, compilation, debugging, and query execution capabilities.

## ✨ Key Features

### 🗂️ Project Management
- **Project Organization**: Structured project management with directory trees
- **File Operations**: Create, open, save, and manage Prolog files
- **Multi-file Projects**: Support for complex projects with multiple .pl files

### 📝 Advanced Editor
- **Syntax Highlighting**: Full Prolog syntax highlighting with keywords, operators, and comments
- **Line Numbering**: Always-visible line numbers with scroll synchronization
- **Multi-tab Interface**: Work with multiple files simultaneously
- **Smart Editing**: Auto-indentation, bracket matching, and intelligent text editing

### 🔧 Compilation System
- **Real-time Compilation**: Compile individual files or entire projects
- **Error Detection**: Advanced error reporting with line-specific feedback
- **Knowledge Base Management**: Intelligent loading and management of Prolog clauses
- **Build Feedback**: Comprehensive compilation output with success/error indicators

### 🏃 Query Execution
- **Interactive Console**: Full-featured Prolog query execution environment
- **Query History**: Navigate through previous queries with arrow keys
- **Multiple Solutions**: Support for backtracking and multiple solution exploration
- **Built-in Predicates**: Complete support for standard Prolog predicates

### 🐛 Complete Debug System
- **Visual Debugger**: Professional debugging interface with breakpoints and step execution
- **Variable Monitoring**: Real-time variable watching with type detection
- **Stack Trace Viewer**: Hierarchical call stack visualization
- **Trace Mode**: Comprehensive execution tracing with detailed logging
- **Step Controls**: Step Into, Step Over, Step Out, and Continue operations

### 🔍 Search & Navigation
- **Find & Replace**: Advanced search with regex support
- **Project-wide Search**: Search across all files in the project
- **Quick Navigation**: Jump to predicates, lines, and definitions
- **Predicate Panel**: Automatic predicate discovery and navigation

## 🚀 Quick Start

### Prerequisites
- Java 8 or higher
- Maven (for building)

### Installation & Launch

1. **Compile the project**:
   ```bash
   mvn compile
   ```

2. **Launch the IDE**:
   ```bash
   java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
   ```

3. **Alternative launch script**:
   ```bash
   ./start-ide.sh
   ```

### Create Your First Project

1. **New Project**: File → New Project... (Ctrl+N)
2. **Select Directory**: Choose the directory that becomes your project root
3. **Automatic Setup**: Directory name becomes the project name
4. **Start Coding**: Edit the `main.pl` file (created if directory is empty)

### Example Code

```prolog
% Factorial predicate
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Test with: ?- factorial(5, X).
```

### Compile and Run

1. **Compile**: Press Ctrl+F9 or use "Compile Project"
2. **Run Query**: Go to Run tab and type `?- factorial(5, X).`
3. **See Result**: `X = 120`

## 📚 Documentation

### Complete Guides
- **[User Manual](USER_MANUAL.md)**: Complete IDE documentation
- **[Debugging Guide](DEBUGGING_GUIDE.md)**: Comprehensive debugging tutorial
- **[Quick Start Guide](QUICK_START.md)**: Get started in 5 minutes
- **[Debug Features Summary](DEBUG_FEATURES_SUMMARY.md)**: Debug system overview

### Key Shortcuts
| Action | Shortcut |
|--------|----------|
| New Project | Ctrl+N |
| Compile Project | Ctrl+F9 |
| Run Query | F5 |
| Toggle Debug | F8 |
| Find in File | Ctrl+F |
| Find in Project | Ctrl+Shift+F |

## 🛠️ Advanced Features

### ISO Prolog Compatibility
- **Standard Operators**: Full operator precedence support
- **If-Then-Else**: `(condition -> then ; else)` constructs
- **Cut Operator**: Proper `!` (cut) semantics and backtracking control
- **Comments**: Both single-line `%` and multi-line `/* */` comments

### Debug System Highlights
- **Professional Interface**: Modern debug panel with intuitive controls
- **Breakpoint Management**: Set breakpoints on predicates with predicate/arity syntax
- **Real-time Monitoring**: Watch variables change during execution
- **Stack Visualization**: See the complete call hierarchy as a tree
- **Trace Integration**: Detailed execution logging with F8 toggle

### Search Capabilities
- **Multi-mode Search**: Find in current file or across entire project
- **Regular Expressions**: Advanced pattern matching support
- **Incremental Search**: Results update as you type
- **Context Display**: See surrounding code for search results

## 🗂️ Project Structure

```
JProlog/
├── src/main/java/it/denzosoft/jprolog/
│   ├── core/                    # Core Prolog engine
│   └── editor/                  # IDE components
│       ├── PrologIDE.java       # Main IDE class
│       ├── DebugPanel.java      # Debug interface
│       ├── FileEditor.java      # Editor component
│       └── ...
├── USER_MANUAL.md               # Complete user guide
├── DEBUGGING_GUIDE.md           # Debug tutorial
├── QUICK_START.md              # 5-minute setup
└── README.md                   # This file
```

## 🧪 Testing

### Debug Testing
```bash
# Test debug features
./test-debug.sh

# Or use the test class
java TestDebugFeatures
```

### Example Files
- `debug_test.pl`: Sample Prolog predicates for testing
- `debug_examples.md`: Debug usage examples

## 🔧 Configuration

Settings are stored in `~/.jprolog-ide.properties`:
```properties
editor.font.size=12
editor.tab.width=4
build.auto.save=true
debug.trace.enabled=false
```

## 🤝 Development

### Building from Source
```bash
git clone [repository-url]
cd JProlog
mvn clean compile
```

### IDE Architecture
- **Modular Design**: Separate components for editing, compilation, debugging
- **Swing-based UI**: Professional desktop application interface
- **Event-driven**: Responsive user interface with proper event handling
- **Extensible**: Clean architecture for adding new features

## 📋 System Requirements

- **Operating System**: Windows, macOS, or Linux
- **Java Runtime**: Java 8 or higher
- **Memory**: 512MB RAM (1GB recommended)
- **Disk Space**: 100MB available space

## 🐛 Troubleshooting

### Common Issues
- **IDE won't start**: Check Java version and classpath
- **Compilation errors**: Review syntax and check Build tab
- **Debug not working**: Ensure project is compiled first
- **Queries fail**: Verify knowledge base is loaded

### Getting Help
- Check the [User Manual](USER_MANUAL.md) for detailed instructions
- See [Debugging Guide](DEBUGGING_GUIDE.md) for debug-specific help
- Review [Quick Start](QUICK_START.md) for basic setup

## 📄 License

JProlog Editor from DenzoSOFT - Integrated Development Environment for Prolog

Copyright © 2024 DenzoSOFT. All rights reserved.

## 🌐 Contact

- **Website**: https://denzosoft.it
- **Support**: For technical support and feature requests
- **Version**: 1.0

---

**Start your Prolog development journey with JProlog Editor from DenzoSOFT today!**

*Professional Prolog Development Made Easy*