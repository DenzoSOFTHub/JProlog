# Quick Start Guide - JProlog Editor from DenzoSOFT

## Get Up and Running in 5 Minutes

### 1. Launch the IDE
```bash
# Compile first (if needed)
mvn compile

# Launch the IDE
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE

# Or use the script
./start-ide.sh
```

### 2. Create Your First Project
1. **File → New Project...** (or Ctrl+N)
2. Select the directory that will become your project root
3. IDE uses the directory name as project name
4. IDE creates `main.pl` if the directory is empty

### 3. Write Your First Prolog Program
Edit `main.pl` with this simple factorial program:
```prolog
% Factorial predicate
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% List membership
member(X, [X|_]).
member(X, [_|T]) :-
    member(X, T).
```

### 4. Compile and Run
1. **Compile**: Press **Ctrl+F9** or click "Compile Project"
2. **Check Build**: Look at the "Build" tab for success messages
3. **Run Query**: Go to "Run" tab and type:
   ```prolog
   ?- factorial(5, X).
   ```
4. **Get Result**: Press Enter to see `X = 120`

### 5. Try More Queries
```prolog
?- factorial(6, F).          % F = 720
?- member(2, [1,2,3]).       % true
?- member(X, [a,b,c]).       % X = a ; X = b ; X = c
```

## Essential Features Tour

### Project Tree
- **Left Panel**: Shows all files in your project directory
- **Double-click**: Opens files in the editor
- **Right-click**: File operations menu
- **Auto-refresh**: Updates when files are added/removed

### Editor Features
- **Syntax Highlighting**: Prolog keywords, operators, comments
- **Line Numbers**: Click to navigate
- **Multiple Tabs**: Work with several files at once
- **Auto-save**: Ctrl+S to save, Ctrl+Shift+S to save all

### Bottom Panel Tabs
- **Output**: General IDE messages
- **Build**: Compilation results and errors
- **Run**: Interactive Prolog query console
- **Debug**: Advanced debugging tools (see Debug Guide)
- **Search**: Project-wide search results

## Quick Debug Session

### Enable Debugging
1. Press **F8** to start debug mode
2. Debug tab opens automatically
3. Click **"🚀 Start Debug"** if needed

### Set a Breakpoint
1. In Debug tab, click **"Add"** under Breakpoints
2. Enter: `factorial/2`
3. Breakpoint appears in the list

### Debug Your Code
1. Go to **Run** tab
2. Execute: `?- factorial(3, X).`
3. Execution stops at your breakpoint
4. Use **⬇️ Step Into** to trace through the code
5. Watch variables update in the Variables table

## Common Tasks

### File Operations
| Task | Shortcut | Menu |
|------|----------|------|
| New Project | Ctrl+N | File → New Project |
| New File | Ctrl+Shift+N | File → New File |
| Save | Ctrl+S | File → Save |
| Save All | Ctrl+Shift+S | File → Save All |

### Prolog Operations
| Task | Shortcut | Menu |
|------|----------|------|
| Compile File | F9 | Prolog → Compile File |
| Compile Project | Ctrl+F9 | Prolog → Compile Project |
| Run Query | F5 | Prolog → Run Query |
| Toggle Debug | F8 | Prolog → Toggle Trace |

### Search and Navigation
| Task | Shortcut | Action |
|------|----------|---------|
| Find in File | Ctrl+F | Search current file |
| Find in Project | Ctrl+Shift+F | Search entire project |
| Close Search | Escape | Hide search panel |

## Example Programs to Try

### 1. List Operations
```prolog
% Append two lists
append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).

% Length of a list
length([], 0).
length([_|T], N) :-
    length(T, N1),
    N is N1 + 1.

% Test queries:
% ?- append([1,2], [3,4], L).
% ?- length([a,b,c,d], N).
```

### 2. Family Relations
```prolog
% Facts
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Rules
grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).

sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% Test queries:
% ?- parent(tom, X).
% ?- grandparent(tom, Who).
% ?- sibling(ann, pat).
```

### 3. Mathematical Functions
```prolog
% Fibonacci sequence
fibonacci(0, 0) :- !.
fibonacci(1, 1) :- !.
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

% Sum of list
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Sum1),
    Sum is H + Sum1.

% Test queries:
% ?- fibonacci(8, F).
% ?- sum_list([1,2,3,4,5], S).
```

## Tips for Success

### 1. Start Simple
- Begin with basic facts and rules
- Test each predicate individually
- Build complexity gradually

### 2. Use the Tools
- **Compile frequently** to catch errors early
- **Use debugging** to understand execution flow
- **Search features** to navigate large projects

### 3. Organize Your Code
- **Group related predicates** together
- **Add comments** to explain complex logic
- **Use meaningful predicate names**

### 4. Test Thoroughly
- **Try edge cases** (empty lists, zero values)
- **Test with different inputs**
- **Use multiple queries** to verify behavior

## Getting Help

### Documentation
- **Full Manual**: [USER_MANUAL.md](USER_MANUAL.md)
- **Debug Guide**: [DEBUGGING_GUIDE.md](DEBUGGING_GUIDE.md)
- **Feature Summary**: [DEBUG_FEATURES_SUMMARY.md](DEBUG_FEATURES_SUMMARY.md)

### Common Issues
- **"No project open"**: Create or open a project first
- **"Please compile first"**: Run Compile Project (Ctrl+F9)
- **Syntax errors**: Check Build tab for detailed error messages
- **Queries fail**: Ensure predicates are compiled and loaded

### Next Steps
1. **Explore the Debug System**: Learn breakpoints, stepping, and variable monitoring
2. **Try Complex Examples**: Work with recursive predicates and data structures
3. **Read the Full Manual**: Understand all IDE features and capabilities
4. **Build Real Projects**: Apply your knowledge to solve actual problems

---

**You're ready to start Prolog development with JProlog Editor from DenzoSOFT!**

*For complete documentation, see [USER_MANUAL.md](USER_MANUAL.md)*
*For debugging help, see [DEBUGGING_GUIDE.md](DEBUGGING_GUIDE.md)*

---

*JProlog Editor from DenzoSOFT - Version 2.0.6 | https://denzosoft.it*