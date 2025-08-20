# Debugging Guide - JProlog Editor from DenzoSOFT

## Table of Contents
1. [Overview](#overview)
2. [Debug Panel Layout](#debug-panel-layout)
3. [Starting a Debug Session](#starting-a-debug-session)
4. [Using Breakpoints](#using-breakpoints)
5. [Trace Mode](#trace-mode)
6. [Step-by-Step Debugging](#step-by-step-debugging)
7. [Variables Monitoring](#variables-monitoring)
8. [Stack Trace Analysis](#stack-trace-analysis)
9. [Debug Output Console](#debug-output-console)
10. [Practical Examples](#practical-examples)
11. [Best Practices](#best-practices)
12. [Troubleshooting](#troubleshooting)

## Overview

The JProlog Editor from DenzoSOFT provides a comprehensive debugging environment for Prolog programs. The integrated debugger allows you to:

- **Step through code execution** line by line
- **Set breakpoints** on predicates or specific rules
- **Monitor variables** and their bindings in real-time
- **Trace program execution** with detailed logging
- **Analyze the call stack** to understand program flow
- **Debug complex recursive predicates** effectively

The debugger is fully integrated into the IDE and provides both novice-friendly and advanced debugging features.

## Debug Panel Layout

The Debug panel is located in the bottom tabbed pane of the IDE. It consists of four main areas:

### 1. **Toolbar Controls** (Top)
- 🚀 **Start Debug**: Begin a new debugging session
- ⏹️ **Stop**: End the current debugging session
- ⬇️ **Step Into**: Enter the next goal/predicate
- ➡️ **Step Over**: Execute the current goal without entering sub-goals
- ⬆️ **Step Out**: Exit the current predicate and return to caller
- ▶️ **Continue**: Resume execution until next breakpoint
- 🔍 **Trace**: Toggle trace mode on/off

### 2. **Stack Trace Viewer** (Left Panel)
- **Hierarchical view** of the current execution stack
- Shows the **call chain** from main goal to current predicate
- **Expandable tree structure** for detailed inspection

### 3. **Variables Watch Table** (Left Panel, Bottom)
- **Real-time display** of variable bindings
- **Columns**: Variable Name, Current Value, Data Type
- **Automatic type detection**: Integer, Float, List, Compound, Atom, Var

### 4. **Breakpoints Manager** (Right Panel, Top)
- **List of active breakpoints** with predicate names
- **Add/Remove/Clear** breakpoint controls
- **Interactive management** of debugging stops

### 5. **Debug Output Console** (Right Panel, Bottom)
- **Detailed trace information** and debug messages
- **Execution flow** with timestamps and status
- **Error messages** and debugging feedback

## Starting a Debug Session

### Method 1: Using the Debug Panel
1. Open your Prolog file in the editor
2. Click on the **"Debug"** tab in the bottom panel
3. Click the **🚀 Start Debug** button
4. The session status will show **"Debug (Active)"**

### Method 2: Using the Menu
1. Go to **Prolog → Toggle Trace** (or press **F8**)
2. This automatically opens the Debug panel and starts a session

### Method 3: Using Keyboard Shortcut
- Press **F8** to quickly toggle trace mode and start debugging

Once a debug session is active, you'll see:
- Debug controls become **enabled** (buttons are no longer grayed out)
- Debug tab title changes to **"Debug (Active)"**
- Ready message in the debug output console

## Using Breakpoints

Breakpoints allow you to pause execution at specific predicates or rules.

### Adding Breakpoints
1. In the **Breakpoints** section, click **"Add"**
2. Enter the breakpoint in one of these formats:
   - **Predicate/Arity**: `factorial/2`, `append/3`
   - **Predicate name**: `member`, `sort`
   - **Line reference**: `line:25` (for specific line numbers)

### Examples of Valid Breakpoints
```prolog
factorial/2     % Stop when factorial/2 is called
append/3        % Stop when append/3 is called  
member/2        % Stop when member/2 is called
process_list/3  % Stop when process_list/3 is called
```

### Managing Breakpoints
- **Remove**: Select a breakpoint and click **"Remove"**
- **Clear All**: Remove all breakpoints at once
- **Status**: Active breakpoints are shown in the list

### Breakpoint Behavior
When execution hits a breakpoint:
1. **Execution pauses** at the specified predicate
2. **Current state** is displayed in variables and stack panels
3. **Step controls** become available for fine-grained debugging
4. **Debug output** shows the breakpoint hit message

## Trace Mode

Trace mode provides **comprehensive logging** of Prolog execution, showing every step of the inference process.

### Enabling Trace Mode
1. **Debug Panel**: Click the **🔍 Trace** button (toggle button)
2. **Menu**: Use **Prolog → Toggle Trace** (F8)
3. **Automatic**: Trace can be enabled when starting a debug session

### What Trace Mode Shows
When trace is active, you'll see detailed information about:

- **Goal calls**: When predicates are invoked
- **Unification attempts**: Variable binding processes
- **Backtracking**: When the system backtracks to find alternatives
- **Success/Failure**: Results of goal attempts
- **Cut operations**: When cuts (!) prevent backtracking

### Trace Output Format
```
🔍 Trace mode ON
Call: factorial(5, F)
  Call: factorial(4, F1)  
    Call: factorial(3, F2)
      Call: factorial(2, F3)
        Call: factorial(1, F4)
          Call: factorial(0, 1) - Success
        Exit: factorial(1, 1) 
      Exit: factorial(2, 2)
    Exit: factorial(3, 6)
  Exit: factorial(4, 24)
Exit: factorial(5, 120)
```

### Trace vs Debug Mode
- **Trace Mode**: Shows **all execution steps** automatically
- **Debug Mode**: Provides **interactive control** with breakpoints and stepping
- **Combined**: Use both for **maximum debugging power**

### Controlling Trace Output
- **Enable**: Click 🔍 Trace or use F8
- **Disable**: Click 🔍 Trace again to turn off
- **Clear**: Debug output can be cleared for fresh trace sessions

## Step-by-Step Debugging

Step debugging gives you **precise control** over program execution.

### Step Controls

#### ⬇️ **Step Into**
- **Purpose**: Enter the next predicate call
- **Use case**: When you want to debug inside a predicate
- **Example**: If you're at `factorial(5, F)`, step into will enter the factorial predicate

#### ➡️ **Step Over**  
- **Purpose**: Execute the current goal without entering sub-predicates
- **Use case**: When you want to skip detailed execution of a known-working predicate
- **Example**: Step over a `write/1` call to avoid tracing output operations

#### ⬆️ **Step Out**
- **Purpose**: Exit the current predicate and return to the caller
- **Use case**: When you've seen enough detail in the current predicate
- **Example**: Step out of a recursive call to return to the parent level

#### ▶️ **Continue**
- **Purpose**: Resume normal execution until the next breakpoint
- **Use case**: When you want to run until a specific point
- **Example**: Continue from current breakpoint to the next one

### Step Debugging Workflow
1. **Set breakpoints** at key predicates
2. **Run your query** in the Run tab
3. **Execution pauses** at first breakpoint
4. **Use step controls** to move through code
5. **Monitor variables** and stack in real-time
6. **Continue or step** as needed

### Example Step Session
```prolog
% Query: ?- factorial(3, F).

Step 1: Hit breakpoint at factorial/2
  Variables: N=3, F=_1
  Stack: factorial(3, _1)

Step 2: Step Into -> N > 0 check
  Variables: N=3, F=_1
  
Step 3: Step Into -> N1 is N - 1
  Variables: N=3, N1=2, F=_1
  
Step 4: Step Into -> recursive call factorial(2, F1)
  Variables: N=3, N1=2, F1=_2, F=_1
  Stack: factorial(3, _1) -> factorial(2, _2)
```

## Variables Monitoring

The Variables Watch Table provides **real-time monitoring** of variable bindings during execution.

### Table Columns
- **Variable**: The variable name (e.g., `N`, `F`, `List`)
- **Value**: Current binding or value (e.g., `5`, `[1,2,3]`, `_unbound`)  
- **Type**: Detected data type (Integer, Float, List, Compound, Atom, Var)

### Variable Types
The debugger automatically detects and displays these types:

#### **Integer**
```prolog
N = 5          (Type: Integer)
Count = 42     (Type: Integer)
```

#### **Float**  
```prolog
Pi = 3.14159   (Type: Float)
Result = 2.5   (Type: Float)
```

#### **List**
```prolog
L = [1,2,3,4]       (Type: List)
Empty = []          (Type: List)  
Tail = [2,3,4]      (Type: List)
```

#### **Compound**
```prolog
Term = f(a,b,c)     (Type: Compound)
Point = point(1,2)  (Type: Compound)
```

#### **Atom**
```prolog
Name = john         (Type: Atom)
Status = success    (Type: Atom)
```

#### **Var** (Unbound Variable)
```prolog
X = _1             (Type: Var)
Result = _unbound  (Type: Var)
```

### Variable Monitoring Features
- **Automatic updates** as execution progresses
- **Type detection** changes dynamically as variables are bound
- **Clear display** of complex terms and structures
- **Persistent view** throughout debugging session

## Stack Trace Analysis

The Stack Trace Viewer shows the **call hierarchy** and execution context.

### Understanding the Stack Display
The stack is shown as a **tree structure** with:
- **Root**: "Execution Stack" 
- **Nodes**: Each active predicate call
- **Hierarchy**: Parent-child relationships between calls

### Example Stack Trace
```
Execution Stack
├── main_query
│   └── factorial(5, F)
│       └── factorial(4, F1)  
│           └── factorial(3, F2)
│               └── factorial(2, F3) ← Current
```

### Stack Information
Each stack frame shows:
- **Predicate name** and arity
- **Current arguments** and their bindings
- **Call depth** in the execution tree
- **Active status** (current execution point)

### Using Stack Analysis
1. **Navigate calls**: Click on stack frames to see context
2. **Understand recursion**: See recursive call patterns
3. **Debug infinite loops**: Detect runaway recursion
4. **Trace execution flow**: Follow the call sequence

## Debug Output Console

The Debug Output Console provides **detailed logging** of all debug activities.

### Types of Output Messages

#### **Session Messages**
```
🚀 Debug session started
Set breakpoints and run queries to begin debugging.

⏹️ Debug session stopped
```

#### **Breakpoint Messages**
```
➕ Added breakpoint: factorial/2
❌ Breakpoint hit: factorial(5, F)
➖ Removed breakpoint: append/3
```

#### **Trace Messages** (when trace is enabled)
```
🔍 Trace mode ON
Call: factorial(5, F)
  Unify: N = 5, F = _1
  Call: N > 0 - Success
  Call: N1 is N - 1
  Unify: N1 = 4
Exit: factorial(5, 120)
```

#### **Step Messages**
```
⬇️ Step Into
Current: factorial(3, F2)
Variables: N=3, F2=_3

➡️ Step Over  
Executed: N1 is N - 1
Result: N1 = 2
```

#### **Error Messages**
```
❌ Error: Breakpoint not found: invalid_predicate/2
⚠️ Warning: Could not enable Prolog trace
```

### Console Features
- **Auto-scroll**: Automatically scrolls to show latest messages
- **Timestamped**: Messages appear in chronological order
- **Formatted**: Uses emojis and formatting for clarity
- **Persistent**: Messages remain for the duration of the session

## Practical Examples

### Example 1: Debugging Factorial

**File: factorial_debug.pl**
```prolog
% Factorial with potential bug
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.
```

**Debug Session:**
1. Set breakpoint: `factorial/2`
2. Run query: `?- factorial(4, F).`
3. Use **Step Into** to trace through recursion
4. Monitor variables: `N`, `F`, `N1`, `F1`
5. Watch stack build up with recursive calls

**Expected Flow:**
```
Call: factorial(4, F)     Variables: N=4, F=_1
Call: factorial(3, F1)    Variables: N=3, F1=_2  
Call: factorial(2, F2)    Variables: N=2, F2=_3
Call: factorial(1, F3)    Variables: N=1, F3=_4
Call: factorial(0, 1)     Variables: N=0, Result=1
```

### Example 2: Debugging List Operations

**File: list_debug.pl**
```prolog
% List append with debugging
append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).

% List membership  
member(X, [X|_]).
member(X, [_|T]) :-
    member(X, T).
```

**Debug Session:**
1. Set breakpoints: `append/3`, `member/2`
2. Run query: `?- append([1,2], [3,4], L), member(3, L).`
3. Use **Step Over** for known predicates
4. Use **Step Into** for detailed list processing
5. Monitor list construction in variables table

### Example 3: Debugging Complex Recursion

**File: complex_debug.pl**
```prolog
% Process list with accumulator
process_list([], Acc, Acc).
process_list([H|T], Acc, Result) :-
    NewAcc is Acc + H * 2,
    process_list(T, NewAcc, Result).
```

**Debug Session:**
1. Enable **Trace Mode** for full visibility
2. Set breakpoint: `process_list/3`
3. Run query: `?- process_list([1,2,3], 0, R).`
4. Watch accumulator changes in variables
5. Use **Continue** to skip repetitive steps

## Best Practices

### Setting Effective Breakpoints
- **Start broad**: Set breakpoints on main predicates first
- **Narrow down**: Add specific breakpoints as needed
- **Use arity**: Specify predicate/arity to avoid ambiguity
- **Key points**: Set breakpoints at decision points and recursive calls

### Using Trace Mode Effectively
- **Enable early**: Turn on trace before complex operations
- **Selective use**: Disable for known-working code sections
- **Combine with stepping**: Use trace + stepping for detailed analysis
- **Output management**: Clear output between trace sessions

### Debugging Strategies
- **Top-down**: Start with main goals and drill down
- **Bottom-up**: Test individual predicates first
- **Isolation**: Debug problem areas in isolation
- **Incremental**: Fix one issue at a time

### Variable Monitoring
- **Watch key variables**: Focus on variables that change
- **Understand types**: Pay attention to type changes
- **List structures**: Carefully monitor list head/tail operations
- **Unification**: Watch variable binding during unification

### Performance Considerations
- **Selective debugging**: Don't debug everything at once
- **Breakpoint management**: Remove unused breakpoints
- **Trace control**: Turn off trace when not needed
- **Session cleanup**: Stop debug sessions when done

## Troubleshooting

### Common Issues

#### **Debug Session Won't Start**
- **Check**: Ensure a Prolog file is open
- **Solution**: Open a .pl file before starting debug
- **Alternative**: Use Menu → Prolog → Toggle Trace

#### **Breakpoints Not Triggering**
- **Check**: Verify predicate name and arity
- **Solution**: Use exact predicate syntax: `predicate/arity`
- **Debug**: Run a simple query to test breakpoint

#### **Variables Not Showing**
- **Cause**: Execution not paused at breakpoint
- **Solution**: Ensure breakpoint is hit and execution is paused
- **Check**: Verify query actually calls the predicate

#### **Trace Output Too Verbose**
- **Solution**: Use Step debugging instead of full trace
- **Alternative**: Set specific breakpoints to reduce output
- **Management**: Clear output console regularly

#### **Step Controls Not Working**
- **Check**: Ensure debug session is active
- **Requirement**: Must be paused at a breakpoint
- **Solution**: Start debug session and set breakpoints first

### Error Messages

#### **"Breakpoint not found"**
- **Cause**: Invalid predicate name or arity
- **Solution**: Check predicate spelling and arity
- **Format**: Use `predicate/number` format

#### **"Could not enable Prolog trace"**
- **Cause**: Communication issue with Prolog engine
- **Solution**: Restart debug session
- **Workaround**: Use manual trace commands in Run tab

#### **"Debug panel not ready"**  
- **Cause**: IDE initialization issue
- **Solution**: Wait for complete IDE loading
- **Retry**: Try starting debug session again

### Getting Help
- **Documentation**: Reference this guide for detailed instructions
- **Examples**: Use provided test files for learning
- **Community**: Check DenzoSOFT documentation and forums
- **Support**: Contact support for complex debugging issues

---

## Quick Reference Card

### Keyboard Shortcuts
- **F8**: Toggle Trace Mode
- **F5**: Run Query (when in Run tab)

### Debug Controls
- 🚀 **Start**: Begin debug session
- ⏹️ **Stop**: End debug session  
- ⬇️ **Step Into**: Enter next predicate
- ➡️ **Step Over**: Skip over current predicate
- ⬆️ **Step Out**: Exit current predicate
- ▶️ **Continue**: Run to next breakpoint
- 🔍 **Trace**: Toggle detailed tracing

### Breakpoint Syntax
- `predicate/arity` (e.g., `factorial/2`)
- `predicate_name` (e.g., `member`)
- Line numbers: `line:25`

### Variable Types
- **Integer**: Whole numbers
- **Float**: Decimal numbers  
- **List**: `[1,2,3]` structures
- **Compound**: `f(a,b)` terms
- **Atom**: Simple identifiers
- **Var**: Unbound variables

---

*This guide covers the complete debugging functionality of JProlog Editor from DenzoSOFT. For additional help, visit: https://denzosoft.it*