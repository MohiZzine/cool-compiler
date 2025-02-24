# cool-student Compiler Report

Welcome to **cool-student** – our complete COOL compiler project. This compiler implements all phases from lexical analysis to binary generation using LLVM. The project is implemented in Go and is designed to support the COOL (Classroom Object-Oriented Language) as defined by its reference manual.

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Project Structure](#project-structure)
3. [Context-Free Grammar](#context-free-grammar-cfg)
4. [Compilation Phases](#compilation-phases)
   - [Lexical Analysis](#lexical-analysis)
   - [Parsing](#parsing)
   - [Semantic Analysis](#semantic-analysis)
   - [IR Generation and Method Generation](#ir-generation-and-method-generation)
   - [Optimizations and Binary Generation](#optimizations-and-binary-generation)
5. [Two‑Phase Method Generation](#two-phase-method-generation)
6. [LLVM IR Optimizations](#llvm-ir-optimizations)
7. [Makefile and Integration](#makefile-and-integration)

---

## Project Overview

**cool-student** is a COOL compiler built as a final project for the Compilers course (CI2 2024/2025). The main objectives are to:

- Deepen understanding of compiler design principles.
- Gain hands-on experience implementing various compiler phases using Go.
- Learn how to generate and optimize LLVM Intermediate Representation (IR).
- Extend the COOL language with additional features (e.g., array support, a standard library, and a module system).

The project implements core phases—from lexing to IR generation—and applies optimizations like constant propagation and dead code elimination.

---

## Project Structure

The repository is organized as follows:

```
├── ast/                  # AST definitions (ast.go)
├── lexer/                # Lexical analyzer (lexer.go) for tokenization and comment removal
├── parser/               # Recursive descent parser (parser.go) using Pratt parsing for operator precedence
├── semantic/             # Semantic analysis including symbol table construction (symbol_table.go, semantic.go)
├── irgen/                # LLVM IR generator (irGenerator.go) using llir/llvm to generate IR and build vtables
├── testcodes/            # Sample COOL programs for testing (e.g., input.cool, semantictest.cool)
├── README.md             # Overview and guidelines (this report)
├── main.go               # Compiler entry point orchestrating all phases
├── go.mod & go.sum       # Go module files listing dependencies
```

Each component contributes to transforming COOL source code into a final executable.

---

## Context-Free Grammar (CFG)

```
<program> ::= <class-list>

<class-list> ::= <class> <class-list>
               | <class>

<class> ::= "class" <type> [ "inherits" <type> ] "{" <feature-list> "}"

<feature-list> ::= <feature> <feature-list>
                 | ε

<feature> ::= <attribute>
            | <method>

<attribute> ::= <object-id> ":" <type> [ "<-" <expr> ] ";"

<method> ::= <object-id> "(" <formal-list> ")" ":" <type> "{" <expr> "}"

<formal-list> ::= <formal> "," <formal-list>
                | <formal>
                | ε

<formal> ::= <object-id> ":" <type>

<expr> ::= <object-id> "<-" <expr>
         | <expr> "@" <type> "." <object-id> "(" <expr-list> ")"
         | <object-id> "(" <expr-list> ")"
         | "if" <expr> "then" <expr> "else" <expr> "fi"
         | "while" <expr> "loop" <expr> "pool"
         | "let" <object-id> ":" <type> [ "<-" <expr> ] { "," <object-id> ":" <type> [ "<-" <expr> ] }* "in" <expr>
         | "new" <type>
         | <integer>
         | <string>
         | "true"
         | "false"

<expr-list> ::= <expr> "," <expr-list>
              | <expr>
              | ε
```

---

## Compilation Phases

### Lexical Analysis

- **Files:** `0x02 Compilers_ Lexical Analysis.pdf`, `lexer.go`
- **Overview:**  
  The lexer reads the COOL source code character by character, discards whitespace and comments, and produces a stream of tokens.  
- **Key Details:**  
  - **Comment Removal:** Single-line comments (starting with `//`) are skipped.
  - **Tokenization:** Functions such as `readIdentifier()`, `readNumber()`, and `readString()` construct tokens with type, literal, line, and column information.

### Parsing

- **Files:** `0x03 Compilers_ Syntax Analysis.pdf`, `0x03.2 Compilers_ Syntax Analysis - Cont.pdf`, `parser.go`
- **Overview:**  
  The parser ensures that the token stream conforms to COOL’s context-free grammar (CFG) and constructs an Abstract Syntax Tree (AST).  
- **Key Details:**  
  - **Recursive Descent Parsing:** The parser begins with the `<program>` nonterminal and processes classes, features, and expressions.
  - **Pratt Parsing:** Operator expressions (e.g., arithmetic, assignment) are parsed using methods such as `parseAssign()`, `parseAddSub()`, and `parseMulDiv()` to manage operator precedence and associativity.
  - **Error Handling:** Syntax errors are reported with precise line and column details.

### Semantic Analysis

- **Files:** `0x04 Compilers_ Semantic Analysis.pdf`, `0x04.2 Compilers_ Semantic Analysis - Cont.pdf`, `symbol_table.go`, `semantic.go`
- **Overview:**  
  Semantic analysis checks the AST for meaning and type correctness. It builds a symbol table representing all classes, attributes, and methods, and injects basic COOL objects into the symbol table.
- **Key Details:**  
  - **Symbol Table Construction:** Implemented in `symbol_table.go` using nested scopes.
  - **Type Checking:** Verifies type conformance, proper usage of identifiers, and correct inheritance.
  - **Inheritance Checks:** Uses depth-first search (DFS) to detect cycles and ensures that a valid `Main` class with a no-parameter `main()` method exists.

### IR Generation and Method Generation

- **Files:** `0x05 Compilers_ Intermediate Code Generation.pdf`, `irGenerator.go`, `0x07 LLVM.pdf`
- **Overview:**  
  The IR generator converts the AST into LLVM IR. This phase creates LLVM struct types for COOL classes and builds virtual tables (vtables) for method dispatch.

### Optimizations and Binary Generation

- **Files:** `0x06 Code Optimization.pdf`
- **Overview:**  
  The generated LLVM IR is optimized using standard LLVM passes before being compiled into an executable.

---

## Two‑Phase Method Generation

### Problem Addressed

During IR generation, vtables must be constructed from all method prototypes before generating method bodies. Without a two-phase approach, new object expressions might attempt to access a vtable that hasn’t been built yet, leading to nil pointer errors.

### Solution

1. **PrototypeMethods:**  
   - Iterates over classes to create function prototypes, registering them in a map.
2. **GenerateMethodBodies:**  
   - Uses the already-created prototypes to generate complete function bodies.

---

## LLVM IR Optimizations

### Constant Propagation

- **Overview:**  
  Precomputes expressions at compile time to reduce runtime computation.

### Dead Code Elimination

- **Overview:**  
  Removes redundant or unused code, improving performance.

---

## Makefile and Integration

A Makefile automates the compilation process:

```make
# Compile COOL source code into LLVM IR
%.ll: %.cl
	$(COMPILER) $< -o $@

# Apply optimization passes
%.opt.ll: %.ll
	$(OPT) -constprop -dce $< -o $@

# Generate assembly
%.s: %.opt.ll
	$(LLC) $< -o $@

# Link to create executable
%: %.s
	$(CC) $< -o $@

clean:
	rm -f *.ll *.opt.ll *.s $(TARGET)
```

