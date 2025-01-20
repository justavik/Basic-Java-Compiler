# Java Subset Compiler

A modern compiler implementation for a subset of Java, written in Java. This compiler performs lexical analysis, parsing, semantic analysis, intermediate code generation, optimization, and generates JVM bytecode.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Features

- Complete compilation pipeline from source code to JVM bytecode
- Lexical analysis with support for Java tokens and keywords
- Recursive descent parser generating an Abstract Syntax Tree (AST)
- Semantic analysis with symbol table and type checking
- Three-address code intermediate representation
- Basic optimization passes
- JVM bytecode generation using ASM
- Support for basic Java constructs:
  - Class declarations
  - Method declarations with parameters
  - Variable declarations and assignments
  - Basic arithmetic operations
  - Method invocations
  - System.out.println support
  - Basic type checking
  - Scanner input handling

## Project Structure

```
.
├── src/
│   └── com/
│       └── compiler/
│           ├── JavaLexer.java         # Lexical analyzer
│           ├── JavaParser.java        # Recursive descent parser
│           ├── SemanticAnalyzer.java  # Type checking and semantic analysis
│           ├── ASTPrinter.java        # AST visualization
│           ├── IntermediateCodeGenerator.java  # IR generation
│           ├── BasicOptimizer.java    # Optimization passes
│           ├── BytecodeGenerator.java # JVM bytecode generation
│           └── Main.java             # Compiler driver
├── lib/
│   └── asm-9.5.jar                   # ASM library for bytecode generation
└── build/                            # Compiled classes
```

## Prerequisites

- Java Development Kit (JDK) 17 or higher
- ASM 9.5 library (included in lib/)
- A Java IDE (recommended: IntelliJ IDEA or Eclipse)

## Building the Project

1. Clone the repository:
```bash
git clone https://github.com/yourusername/java-subset-compiler.git
cd java-subset-compiler
```

2. Compile the project:
```bash
javac -cp "lib/*" src/com/compiler/*.java -d build/
```

## Usage

The compiler can be used to compile Java source files that conform to the supported subset of Java. Here's an example:

```java
import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        Scanner ac = new Scanner(System.in);
        int a = ac.nextInt();
        int b = ac.nextInt();
        int c = a + b;
        System.out.println("Output = " + c);
        ac.close();
    }
}
```

To compile and run:

```bash
# Compile the source file
java -cp "build:lib/*" com.compiler.Main Test.java

# Run the compiled program
java Main
```

## Compiler Phases

### 1. Lexical Analysis
The `JavaLexer` class performs lexical analysis, converting the input source code into a stream of tokens. It supports:
- Keywords
- Identifiers
- Literals (integers and strings)
- Operators
- Delimiters

### 2. Parsing
The `JavaParser` implements a recursive descent parser that:
- Constructs an Abstract Syntax Tree (AST)
- Handles class declarations, methods, statements, and expressions
- Provides detailed error reporting

### 3. Semantic Analysis
The `SemanticAnalyzer` performs:
- Symbol table management
- Type checking
- Scope resolution
- Variable declaration checks
- Method call validation

### 4. Intermediate Code Generation
The `IntermediateCodeGenerator` produces three-address code intermediate representation for:
- Arithmetic operations
- Method calls
- Variable assignments
- Control flow

### 5. Optimization
The `BasicOptimizer` implements:
- Constant folding
- Dead code elimination
- Simple algebraic simplifications

### 6. Bytecode Generation
The `BytecodeGenerator` uses ASM to:
- Generate JVM bytecode
- Handle method compilation
- Manage stack operations
- Generate proper class files

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## Future Improvements

- [ ] Add support for if/else statements
- [ ] Implement for and while loops
- [ ] Add array support
- [ ] Implement more optimization passes
- [ ] Add support for classes and objects
- [ ] Implement function overloading
- [ ] Add support for interfaces

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- ASM library for bytecode generation
- The Dragon Book for compiler design principles
- Java Language Specification
