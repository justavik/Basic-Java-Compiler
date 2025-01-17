package com.compiler;

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class JavaCompiler {
    private final DiagnosticCollector diagnostics;
    
    public JavaCompiler() {
        this.diagnostics = new DiagnosticCollector();
    }
    
    public boolean compile(String sourceFile, String outputDir) {
        try {
            // Create output directory if it doesn't exist
            Files.createDirectories(Paths.get(outputDir));
            
            // Read source file
            String source = new String(Files.readAllBytes(Paths.get(sourceFile)));
            
            // Phase 1: Lexical Analysis
            JavaLexer lexer = new JavaLexer(source);
            
            // Phase 2: Parsing
            JavaParser parser = new JavaParser(lexer);
            CompilationUnit compilationUnit;
            
            try {
                compilationUnit = parser.parseCompilationUnit();
            } catch (ParserException e) {
                diagnostics.report(new CompilerDiagnostic(
                    DiagnosticType.ERROR,
                    e.getMessage(),
                    sourceFile,
                    e.line,
                    e.column
                ));
                return false;
            }
            
            // Phase 3: Semantic Analysis
            SemanticAnalyzer analyzer = new SemanticAnalyzer(diagnostics);
            if (!analyzer.analyze(compilationUnit)) {
                return false;
            }
            
            // Phase 4: Bytecode Generation
            BytecodeGenerator generator = new BytecodeGenerator(compilationUnit);
            byte[] bytecode;
            
            try {
                bytecode = generator.generate();
            } catch (BytecodeGenerationException e) {
                diagnostics.report(new CompilerDiagnostic(
                    DiagnosticType.ERROR,
                    e.getMessage(),
                    sourceFile,
                    e.line,
                    e.column
                ));
                return false;
            }
            
            // Write class file
            String className = compilationUnit.mainClass.name;
            Path outputPath = Paths.get(outputDir, className + ".class");
            Files.write(outputPath, bytecode);
            
            return true;
            
        } catch (IOException e) {
            diagnostics.report(new CompilerDiagnostic(
                DiagnosticType.ERROR,
                "I/O Error: " + e.getMessage(),
                sourceFile,
                0,
                0
            ));
            return false;
        }
    }
    
    public List<CompilerDiagnostic> getDiagnostics() {
        return diagnostics.getDiagnostics();
    }
}

// Semantic analyzer to perform basic type checking and symbol resolution
class SemanticAnalyzer {
    private final DiagnosticCollector diagnostics;
    private final Map<String, ClassSymbol> classes;
    private final Deque<Map<String, Symbol>> scopes;
    
    public SemanticAnalyzer(DiagnosticCollector diagnostics) {
        this.diagnostics = diagnostics;
        this.classes = new HashMap<>();
        this.scopes = new ArrayDeque<>();
        initializeBuiltinTypes();
    }
    
    private void initializeBuiltinTypes() {
        System.out.println("Initializing built-in types"); // Debug
        
        // Add basic types
        classes.put("int", new ClassSymbol("int", null));
        classes.put("void", new ClassSymbol("void", null));
        classes.put("String", new ClassSymbol("java.lang.String", null));
        
        // Add PrintStream with println method
        ClassSymbol printStream = new ClassSymbol("java.io.PrintStream", null);
        // Add method symbols if needed
        classes.put("PrintStream", printStream);
        
        // Add InputStream type
        ClassSymbol inputStream = new ClassSymbol("java.io.InputStream", null);
        classes.put("InputStream", inputStream);
        
        // Add Scanner class
        ClassSymbol scanner = new ClassSymbol("java.util.Scanner", null);
        classes.put("Scanner", scanner);
        
        // Add System class with static fields
        ClassSymbol system = new ClassSymbol("java.lang.System", null);
        system.addField(new FieldSymbol("out", printStream));
        system.addField(new FieldSymbol("in", inputStream));
        classes.put("System", system);
        
        // Add to global scope
        enterScope();
        getCurrentScope().put("System", system);
        
        System.out.println("Built-in types initialized"); // Debug
        System.out.println("Available classes: " + classes.keySet()); // Debug
    }
    
    public boolean analyze(CompilationUnit unit) {
        try {
            // Process imports
            for (ImportDeclaration imp : unit.imports) {
                processImport(imp);
            }
            
            // Analyze main class
            analyzeClass(unit.mainClass);
            
            return !diagnostics.hasErrors();
            
        } catch (SemanticException e) {
            diagnostics.report(new CompilerDiagnostic(
                DiagnosticType.ERROR,
                e.getMessage(),
                null,
                e.line,
                e.column
            ));
            return false;
        }
    }
    
    private void processImport(ImportDeclaration imp) {
        // For now, we just verify that imported types exist in our builtin types
        String[] parts = imp.qualifiedName.split("\\.");
        String className = parts[parts.length - 1];
        
        if (!classes.containsKey(className)) {
            throw new SemanticException(
                "Unknown type: " + imp.qualifiedName,
                imp.line,
                imp.column
            );
        }
    }
    
    private void analyzeClass(ClassDeclaration classDecl) {
        // Create class symbol
        ClassSymbol classSymbol = new ClassSymbol(classDecl.name, null);
        classes.put(classDecl.name, classSymbol);
        
        // Enter class scope
        enterScope();
        
        // Analyze fields
        for (FieldDeclaration field : classDecl.fields) {
            analyzeField(field);
        }
        
        // Analyze methods
        for (MethodDeclaration method : classDecl.methods) {
            analyzeMethod(method);
        }
        
        // Exit class scope
        exitScope();
    }
    
    private void analyzeField(FieldDeclaration field) {
        // Verify type exists
        if (!classes.containsKey(field.type.name)) {
            throw new SemanticException(
                "Unknown type: " + field.type.name,
                field.line,
                field.column
            );
        }
        
        // Add to current scope
        FieldSymbol symbol = new FieldSymbol(field.name, classes.get(field.type.name));
        getCurrentScope().put(field.name, symbol);
    }
    
    private void analyzeMethod(MethodDeclaration method) {
        // Enter method scope
        enterScope();
        
        // Add parameters to scope
        for (ParameterDeclaration param : method.parameters) {
            if (!classes.containsKey(param.type.name)) {
                throw new SemanticException(
                    "Unknown parameter type: " + param.type.name,
                    param.line,
                    param.column
                );
            }
            
            VariableSymbol symbol = new VariableSymbol(
                param.name,
                classes.get(param.type.name)
            );
            getCurrentScope().put(param.name, symbol);
        }
        
        // Analyze method body
        analyzeBlock(method.body);
        
        // Exit method scope
        exitScope();
    }
    
    private void analyzeBlock(Block block) {
        enterScope();
        
        for (Statement stmt : block.statements) {
            analyzeStatement(stmt);
        }
        
        exitScope();
    }
    
    private void analyzeStatement(Statement stmt) {
        if (stmt instanceof LocalVariableDeclaration) {
            analyzeLocalVariable((LocalVariableDeclaration) stmt);
        } else if (stmt instanceof ExpressionStatement) {
            analyzeExpression(((ExpressionStatement) stmt).expression);
        }
    }
    
    private void analyzeLocalVariable(LocalVariableDeclaration decl) {
        // Verify type exists
        if (!classes.containsKey(decl.type.name)) {
            throw new SemanticException(
                "Unknown type: " + decl.type.name,
                decl.line,
                decl.column
            );
        }
        
        // Analyze initializer if present
        if (decl.initializer != null) {
            ClassSymbol initType = analyzeExpression(decl.initializer);
            if (!isAssignable(initType, classes.get(decl.type.name))) {
                throw new SemanticException(
                    "Type mismatch: cannot convert from " + initType.name + " to " + decl.type.name,
                    decl.line,
                    decl.column
                );
            }
        }
        
        // Add to current scope
        VariableSymbol symbol = new VariableSymbol(
            decl.name,
            classes.get(decl.type.name)
        );
        getCurrentScope().put(decl.name, symbol);
    }

    private ClassSymbol analyzeObjectCreation(ObjectCreation expr) {
        // Verify the type exists
        ClassSymbol type = classes.get(expr.typeName);
        if (type == null) {
            throw new SemanticException(
                "Unknown type: " + expr.typeName,
                expr.line,
                expr.column
            );
        }
        
        // For Scanner constructor with System.in
        if (expr.typeName.equals("Scanner") && expr.arguments.size() == 1) {
            Expression arg = expr.arguments.get(0);
            if (arg instanceof FieldAccess) {
                FieldAccess fieldAccess = (FieldAccess) arg;
                if (fieldAccess.target instanceof VariableReference) {
                    VariableReference target = (VariableReference) fieldAccess.target;
                    if (target.name.equals("System") && fieldAccess.fieldName.equals("in")) {
                        return type;
                    }
                }
            }
        }
        
        // Analyze constructor arguments
        for (Expression arg : expr.arguments) {
            analyzeExpression(arg);
        }
        
        return type;
    }
    
    private ClassSymbol analyzeBinaryExpression(BinaryExpression expr) {
        ClassSymbol leftType = analyzeExpression(expr.left);
        ClassSymbol rightType = analyzeExpression(expr.right);
        
        // For now, only support numeric operations
        if (leftType != classes.get("int") || rightType != classes.get("int")) {
            throw new SemanticException(
                "Operator " + expr.operator + " not defined for types " + 
                leftType.name + " and " + rightType.name,
                expr.line,
                expr.column
            );
        }
        
        return classes.get("int");
    }
    
    private ClassSymbol analyzeMethodInvocation(MethodInvocation expr) {
        System.out.println("Analyzing method invocation: " + expr.methodName);
        
        // First analyze the target
        ClassSymbol targetType = null;
        if (expr.target != null) {
            System.out.println("Target exists, analyzing target expression");
            targetType = analyzeExpression(expr.target);
            System.out.println("Target type resolved to: " + (targetType != null ? targetType.name : "null"));
            
            // Handle PrintStream methods (System.out.println)
            if (targetType == classes.get("PrintStream")) {
                System.out.println("Found PrintStream target");
                if (expr.methodName.equals("println")) {
                    return classes.get("void");
                }
            }
            
            // Handle Scanner methods
            if (targetType == classes.get("Scanner")) {
                System.out.println("Found Scanner target");
                switch (expr.methodName) {
                    case "nextInt":
                        return classes.get("int");
                    case "close":
                        return classes.get("void");
                }
            }
    
            // If the target is System.out, resolve to PrintStream
            if (expr.target instanceof FieldAccess) {
                FieldAccess fieldAccess = (FieldAccess) expr.target;
                if (fieldAccess.target instanceof VariableReference) {
                    VariableReference varRef = (VariableReference) fieldAccess.target;
                    if (varRef.name.equals("System") && fieldAccess.fieldName.equals("out")) {
                        if (expr.methodName.equals("println")) {
                            return classes.get("void");
                        }
                    }
                }
            }
        }
    
        throw new SemanticException(
            "Cannot resolve method: " + expr.methodName + 
            (targetType != null ? " on type " + targetType.name : ""),
            expr.line,
            expr.column
        );
    }
    
    private ClassSymbol analyzeFieldAccess(FieldAccess expr) {
        System.out.println("Analyzing field access: " + expr.fieldName);
        
        ClassSymbol targetType = analyzeExpression(expr.target);
        System.out.println("Field access target type: " + (targetType != null ? targetType.name : "null"));
        
        // Handle System.out and System.in
        if (targetType == classes.get("System")) {
            System.out.println("Found System type, checking field " + expr.fieldName);
            if (expr.fieldName.equals("out")) {
                ClassSymbol printStream = classes.get("PrintStream");
                System.out.println("Resolved System.out to PrintStream");
                return printStream;
            } else if (expr.fieldName.equals("in")) {
                return classes.get("InputStream");
            }
        }
        
        throw new SemanticException(
            "Cannot resolve field: " + expr.fieldName + 
            " on type " + (targetType != null ? targetType.name : "unknown"),
            expr.line,
            expr.column
        );
    }
    
    private ClassSymbol analyzeExpression(Expression expr) {
        System.out.println("Analyzing expression: " + expr.getClass().getSimpleName());
        
        if (expr instanceof BinaryExpression) {
            return analyzeBinaryExpression((BinaryExpression) expr);
        } else if (expr instanceof MethodInvocation) {
            return analyzeMethodInvocation((MethodInvocation) expr);
        } else if (expr instanceof VariableReference) {
            return analyzeVariableReference((VariableReference) expr);
        } else if (expr instanceof IntegerLiteral) {
            return classes.get("int");
        } else if (expr instanceof StringLiteral) {
            return classes.get("String");
        } else if (expr instanceof ObjectCreation) {
            return analyzeObjectCreation((ObjectCreation) expr);
        } else if (expr instanceof FieldAccess) {
            return analyzeFieldAccess((FieldAccess) expr);
        }
        
        throw new SemanticException(
            "Unsupported expression type: " + expr.getClass().getSimpleName(),
            expr.line,
            expr.column
        );
    }
    
    private ClassSymbol analyzeVariableReference(VariableReference expr) {
        System.out.println("Analyzing variable reference: " + expr.name); // Debug
        
        // Special case for System
        if (expr.name.equals("System")) {
            ClassSymbol systemClass = classes.get("System");
            System.out.println("Found System class reference"); // Debug
            return systemClass;
        }
        
        Symbol symbol = resolveSymbol(expr.name);
        if (symbol == null) {
            throw new SemanticException(
                "Cannot resolve variable: " + expr.name,
                expr.line,
                expr.column
            );
        }
        
        System.out.println("Resolved variable " + expr.name + " to type " + symbol.type.name); // Debug
        return symbol.type;
    }
    
    private Symbol resolveSymbol(String name) {
        // Search through all scopes from inner to outer
        for (Map<String, Symbol> scope : scopes) {
            Symbol symbol = scope.get(name);
            if (symbol != null) {
                return symbol;
            }
        }
        return null;
    }
    
    private boolean isAssignable(ClassSymbol from, ClassSymbol to) {
        // For now, just check exact type match
        return from == to;
    }
    
    private void enterScope() {
        scopes.push(new HashMap<>());
    }
    
    private void exitScope() {
        scopes.pop();
    }
    
    private Map<String, Symbol> getCurrentScope() {
        return scopes.peek();
    }
}

// Symbol table classes
abstract class Symbol {
    final String name;
    final ClassSymbol type;
    
    Symbol(String name, ClassSymbol type) {
        this.name = name;
        this.type = type;
    }
}

class ClassSymbol extends Symbol {
    private final Map<String, FieldSymbol> fields;
    
    ClassSymbol(String name, ClassSymbol superclass) {
        super(name, null);
        this.fields = new HashMap<>();
    }
    
    void addField(FieldSymbol field) {
        fields.put(field.name, field);
    }
    
    FieldSymbol getField(String name) {
        return fields.get(name);
    }
}

class FieldSymbol extends Symbol {
    FieldSymbol(String name, ClassSymbol type) {
        super(name, type);
    }
}

class VariableSymbol extends Symbol {
    VariableSymbol(String name, ClassSymbol type) {
        super(name, type);
    }
}

// Diagnostic collection system
enum DiagnosticType {
    ERROR, WARNING
}

class CompilerDiagnostic {
    final DiagnosticType type;
    final String message;
    final String source;
    final int line;
    final int column;
    
    CompilerDiagnostic(DiagnosticType type, String message, String source, int line, int column) {
        this.type = type;
        this.message = message;
        this.source = source;
        this.line = line;
        this.column = column;
    }
    
    @Override
    public String toString() {
        return String.format("%s: %s at %s:%d:%d", 
            type, message, source, line, column);
    }
}

class DiagnosticCollector {
    private final List<CompilerDiagnostic> diagnostics;
    
    DiagnosticCollector() {
        this.diagnostics = new ArrayList<>();
    }
    
    void report(CompilerDiagnostic diagnostic) {
        diagnostics.add(diagnostic);
    }
    
    List<CompilerDiagnostic> getDiagnostics() {
        return Collections.unmodifiableList(diagnostics);
    }
    
    boolean hasErrors() {
        return diagnostics.stream()
            .anyMatch(d -> d.type == DiagnosticType.ERROR);
    }
}

class SemanticException extends RuntimeException {
    final int line;
    final int column;
    
    SemanticException(String message, int line, int column) {
        super(message);
        this.line = line;
        this.column = column;
    }
}