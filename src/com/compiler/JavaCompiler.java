// src/com/compiler/JavaCompiler.java

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
    private final Map<String, Symbol> globalScope;
    private final Map<Expression, ClassSymbol> expressionTypes;  // Cache for expression analysis

    public SemanticAnalyzer(DiagnosticCollector diagnostics) {
        this.diagnostics = diagnostics;
        this.classes = new HashMap<>();
        this.scopes = new ArrayDeque<>();
        this.globalScope = new HashMap<>();
        this.expressionTypes = new HashMap<>();
        initializeBuiltinTypes();
    }

    private ClassSymbol analyzeExpression(Expression expr) {
        System.out.println("\nDEBUG: [analyzeExpression] START type = " + expr.getClass().getSimpleName());
        
        try {
            // Check the cache first
            if (expressionTypes.containsKey(expr)) {
                ClassSymbol cached = expressionTypes.get(expr);
                System.out.println("DEBUG: [analyzeExpression] Using cached type: " + 
                    (cached != null ? cached.name : "null") + 
                    " for " + expr.getClass().getSimpleName());
                return cached;
            }
            
            ClassSymbol result = null;
            
            // Analyze based on expression type
            if (expr instanceof BinaryExpression) {
                result = analyzeBinaryExpression((BinaryExpression) expr);
            } else if (expr instanceof MethodInvocation) {
                result = analyzeMethodInvocation((MethodInvocation) expr);
            } else if (expr instanceof VariableReference) {
                result = analyzeVariableReference((VariableReference) expr);
            } else if (expr instanceof IntegerLiteral) {
                result = classes.get("int");
            } else if (expr instanceof StringLiteral) {
                result = classes.get("String");
            } else if (expr instanceof ObjectCreation) {
                result = analyzeObjectCreation((ObjectCreation) expr);
            } else if (expr instanceof FieldAccess) {
                result = analyzeFieldAccess((FieldAccess) expr);
            }
            
            // Cache the result
            if (result != null) {
                System.out.println("DEBUG: [analyzeExpression] Caching type: " + result.name + 
                    " for " + expr.getClass().getSimpleName());
                expressionTypes.put(expr, result);
            }
            
            System.out.println("DEBUG: [analyzeExpression] END result = " + 
                (result != null ? result.name : "null") + 
                " for " + expr.getClass().getSimpleName());
                
            return result;
            
        } catch (SemanticException se) {
            System.out.println("DEBUG: [analyzeExpression] SemanticException: " + se.getMessage());
            throw se;
        }
    }

    private void initializeBuiltinTypes() {
        System.out.println("DEBUG: Initializing built-in types...");
    
        // First initialize all the class symbols
        ClassSymbol intClass = new ClassSymbol("int", null);
        ClassSymbol voidClass = new ClassSymbol("void", null);
        ClassSymbol booleanClass = new ClassSymbol("boolean", null);
        ClassSymbol stringClass = new ClassSymbol("java.lang.String", null);
        
        System.out.println("DEBUG: Created primitive types");
        
        // Add primitive types to classes map
        classes.put("int", intClass);
        classes.put("void", voidClass);
        classes.put("boolean", booleanClass);
        classes.put("String", stringClass);
        
        System.out.println("DEBUG: Added primitive types to classes map");
    
        // Initialize PrintStream with println method
        ClassSymbol printStreamClass = new ClassSymbol("java.io.PrintStream", null);
        printStreamClass.addMethod(new MethodSymbol(
            "println",
            voidClass,
            Arrays.asList(new ParameterSymbol("value", stringClass))
        ));
        // Add overload for int
        printStreamClass.addMethod(new MethodSymbol(
            "println",
            voidClass,
            Arrays.asList(new ParameterSymbol("value", intClass))
        ));
        classes.put("PrintStream", printStreamClass);
        System.out.println("DEBUG: Added PrintStream with println method");
    
        // Initialize InputStream
        ClassSymbol inputStreamClass = new ClassSymbol("java.io.InputStream", null);
        classes.put("InputStream", inputStreamClass);
        System.out.println("DEBUG: Added InputStream class");
    
        // Initialize System class with static fields
        ClassSymbol systemClass = new ClassSymbol("java.lang.System", null);
        FieldSymbol outField = new FieldSymbol("out", printStreamClass, true);
        FieldSymbol inField = new FieldSymbol("in", inputStreamClass, true);
        systemClass.addField(outField);
        systemClass.addField(inField);
        
        System.out.println("DEBUG: Created System class with fields:");
        System.out.println("DEBUG:  - out: " + (systemClass.getField("out") != null));
        System.out.println("DEBUG:  - in: " + (systemClass.getField("in") != null));
    
        // Important: Put System in classes map FIRST
        classes.put("System", systemClass);
        
        // Then add System to the global scope
        globalScope.put("System", systemClass);
        
        System.out.println("DEBUG: Added System to classes and globalScope");
        System.out.println("DEBUG: Verify System in maps:");
        System.out.println("DEBUG:  - classes: " + (classes.get("System") != null));
        System.out.println("DEBUG:  - globalScope: " + (globalScope.get("System") != null));
    
        // Initialize Scanner class
        ClassSymbol scannerClass = new ClassSymbol("java.util.Scanner", null);
        scannerClass.addMethod(new MethodSymbol(
            "nextInt",
            intClass,
            Collections.emptyList()
        ));
        scannerClass.addMethod(new MethodSymbol(
            "close",
            voidClass,
            Collections.emptyList()
        ));
        // Add Scanner constructor
        scannerClass.addMethod(new MethodSymbol(
            "<init>",
            voidClass,
            Arrays.asList(new ParameterSymbol("source", inputStreamClass))
        ));
        classes.put("Scanner", scannerClass);
        
        System.out.println("DEBUG: Added Scanner class with methods");
    
        // Initialize base scope with global symbols AFTER all types are initialized
        enterScope();
        Map<String, Symbol> currentScope = getCurrentScope();
        currentScope.putAll(globalScope);  // Copy ALL global symbols
        
        System.out.println("DEBUG: Initialized base scope with all global symbols");
        System.out.println("DEBUG: Base scope contains System? " + 
            currentScope.containsKey("System") + 
            ", Global scope size: " + globalScope.size() + 
            ", Current scope size: " + currentScope.size());
        
        // Verify System is accessible in current scope
        System.out.println("DEBUG: System accessibility check:");
        System.out.println("DEBUG: - In classes map: " + (classes.get("System") != null));
        System.out.println("DEBUG: - In global scope: " + (globalScope.get("System") != null));
        System.out.println("DEBUG: - In current scope: " + (currentScope.get("System") != null));
    }

    private void enterScope() {
        Map<String, Symbol> newScope = new HashMap<>();
        if (!scopes.isEmpty()) {
            newScope.putAll(scopes.peek());
        } else {
            newScope.putAll(globalScope);
        }
        scopes.push(newScope);
        System.out.println("DEBUG: Entered new scope. Depth: " + scopes.size() + 
                         " Contains System? " + newScope.containsKey("System"));
    }

    private void exitScope() {
        if (!scopes.isEmpty()) {
            scopes.pop();
        }
    }

    // private Symbol resolveSymbol(String name) {
    //     System.out.println("DEBUG: resolveSymbol called for: " + name);
    //     System.out.println("DEBUG: Current scope depth: " + scopes.size());
        
    //     // Special handling for System
    //     if (name.equals("System")) {
    //         Symbol sys = globalScope.get("System");
    //         System.out.println("DEBUG: Looking for System in global scope, found: " + (sys != null));
    //         if (sys != null) return sys;
    //     }
        
    //     // Check current scope first
    //     if (!scopes.isEmpty()) {
    //         Symbol symbol = scopes.peek().get(name);
    //         System.out.println("DEBUG: Found in current scope? " + (symbol != null));
    //         if (symbol != null) {
    //             return symbol;
    //         }
    //     }
        
    //     // Check global scope
    //     Symbol symbol = globalScope.get(name);
    //     System.out.println("DEBUG: Found in global scope? " + (symbol != null));
    //     return symbol;
    // }

    private Map<String, Symbol> getCurrentScope() {
        if (scopes.isEmpty()) {
            enterScope();
        }
        Map<String, Symbol> currentScope = scopes.peek();
        System.out.println("DEBUG: Getting current scope. Contains System? " + 
                         currentScope.containsKey("System"));
        return currentScope;
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
        
        // Handle String concatenation
        if (expr.operator == TokenType.PLUS) {
            // If either operand is a String, it's string concatenation
            if (leftType == classes.get("String") || rightType == classes.get("String")) {
                return classes.get("String");
            }
            
            // For numeric operations
            if (leftType == classes.get("int") && rightType == classes.get("int")) {
                return classes.get("int");
            }
        }
        
        // Handle other numeric operations
        if (expr.operator == TokenType.MINUS || 
            expr.operator == TokenType.STAR || 
            expr.operator == TokenType.SLASH) {
            if (leftType == classes.get("int") && rightType == classes.get("int")) {
                return classes.get("int");
            }
            throw new SemanticException(
                "Operator " + expr.operator + " only defined for numeric types, got " + 
                leftType.name + " and " + rightType.name,
                expr.line,
                expr.column
            );
        }
        
        throw new SemanticException(
            "Operator " + expr.operator + " not defined for types " + 
            leftType.name + " and " + rightType.name,
            expr.line,
            expr.column
        );
    }

    private ClassSymbol analyzeMethodInvocation(MethodInvocation expr) {
        System.out.println("\nDEBUG: [analyzeMethodInvocation] START");
        
        try {
            // First analyze the target
            ClassSymbol targetType = null;
            if (expr.target != null) {
                targetType = analyzeExpression(expr.target);
                System.out.println("DEBUG: [analyzeMethodInvocation] Analyzed target type: " + 
                    (targetType != null ? targetType.name : "null"));
            }
    
            if (targetType == null) {
                throw new SemanticException(
                    "Cannot invoke method on null",
                    expr.line,
                    expr.column
                );
            }
    
            // Handle System.out.println special case
            if (targetType.name.equals("java.io.PrintStream")) {
                if (expr.methodName.equals("println")) {
                    if (expr.arguments.size() != 1) {
                        throw new SemanticException(
                            "println requires exactly one argument",
                            expr.line,
                            expr.column
                        );
                    }
    
                    // Analyze argument once and store result
                    ClassSymbol argType = analyzeExpression(expr.arguments.get(0));
                    System.out.println("DEBUG: [analyzeMethodInvocation] println argument type: " + 
                        (argType != null ? argType.name : "null"));
    
                    if (argType == null) {
                        throw new SemanticException(
                            "println argument has no type",
                            expr.line,
                            expr.column
                        );
                    }
    
                    // Use the stored argument type
                    if (argType == classes.get("String") || 
                        argType == classes.get("int")) {
                        System.out.println("DEBUG: [analyzeMethodInvocation] println valid argument type");
                        return classes.get("void");
                    }
    
                    throw new SemanticException(
                        "println argument must be String or int, got " + argType.name,
                        expr.line,
                        expr.column
                    );
                }
            }
    
            // Handle Scanner methods
            if (targetType.name.equals("java.util.Scanner")) {
                if (expr.methodName.equals("nextInt")) {
                    if (!expr.arguments.isEmpty()) {
                        throw new SemanticException(
                            "nextInt takes no arguments",
                            expr.line,
                            expr.column
                        );
                    }
                    return classes.get("int");
                }
                if (expr.methodName.equals("close")) {
                    if (!expr.arguments.isEmpty()) {
                        throw new SemanticException(
                            "close takes no arguments",
                            expr.line,
                            expr.column
                        );
                    }
                    return classes.get("void");
                }
            }
    
            // If we get here, couldn't resolve the method
            throw new SemanticException(
                "Cannot find method " + expr.methodName + " on type " + targetType.name,
                expr.line,
                expr.column
            );
            
        } catch (SemanticException se) {
            System.out.println("DEBUG: [analyzeMethodInvocation] SemanticException: " + se.getMessage());
            throw se;
        }
    }
    
    private ClassSymbol analyzeFieldAccess(FieldAccess expr) {
        System.out.println("\nDEBUG: [analyzeFieldAccess] START");
        System.out.println("DEBUG: [analyzeFieldAccess] target type = " + 
            expr.target.getClass().getSimpleName() + 
            ", field = " + expr.fieldName);
    
        try {
            // Analyze target FIRST
            System.out.println("DEBUG: [analyzeFieldAccess] Analyzing target expression");
            ClassSymbol targetType = analyzeExpression(expr.target);
            
            System.out.println("DEBUG: [analyzeFieldAccess] Target analyzed, type: " + 
                (targetType != null ? targetType.name : "null"));
    
            if (targetType == null) {
                System.out.println("DEBUG: [analyzeFieldAccess] Target type is null!");
                throw new SemanticException(
                    "Field access target returned null type",
                    expr.line,
                    expr.column
                );
            }
    
            // Get the field
            System.out.println("DEBUG: [analyzeFieldAccess] Looking up field: " + expr.fieldName);
            FieldSymbol field = targetType.getField(expr.fieldName);
            
            System.out.println("DEBUG: [analyzeFieldAccess] Field lookup result: " + 
                (field != null ? "found" : "not found"));
    
            if (field == null) {
                throw new SemanticException(
                    "Cannot find field '" + expr.fieldName + "' in type " + targetType.name,
                    expr.line,
                    expr.column
                );
            }
    
            // Verify static access for System fields
            if (targetType.name.equals("java.lang.System")) {
                if (!field.isStatic) {
                    throw new SemanticException(
                        "Cannot access non-static field '" + expr.fieldName + 
                        "' in static context",
                        expr.line,
                        expr.column
                    );
                }
                System.out.println("DEBUG: [analyzeFieldAccess] Verified static access");
            }
    
            if (field.type == null) {
                System.out.println("DEBUG: [analyzeFieldAccess] Field type is null!");
                throw new SemanticException(
                    "Field '" + expr.fieldName + "' has no type",
                    expr.line,
                    expr.column
                );
            }
    
            System.out.println("DEBUG: [analyzeFieldAccess] END - returning type: " + field.type.name);
            return field.type;
            
        } catch (SemanticException se) {
            System.out.println("DEBUG: [analyzeFieldAccess] SemanticException: " + se.getMessage());
            throw se;
        } catch (Exception e) {
            System.out.println("DEBUG: [analyzeFieldAccess] Unexpected exception: " + e);
            e.printStackTrace();
            throw e;
        }
    }
    
    private ClassSymbol analyzeVariableReference(VariableReference expr) {
        System.out.println("\nDEBUG: [analyzeVariableReference] START: " + expr.name);
        
        try {
            // Special handling for System
            if (expr.name.equals("System")) {
                debugSystemAvailability("analyzeVariableReference");
                
                // Try to get System from classes map
                ClassSymbol systemClass = classes.get("System");
                System.out.println("DEBUG: [analyzeVariableReference] System lookup from classes: " + 
                    (systemClass != null));
                    
                if (systemClass != null) {
                    verifyClassSymbol(systemClass, "System verification");
                    System.out.println("DEBUG: [analyzeVariableReference] END - returning System class");
                    return systemClass;  // Return the actual class symbol
                }
                
                throw new SemanticException(
                    "System class not found",
                    expr.line,
                    expr.column
                );
            }
    
            // For regular variables
            Map<String, Symbol> currentScope = getCurrentScope();
            Symbol symbol = currentScope.get(expr.name);
            
            System.out.println("DEBUG: [analyzeVariableReference] Variable lookup in current scope: " + 
                (symbol != null));
    
            if (symbol != null) {
                if (symbol.type == null) {
                    throw new SemanticException(
                        "Symbol found but has no type: " + expr.name,
                        expr.line,
                        expr.column
                    );
                }
                System.out.println("DEBUG: [analyzeVariableReference] END - returning type: " + 
                    symbol.type.name);
                return symbol.type;
            }
    
            throw new SemanticException(
                "Unknown variable: " + expr.name,
                expr.line,
                expr.column
            );
            
        } catch (SemanticException se) {
            System.out.println("DEBUG: [analyzeVariableReference] SemanticException: " + 
                se.getMessage());
            throw se;
        } catch (Exception e) {
            System.out.println("DEBUG: [analyzeVariableReference] Unexpected exception: " + e);
            e.printStackTrace();
            throw e;
        }
    }

    private void verifyClassSymbol(ClassSymbol cls, String context) {
        if (cls == null) {
            System.out.println("DEBUG: " + context + ": ClassSymbol is null!");
            return;
        }
        
        System.out.println("DEBUG: " + context + " ClassSymbol details:");
        System.out.println("  - Name: " + cls.name);
        System.out.println("  - Fields count: " + 
            (cls.getField("out") != null ? "has out" : "no out") + ", " +
            (cls.getField("in") != null ? "has in" : "no in"));
            
        // Verify the fields themselves
        FieldSymbol outField = cls.getField("out");
        if (outField != null) {
            System.out.println("  - out field type: " + 
                (outField.type != null ? outField.type.name : "null") +
                ", static: " + outField.isStatic);
        }
        
        FieldSymbol inField = cls.getField("in");
        if (inField != null) {
            System.out.println("  - in field type: " + 
                (inField.type != null ? inField.type.name : "null") +
                ", static: " + inField.isStatic);
        }
    }
    
    private boolean isAssignable(ClassSymbol from, ClassSymbol to) {
        // For now, just check exact type match
        return from == to;
    }

    private void debugSystemAvailability(String location) {
        System.out.println("DEBUG: System availability at " + location + ":");
        System.out.println("  - In classes map: " + (classes.get("System") != null));
        System.out.println("  - In global scope: " + (globalScope.get("System") != null));
        if (!scopes.isEmpty()) {
            System.out.println("  - In current scope: " + (scopes.peek().get("System") != null));
        }
        System.out.println("  - Current scope depth: " + scopes.size());
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
    
    public ClassSymbol getType() {
        return type;
    }
}

class FieldSymbol extends Symbol {
    final boolean isStatic;
    
    FieldSymbol(String name, ClassSymbol type) {
        this(name, type, false);
    }
    
    FieldSymbol(String name, ClassSymbol type, boolean isStatic) {
        super(name, type);
        this.isStatic = isStatic;
    }
}

// Add MethodSymbol class
class MethodSymbol extends Symbol {
    final List<ParameterSymbol> parameters;
    
    MethodSymbol(String name, ClassSymbol returnType, List<ParameterSymbol> parameters) {
        super(name, returnType);
        this.parameters = parameters;
    }
}

// Add ParameterSymbol class
class ParameterSymbol extends Symbol {
    ParameterSymbol(String name, ClassSymbol type) {
        super(name, type);
    }
}

// Update ClassSymbol to include methods
class ClassSymbol extends Symbol {
    private final Map<String, FieldSymbol> fields;
    private final Map<String, MethodSymbol> methods;
    
    ClassSymbol(String name, ClassSymbol superclass) {
        super(name, null);  // ClassSymbol is its own type
        this.fields = new HashMap<>();
        this.methods = new HashMap<>();
    }
    
    @Override
    public ClassSymbol getType() {
        return this;  // ClassSymbol is its own type
    }
    
    void addField(FieldSymbol field) {
        System.out.println("DEBUG: Adding field " + field.name + " to class " + this.name);
        fields.put(field.name, field);
    }
    
    void addMethod(MethodSymbol method) {
        System.out.println("DEBUG: Adding method " + method.name + " to class " + this.name);
        methods.put(method.name, method);
    }
    
    FieldSymbol getField(String name) {
        FieldSymbol field = fields.get(name);
        System.out.println("DEBUG: Getting field " + name + " from class " + this.name + 
                         " found? " + (field != null));
        return field;
    }
    
    MethodSymbol getMethod(String name) {
        return methods.get(name);
    }
    
    Map<String, FieldSymbol> getFields() {
        return Collections.unmodifiableMap(fields);
    }
    
    @Override
    public String toString() {
        return "ClassSymbol{name='" + name + "', fields=" + fields.keySet() + "}";
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