package com.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class SemanticAnalyzer {
    private final SymbolTable symbolTable = new SymbolTable();
    private final List<SemanticError> errors = new ArrayList<>();
    private ClassDeclaration currentClass;
    private MethodDeclaration currentMethod;

    public SemanticAnalyzer() {

        symbolTable.addSymbol("System", new Symbol("System", new TypeNode("System", false, 0, 0)));
        symbolTable.addSymbol("Scanner", new Symbol("Scanner", new TypeNode("Scanner", false, 0, 0)));
        symbolTable.addSymbol("InputStream", new Symbol("InputStream", new TypeNode("InputStream", false, 0, 0)));
        symbolTable.addSymbol("PrintStream", new Symbol("PrintStream", new TypeNode("PrintStream", false, 0, 0)));
    }

    public void analyze(CompilationUnit ast) {
        System.out.println("Starting semantic analysis...");
        visitCompilationUnit(ast);
        System.out.println("Semantic analysis completed!");
        System.out.println("\nFinal Symbol Table:");
        symbolTable.print();
    }

    public List<SemanticError> getErrors() {
        return errors;
    }

    public boolean hasErrors() {
        return !errors.isEmpty();
    }

    private void visitCompilationUnit(CompilationUnit node) {
        node.imports.forEach(this::visitImport);
        visitClass(node.mainClass);
    }

    private void visitImport(ImportDeclaration node) {

    }

    private void visitClass(ClassDeclaration node) {
        currentClass = node;
        symbolTable.enterScope();

        for (FieldDeclaration field : node.fields) {
            symbolTable.addSymbol(field.name, new Symbol(field.name, field.type));
        }

        for (MethodDeclaration method : node.methods) {
            visitMethod(method);
        }

        symbolTable.exitScope();
        currentClass = null;
    }

    private void visitMethod(MethodDeclaration node) {
        currentMethod = node;
        symbolTable.enterScope();

        for (ParameterDeclaration param : node.parameters) {
            symbolTable.addSymbol(param.name, new Symbol(param.name, param.type));
        }

        visitBlock(node.body);

        symbolTable.exitScope();
        currentMethod = null;
    }

    private void visitBlock(Block node) {
        symbolTable.enterScope();
        for (Statement stmt : node.statements) {
            visitStatement(stmt);
        }
        symbolTable.exitScope();
    }

    private void visitStatement(Statement stmt) {
        if (stmt instanceof LocalVariableDeclaration) {
            visitLocalVarDeclaration((LocalVariableDeclaration) stmt);
        } else if (stmt instanceof ExpressionStatement) {
            visitExpression(((ExpressionStatement) stmt).expression);
        }
    }

    private void visitLocalVarDeclaration(LocalVariableDeclaration node) {

        if (symbolTable.lookup(node.name) != null) {
            addError(node.line, node.column, "Duplicate variable '" + node.name + "'");
            return;
        }

        symbolTable.addSymbol(node.name, new Symbol(node.name, node.type));
        System.out.printf("Declared variable: %s (%s) at %d:%d%n",
            node.name, node.type.name, node.line, node.column);

        if (node.initializer != null) {
            TypeNode exprType = visitExpression(node.initializer);
            if (!isAssignable(node.type, exprType)) {
                addError(node.line, node.column, "Type mismatch: " + node.type.name + " vs " + exprType.name);
            }
        }
    }

    private TypeNode visitExpression(Expression expr) {
        System.out.printf("Analyzing expression at %d:%d: %s%n",
            expr.line, expr.column, expr.getClass().getSimpleName());

        if (expr instanceof BinaryExpression) {
            return visitBinaryExpr((BinaryExpression) expr);
        } else if (expr instanceof MethodInvocation) {
            return visitMethodCall((MethodInvocation) expr);
        } else if (expr instanceof VariableReference) {
            return visitVarRef((VariableReference) expr);
        } else if (expr instanceof ObjectCreation) {
            return visitNewObject((ObjectCreation) expr);
        } else if (expr instanceof FieldAccess) {
            return visitFieldAccess((FieldAccess) expr);
        } else if (expr instanceof IntegerLiteral) {
            return new TypeNode("int", false, expr.line, expr.column);
        } else if (expr instanceof StringLiteral) {
            return new TypeNode("String", false, expr.line, expr.column);
        }
        return new TypeNode("unknown", false, expr.line, expr.column);
    }

    private TypeNode visitBinaryExpr(BinaryExpression node) {
        TypeNode leftType = visitExpression(node.left);
        TypeNode rightType = visitExpression(node.right);

        if (!isOperatorValid(node.operator, leftType, rightType)) {
            addError(node.line, node.column, "Invalid operator " + node.operator + 
                     " for types " + leftType.name + " and " + rightType.name);
        }

        return getResultType(node.operator, leftType, rightType);
    }

    private TypeNode visitMethodCall(MethodInvocation node) {
        TypeNode targetType = node.target != null ? visitExpression(node.target) : null;
        List<TypeNode> argTypes = new ArrayList<>();

        for (Expression arg : node.arguments) {
            argTypes.add(visitExpression(arg));
        }

        if (node.methodName.equals("nextInt")) {
            if (targetType == null || !targetType.name.equals("Scanner")) {
                addError(node.line, node.column, "nextInt() can only be called on Scanner");
            }
            return new TypeNode("int", false, node.line, node.column);
        } else if (node.methodName.equals("println")) {
            if (argTypes.size() != 1 || !argTypes.get(0).name.equals("String")) {
                addError(node.line, node.column, "println requires single String argument");
            }
            return new TypeNode("void", false, node.line, node.column);
        } else if (node.methodName.equals("close")) {
            if (targetType == null || !targetType.name.equals("Scanner")) {
                addError(node.line, node.column, "close() requires Scanner instance");
            }
            return new TypeNode("void", false, node.line, node.column);
        }

        addError(node.line, node.column, "Unknown method: " + node.methodName);
        return new TypeNode("error", false, node.line, node.column);
    }

    private TypeNode visitVarRef(VariableReference node) {
        Symbol symbol = symbolTable.lookup(node.name);
        if (symbol == null) {
            addError(node.line, node.column, "Undefined variable: " + node.name);
            return new TypeNode("error", false, node.line, node.column);
        }
        return symbol.type;
    }

    private TypeNode visitNewObject(ObjectCreation node) {
        System.out.printf("Analyzing object creation: new %s at %d:%d%n",
            node.typeName, node.line, node.column);

        if (node.typeName.equals("Scanner")) {
            if (node.arguments.size() != 1) {
                addError(node.line, node.column, "Scanner requires exactly 1 argument");
            } else {
                TypeNode argType = visitExpression(node.arguments.get(0));
                if (!argType.name.equals("InputStream")) {
                    addError(node.line, node.column, 
                        "Scanner requires InputStream, got " + argType.name);
                }
            }
            return new TypeNode("Scanner", false, node.line, node.column);
        }

        addError(node.line, node.column, "Unknown type: " + node.typeName);
        return new TypeNode("error", false, node.line, node.column);
    }

    private TypeNode visitFieldAccess(FieldAccess node) {
        System.out.printf("Analyzing field access: %s.%s at %d:%d%n",
            node.target, node.fieldName, node.line, node.column);

        TypeNode targetType = visitExpression(node.target);

        if (targetType.name.equals("System")) {
            switch (node.fieldName) {
                case "in":
                    System.out.println("Resolved System.in as InputStream");
                    return new TypeNode("InputStream", false, node.line, node.column);
                case "out":
                    System.out.println("Resolved System.out as PrintStream");
                    return new TypeNode("PrintStream", false, node.line, node.column);
            }
        }

        if (targetType.name.equals("PrintStream") && node.fieldName.equals("println")) {
            System.out.println("Resolved PrintStream.println as void method");
            return new TypeNode("void", false, node.line, node.column);
        }

        Symbol fieldSymbol = symbolTable.lookup(node.fieldName);
        if (fieldSymbol == null) {
            addError(node.line, node.column, "Unknown field: " + node.fieldName);
            return new TypeNode("error", false, node.line, node.column);
        }
        return fieldSymbol.type;
    }

    private boolean isOperatorValid(TokenType op, TypeNode left, TypeNode right) {
        if (op == TokenType.PLUS) {
            return left.name.equals("int") && right.name.equals("int") ||
                   left.name.equals("String") || right.name.equals("String");
        }
        return left.name.equals("int") && right.name.equals("int");
    }

    private TypeNode getResultType(TokenType op, TypeNode left, TypeNode right) {
        if (op == TokenType.PLUS && (left.name.equals("String") || right.name.equals("String"))) {
            return new TypeNode("String", false, left.line, left.column);
        }
        return new TypeNode("int", false, left.line, left.column);
    }

    private boolean isAssignable(TypeNode target, TypeNode source) {
        return target.name.equals(source.name) || 
              (target.name.equals("String") && source.name.equals("int"));
    }

    private void addError(int line, int column, String message) {
        errors.add(new SemanticError(line, column, message));
    }
}

class SemanticError {
    public final int line;
    public final int column;
    public final String message;

    public SemanticError(int line, int column, String message) {
        this.line = line;
        this.column = column;
        this.message = message;
    }

    @Override
    public String toString() {
        return String.format("%d:%d: %s", line, column, message);
    }
}

class SymbolTable {
    private final Stack<Map<String, Symbol>> scopes = new Stack<>();

    public SymbolTable() {
        enterScope(); 
    }

    public void enterScope() {
        scopes.push(new HashMap<>());
    }

    public void exitScope() {
        if (scopes.size() > 1) {
            scopes.pop();
        }
    }

    public void addSymbol(String name, Symbol symbol) {
        scopes.peek().put(name, symbol);
    }

    public Symbol lookup(String name) {
        for (int i = scopes.size() - 1; i >= 0; i--) {
            if (scopes.get(i).containsKey(name)) {
                return scopes.get(i).get(name);
            }
        }
        return null;
    }

    public void print() {
        System.out.println("Symbol Table Contents:");
        for (int i = scopes.size() - 1; i >= 0; i--) {
            System.out.printf("Scope %d:%n", i);
            for (Map.Entry<String, Symbol> entry : scopes.get(i).entrySet()) {
                System.out.printf("  %s: %s (%s)%n",
                    entry.getKey(), entry.getValue().type.name, entry.getValue().type.isArray ? "array" : "scalar");
            }
        }
    }
}

class Symbol {
    public final String name;
    public final TypeNode type;

    public Symbol(String name, TypeNode type) {
        this.name = name;
        this.type = type;
    }
}