// src/com/compiler/ASTPrinter.java

package com.compiler;

class ASTPrinter {
    private int indentLevel = 0;
    private static final String INDENT = "  "; // Two spaces for each level
    
    public String print(CompilationUnit unit) {
        StringBuilder sb = new StringBuilder();
        printCompilationUnit(unit, sb);
        return sb.toString();
    }
    
    private void printIndent(StringBuilder sb) {
        for (int i = 0; i < indentLevel; i++) {
            sb.append(INDENT);
        }
    }
    
    private void printCompilationUnit(CompilationUnit unit, StringBuilder sb) {
        sb.append("CompilationUnit\n");
        indentLevel++;
        
        // Print imports
        if (!unit.imports.isEmpty()) {
            printIndent(sb);
            sb.append("Imports:\n");
            indentLevel++;
            for (ImportDeclaration imp : unit.imports) {
                printIndent(sb);
                sb.append(imp.qualifiedName).append("\n");
            }
            indentLevel--;
        }
        
        // Print main class
        printClass(unit.mainClass, sb);
        
        indentLevel--;
    }
    
    private void printClass(ClassDeclaration clazz, StringBuilder sb) {
        printIndent(sb);
        sb.append("Class: ").append(clazz.name).append("\n");
        indentLevel++;
        
        // Print fields
        if (!clazz.fields.isEmpty()) {
            printIndent(sb);
            sb.append("Fields:\n");
            indentLevel++;
            for (FieldDeclaration field : clazz.fields) {
                printField(field, sb);
            }
            indentLevel--;
        }
        
        // Print methods
        if (!clazz.methods.isEmpty()) {
            printIndent(sb);
            sb.append("Methods:\n");
            indentLevel++;
            for (MethodDeclaration method : clazz.methods) {
                printMethod(method, sb);
            }
            indentLevel--;
        }
        
        indentLevel--;
    }
    
    private void printField(FieldDeclaration field, StringBuilder sb) {
        printIndent(sb);
        sb.append(field.type.name).append(" ").append(field.name);
        if (field.initializer != null) {
            sb.append(" = ");
            printExpression(field.initializer, sb);
        }
        sb.append("\n");
    }
    
    private void printMethod(MethodDeclaration method, StringBuilder sb) {
        printIndent(sb);
        sb.append(method.returnType.name).append(" ").append(method.name).append("(");
        
        // Print parameters
        for (int i = 0; i < method.parameters.size(); i++) {
            if (i > 0) sb.append(", ");
            ParameterDeclaration param = method.parameters.get(i);
            sb.append(param.type.name).append(" ").append(param.name);
        }
        sb.append(")\n");
        
        // Print method body
        printBlock(method.body, sb);
    }
    
    private void printBlock(Block block, StringBuilder sb) {
        indentLevel++;
        for (Statement stmt : block.statements) {
            printStatement(stmt, sb);
        }
        indentLevel--;
    }
    
    private void printStatement(Statement stmt, StringBuilder sb) {
        printIndent(sb);
        
        if (stmt instanceof LocalVariableDeclaration) {
            LocalVariableDeclaration decl = (LocalVariableDeclaration) stmt;
            sb.append(decl.type.name).append(" ").append(decl.name);
            if (decl.initializer != null) {
                sb.append(" = ");
                printExpression(decl.initializer, sb);
            }
        } else if (stmt instanceof ExpressionStatement) {
            printExpression(((ExpressionStatement) stmt).expression, sb);
        }
        
        sb.append("\n");
    }
    
    private void printExpression(Expression expr, StringBuilder sb) {
        if (expr instanceof BinaryExpression) {
            BinaryExpression binExpr = (BinaryExpression) expr;
            sb.append("(");
            printExpression(binExpr.left, sb);
            sb.append(" ").append(binExpr.operator).append(" ");
            printExpression(binExpr.right, sb);
            sb.append(")");
        } else if (expr instanceof MethodInvocation) {
            MethodInvocation methodInv = (MethodInvocation) expr;
            if (methodInv.target != null) {
                printExpression(methodInv.target, sb);
                sb.append(".");
            }
            sb.append(methodInv.methodName).append("(");
            for (int i = 0; i < methodInv.arguments.size(); i++) {
                if (i > 0) sb.append(", ");
                printExpression(methodInv.arguments.get(i), sb);
            }
            sb.append(")");
        } else if (expr instanceof VariableReference) {
            sb.append(((VariableReference) expr).name);
        } else if (expr instanceof IntegerLiteral) {
            sb.append(((IntegerLiteral) expr).value);
        } else if (expr instanceof StringLiteral) {
            sb.append("\"").append(((StringLiteral) expr).value).append("\"");
        } else if (expr instanceof ObjectCreation) {
            ObjectCreation objCreate = (ObjectCreation) expr;
            sb.append("new ").append(objCreate.typeName).append("(");
            for (int i = 0; i < objCreate.arguments.size(); i++) {
                if (i > 0) sb.append(", ");
                printExpression(objCreate.arguments.get(i), sb);
            }
            sb.append(")");
        } else if (expr instanceof FieldAccess) {
            FieldAccess fieldAccess = (FieldAccess) expr;
            printExpression(fieldAccess.target, sb);
            sb.append(".").append(fieldAccess.fieldName);
        }
    }
}