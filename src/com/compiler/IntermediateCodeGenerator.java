package com.compiler;

import java.util.ArrayList;
import java.util.List;

public class IntermediateCodeGenerator {
    private List<String> code = new ArrayList<>();
    private int tempCounter = 0;

    public List<String> generate(CompilationUnit ast) {
        visitCompilationUnit(ast);
        return code;
    }

    private void visitCompilationUnit(CompilationUnit node) {
        visitClass(node.mainClass);
    }

    private void visitClass(ClassDeclaration node) {
        for (MethodDeclaration method : node.methods) {
            visitMethod(method);
        }
    }

    private void visitMethod(MethodDeclaration node) {
        code.add("METHOD " + node.name + ":");
        visitBlock(node.body);
        code.add("END METHOD " + node.name);
    }

    private void visitBlock(Block node) {
        for (Statement stmt : node.statements) {
            visitStatement(stmt);
        }
    }

    private void visitStatement(Statement stmt) {
        if (stmt instanceof LocalVariableDeclaration) {
            visitLocalVarDeclaration((LocalVariableDeclaration) stmt);
        } else if (stmt instanceof ExpressionStatement) {
            visitExpression(((ExpressionStatement) stmt).expression);
        }
    }

    private void visitLocalVarDeclaration(LocalVariableDeclaration node) {
        String temp = visitExpression(node.initializer);
        code.add(node.name + " = " + temp);
    }

    private String visitExpression(Expression expr) {
        if (expr instanceof BinaryExpression) {
            return visitBinaryExpr((BinaryExpression) expr);
        } else if (expr instanceof IntegerLiteral) {
            return String.valueOf(((IntegerLiteral) expr).value);
        } else if (expr instanceof VariableReference) {
            return ((VariableReference) expr).name;
        } else if (expr instanceof MethodInvocation) {
            return visitMethodCall((MethodInvocation) expr);
        }
        return "UNSUPPORTED_EXPR";
    }

    private String visitBinaryExpr(BinaryExpression expr) {
        String left = visitExpression(expr.left);
        String right = visitExpression(expr.right);
        String temp = "t" + (tempCounter++);
        code.add(temp + " = " + left + " " + expr.operator + " " + right);
        return temp;
    }

    private String visitMethodCall(MethodInvocation call) {
        StringBuilder args = new StringBuilder();
        for (Expression arg : call.arguments) {
            args.append(visitExpression(arg)).append(", ");
        }
        if (args.length() > 0) {
            args.setLength(args.length() - 2); 
        }
        String temp = "t" + (tempCounter++);
        code.add(temp + " = CALL " + call.methodName + "(" + args + ")");
        return temp;
    }
}