package com.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class BytecodeGenerator {
    private final ClassWriter cw;
    private final String className;

    public BytecodeGenerator(String className) {
        this.className = className;
        this.cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
    }

    public byte[] generate(CompilationUnit ast) {

        cw.visit(
            Opcodes.V17, 
            Opcodes.ACC_PUBLIC, 
            className, 
            null, 
            "java/lang/Object", 
            null
        );

        ast.mainClass.methods.forEach(this::generateMethod);

        cw.visitEnd();
        return cw.toByteArray();
    }

    private void generateMethod(MethodDeclaration method) {
        String methodName = method.name;
        String descriptor = getMethodDescriptor(method);

        MethodVisitor mv = cw.visitMethod(
            Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
            methodName,
            descriptor,
            null,
            null
        );

        method.body.statements.forEach(stmt -> generateStatement(stmt, mv));

        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(0, 0); 
        mv.visitEnd();
    }

    private String getMethodDescriptor(MethodDeclaration method) {

        return "([Ljava/lang/String;)V";
    }

    private void generateStatement(Statement stmt, MethodVisitor mv) {
        if (stmt instanceof LocalVariableDeclaration) {
            generateLocalVarDecl((LocalVariableDeclaration) stmt, mv);
        } else if (stmt instanceof ExpressionStatement) {
            generateExpression(((ExpressionStatement) stmt).expression, mv);
        }
    }

    private void generateLocalVarDecl(LocalVariableDeclaration decl, MethodVisitor mv) {
        generateExpression(decl.initializer, mv);
        int varIndex = getVarIndex(decl.name);

        if (decl.type.name.equals("Scanner")) {
            mv.visitVarInsn(Opcodes.ASTORE, varIndex);
        } else {
            mv.visitVarInsn(Opcodes.ISTORE, varIndex);
        }
    }

    private void generateExpression(Expression expr, MethodVisitor mv) {
        if (expr instanceof VariableReference) {
            VariableReference varRef = (VariableReference) expr;
            String varName = varRef.name;
            int index = getVarIndex(varName);

            if (varName.equals("ac")) {
                mv.visitVarInsn(Opcodes.ALOAD, index);
            } else {
                mv.visitVarInsn(Opcodes.ILOAD, index);
            }
        } else if (expr instanceof MethodInvocation) {
            generateMethodCall((MethodInvocation) expr, mv);
        } else if (expr instanceof BinaryExpression) {
            generateBinaryExpr((BinaryExpression) expr, mv);
        } else if (expr instanceof ObjectCreation) {
            generateObjectCreation((ObjectCreation) expr, mv);
        } else if (expr instanceof FieldAccess) {
            generateFieldAccess((FieldAccess) expr, mv);
        } else if (expr instanceof StringLiteral) {
            String value = ((StringLiteral) expr).value;
            mv.visitLdcInsn(value);
        } else if (expr instanceof IntegerLiteral) {
            int value = ((IntegerLiteral) expr).value;
            mv.visitIntInsn(Opcodes.BIPUSH, value); 
        }
    }

    private int getVarIndex(String varName) {

        switch (varName) {
            case "ac": return 1; 
            case "a": return 2;  
            case "b": return 3;  
            case "c": return 4;  
            default:
                throw new IllegalArgumentException("Unknown variable: " + varName);
        }
    }

    private void generateFieldAccess(FieldAccess expr, MethodVisitor mv) {

        String owner = "java/lang/System";
        String fieldName = expr.fieldName;
        String descriptor = fieldName.equals("in") ? "Ljava/io/InputStream;" : "Ljava/io/PrintStream;";

        mv.visitFieldInsn(Opcodes.GETSTATIC, owner, fieldName, descriptor);
    }

    private void generateMethodCall(MethodInvocation call, MethodVisitor mv) {
        String owner;
        String methodName = call.methodName;
        String descriptor;

        if ("nextInt".equals(methodName)) {
            owner = "java/util/Scanner";
            descriptor = "()I"; 
        } else if ("println".equals(methodName)) {
            owner = "java/io/PrintStream";
            descriptor = "(Ljava/lang/String;)V"; 
        } else if ("close".equals(methodName)) {
            owner = "java/util/Scanner";
            descriptor = "()V"; 
        } else {
            throw new UnsupportedOperationException("Unknown method: " + methodName);
        }

        generateExpression(call.target, mv);

        if (!call.arguments.isEmpty()) {
            generateExpression(call.arguments.get(0), mv);
        }

        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner, methodName, descriptor, false);
    }

    private void generateBinaryExpr(BinaryExpression expr, MethodVisitor mv) {
        if (expr.operator == TokenType.PLUS && (isStringType(expr.left) || isStringType(expr.right))) {

            mv.visitTypeInsn(Opcodes.NEW, "java/lang/StringBuilder");
            mv.visitInsn(Opcodes.DUP);
            mv.visitMethodInsn(
                Opcodes.INVOKESPECIAL,
                "java/lang/StringBuilder",
                "<init>",
                "()V",
                false
            );

            generateExpression(expr.left, mv);
            String leftDescriptor = isStringType(expr.left) ? "Ljava/lang/String;" : "I";
            mv.visitMethodInsn(
                Opcodes.INVOKEVIRTUAL,
                "java/lang/StringBuilder",
                "append",
                "(" + leftDescriptor + ")Ljava/lang/StringBuilder;",
                false
            );

            generateExpression(expr.right, mv);
            String rightDescriptor = isStringType(expr.right) ? "Ljava/lang/String;" : "I";
            mv.visitMethodInsn(
                Opcodes.INVOKEVIRTUAL,
                "java/lang/StringBuilder",
                "append",
                "(" + rightDescriptor + ")Ljava/lang/StringBuilder;",
                false
            );

            mv.visitMethodInsn(
                Opcodes.INVOKEVIRTUAL,
                "java/lang/StringBuilder",
                "toString",
                "()Ljava/lang/String;",
                false
            );
        } else {

            generateExpression(expr.left, mv);
            generateExpression(expr.right, mv);
            switch (expr.operator) {
                case PLUS: mv.visitInsn(Opcodes.IADD); break;
                case MINUS: mv.visitInsn(Opcodes.ISUB); break;
                case STAR: mv.visitInsn(Opcodes.IMUL); break;
                case SLASH: mv.visitInsn(Opcodes.IDIV); break;
                default: throw new UnsupportedOperationException("Unsupported operator: " + expr.operator);
            }
        }
    }

    private boolean isStringType(Expression expr) {

        return expr instanceof StringLiteral || (expr instanceof VariableReference && "String".equals(getType(expr)));
    }

    private String getType(Expression expr) {

        return "int"; 
    }

    private void generateObjectCreation(ObjectCreation expr, MethodVisitor mv) {

        String owner = "java/util/Scanner";
        mv.visitTypeInsn(Opcodes.NEW, owner);
        mv.visitInsn(Opcodes.DUP);
        generateExpression(expr.arguments.get(0), mv); 
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, owner, "<init>", "(Ljava/io/InputStream;)V", false);
    }
}