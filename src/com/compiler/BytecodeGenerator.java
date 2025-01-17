package com.compiler;

import org.objectweb.asm.*;
import java.util.*;

public class BytecodeGenerator implements Opcodes {
    private final CompilationUnit compilationUnit;
    private ClassWriter classWriter;
    private MethodVisitor currentMethod;
    private final Map<String, Integer> localVariables;
    private int nextLocalVariable;
    
    public BytecodeGenerator(CompilationUnit compilationUnit) {
        this.compilationUnit = compilationUnit;
        this.localVariables = new HashMap<>();
        this.nextLocalVariable = 0;
    }
    
    public byte[] generate() {
        // Create ClassWriter with automatic computation of frames
        classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        
        // Generate class
        ClassDeclaration mainClass = compilationUnit.mainClass;
        String className = mainClass.name;
        
        // Start class definition
        classWriter.visit(
            V17,                    // Java version (17)
            ACC_PUBLIC,             // Access flags
            className,              // Class name
            null,                   // Signature (null for non-generic)
            "java/lang/Object",     // Superclass
            null                    // Interfaces
        );
        
        // Generate default constructor
        generateDefaultConstructor();
        
        // Generate methods
        for (MethodDeclaration method : mainClass.methods) {
            generateMethod(method);
        }
        
        classWriter.visitEnd();
        return classWriter.toByteArray();
    }
    
    private void generateDefaultConstructor() {
        MethodVisitor mv = classWriter.visitMethod(
            ACC_PUBLIC,
            "<init>",
            "()V",
            null,
            null
        );
        
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
    }
    
    private void generateMethod(MethodDeclaration method) {
        // Reset local variable tracking for new method
        localVariables.clear();
        nextLocalVariable = 0;
        
        // Calculate method access flags
        int access = 0;
        for (Modifier modifier : method.modifiers) {
            switch (modifier) {
                case PUBLIC: access |= ACC_PUBLIC; break;
                case PRIVATE: access |= ACC_PRIVATE; break;
                case PROTECTED: access |= ACC_PROTECTED; break;
                case STATIC: access |= ACC_STATIC; break;
                case FINAL: access |= ACC_FINAL; break;
                case ABSTRACT: access |= ACC_ABSTRACT; break;
                default: break; // Handle any other modifiers
            }
        }
        
        // Build method descriptor
        String descriptor = buildMethodDescriptor(method);
        
        // Create method visitor
        currentMethod = classWriter.visitMethod(
            access,
            method.name,
            descriptor,
            null,
            null
        );
        
        currentMethod.visitCode();
        
        // If method is static, we start local variables from 0
        // Otherwise, 0 is reserved for 'this'
        nextLocalVariable = (access & ACC_STATIC) != 0 ? 0 : 1;
        
        // Add parameters to local variables
        for (ParameterDeclaration param : method.parameters) {
            localVariables.put(param.name, nextLocalVariable++);
        }
        
        // Generate code for method body
        generateBlock(method.body);
        
        // Add return statement if none exists
        if (method.returnType.name.equals("void")) {
            currentMethod.visitInsn(RETURN);
        }
        
        // Finalize method
        currentMethod.visitMaxs(0, 0); // ASM will compute the real values
        currentMethod.visitEnd();
    }
    
    private String buildMethodDescriptor(MethodDeclaration method) {
        StringBuilder descriptor = new StringBuilder("(");
        
        // Add parameter types
        for (ParameterDeclaration param : method.parameters) {
            descriptor.append(getTypeDescriptor(param.type));
        }
        
        descriptor.append(")");
        descriptor.append(getTypeDescriptor(method.returnType));
        
        return descriptor.toString();
    }
    
    private String getTypeDescriptor(TypeNode type) {
        if (type.isArray) {
            return "[" + getBasicTypeDescriptor(type.name);
        }
        return getBasicTypeDescriptor(type.name);
    }
    
    private String getBasicTypeDescriptor(String typeName) {
        switch (typeName) {
            case "void": return "V";
            case "boolean": return "Z";
            case "char": return "C";
            case "byte": return "B";
            case "short": return "S";
            case "int": return "I";
            case "long": return "J";
            case "float": return "F";
            case "double": return "D";
            case "String": return "Ljava/lang/String;";
            case "Scanner": return "Ljava/util/Scanner;";
            default: return "L" + typeName.replace('.', '/') + ";";
        }
    }
    
    private void generateBlock(Block block) {
        for (Statement stmt : block.statements) {
            generateStatement(stmt);
        }
    }
    
    private void generateStatement(Statement stmt) {
        if (stmt instanceof LocalVariableDeclaration) {
            generateLocalVariable((LocalVariableDeclaration) stmt);
        } else if (stmt instanceof ExpressionStatement) {
            generateExpression(((ExpressionStatement) stmt).expression);
            
            // Pop value if not used
            Type type = getExpressionType(((ExpressionStatement) stmt).expression);
            if (type != Type.VOID) {
                currentMethod.visitInsn(POP);
            }
        }
    }
    
    private void generateLocalVariable(LocalVariableDeclaration decl) {
        if (decl.type.name.equals("Scanner")) {
            // Generate Scanner initialization code
            generateScannerInitialization(currentMethod);
        } else if (decl.initializer != null) {
            generateExpression(decl.initializer);
        }
        
        // Store variable index
        int index = nextLocalVariable++;
        localVariables.put(decl.name, index);
        
        // Store value in local variable
        switch (decl.type.name) {
            case "Scanner":
                currentMethod.visitVarInsn(ASTORE, index);
                break;
            case "int":
                currentMethod.visitVarInsn(ISTORE, index);
                break;
            case "long":
                currentMethod.visitVarInsn(LSTORE, index);
                break;
            case "float":
                currentMethod.visitVarInsn(FSTORE, index);
                break;
            case "double":
                currentMethod.visitVarInsn(DSTORE, index);
                break;
            default:
                currentMethod.visitVarInsn(ASTORE, index);
        }
    }
    
    private void generateExpression(Expression expr) {
        if (expr instanceof FieldAccess) {
            generateFieldAccess((FieldAccess) expr);
        } else if (expr instanceof ObjectCreation) {
            generateObjectCreation((ObjectCreation) expr);
        } else if (expr instanceof BinaryExpression) {
            generateBinaryExpression((BinaryExpression) expr);
        } else if (expr instanceof MethodInvocation) {
            generateMethodInvocation((MethodInvocation) expr);
        } else if (expr instanceof VariableReference) {
            generateVariableReference((VariableReference) expr);
        } else if (expr instanceof IntegerLiteral) {
            generateIntegerLiteral((IntegerLiteral) expr);
        } else if (expr instanceof StringLiteral) {
            generateStringLiteral((StringLiteral) expr);
        }
    }

    private void generateFieldAccess(FieldAccess expr) {
        // Special handling for System.in
        if (expr.target instanceof VariableReference &&
            ((VariableReference)expr.target).name.equals("System") &&
            expr.fieldName.equals("in")) {
            currentMethod.visitFieldInsn(
                GETSTATIC,
                "java/lang/System",
                "in",
                "Ljava/io/InputStream;"
            );
            return;
        }
        
        // Generate code for the target object
        generateExpression(expr.target);
        
        // Access the field
        currentMethod.visitFieldInsn(
            GETFIELD,
            "java/lang/Object", // This should be the actual class name
            expr.fieldName,
            "Ljava/lang/Object;" // This should be the actual field type
        );
    }

    private void generateObjectCreation(ObjectCreation expr) {
        String internalName = expr.typeName.replace('.', '/');
        
        // Create new object
        currentMethod.visitTypeInsn(NEW, internalName);
        currentMethod.visitInsn(DUP);
        
        // Generate code for constructor arguments
        if (expr.typeName.equals("Scanner")) {
            // Special handling for Scanner(System.in)
            currentMethod.visitFieldInsn(
                GETSTATIC,
                "java/lang/System",
                "in",
                "Ljava/io/InputStream;"
            );
            
            // Call Scanner constructor with InputStream
            currentMethod.visitMethodInsn(
                INVOKESPECIAL,
                "java/util/Scanner",
                "<init>",
                "(Ljava/io/InputStream;)V",
                false
            );
        } else {
            // Generic handling for other constructors
            for (Expression arg : expr.arguments) {
                generateExpression(arg);
            }
            
            // Build constructor descriptor
            StringBuilder descriptor = new StringBuilder("(");
            for (int i = 0; i < expr.arguments.size(); i++) {
                descriptor.append("Ljava/lang/Object;");
            }
            descriptor.append(")V");
            
            // Call constructor
            currentMethod.visitMethodInsn(
                INVOKESPECIAL,
                internalName,
                "<init>",
                descriptor.toString(),
                false
            );
        }
    }
    
    private void generateBinaryExpression(BinaryExpression expr) {
        // Generate code for operands
        generateExpression(expr.left);
        generateExpression(expr.right);
        
        // Generate operation
        switch (expr.operator) {
            case PLUS:
                currentMethod.visitInsn(IADD);
                break;
            case MINUS:
                currentMethod.visitInsn(ISUB);
                break;
            case STAR:
                currentMethod.visitInsn(IMUL);
                break;
            case SLASH:
                currentMethod.visitInsn(IDIV);
                break;
            default:
                throw new BytecodeGenerationException(
                    "Unsupported binary operator: " + expr.operator,
                    expr.line,
                    expr.column
                );
        }
    }
    
    private void generateMethodInvocation(MethodInvocation expr) {
        // Load target object for the method call
        if (expr.target != null) {
            if (expr.target instanceof VariableReference) {
                VariableReference target = (VariableReference) expr.target;
                
                // Special case for System.out.println
                if (target.name.equals("System") && expr.methodName.equals("println")) {
                    currentMethod.visitFieldInsn(
                        GETSTATIC,
                        "java/lang/System",
                        "out",
                        "Ljava/io/PrintStream;"
                    );
                    
                    // Use generateStringConcatenation for multiple arguments
                    if (expr.arguments.size() > 1) {
                        generateStringConcatenation(expr.arguments.toArray(new Expression[0]));
                    } else {
                        // Single argument
                        generateExpression(expr.arguments.get(0));
                    }
                    
                    currentMethod.visitMethodInsn(
                        INVOKEVIRTUAL,
                        "java/io/PrintStream",
                        "println",
                        "(Ljava/lang/String;)V",
                        false
                    );
                    return;
                }
                
                // Handle Scanner.nextInt()
                if (expr.methodName.equals("nextInt")) {
                    // Load Scanner instance
                    Integer index = localVariables.get(target.name);
                    if (index == null) {
                        throw new BytecodeGenerationException(
                            "Unknown variable: " + target.name,
                            target.line,
                            target.column
                        );
                    }
                    currentMethod.visitVarInsn(ALOAD, index);
                    generateScannerNextInt(currentMethod);
                    return;
                }
                
                // Handle Scanner.close()
                if (expr.methodName.equals("close")) {
                    Integer index = localVariables.get(target.name);
                    if (index == null) {
                        throw new BytecodeGenerationException(
                            "Unknown variable: " + target.name,
                            target.line,
                            target.column
                        );
                    }
                    currentMethod.visitVarInsn(ALOAD, index);
                    
                    currentMethod.visitMethodInsn(
                        INVOKEVIRTUAL,
                        "java/util/Scanner",
                        "close",
                        "()V",
                        false
                    );
                    return;
                }
            }
            
            // Default case - generate target
            generateExpression(expr.target);
        }
        
        // Handle arguments
        for (Expression arg : expr.arguments) {
            generateExpression(arg);
        }
        
        // Call method
        currentMethod.visitMethodInsn(
            INVOKEVIRTUAL,
            "java/lang/Object",
            expr.methodName,
            "()V",
            false
        );
    }
    
    private void generateVariableReference(VariableReference expr) {
        Integer index = localVariables.get(expr.name);
        if (index == null) {
            throw new BytecodeGenerationException(
                "Unknown variable: " + expr.name,
                expr.line,
                expr.column
            );
        }
        
        // Handle different types of variables
        if (expr.name.equals("ac")) {
            // Scanner objects are reference types
            currentMethod.visitVarInsn(ALOAD, index);
        } else {
            // Default to int for now (you may want to track variable types)
            currentMethod.visitVarInsn(ILOAD, index);
        }
    }
    
    private void generateIntegerLiteral(IntegerLiteral expr) {
        currentMethod.visitLdcInsn(expr.value);
    }
    
    private void generateStringLiteral(StringLiteral expr) {
        currentMethod.visitLdcInsn(expr.value);
    }
    
    private enum Type {
        VOID, INT, LONG, FLOAT, DOUBLE, BOOLEAN, OBJECT
    }
    
    private Type getExpressionType(Expression expr) {
        if (expr instanceof IntegerLiteral) return Type.INT;
        if (expr instanceof StringLiteral) return Type.OBJECT;
        if (expr instanceof BinaryExpression) return Type.INT;
        if (expr instanceof MethodInvocation) {
            MethodInvocation methodInvocation = (MethodInvocation) expr;
            if (methodInvocation.methodName.equals("nextInt")) {
                return Type.INT;
            }
            return Type.VOID;
        }
        return Type.OBJECT;
    }

    private void generateScannerInitialization(MethodVisitor mv) {
        // Create new Scanner object
        mv.visitTypeInsn(NEW, "java/util/Scanner");
        mv.visitInsn(DUP);
        
        // Get System.in
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;");
        
        // Call Scanner constructor
        mv.visitMethodInsn(
            INVOKESPECIAL,
            "java/util/Scanner",
            "<init>",
            "(Ljava/io/InputStream;)V",
            false
        );
    }
    
    private void generateScannerNextInt(MethodVisitor mv) {
        // Call nextInt() method
        mv.visitMethodInsn(
            INVOKEVIRTUAL,
            "java/util/Scanner",
            "nextInt",
            "()I",
            false
        );
    }
    
    private void generateStringConcatenation(Expression... expressions) {
        // Create StringBuilder
        currentMethod.visitTypeInsn(NEW, "java/lang/StringBuilder");
        currentMethod.visitInsn(DUP);
        currentMethod.visitMethodInsn(
            INVOKESPECIAL,
            "java/lang/StringBuilder",
            "<init>",
            "()V",
            false
        );
        
        // Append each expression
        for (Expression expr : expressions) {
            generateExpression(expr);
            
            // Determine append method based on expression type
            String descriptor = getAppendDescriptor(getExpressionType(expr));
            
            currentMethod.visitMethodInsn(
                INVOKEVIRTUAL,
                "java/lang/StringBuilder",
                "append",
                descriptor,
                false
            );
        }
        
        // Convert to String
        currentMethod.visitMethodInsn(
            INVOKEVIRTUAL,
            "java/lang/StringBuilder",
            "toString",
            "()Ljava/lang/String;",
            false
        );
    }
    
    private String getAppendDescriptor(Type type) {
        switch (type) {
            case INT:
                return "(I)Ljava/lang/StringBuilder;";
            case LONG:
                return "(J)Ljava/lang/StringBuilder;";
            case FLOAT:
                return "(F)Ljava/lang/StringBuilder;";
            case DOUBLE:
                return "(D)Ljava/lang/StringBuilder;";
            case BOOLEAN:
                return "(Z)Ljava/lang/StringBuilder;";
            default:
                return "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
        }
    }        
}

class BytecodeGenerationException extends RuntimeException {
    final int line;
    final int column;
    
    BytecodeGenerationException(String message, int line, int column) {
        super(String.format("%s at %d:%d", message, line, column));
        this.line = line;
        this.column = column;
    }
}