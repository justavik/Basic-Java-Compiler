// src/com/compiler/JavaParser.java

package com.compiler;

import java.util.*;
import java.util.Objects;

// The AST node classes - these represent the actual program structure
abstract class ASTNode {
    final int line;
    final int column;
    
    ASTNode(int line, int column) {
        this.line = line;
        this.column = column;
    }
}

class CompilationUnit extends ASTNode {
    final List<ImportDeclaration> imports;
    final ClassDeclaration mainClass;
    
    CompilationUnit(List<ImportDeclaration> imports, ClassDeclaration mainClass, int line, int column) {
        super(line, column);
        this.imports = imports;
        this.mainClass = mainClass;
    }
}

class ImportDeclaration extends ASTNode {
    final String qualifiedName;
    
    ImportDeclaration(String qualifiedName, int line, int column) {
        super(line, column);
        this.qualifiedName = qualifiedName;
    }
}

class ClassDeclaration extends ASTNode {
    final String name;
    final List<MethodDeclaration> methods;
    final List<FieldDeclaration> fields;
    
    ClassDeclaration(String name, List<MethodDeclaration> methods, List<FieldDeclaration> fields, int line, int column) {
        super(line, column);
        this.name = name;
        this.methods = methods;
        this.fields = fields;
    }
}

class MethodDeclaration extends ASTNode {
    final List<Modifier> modifiers;
    final TypeNode returnType;
    final String name;
    final List<ParameterDeclaration> parameters;
    final Block body;
    
    MethodDeclaration(List<Modifier> modifiers, TypeNode returnType, String name, 
                     List<ParameterDeclaration> parameters, Block body, int line, int column) {
        super(line, column);
        this.modifiers = modifiers;
        this.returnType = returnType;
        this.name = name;
        this.parameters = parameters;
        this.body = body;
    }
}

class FieldDeclaration extends ASTNode {
    final List<Modifier> modifiers;
    final TypeNode type;
    final String name;
    final Expression initializer;
    
    FieldDeclaration(List<Modifier> modifiers, TypeNode type, String name, Expression initializer, int line, int column) {
        super(line, column);
        this.modifiers = modifiers;
        this.type = type;
        this.name = name;
        this.initializer = initializer;
    }
}

class ParameterDeclaration extends ASTNode {
    final TypeNode type;
    final String name;
    
    ParameterDeclaration(TypeNode type, String name, int line, int column) {
        super(line, column);
        this.type = type;
        this.name = name;
    }
}

class Block extends ASTNode {
    final List<Statement> statements;
    
    Block(List<Statement> statements, int line, int column) {
        super(line, column);
        this.statements = statements;
    }
}

abstract class Statement extends ASTNode {
    Statement(int line, int column) {
        super(line, column);
    }
}

class ExpressionStatement extends Statement {
    final Expression expression;
    
    ExpressionStatement(Expression expression, int line, int column) {
        super(line, column);
        this.expression = expression;
    }
}

class LocalVariableDeclaration extends Statement {
    final TypeNode type;
    final String name;
    final Expression initializer;
    
    LocalVariableDeclaration(TypeNode type, String name, Expression initializer, int line, int column) {
        super(line, column);
        this.type = type;
        this.name = name;
        this.initializer = initializer;
    }
}

abstract class Expression extends ASTNode {
    Expression(int line, int column) {
        super(line, column);
    }

    @Override
    public int hashCode() {
        return Objects.hash(line, column);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Expression that = (Expression) o;
        return line == that.line && column == that.column;
    }
}

class BinaryExpression extends Expression {
    final Expression left;
    final TokenType operator;
    final Expression right;
    
    BinaryExpression(Expression left, TokenType operator, Expression right, int line, int column) {
        super(line, column);
        this.left = left;
        this.operator = operator;
        this.right = right;
    }
}

class MethodInvocation extends Expression {
    final Expression target;
    final String methodName;
    final List<Expression> arguments;
    
    MethodInvocation(Expression target, String methodName, List<Expression> arguments, int line, int column) {
        super(line, column);
        this.target = target;
        this.methodName = methodName;
        this.arguments = arguments;
    }
}

class VariableReference extends Expression {
    final String name;
    
    VariableReference(String name, int line, int column) {
        super(line, column);
        this.name = name;
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        VariableReference that = (VariableReference) o;
        return Objects.equals(name, that.name);
    }
}

class IntegerLiteral extends Expression {
    final int value;
    
    IntegerLiteral(int value, int line, int column) {
        super(line, column);
        this.value = value;
    }
}

class StringLiteral extends Expression {
    final String value;
    
    StringLiteral(String value, int line, int column) {
        super(line, column);
        this.value = value;
    }
}

class TypeNode extends ASTNode {
    final String name;
    final boolean isArray;
    
    TypeNode(String name, boolean isArray, int line, int column) {
        super(line, column);
        this.name = name;
        this.isArray = isArray;
    }
}

enum Modifier {
    PUBLIC, PRIVATE, PROTECTED, STATIC, FINAL, ABSTRACT
}

class ObjectCreation extends Expression {
    final String typeName;
    final List<Expression> arguments;
    
    ObjectCreation(String typeName, List<Expression> arguments, int line, int column) {
        super(line, column);
        this.typeName = typeName;
        this.arguments = arguments;
    }
}

class FieldAccess extends Expression {
    final Expression target;
    final String fieldName;
    
    FieldAccess(Expression target, String fieldName, int line, int column) {
        super(line, column);
        this.target = target;
        this.fieldName = fieldName;
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), target, fieldName);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        FieldAccess that = (FieldAccess) o;
        return Objects.equals(target, that.target) && Objects.equals(fieldName, that.fieldName);
    }
}

// The actual parser implementation
public class JavaParser {
    private final JavaLexer lexer;
    private Token currentToken;

    public JavaParser(JavaLexer lexer) {
        this.lexer = lexer;
        this.currentToken = lexer.nextToken();
    }
    
    private void consume(TokenType expected) {
        if (currentToken.type != expected) {
            throw new ParserException(
                String.format("Expected token %s but got %s", expected, currentToken.type),
                currentToken.line,
                currentToken.column
            );
        }
        currentToken = lexer.nextToken();
    }
    
    private boolean match(TokenType... types) {
        for (TokenType type : types) {
            if (currentToken.type == type) {
                currentToken = lexer.nextToken();
                return true;
            }
        }
        return false;
    }
    
    public CompilationUnit parseCompilationUnit() {
        int line = currentToken.line;
        int column = currentToken.column;
        
        List<ImportDeclaration> imports = new ArrayList<>();
        while (match(TokenType.IMPORT)) {
            imports.add(parseImportDeclaration());
        }
        
        ClassDeclaration mainClass = parseClassDeclaration();
        return new CompilationUnit(imports, mainClass, line, column);
    }
    
    private ImportDeclaration parseImportDeclaration() {
        int line = currentToken.line;
        int column = currentToken.column;
        
        StringBuilder qualifiedName = new StringBuilder();
        
        // Handle the first identifier
        qualifiedName.append(currentToken.lexeme);
        consume(TokenType.IDENTIFIER);
        
        // Keep consuming identifiers and dots
        while (match(TokenType.DOT)) {
            qualifiedName.append('.');
            if (currentToken.type != TokenType.IDENTIFIER) {
                throw new ParserException(
                    "Expected identifier after '.' in import statement",
                    currentToken.line,
                    currentToken.column
                );
            }
            qualifiedName.append(currentToken.lexeme);
            consume(TokenType.IDENTIFIER);
        }
        
        consume(TokenType.SEMICOLON);
        return new ImportDeclaration(qualifiedName.toString(), line, column);
    }


    
    private ClassDeclaration parseClassDeclaration() {
        int line = currentToken.line;
        int column = currentToken.column;
        
        // Parse class modifiers
        List<Modifier> modifiers = new ArrayList<>();
        while (currentToken.type == TokenType.PUBLIC || currentToken.type == TokenType.FINAL) {
            modifiers.add(Modifier.valueOf(currentToken.lexeme.toUpperCase()));
            currentToken = lexer.nextToken();
        }
        
        consume(TokenType.CLASS);
        String className = currentToken.lexeme;
        consume(TokenType.IDENTIFIER);
        
        consume(TokenType.LBRACE);
        
        List<FieldDeclaration> fields = new ArrayList<>();
        List<MethodDeclaration> methods = new ArrayList<>();
        
        while (currentToken.type != TokenType.RBRACE) {
            // Collect modifiers
            List<Modifier> memberModifiers = new ArrayList<>();
            while (isModifier(currentToken.type)) {
                memberModifiers.add(Modifier.valueOf(currentToken.lexeme.toUpperCase()));
                currentToken = lexer.nextToken();
            }
            
            // Save current position
            int memberLine = currentToken.line;
            int memberColumn = currentToken.column;
            
            // Parse type using common method
            TypeNode type = parseType();
            
            // Get the name
            if (currentToken.type != TokenType.IDENTIFIER) {
                throw new ParserException(
                    "Expected identifier but got " + currentToken.type,
                    currentToken.line,
                    currentToken.column
                );
            }
            
            String name = currentToken.lexeme;
            consume(TokenType.IDENTIFIER);
            
            if (currentToken.type == TokenType.LPAREN) {
                // This is a method
                methods.add(parseMethodRest(memberModifiers, type, name, memberLine, memberColumn));
            } else {
                // This is a field
                fields.add(parseFieldRest(memberModifiers, type, name, memberLine, memberColumn));
            }
        }
        
        consume(TokenType.RBRACE);
        return new ClassDeclaration(className, methods, fields, line, column);
    }

    private MethodDeclaration parseMethodRest(List<Modifier> modifiers, TypeNode returnType, 
                                        String name, int line, int column) {
        List<ParameterDeclaration> parameters = new ArrayList<>();
        
        consume(TokenType.LPAREN);
        
        if (currentToken.type != TokenType.RPAREN) {
            do {
                parameters.add(parseParameter());
            } while (match(TokenType.COMMA));
        }
        
        consume(TokenType.RPAREN);
        Block body = parseBlock();
        
        return new MethodDeclaration(modifiers, returnType, name, parameters, body, line, column);
    }

    private FieldDeclaration parseFieldRest(List<Modifier> modifiers, TypeNode type,
                                        String name, int line, int column) {
        Expression initializer = null;
        if (match(TokenType.EQUALS)) {
            initializer = parseExpression();
        }
        
        consume(TokenType.SEMICOLON);
        return new FieldDeclaration(modifiers, type, name, initializer, line, column);
    }
    
    private boolean isModifier(TokenType type) {
        return type == TokenType.PUBLIC || type == TokenType.PRIVATE || 
               type == TokenType.PROTECTED || type == TokenType.STATIC || 
               type == TokenType.FINAL || type == TokenType.ABSTRACT;
    }
    
    private TypeNode parseType() {
        // int line = currentToken.line;
        // int column = currentToken.column;
        
        // Save the type token information
        String typeName = currentToken.lexeme;
        Token typeToken = currentToken;
        
        // Advance token
        currentToken = lexer.nextToken();
        
        // Handle array notation if present
        boolean isArray = false;
        if (currentToken.type == TokenType.LBRACKET) {
            consume(TokenType.LBRACKET);
            consume(TokenType.RBRACKET);
            isArray = true;
        }
        
        return new TypeNode(typeName, isArray, typeToken.line, typeToken.column);
    }
    
    private ParameterDeclaration parseParameter() {
        int line = currentToken.line;
        int column = currentToken.column;
        
        TypeNode type = parseType();  // This already handles array notation
        
        String name = currentToken.lexeme;
        consume(TokenType.IDENTIFIER);
        
        return new ParameterDeclaration(type, name, line, column);
    }
    
    private Block parseBlock() {
        int line = currentToken.line;
        int column = currentToken.column;
        
        consume(TokenType.LBRACE);
        List<Statement> statements = new ArrayList<>();
        
        while (currentToken.type != TokenType.RBRACE) {
            statements.add(parseStatement());
        }
        
        consume(TokenType.RBRACE);
        return new Block(statements, line, column);
    }
    
    private Statement parseStatement() {
        switch (currentToken.type) {
            case INT:
            case STRING:
            case VOID:
            case IDENTIFIER:
                return parseLocalVariableDeclarationOrExpressionStatement();
            default:
                // Making sure expression statements can handle method calls
                int line = currentToken.line;    // Add this
                int column = currentToken.column; // Add this
                Expression expr = parseExpression(TokenType.SEMICOLON);
                consume(TokenType.SEMICOLON);
                return new ExpressionStatement(expr, line, column);  // Pass line and column
        }
    }

    private Statement parseLocalVariableDeclarationOrExpressionStatement() {
        int line = currentToken.line;
        int column = currentToken.column;
        
        // Save current position in case we need to backtrack
        Token saved = currentToken;
        int savePoint = lexer.getCurrentPosition(); // Add this method to your lexer to track position
        
        try {
            // First, check if this might be a variable declaration
            if (isTypeToken(currentToken.type)) {
                TypeNode type = parseType();
                
                if (currentToken.type == TokenType.IDENTIFIER) {
                    String name = currentToken.lexeme;
                    consume(TokenType.IDENTIFIER);
                    
                    // Now we're at a point where we can determine if this is definitely a declaration
                    if (currentToken.type == TokenType.EQUALS || currentToken.type == TokenType.SEMICOLON) {
                        Expression initializer = null;
                        if (match(TokenType.EQUALS)) {
                            initializer = parseExpression(TokenType.SEMICOLON);
                        }
                        consume(TokenType.SEMICOLON);
                        return new LocalVariableDeclaration(type, name, initializer, line, column);
                    }
                }
            }
            
            // If we get here, either:
            // 1. This wasn't a type token to begin with
            // 2. We started parsing what looked like a declaration but wasn't
            // In either case, reset and parse as expression
            currentToken = saved;
            lexer.setPosition(savePoint); // Add this method to your lexer
            
            Expression expr = parseExpression(TokenType.SEMICOLON);
            consume(TokenType.SEMICOLON);
            return new ExpressionStatement(expr, line, column);
            
        } catch (ParserException e) {
            // If anything goes wrong, try parsing as expression
            currentToken = saved;
            lexer.setPosition(savePoint); // Add this method to your lexer
            Expression expr = parseExpression(TokenType.SEMICOLON);
            consume(TokenType.SEMICOLON);
            return new ExpressionStatement(expr, line, column);
        }
    }
    
    private boolean isTypeToken(TokenType type) {
        return type == TokenType.INT || 
               type == TokenType.STRING || 
               type == TokenType.VOID || 
               type == TokenType.BOOLEAN ||
               type == TokenType.IDENTIFIER;  // For custom types
    }
    
    private void debug(String method, String message) {
        System.out.printf("[DEBUG] %s: %s (Current Token: %s '%s' at %d:%d)\n",
            method,
            message,
            currentToken.type,
            currentToken.lexeme,
            currentToken.line,
            currentToken.column
        );
    }
    
    private static final Map<TokenType, Integer> PRECEDENCE;
    static {
        PRECEDENCE = new HashMap<>();
        PRECEDENCE.put(TokenType.PLUS, 1);
        PRECEDENCE.put(TokenType.MINUS, 1);
        PRECEDENCE.put(TokenType.STAR, 2);
        PRECEDENCE.put(TokenType.SLASH, 2);
    }
    
    private Expression parsePrimary() {
        debug("parsePrimary", "Starting");
        Token token = currentToken;
        
        switch (token.type) {
            case INTEGER_LITERAL:
                debug("parsePrimary", "Handling integer literal");
                consume(TokenType.INTEGER_LITERAL);
                return new IntegerLiteral(Integer.parseInt(token.lexeme), token.line, token.column);
                
            case STRING_LITERAL:
                debug("parsePrimary", "Handling string literal");
                consume(TokenType.STRING_LITERAL);
                return new StringLiteral(token.lexeme, token.line, token.column);
                
            case NEW:
                debug("parsePrimary", "Handling object creation");
                consume(TokenType.NEW);
                String typeName = currentToken.lexeme;
                consume(TokenType.IDENTIFIER);
                consume(TokenType.LPAREN);
                List<Expression> arguments = parseArguments();
                return new ObjectCreation(typeName, arguments, token.line, token.column);
                
            case IDENTIFIER:
                debug("parsePrimary", "Handling identifier");
                consume(TokenType.IDENTIFIER);
                return new VariableReference(token.lexeme, token.line, token.column);
                
            case LPAREN:
                debug("parsePrimary", "Handling parenthesized expression");
                consume(TokenType.LPAREN);
                Expression expr = parseExpression();
                consume(TokenType.RPAREN);
                return expr;
                
            default:
                debug("parsePrimary", "Unexpected token");
                throw new ParserException(
                    "Unexpected token in primary expression: " + token.type,
                    token.line,
                    token.column
                );
        }
    }

    private List<Expression> parseArguments() {
        List<Expression> arguments = new ArrayList<>();
        if (currentToken.type != TokenType.RPAREN) {
            do {
                arguments.add(parseExpression(TokenType.COMMA, TokenType.RPAREN));
            } while (match(TokenType.COMMA));
        }
        consume(TokenType.RPAREN);
        return arguments;
    }
    
    private Expression parseExpression(TokenType... terminators) {
        debug("parseExpression", "Starting expression");
        Expression left = parsePrimary();
        
        while (!isTerminator(currentToken.type, terminators)) {
            if (currentToken.type == TokenType.DOT) {
                debug("parseExpression", "Found dot operator");
                System.out.println("DEBUG: Before dot - left is: " + left.getClass().getSimpleName());
                
                consume(TokenType.DOT);
                String identifier = currentToken.lexeme;
                int idLine = currentToken.line;
                int idColumn = currentToken.column;
                consume(TokenType.IDENTIFIER);
                System.out.println("DEBUG: After identifier: " + identifier);
                
                // Check for method call
                if (currentToken.type == TokenType.LPAREN) {
                    System.out.println("DEBUG: Found method call: " + identifier);
                    consume(TokenType.LPAREN);
                    List<Expression> arguments = new ArrayList<>();
                    if (currentToken.type != TokenType.RPAREN) {
                        do {
                            arguments.add(parseExpression(TokenType.COMMA, TokenType.RPAREN));
                        } while (match(TokenType.COMMA));
                    }
                    consume(TokenType.RPAREN);
                    left = new MethodInvocation(left, identifier, arguments, idLine, idColumn);
                } else {
                    System.out.println("DEBUG: Creating field access: " + identifier);
                    left = new FieldAccess(left, identifier, idLine, idColumn);
                }
                System.out.println("DEBUG: After dot processing - left is: " + left.getClass().getSimpleName());
            } else if (PRECEDENCE.containsKey(currentToken.type)) {
                TokenType operator = currentToken.type;
                consume(operator);
                Expression right = parsePrimary();
                left = new BinaryExpression(left, operator, right, left.line, left.column);
            } else {
                break;
            }
        }
        
        return left;
    }
    
    private boolean isTerminator(TokenType type, TokenType... terminators) {
        for (TokenType terminator : terminators) {
            if (type == terminator) return true;
        }
        return false;
    }
}

// Custom exception for parser errors
class ParserException extends RuntimeException {
    final int line;
    final int column;
    
    ParserException(String message, int line, int column) {
        super(String.format("%s at %d:%d", message, line, column));
        this.line = line;
        this.column = column;
    }
}