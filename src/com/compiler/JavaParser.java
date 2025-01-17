package com.compiler;

import java.util.*;

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
        System.out.println("Current token: " + currentToken.type + " at " + currentToken.line + ":" + currentToken.column + " ('" + currentToken.lexeme + "')");  // Debug line
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
                return parseExpressionStatement();
        }
    }

    private Statement parseLocalVariableDeclarationOrExpressionStatement() {
        int line = currentToken.line;
        int column = currentToken.column;
        
        // Save current position in case we need to backtrack
        Token saved = currentToken;
        
        try {
            // Try parsing as variable declaration
            TypeNode type;
            if (currentToken.type == TokenType.IDENTIFIER) {
                String typeName = currentToken.lexeme;
                consume(TokenType.IDENTIFIER);
                type = new TypeNode(typeName, false, line, column);
            } else {
                type = parseType();
            }
            
            // Must be followed by identifier for variable name
            if (currentToken.type != TokenType.IDENTIFIER) {
                // Not a variable declaration, restore position and try expression
                currentToken = saved;
                return parseExpressionStatement();
            }
            
            String name = currentToken.lexeme;
            consume(TokenType.IDENTIFIER);
            
            // Handle initializer
            Expression initializer = null;
            if (match(TokenType.EQUALS)) {
                initializer = parseExpression();
            }
            
            consume(TokenType.SEMICOLON);
            return new LocalVariableDeclaration(type, name, initializer, line, column);
            
        } catch (ParserException e) {
            // If variable declaration fails, try parsing as expression
            currentToken = saved;
            return parseExpressionStatement();
        }
    }
    
    private Statement parseExpressionStatement() {
        int line = currentToken.line;
        int column = currentToken.column;
        
        Expression expr = parseExpression();
        consume(TokenType.SEMICOLON);
        return new ExpressionStatement(expr, line, column);
    }
    
    // private LocalVariableDeclaration parseLocalVariableDeclaration() {
    //     int line = currentToken.line;
    //     int column = currentToken.column;
        
    //     // Parse type
    //     TypeNode type;
    //     if (currentToken.type == TokenType.IDENTIFIER) {
    //         String typeName = currentToken.lexeme;
    //         consume(TokenType.IDENTIFIER);
    //         type = new TypeNode(typeName, false, line, column);
    //     } else {
    //         type = parseType();
    //     }
        
    //     // Parse variable name
    //     String name = currentToken.lexeme;
    //     consume(TokenType.IDENTIFIER);
        
    //     // Handle initializer
    //     Expression initializer = null;
    //     if (match(TokenType.EQUALS)) {
    //         initializer = parseExpression();
    //     }
        
    //     consume(TokenType.SEMICOLON);
    //     return new LocalVariableDeclaration(type, name, initializer, line, column);
    // }
    
    private Expression parseExpression() {
        if (currentToken.type == TokenType.NEW) {
            Token newToken = currentToken;
            consume(TokenType.NEW);
            String typeName = currentToken.lexeme;
            consume(TokenType.IDENTIFIER);
            
            consume(TokenType.LPAREN);
            List<Expression> arguments = new ArrayList<>();
            
            if (currentToken.type != TokenType.RPAREN) {
                do {
                    arguments.add(parsePrimary());
                } while (match(TokenType.COMMA));
            }
            
            consume(TokenType.RPAREN);
            return new ObjectCreation(typeName, arguments, newToken.line, newToken.column);
        }
        
        return parseBinaryExpression(0);
    }
    
    private static final Map<TokenType, Integer> PRECEDENCE;
    static {
        PRECEDENCE = new HashMap<>();
        PRECEDENCE.put(TokenType.PLUS, 1);
        PRECEDENCE.put(TokenType.MINUS, 1);
        PRECEDENCE.put(TokenType.STAR, 2);
        PRECEDENCE.put(TokenType.SLASH, 2);
    }
    
    private Expression parseBinaryExpression(int precedence) {
        Expression left = parsePrimary();
        
        while (true) {
            // If we're at the end or hit a semicolon, break
            if (currentToken.type == TokenType.EOF || currentToken.type == TokenType.SEMICOLON) {
                break;
            }
            
            TokenType operator = currentToken.type;
            int newPrecedence = PRECEDENCE.getOrDefault(operator, -1);
            
            if (newPrecedence < precedence) {
                break;
            }
            
            consume(operator);  // Consume the operator
            Expression right = parsePrimary();  // Changed from parseBinaryExpression to parsePrimary
            left = new BinaryExpression(left, operator, right, left.line, left.column);
        }
        
        return left;
    }
    
    private Expression parsePrimary() {
        Token token = currentToken;
        
        switch (token.type) {
            case INTEGER_LITERAL:
                consume(TokenType.INTEGER_LITERAL);
                return new IntegerLiteral(Integer.parseInt(token.lexeme), token.line, token.column);
                
            case STRING_LITERAL:
                consume(TokenType.STRING_LITERAL);
                return new StringLiteral(token.lexeme, token.line, token.column);
                
            case IDENTIFIER:
                return parseIdentifierExpression();
                
            case LPAREN:
                consume(TokenType.LPAREN);
                Expression expr = parseExpression();
                consume(TokenType.RPAREN);
                return expr;
                
            default:
                throw new ParserException(
                    "Unexpected token in primary expression: " + token.type,
                    token.line,
                    token.column
                );
        }
    }
    
    private Expression parseIdentifierExpression() {
        Token token = currentToken;
        consume(TokenType.IDENTIFIER);
        
        Expression expr = new VariableReference(token.lexeme, token.line, token.column);
        
        // Handle field access and method calls
        while (currentToken.type == TokenType.DOT || currentToken.type == TokenType.LPAREN) {
            if (currentToken.type == TokenType.DOT) {
                Token dotToken = currentToken;  // Save dot token position
                consume(TokenType.DOT);
                Token field = currentToken;
                consume(TokenType.IDENTIFIER);
                expr = new FieldAccess(expr, field.lexeme, dotToken.line, dotToken.column);
            } else if (currentToken.type == TokenType.LPAREN) {
                expr = parseMethodInvocationRest(expr);
            }
        }
        
        return expr;
    }

    private Expression parseMethodInvocationRest(Expression target) {
        int line = currentToken.line;
        int column = currentToken.column;
        
        consume(TokenType.LPAREN);
        List<Expression> arguments = new ArrayList<>();
        
        if (currentToken.type != TokenType.RPAREN) {
            do {
                arguments.add(parseExpression());
            } while (match(TokenType.COMMA));
        }
        
        consume(TokenType.RPAREN);
        
        String methodName;
        Expression actualTarget;
        
        if (target instanceof FieldAccess) {
            FieldAccess fieldAccess = (FieldAccess) target;
            methodName = fieldAccess.fieldName;
            actualTarget = fieldAccess.target;
        } else if (target instanceof VariableReference) {
            methodName = ((VariableReference) target).name;
            actualTarget = null;
        } else {
            throw new ParserException(
                "Invalid method call target",
                line,
                column
            );
        }
        
        return new MethodInvocation(actualTarget, methodName, arguments, line, column);
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