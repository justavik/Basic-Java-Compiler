package com.compiler;

import java.util.*;

public class JavaLexer {
    private final String input;
    private int position = 0;
    private int line = 1;
    private int column = 0;
    private Token currentToken = null;  // Add this field
    
    private static final Map<String, TokenType> KEYWORDS;
    static {
        KEYWORDS = new HashMap<>();
        KEYWORDS.put("class", TokenType.CLASS);
        KEYWORDS.put("public", TokenType.PUBLIC);
        KEYWORDS.put("static", TokenType.STATIC);
        KEYWORDS.put("void", TokenType.VOID);
        KEYWORDS.put("int", TokenType.INT);
        KEYWORDS.put("String", TokenType.STRING);
        KEYWORDS.put("new", TokenType.NEW);
        KEYWORDS.put("import", TokenType.IMPORT);
    }

    public JavaLexer(String input) {
        this.input = input;
    }

    private char peek() {
        if (position >= input.length()) return '\0';
        return input.charAt(position);
    }

    private char advance() {
        char current = peek();
        position++;
        if (current == '\n') {
            line++;
            column = 0;
        } else {
            column++;
        }
        return current;
    }

    private void skipWhitespace() {
        while (Character.isWhitespace(peek())) {
            advance();
        }
    }

    private Token identifier() {
        StringBuilder builder = new StringBuilder();
        int startPos = position;  // Save starting position
        while (Character.isJavaIdentifierPart(peek())) {
            builder.append(advance());
        }
        
        String word = builder.toString();
        TokenType type = KEYWORDS.get(word);
        if (type != null) {
            return new Token(type, word, line, column, startPos);
        }
        
        return new Token(TokenType.IDENTIFIER, word, line, column, startPos);
    }
    
    private Token number() {
        StringBuilder builder = new StringBuilder();
        int startPos = position;  // Save starting position
        while (Character.isDigit(peek())) {
            builder.append(advance());
        }
        return new Token(TokenType.INTEGER_LITERAL, builder.toString(), line, column, startPos);
    }
    
    private Token string() {
        int startPos = position;  // Save starting position
        advance(); // Skip opening quote
        StringBuilder builder = new StringBuilder();
        while (peek() != '"' && peek() != '\0') {
            if (peek() == '\\') {
                advance();
                switch (peek()) {
                    case 'n': builder.append('\n'); break;
                    case 't': builder.append('\t'); break;
                    case 'r': builder.append('\r'); break;
                    case '"': builder.append('"'); break;
                    case '\\': builder.append('\\'); break;
                }
                advance();
            } else {
                builder.append(advance());
            }
        }
        
        if (peek() == '\0') {
            throw new LexerException("Unterminated string literal", line, column);
        }
        
        advance(); // Skip closing quote
        return new Token(TokenType.STRING_LITERAL, builder.toString(), line, column, startPos);
    }
    
    public Token nextToken() {
        // If we have a pushed-back token, return it
        if (currentToken != null) {
            Token token = currentToken;
            currentToken = null;  // Clear it after use
            return token;
        }
        skipWhitespace();
        
        if (position >= input.length()) {
            return new Token(TokenType.EOF, "", line, column, position);
        }
    
        char c = peek();
        int startPos = position;  // Save starting position
        
        // Handle identifiers and keywords
        if (Character.isJavaIdentifierStart(c)) {
            return identifier();
        }
        
        // Handle numbers
        if (Character.isDigit(c)) {
            return number();
        }
        
        // Handle string literals
        if (c == '"') {
            return string();
        }
        
        // Handle operators and punctuation
        switch (c) {
            case '+': advance(); return new Token(TokenType.PLUS, "+", line, column, startPos);
            case '-': advance(); return new Token(TokenType.MINUS, "-", line, column, startPos);
            case '*': advance(); return new Token(TokenType.STAR, "*", line, column, startPos);
            case '/': advance(); return new Token(TokenType.SLASH, "/", line, column, startPos);
            case '=': 
                advance();
                if (peek() == '=') {
                    advance();
                    return new Token(TokenType.EQUALS_EQUALS, "==", line, column, startPos);
                }
                return new Token(TokenType.EQUALS, "=", line, column, startPos);
            case '(': advance(); return new Token(TokenType.LPAREN, "(", line, column, startPos);
            case ')': advance(); return new Token(TokenType.RPAREN, ")", line, column, startPos);
            case '{': advance(); return new Token(TokenType.LBRACE, "{", line, column, startPos);
            case '}': advance(); return new Token(TokenType.RBRACE, "}", line, column, startPos);
            case '[': advance(); return new Token(TokenType.LBRACKET, "[", line, column, startPos);
            case ']': advance(); return new Token(TokenType.RBRACKET, "]", line, column, startPos);
            case ';': advance(); return new Token(TokenType.SEMICOLON, ";", line, column, startPos);
            case '.': advance(); return new Token(TokenType.DOT, ".", line, column, startPos);
            case ',': advance(); return new Token(TokenType.COMMA, ",", line, column, startPos);
        }
        
        throw new LexerException("Unexpected character: " + c, line, column);
    }
    
    public void pushBack(Token token) {
        // Save the current token
        this.position = token.position;
        this.currentToken = token;
    }
}

// Token types enum
enum TokenType {
    // Existing tokens...
    CLASS, PUBLIC, STATIC, VOID, INT, STRING, NEW, IMPORT,
    PRIVATE, PROTECTED, FINAL, ABSTRACT, CONSTRUCTOR,
    
    // Add new tokens
    SYSTEM, IN, OUT, PRINTLN,
    
    // Existing tokens...
    IDENTIFIER, INTEGER_LITERAL, STRING_LITERAL,
    PLUS, MINUS, STAR, SLASH, EQUALS, EQUALS_EQUALS,
    LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET,
    SEMICOLON, DOT, COMMA,
    EOF
}

// Token class
class Token {
    final TokenType type;
    final String lexeme;
    final int line;
    final int column;
    final int position;
    
    // Original constructor with implied position of -1
    Token(TokenType type, String lexeme, int line, int column) {
        this(type, lexeme, line, column, -1);
    }
    
    // New constructor with position
    Token(TokenType type, String lexeme, int line, int column, int position) {
        this.type = type;
        this.lexeme = lexeme;
        this.line = line;
        this.column = column;
        this.position = position;
    }
    
    @Override
    public String toString() {
        return String.format("%s '%s' at %d:%d", type, lexeme, line, column);
    }
}

// Custom exception for lexer errors
class LexerException extends RuntimeException {
    final int line;
    final int column;
    
    LexerException(String message, int line, int column) {
        super(String.format("%s at %d:%d", message, line, column));
        this.line = line;
        this.column = column;
    }
}