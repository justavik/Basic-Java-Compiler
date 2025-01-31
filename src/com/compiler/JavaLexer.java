package com.compiler;

import java.util.*;

public class JavaLexer {
    private final String input;
    private int position = 0;
    private int line = 1;
    private int column = 0;
    private Token pushedBackToken = null;  

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
        KEYWORDS.put("boolean", TokenType.BOOLEAN);
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
        int startPos = position;  
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
        int startPos = position;  
        while (Character.isDigit(peek())) {
            builder.append(advance());
        }
        return new Token(TokenType.INTEGER_LITERAL, builder.toString(), line, column, startPos);
    }

    private Token string() {
        int startPos = position;  
        advance(); 
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

        advance(); 
        return new Token(TokenType.STRING_LITERAL, builder.toString(), line, column, startPos);
    }

    public Token nextToken() {

        if (pushedBackToken != null) {
            Token token = pushedBackToken;
            pushedBackToken = null;  
            return token;
        }

        skipWhitespace();

        if (position >= input.length()) {
            return new Token(TokenType.EOF, "", line, column, position);
        }

        int startPos = position;
        int startLine = line;
        int startColumn = column;

        char c = peek();

        if (Character.isJavaIdentifierStart(c)) {
            return identifier();
        }

        if (Character.isDigit(c)) {
            return number();
        }

        if (c == '"') {
            return string();
        }

        advance(); 

        switch (c) {
            case '+': return new Token(TokenType.PLUS, "+", startLine, startColumn, startPos);
            case '-': return new Token(TokenType.MINUS, "-", startLine, startColumn, startPos);
            case '*': return new Token(TokenType.STAR, "*", startLine, startColumn, startPos);
            case '/': return new Token(TokenType.SLASH, "/", startLine, startColumn, startPos);
            case '=': 
                if (peek() == '=') {
                    advance();
                    return new Token(TokenType.EQUALS_EQUALS, "==", startLine, startColumn, startPos);
                }
                return new Token(TokenType.EQUALS, "=", startLine, startColumn, startPos);
            case '(': return new Token(TokenType.LPAREN, "(", startLine, startColumn, startPos);
            case ')': return new Token(TokenType.RPAREN, ")", startLine, startColumn, startPos);
            case '{': return new Token(TokenType.LBRACE, "{", startLine, startColumn, startPos);
            case '}': return new Token(TokenType.RBRACE, "}", startLine, startColumn, startPos);
            case '[': return new Token(TokenType.LBRACKET, "[", startLine, startColumn, startPos);
            case ']': return new Token(TokenType.RBRACKET, "]", startLine, startColumn, startPos);
            case ';': return new Token(TokenType.SEMICOLON, ";", startLine, startColumn, startPos);
            case '.': return new Token(TokenType.DOT, ".", startLine, startColumn, startPos);
            case ',': return new Token(TokenType.COMMA, ",", startLine, startColumn, startPos);
            default:
                throw new LexerException("Unexpected character: " + c, line, column);
        }
    }

    public void pushBack(Token token) {

        if (pushedBackToken != null) {
            throw new IllegalStateException("Cannot push back multiple tokens");
        }
        this.position = token.position;
        this.pushedBackToken = token;
    }

    public int getCurrentPosition() {
        return position;
    }

    public void setPosition(int newPosition) {

        if (newPosition < 0 || newPosition > input.length()) {
            throw new IllegalArgumentException("Invalid position: " + newPosition);
        }

        if (newPosition < position) {

            line = 1;
            column = 0;

            for (int i = 0; i < newPosition; i++) {
                if (input.charAt(i) == '\n') {
                    line++;
                    column = 0;
                } else {
                    column++;
                }
            }
        }

        position = newPosition;
        pushedBackToken = null; 
    }
}

enum TokenType {

    CLASS, PUBLIC, STATIC, VOID, INT, STRING, NEW, IMPORT,
    PRIVATE, PROTECTED, FINAL, ABSTRACT, CONSTRUCTOR, BOOLEAN,

    SYSTEM, IN, OUT, PRINTLN,

    IDENTIFIER, INTEGER_LITERAL, STRING_LITERAL,
    PLUS, MINUS, STAR, SLASH, EQUALS, EQUALS_EQUALS,
    LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET,
    SEMICOLON, DOT, COMMA,
    EOF
}

class Token {
    final TokenType type;
    final String lexeme;
    final int line;
    final int column;
    final int position;

    Token(TokenType type, String lexeme, int line, int column) {
        this(type, lexeme, line, column, -1);
    }

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

class LexerException extends RuntimeException {
    final int line;
    final int column;

    LexerException(String message, int line, int column) {
        super(String.format("%s at %d:%d", message, line, column));
        this.line = line;
        this.column = column;
    }
}