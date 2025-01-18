// src/com/compiler/Main.java

package com.compiler;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: java Main <source-file> <output-dir>");
            System.exit(1);
        }

        String sourceFile = args[0];
        String outputDir = args[1];

        try {
            // Read source file content
            String source = new String(Files.readAllBytes(Paths.get(sourceFile)));
            
            // Parse the source
            JavaLexer lexer = new JavaLexer(source);
            JavaParser parser = new JavaParser(lexer);
            
            // Generate and print AST
            CompilationUnit ast = parser.parseCompilationUnit();
            
            System.out.println("\nAbstract Syntax Tree:");
            System.out.println("=====================");
            ASTPrinter printer = new ASTPrinter();
            System.out.println(printer.print(ast));
            
            // Continue with compilation...
            JavaCompiler compiler = new JavaCompiler();
            boolean success = compiler.compile(sourceFile, outputDir);

            // Print any diagnostics
            compiler.getDiagnostics().forEach(System.err::println);

            // Exit with appropriate status code
            System.exit(success ? 0 : 1);
            
        } catch (IOException e) {
            System.err.println("Error reading source file: " + e.getMessage());
            System.exit(1);
        }
    }
}