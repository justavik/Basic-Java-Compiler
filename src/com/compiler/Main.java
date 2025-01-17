package com.compiler;

public class Main {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: java Main <source-file> <output-dir>");
            System.exit(1);
        }

        String sourceFile = args[0];
        String outputDir = args[1];

        JavaCompiler compiler = new JavaCompiler();
        boolean success = compiler.compile(sourceFile, outputDir);

        // Print any diagnostics
        compiler.getDiagnostics().forEach(System.err::println);

        // Exit with appropriate status code
        System.exit(success ? 0 : 1);
    }
}