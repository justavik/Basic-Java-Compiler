package com.compiler;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        String testProgram = 
            "import java.util.Scanner;\n" +
            "class Main {\n" +
            "    public static void main(String[] args) {\n" +
            "        Scanner ac = new Scanner(System.in);\n" +
            "        int a = ac.nextInt();\n" +
            "        int b = ac.nextInt();\n" +
            "        int c = a + b;\n" +
            "        System.out.println(\"Output = \" + c);\n" +
            "        ac.close();\n" +
            "    }\n" +
            "}";

        try {
            // Lexing and Parsing
            JavaLexer lexer = new JavaLexer(testProgram);
            JavaParser parser = new JavaParser(lexer);
            CompilationUnit ast = parser.parseCompilationUnit();

            // Semantic Analysis
            SemanticAnalyzer analyzer = new SemanticAnalyzer();
            analyzer.analyze(ast);

            // Intermediate Code Generation
            IntermediateCodeGenerator icg = new IntermediateCodeGenerator();
            List<String> intermediateCode = icg.generate(ast);

            System.out.println("Intermediate Code:");
            intermediateCode.forEach(System.out::println);

            // Optimization
            BasicOptimizer optimizer = new BasicOptimizer();
            List<String> optimizedCode = optimizer.optimize(intermediateCode);

            System.out.println("\nOptimized Code:");
            optimizedCode.forEach(System.out::println);

            // Bytecode Generation (existing)
            BytecodeGenerator generator = new BytecodeGenerator("Main");
            byte[] bytecode = generator.generate(ast);
            Files.write(Paths.get("Main.class"), bytecode);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}