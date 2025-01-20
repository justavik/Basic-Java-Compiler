package com.compiler;

import java.util.ArrayList;
import java.util.List;

public class BasicOptimizer {
    public List<String> optimize(List<String> code) {
        List<String> optimizedCode = new ArrayList<>();
        for (String line : code) {
            if (line.contains("=")) {
                String[] parts = line.split(" = ");
                String lhs = parts[0];
                String rhs = parts[1];

                if (rhs.matches("\\d+ [+\\-*/] \\d+")) {
                    String[] ops = rhs.split(" ");
                    int left = Integer.parseInt(ops[0]);
                    int right = Integer.parseInt(ops[2]);
                    int result = 0;
                    switch (ops[1]) {
                        case "+": result = left + right; break;
                        case "-": result = left - right; break;
                        case "*": result = left * right; break;
                        case "/": result = left / right; break;
                    }
                    optimizedCode.add(lhs + " = " + result);
                    continue;
                }
            }
            optimizedCode.add(line);
        }
        return optimizedCode;
    }
}