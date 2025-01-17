import java.util.Scanner;
class Main {
    public static void main(String[] args) {
        Scanner ac = new Scanner(System.in);
        int a = ac.nextInt();
        int b = ac.nextInt();
        int c = a + b;
        System.out.println("Output = " + c);
        ac.close();
    }
}