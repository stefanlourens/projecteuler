package problems;

/*
 * The sum of the squares of the first ten natural numbers is, 
 * 12 + 22 + ... + 102 = 385
 *
 * The square of the sum of the first ten natural numbers is, 
 * (1 + 2 + ... + 10)2 = 552 = 3025
 *
 * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is
 * 3025 − 385 = 2640.
 *
 * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the
 * sum.
 *
 */
public class Problem_0006 {

    private static double solve() {
        double sumOfSq = 0;
        double sqOfSum = 0;
        
        for (double i = 1; i <= 100; i++) {
            sumOfSq += Math.pow(i, 2);
            sqOfSum += i;
        }
        
        return Math.pow(sqOfSum, 2) - sumOfSq;
    }

    public static void main(String... args) {
        System.out.printf("%.0f", solve());
    }
}
