package org.projecteuler;

/**
 * Each new term in the Fibonacci sequence is generated by adding the previous
 * two terms. By starting with 1 and 2, the first 10 terms will be: 1, 2, 3, 5,
 * 8, 13, 21, 34, 55, 89, ... By considering the terms in the Fibonacci sequence
 * whose values do not exceed four million, find the sum of the even-valued
 * terms.
 */
public class Problem_0002 {

    private static int solve() {
        int sum = 0;

        for (int p = 1, c = 2, n = 0; n < 4e6; n = p + c, p = c, c = n) {
            if (c % 2 == 0) {
                sum += c;
            }
        }

        return sum;
    }

    public static void main(String... args) {
        long start = System.currentTimeMillis();
        System.out.println(solve());
        System.out.println(System.currentTimeMillis() - start);
    }
}
