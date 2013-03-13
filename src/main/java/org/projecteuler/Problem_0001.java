package org.projecteuler;

/**
 * If we list all the natural numbers below 10 that are multiples of 3 or 5,
 * we get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *
 * @author Stefan Lourens <stefan.lourens@gmail.com>
 */
public class Problem_0001 {

    private static int solve() {
        int sum = 0;

        for (int i = 0; i < 1000; i++) {
            if (i % 3 == 0) {
                sum += i;
            } else if (i % 5 == 0) {
                sum += i;
            }
        }

        return sum;
    }

    public static void main(String ... args) {
        long start = System.currentTimeMillis();
        System.out.println(solve());
        System.out.println(System.currentTimeMillis() - start);
    }
}
