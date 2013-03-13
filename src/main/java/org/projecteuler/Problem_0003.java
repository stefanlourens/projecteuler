package org.projecteuler;

import java.util.Stack;

/**
 * The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime
 * factor of the number 600851475143 ?
 */
public class Problem_0003 {

    private static final long num = 600851475143L;

    /**
     * Its 01:21 on a Saturday, this is going to be "good"
     * Could not figure out why the initial version was talking so long,
     * was calculating for all numbers where div < num / 2 instead of sqrt(num) O_o
     *
     */
    private static long solve() {
        Stack<Long> factors = determineFactors(num);

        while (!factors.empty()) {
            long factor = factors.pop();

            if (isPrime(factor)) {
                return factor;
            }
        }

        return 1;
    }

    private static Stack<Long> determineFactors(long num) {
        Stack<Long> factors = new Stack<Long>();

        if (num % 2 == 0) {
            factors.add(2L);
        }

        long devisor = 3;

        while (devisor < (Math.sqrt(num))) {
            devisor += 2;

            if (num % devisor == 0) {
                factors.push(devisor);
            }
        }

        return factors;
    }

    private static boolean isPrime(long num) {
        long divisor = 1;

        if (num % 2 == 0) {
            return false;
        } else {
            while (divisor < (Math.sqrt(num))) {
                divisor += 2;

                if (num % divisor == 0) {
                    return false;
                }
            }

            return true;
        }
    }

    public static void main(String... args) {
        System.out.println(solve());
    }
}
