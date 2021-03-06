package org.projecteuler;

/*
 * 2520 is the smallest number that can be divided by each of the numbers from 1
 * to 10 without any remainder. What is the smallest positive number that is
 * evenly divisible by all of the numbers from 1 to 20?
 */
public class Problem_0005 {

    private static int solve() {
        int i = 20;
        
        while (true) {
            for (int j = 1; j <= 20 && i % j == 0; j++) {
                if (j == 20) {
                    return i;
                }
            }
            
            i += 20;
        }
    }

    public static void main(String... args) {
        System.out.println(solve());
    }
}
