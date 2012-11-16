package org.projecteuler;

/**
 * A palindromic number reads the same both ways. 
 * The largest palindrome made from the product of two 2-digit numbers is
 * 9009 = 91 × 99. Find the largest palindrome made from the product of two 3-digit numbers.
 */
public class Problem_0004 {

    private static int solve() {
        int palindrome = 0;
        int product;
        StringBuilder productStr = new StringBuilder();

        for (int i = 100; i <= 999; i++) {
            for (int j = 100; j <= 999; j++) {
                product = i * j;
                productStr.delete(0, productStr.length()).append(product);

                if (product > palindrome
                        && productStr.toString().equals(productStr.reverse().toString())) {
                    palindrome = product;
                }
            }
        }

        return palindrome;
    }

    public static void main(String... args) {
        long start = System.currentTimeMillis();
        System.out.println(solve());
        System.out.println(System.currentTimeMillis() - start);
    }
}
