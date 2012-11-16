package problems;

/*
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 * What is the 10 001st prime number?
 *
 */
public class Problem_0007 {

    private static int solve() {
        final int nthPrimeToFind = 10001;
        int primeCount = 1;

        for (int i = 3; true; i += 2) {
            if (isPrime(i)) {
                primeCount++;
                
                if (primeCount == nthPrimeToFind) {
                    return i;
                }
            }
        }
    }

    private static boolean isPrime(int num) {
        switch(num) {
            case 1: return false;
            case 2:
            case 3: return true;
        }
        
        if (num % 2 == 0) {
            return false;
        } else {
            int devisor = 1;
            
            while (devisor < (Math.sqrt(num))) {
                devisor += 2;

                if (num % devisor == 0) {
                    return false;
                }
            }

            return true;
        }
    }

    public static void main(String... args) {
        long start = System.currentTimeMillis();
        System.out.println(solve());
        System.out.println(System.currentTimeMillis() - start);
    }
}
