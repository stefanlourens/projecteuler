package problems;

/*
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 * 
 */
public class Problem_0010 {

    private static long solve() {
        final int max = 2000000;
        long primeSum = 2;
        
        for (int i = 3; i < max; i +=2) {
            if (isPrime(i)) {
                primeSum += i;
            }
        }
        
        return primeSum;
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
