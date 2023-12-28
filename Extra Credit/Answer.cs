using System;
using System.IO;
using System.Numerics;
using System.Linq;

namespace Program {
    class Answer {
        // Finding the numbers are that divisible by 2, 3, and 5
        // Using BigInteger, since the results will be large
        public static BigInteger Calculator(int element) {
            // Storing the divisors
            BigInteger div1 = 2, div2 = 3, div3 = 5;

            // Initializing an array to store the numbers created
            var array = new BigInteger[element];
            // Making the first element to be 1
            array[0] = 1;

            // Intializing the variables to store the multiples of
            // the divisors
            BigInteger two_mul = 2, three_mul = 3, five_mul = 5;
            
            // Intializing the pointers for the products
            int i = 0, j = 0, k = 0;

            // Making a loop to find the numbers
            for (int p = 1; p < element; p++) {
                // Putting the multiples in order
                array[p] = BigInteger.Min(two_mul, BigInteger.Min(three_mul, five_mul));
                // Doing the multiplications
                if (array[p] == two_mul) {
                    two_mul = div1 * array[++i];
                };
                if (array[p] == three_mul) {
                    three_mul = div2 * array[++j];
                };
                if (array[p] == five_mul) {
                    five_mul = div3 * array[++k];
                };
            }

            // Returning the h - 1, because the array starts from index 0
            return array[element - 1];
        }

        // Main function to return the values based on the element entered
        static void Main(string[] args) {
            // Printing out the 10^8 element
            Console.WriteLine("The 10^8 element is:");
            Console.WriteLine(Calculator(100000000));
        }
    }
}
