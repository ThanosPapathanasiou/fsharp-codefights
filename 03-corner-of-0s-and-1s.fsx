(* kill the k-th bit

In order to stop the Mad Coder evil genius you need to decipher the encrypted message he sent to his minions. The message contains several numbers that, when typed into a supercomputer, will launch a missile into the sky blocking out the sun, and making all the people on Earth grumpy and sad.
You figured out that some numbers have a modified single digit in their binary representation. More specifically, in the given number n the kth bit from the right was initially set to 0, but its current value might be different. It's now up to you to write a function that will change the kth bit of n back to 0.
Example
For n = 37 and k = 3, the output should be
killKthBit(n, k) = 33.
3710 = 1001012 ~> 1000012 = 3310.
For n = 37 and k = 4, the output should be

killKthBit(n, k) = 37.
The 4th bit is 0 already (looks like the Mad Coder forgot to encrypt this number), so the answer is still 37.


*)

let killKthBit n k = 
    n &&& (~~~(pown 2 (k-1)))

(* array packing

You are given an array of up to four non-negative integers, each less than 256.
Your task is to pack these integers into one number M in the following way:
The first element of the array occupies the first 8 bits of M;
The second element occupies next 8 bits, and so on.
Return the obtained integer M.
Note: the phrase "first bits of M" refers to the least significant bits of M - the right-most bits of an integer. For further clarification see the following example.

Example
For a = [24, 85, 0], the output should be
arrayPacking(a) = 21784.
An array [24, 85, 0] looks like [00011000, 01010101, 00000000] in binary.
After packing these into one number we get 00000000 01010101 00011000 (spaces are placed for convenience), which equals to 21784.

*)

let arrayPacking (a:int array) =
    let mutable ret = 0
    for i=a.Length downto 1  do
        ret <- ret ||| (a.[i-1] <<< ( (i-1) * 8))
    ret
