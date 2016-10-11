(* Least Factorial

Given an integer n, find the minimal k such that
k = m! (where m! = 1 * 2 * ... * m) for some integer m;
k >= n.
In other words, find the smallest factorial which is not less than n.

Example
For n = 17, the output should be
leastFactorial(n) = 24.
17 < 24 = 4! = 1 * 2 * 3 * 4, while 3! = 1 * 2 * 3 = 6 < 17).

*)

let leastFactorial n =
    if n = 1 then 1 else
    let factorialGenerator (a, b) = (a+1), (a+1)*b
    let facSeq = Seq.unfold (fun (a,b) -> Some( (a,b), factorialGenerator (a,b) ) ) (0,1)
    facSeq 
    |> Seq.takeWhile (fun (x,y) -> y < n) 
    |> Seq.last 
    |> factorialGenerator 
    |> snd

(* Count Sum of Two Representations 2 

Given integers n, l and r,
find the number of ways to represent n as
a sum of two integers A and B such that l ≤ A ≤ B ≤ r.

Example

For n = 6, l = 2 and r = 4, the output should be
countSumOfTwoRepresentations2(n, l, r) = 2.
There are just two ways to write 6 as A + B,
where 2 ≤ A ≤ B ≤ 4: 6 = 2 + 4 and 6 = 3 + 3.

*)

//exceeds excecution time :(
let countSumOfTwoRepresentations2'' n l r =
    let mutable count = 0 
    for i in l .. r do 
        for j in i .. r do
            if i+j = n then 
                count <- count + 1 
    count

//exceeds excecution time :(
let countSumOfTwoRepresentations2' n l r =
    seq { for i in l .. r do 
            for j in i .. r do
                yield (i,j) } 
                |> Seq.filter (fun x -> (fst x) + (snd x) = n ) 
                |> Seq.length