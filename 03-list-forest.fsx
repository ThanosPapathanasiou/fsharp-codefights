(* Create Array

Given an integer size, return array of length size filled with 1s.
Example
For size = 4, the output should be
createArray(size) = [1, 1, 1, 1].

 *)

let createArray size =
    Array.init size (fun item -> 1 )

(* Array Replace

Given an array of integers,
replace all the occurrences of elemToReplace with substitutionElem.

Example
For inputArray = [1, 2, 1], elemToReplace = 1 and substitutionElem = 3, the output should be
arrayReplace(inputArray, elemToReplace, substitutionElem) = [3, 2, 3].
*)

let arrayReplace inputArray elemToReplace substitutionElem =
    let inline replace x = 
        if x = elemToReplace then substitutionElem else x
    inputArray
    |> Array.map replace

(* First Reverse try

Reversing an array can be a tough task,especially for a novice programmer.
Mary just started coding, so she would like to start with something basic at first.
Instead of reversing the array entirely, she wants to swap just its first and last elements.
Given an array arr, swap its first and last elements and return the resulting array.

Example
For arr = [1, 2, 3, 4, 5], the output should be
firstReverseTry(arr) = [5, 2, 3, 4, 1].

 *)

let firstReverseTry (arr:int array) =
    if arr.Length = 0 then [||]
    else 
    let mutable arr' = arr
    let l = arr'.Length - 1
    let h = arr.[0]
    arr'.[0] <- arr'.[l]
    arr'.[l] <- h
    arr'

(* Concatenate Arrays

Given two arrays of integers a and b, obtain the array formed by the elements of a followed by the elements of b.

Example
For a = [2, 2, 1] and b = [10, 11], the output should be
concatenateArrays(a, b) = [2, 2, 1, 10, 11].

*)

let concatenateArrays a b =
    Array.concat [| a ; b |]

(* Remove Array Part

Remove a part of a given array between given 0-based indexes l and r (inclusive).

Example
For inputArray = [2, 3, 2, 3, 4, 5], l = 2 and r = 4, the output should be
removeArrayPart(inputArray, l, r) = [2, 3, 5].

*)

let removeArrayPart (inputArray:int array) l r =
    let result = Array.init ((inputArray.Length - 1) - (r - l)) (fun i -> 0)
    let mutable index = 0
    for i = 0 to inputArray.Length - 1  do
        if not (l<=i && i<=r) then 
            result.[index] <- inputArray.[i]
            index <- index + 1
    result
