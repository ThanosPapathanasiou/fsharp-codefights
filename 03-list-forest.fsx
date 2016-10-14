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

(* is smooth

We define the middle of the array arr as follows:

if arr contains an odd number of elements, its middle is the element whose index number is the same when counting from the beginning of the array and from its end;
if arr contains an even number of elements, its middle is the sum of the two elements whose index numbers when counting from the beginning and from the end of the array differ by one.
An array is called smooth if its first and its last elements are equal to one another and to the middle. Given an array arr, determine if it is smooth or not.

Example

For arr = [7, 2, 2, 5, 10, 7], the output should be
isSmooth(arr) = true.

The first and the last elements of arr are equal to 7, and its middle also equals 2 + 5 = 7. Thus, the array is smooth and the output is true.

For arr = [-5, -5, 10], the output should be
isSmooth(arr) = false.

The first and middle elements are equal to -5, but the last element equals 10. Thus, arr is not smooth and the output is false.

*)


let isSmooth (arr:int array) =
    let middle = 
        if arr.Length % 2 = 1 then arr.[(arr.Length - 1) / 2]
        else (arr.[(arr.Length / 2) - 1] + arr.[(arr.Length / 2)])
    (middle = arr.[0]) && (middle = arr.[arr.Length - 1])

(* Replace Middle

We define the middle of the array arr as follows:

if arr contains an odd number of elements, its middle is the element whose index number is the same when counting from the beginning of the array and from its end;
if arr contains an even number of elements, its middle is the sum of the two elements whose index numbers when counting from the beginning and from the end of the array differ by one.
Given array arr, your task is to find its middle, and, if it consists of two elements, replace those elements with the value of middle. Return the resulting array as the answer.

Example

For arr = [7, 2, 2, 5, 10, 7], the output should be
replaceMiddle(arr) = [7, 2, 7, 10, 7].

The middle consists of two elements, 2 and 5. These two elements should be replaced with their sum, i.e. 7.

For arr = [-5, -5, 10], the output should be
replaceMiddle(arr) = [-5, -5, 10].

The middle is defined as a single element -5, so the initial array with no changes should be returned.

*)

let replaceMiddle (arr:int array) =
    if arr.Length % 2 = 1 then arr
    else 
        let result = Array.create (arr.Length - 1) 0
        let m = ((arr.Length / 2) - 1)
        let v = (arr.[m] + arr.[m + 1])
        Array.blit arr 0 result 0 m
        Array.blit [|v|] 0 result m 1
        Array.blit arr (m + 2) result (m + 1) m
        result

(* Make Array Consecutive 2

Given an array of integers, we need to find the number of "holes" that need to be filled such that it contains all the integers from some range.

Example

For sequence = [6, 2, 3, 8], the output should be
makeArrayConsecutive2(sequence) = 3.

We need to add in 4, 5 and 7.
*)

let makeArrayConsecutive2 (sequence:int array) =
    let inline exists (arr:int array) (i:int) = 
        arr |> Array.exists (fun x -> x = i) 
    let min = sequence |> Array.min
    let max = sequence |> Array.max
    [|min .. max|]
    |> Array.filter (fun x -> not (exists sequence x)) 
    |> Array.length 