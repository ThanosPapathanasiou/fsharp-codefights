(* add two digits

You are given a two-digit integer n. Return the sum of its digits.

Example
For n = 29, the output should be
addTwoDigits(n) = 11.

 *)

open System

let addTwoDigits (n:int) =
   n.ToString() 
   |> List.ofSeq 
   |> List.map Char.GetNumericValue 
   |> List.sum 
   |> (int)

(* Largest Number

Given an integer n, return the largest number that contains exactly n digits.

*)

let largestNumber n =
    let n' = Math.Pow (10.0, (float)n)
    (int)n' - 1

(* candies 

n children have got m pieces of candy. They want to eat as much candy as they can, but each child must eat exactly the same amount of candy as any other child. Determine how many pieces of candy will be eaten by all the children together. Individual pieces of candy cannot be split.

Example
For n = 3 and m = 10, the output should be
candies(n, m) = 9.
Each child will eat 3 pieces. So the answer is 9.

*)

let candies n m =
    (int) (m / n) * n

(* seats in theater

Your friend advised you to see a new performance in the most popular theater in the city. He knows a lot about art and his advice is usually good, but not this time: the performance turned out to be awfully dull. It's so bad you want to sneak out, which is quite simple, especially since the exit is located right behind your row to the left. All you need to do is climb over your seat and make your way to the exit.
The main problem is your shyness: you're afraid that you'll end up blocking the view (even if only for a couple of seconds) of all the people who sit behind you and in your column or the columns to your left. To gain some courage, you decide to calculate the number of such people and see if you can possibly make it to the exit without disturbing too many people.
Given the total number of rows and columns in the theater (nRows and nCols, respectively), and the row and column you're sitting in, return the number of people who sit strictly behind you and in your column or to the left, assuming all seats are occupied.

Example
For nCols = 16, nRows = 11, col = 5 and row = 3, the output should be
seatsInTheater(nCols, nRows, col, row) = 96.

*)

let seatsInTheater nCols nRows col row =
    (nCols - col + 1) * (nRows - row)

(* max multiple 

Given a divisor and a bound, find the largest integer N such that:
N is divisible by divisor.
N is less than or equal to bound.
N is greater than 0.
It is guaranteed that such a number exists.

Example
For divisor = 3 and bound = 10, the output should be
maxMultiple(divisor, bound) = 9.
The largest integer divisible by 3 and not larger than 10 is 9.

*)

let maxMultiple divisor bound =
    seq { bound .. -1 .. 0}
    |> Seq.filter (fun x -> x%divisor = 0)
    |> Seq.head

(* circle of numbers

Consider integer numbers from 0 to n - 1 written down along the circle in such a way that the distance between any two neighbouring numbers is equal (note that 0 and n - 1 are neighbouring, too).
Given n and firstNumber, find the number which is written in the radially opposite position to firstNumber.

Example
For n = 10 and firstNumber = 2, the output should be
circleOfNumbers(n, firstNumber) = 7.

*)

let circleOfNumbers n firstNumber =
    let middle = n / 2
    (middle + firstNumber) % n

(* late ride 

One night you go for a ride on your motorcycle. At 00:00 you start your engine, and the built-in timer automatically begins counting the length of your ride, in minutes. Off you go to explore the neighborhood.
When you finally decide to head back, you realize there's a chance the bridges on your route home are up, leaving you stranded! Unfortunately, you don't have your watch on you and don't know what time it is. All you know thanks to the bike's timer is that n minutes have passed since 00:00.
Using the bike's timer, calculate the current time. Return an answer as the sum of digits that the digital timer in the format hh:mm would show.

*)

let lateRide n =
    let addTwoDigits (n:int) =
        let nAsString = n.ToString()
        nAsString |> List.ofSeq |> List.map Char.GetNumericValue |> List.sum |> (int) 
    let timeH = (int)(n/60) 
    let timeM = n % 60
    (addTwoDigits timeH) + (addTwoDigits timeM)


(* phone call

Some phone usage rate may be described as follows:
first minute of a talk costs min1 cents,
each minute from the 2nd up to 10th (inclusive) costs min2_10 cents
each minute after 10th costs min11 cents.
You have S cents on your account before the call. What is the duration of the longest call (in minutes rounded down to the nearest integer) you can have?
Example
For min1 = 3, min2_10 = 1, min11 = 2 and S = 20, the output should be
phoneCall(min1, min2_10, min11, S) = 14.

*)

let phoneCall min1 min2_10 min11 S = 
    let rec phoneCall' S' minute =
        match minute with 
        | 0 ->  if S' < min1 then minute else phoneCall' (S' - min1) (minute + 1)
        | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9   -> 
            if S' < min2_10 then minute else phoneCall' (S' - min2_10) (minute + 1)
        | _ -> if S' < min11 then minute else phoneCall' (S' - min11) (minute + 1)
    phoneCall' S 0

