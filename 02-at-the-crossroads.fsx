(* reach next level

You are playing an RPG. Currently your points total is equal to experience. To reach the next level your XP should be at least at threshold. If you kill the monster in front of you, you will gain more experience points in the amount of the reward.
Given values experience, threshold and reward, check if you reach the next level after killing the monster.

Example
For experience = 10, threshold = 15 and reward = 5, the output should be
reachNextLevel(experience, threshold, reward) = true;
For experience = 10, threshold = 15 and reward = 4, the output should be
reachNextLevel(experience, threshold, reward) = false.

*)

let reachNextLevel experience threshold reward =
    experience + reward >= threshold

(* knapsack light 

You found two items in a treasure chest! The first item weights weight1 and is worth value1, and the second item weights weight2 and is worth value2. What is the total maximum value of the items you can take with you, assuming that your max weight capacity is maxW and you can't come back for the items later?

Example
For value1 = 10, weight1 = 5, value2 = 6, weight2 = 4 and maxW = 8, the output should be
knapsackLight(value1, weight1, value2, weight2, maxW) = 10.
You can only carry the first item.
For value1 = 10, weight1 = 5, value2 = 6, weight2 = 4 and maxW = 9, the output should be
knapsackLight(value1, weight1, value2, weight2, maxW) = 16.
You're strong enough to take both of the items with you.

*)

let knapsackLight (value1:int) (weight1:int) (value2:int) (weight2:int) (maxW:int) =
    let canCarry w = w <= maxW
    if canCarry (weight1 + weight2) then (value1 + value2)
    elif canCarry weight1 && canCarry weight2 then
        if value1 > value2 then value1 else value2
    elif canCarry weight1 then value1
    elif canCarry weight2 then value2
    else 0

(* extra number 

You're given three integers, a, b and c. It is guaranteed that two of these integers are equal to each other. What is the value of the third integer?

Example
For a = 2, b = 7 and c = 2, the output should be
extraNumber(a, b, c) = 7.
The two equal numbers are a and c. The third number (b) equals 7, which is the answer.

*)

let extraNumber a b c =
    if a = b then c
    elif a = c then b
    else a

(* is infinite process 

Given integers a and b, determine whether the following pseudocode results in an infinite loop
while a is not equal to b do
  increase a by 1
  decrease b by 1
Assume that the program is executed on a virtual machine which can store arbitrary long numbers and execute forever.

Example
For a = 2 and b = 6, the output should be
isInfiniteProcess(a, b) = false;
For a = 2 and b = 3, the output should be
isInfiniteProcess(a, b) = true.

*)

let isInfiniteProcess a b =
    if a = b then false
    elif a > b then true
    elif (a - b) % 2 = 0 then false
    else true

(* arithmetic expression 

Consider an arithmetic expression of the form A#B=C. Check whether it is possible to replace # with one of the four signs: +, -, * or / to obtain a correct expression.

Example
For A = 2, B = 3 and C = 5, the output should be
arithmeticExpression(A, B, C) = true.
We can replace # with a + to obtain 2 + 3 = 5, so the answer is true.
For A = 8, B = 2 and C = 4, the output should be
arithmeticExpression(A, B, C) = true.
We can replace # with a / to obtain 8 / 2 = 4, so the answer is true.
For A = 8, B = 3 and C = 2, the output should be
arithmeticExpression(A, B, C) = false.
8 + 3 = 11 ≠ 2;
8 - 3 = 5 ≠ 2;
8 * 3 = 24 ≠ 2;
8 / 3 = 2.(6) ≠ 2.
So the answer is false.

*)

let arithmeticExpression A B C =
    if A + B = C then true
    elif A - B = C then true
    elif A * B = C then true
    elif A % B = 0 then A / B = C 
    else false

(* tennis set 

In tennis, a set is finished when one of the players wins 6 games and the other one wins less than 5, or, if both players win at least 5 games, until one of the players win 7 games.
Determine if it is possible for a tennis set to be finished with the score score1 : score2.

Example
For score1 = 3 and score2 = 6, the output should be
tennisSet(score1, score2) = true;
For score1 = 8 and score2 = 5, the output should be
tennisSet(score1, score2) = false;
For score1 = 6 and score2 = 5, the output should be
tennisSet(score1, score2) = false.

*)

let tennisSet score1 score2 =
    let bothOver n = (score1 >= n) && (score2 >= n)
    let eitherUnder n = (score1 < n) || (score2 < n)
    let oneIs n = (score1 = n) <> (score2 = n)
    let eitherIs n = (score1 = n) || (score2 = n)
    if eitherIs 6 && eitherUnder 5 then true
    elif bothOver 5 then oneIs 7
    else false

(* will you?

Once Mary heard a famous song, and a line from it stuck in her head. That line was "Will you still love me when I'm no longer young and beautiful?". Mary believes that a person is loved if and only if he/she is both young and beautiful, but this is quite a depressing thought, so she wants to put her belief to the test.
Knowing whether a person is young, beautiful and loved, find out if they contradict Mary's belief.

Example
For young = true, beautiful = true and loved = true, the output should be
willYou(young, beautiful, loved) = false.
Young and beautiful people are loved according to Mary's belief.
For young = true, beautiful = false and loved = true, the output should be
willYou(young, beautiful, loved) = true.
Mary doesn't believe that not beautiful people can be loved.

 *)

let willYou young beautiful loved =
   let yab = (young && beautiful)
   let b1 = yab = (not loved)
   let b2 = (not beautiful) && loved
   (b1 || b2)

(* metro card

You just bought a public transit card that allows you to ride the Metro for a certain number of days.
Here is how it works: upon first receiving the card, the system allocates you a 31-day pass, which equals the number of days in January. The second time you pay for the card, your pass is extended by 28 days, i.e. the number of days in February (note that leap years are not considered), and so on. The 13th time you extend the pass, you get 31 days again.
You just ran out of days on the card, and unfortunately you've forgotten how many times your pass has been extended so far. However, you do remember the number of days you were able to ride the Metro during this most recent month. Figure out the number of days by which your pass will now be extended, and return all the options as an array sorted in increasing order.

Example
For lastNumberOfDays = 30, the output should be
metroCard(lastNumberOfDays) = [31].
There are 30 days in April, June, September and November, so the next months to consider are May, July, October or December. All of them have exactly 31 days, which means that you will definitely get a 31-days pass the next time you extend your card.

 *)

let metroCard lastNumberOfDays =
    [|31;28;31;30;31;30;31;31;30;31;30;31|]
    |> Array.toSeq
    |> Seq.pairwise
    |> Seq.filter (fun x -> (fst x) = lastNumberOfDays )
    |> Seq.map snd
    |> Seq.distinct
    |> Seq.toArray
    |> Array.sort 