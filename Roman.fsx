(*
Author: Daniel Shaw
Date: 10/22/2023
*)

type Numeral = M | D | C | L | X | V | I

// Map a numeral to its equivalent decimal value
let valueOf = function
    | M -> 1000
    | D -> 500
    | C -> 100
    | L -> 50
    | X -> 10
    | V -> 5
    | I -> 1

// Generate a mapping from decimal value -> numeral
let numerals =
    [ M; D; C; L; X; V; I ]
    |> List.map (fun numeral -> valueOf numeral, numeral)

// Given a number `x`, find the first numeral where `x` is >= the decimal value
// This depends on the ordering of the numerals above, since it'll always return I
// if the list is reversed (x >= 1 when the inputs are constrained to always >= 1)
let largestNumeralFor x = numerals |> List.find (fun (value, _) -> x >= value)

// Rather than figure out a complex solution to handling this with math,
// just hardcode the 6 edge cases. These can be considered "pseudo-numerals"
// in the sense that the code below handles them identically, just pushing
// the two numerals as opposed to only one.
let exceptions = function
    | 4 -> Some (4, [I; V])
    | 9 -> Some (9, [I; X])
    // 4 and 9 don't require this because there aren't values between
    // them and the next numeral step up (5, 10 respectively).
    | x when x >= 40 && x < 50 -> Some (40, [X; L])
    | x when x >= 90 && x < 100 -> Some (90, [X; C])
    | x when x >= 400 && x < 500 -> Some (400, [C; D])
    | x when x >= 900 && x < 1000 -> Some (900, [C; M])
    | _ -> None

let rec toRoman = function
    | x when x <= 0 -> []
    | x ->
        // First, check the exceptions table to see if any are applicable.
        // If Some is returned, x is an edge case that requires the exceptions
        // as opposed to normal formatting. This fixes things like [V; I; I; I; I].
        match exceptions x with
        | Some (value, [first; second]) -> first :: second :: toRoman (x - value)
        | _ ->
            // Otherwise, find the largest numeral that x is bigger than,
            // push it onto the list, and continue to the next reduction.
            let value, numeral = largestNumeralFor x
            numeral :: toRoman (x - value)

let toInt =
    let aux (acc, previous) next =
        let next = valueOf next
        match previous with
        // empty stack -> acc, [V]
        | -1 -> acc, next
        // IV -> acc + IV, []
        | previous when previous < next -> acc + next - previous, -1
        // VI -> acc + V, [I]
        | previous -> acc + previous, next
            
    let sumState = function
        | a, -1 -> a
        | a, b -> a + b
    List.fold aux (0, -1) >> sumState

[
    toRoman 5 // [V]
    toRoman 10 // [X]
    toRoman 9 // [I; X]
    toRoman 8 // [V; I; I; I]
] |> List.map toInt