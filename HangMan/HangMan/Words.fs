module HangMan.Words

open System
open System.IO

let allWords = File.ReadAllLines "words.txt" |> Array.map (fun s -> s.ToLower ())

let pseudoRandomWord words seed =
    let rng = Random seed
    let wordCount = Array.length words
    rng.Next(0, wordCount) |> Array.get words


//Impure
let getRandomWordUsingTimeSeed words =
    let seed = DateTime.Now.Millisecond
    pseudoRandomWord words seed