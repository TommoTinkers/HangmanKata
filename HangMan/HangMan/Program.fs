open System
open HangMan.Game
open HangMan.Words

let lowerCaseLetters = ['a'..'z'] |> Set 

let rec play currentState =
    
    let unguessedLetters = currentState.LettersGuessed
                           |> Set.difference lowerCaseLetters
                           |> Set.map string
                           |> Set.fold (fun a b -> a + " " + b) String.Empty
                           
    printfn $"Guesses left: {countRemainingGuesses currentState}"
    printfn $"Progress: {currentState |> getWordProgress}"
    printfn $"Unguessed Letters: {unguessedLetters}"
    let input = Console.ReadLine ()
    
    let guess = match input.Length with
                | l when l = 1 -> Letter input.[0]
                | _ -> Word input
                
    let newState = guess |> makeAGuess (currentState |> Guessing) 
    
    match newState with
    | Guessing g -> play g
    | Won -> $"Congratulations - The word was {currentState.WordBeingGuessed}"
    | Lost -> $"Unlucky, you ran out of guesses - The word was {currentState.WordBeingGuessed}"
    
[<EntryPoint>]
let main argv =
    let words = allWords |> Array.filter (fun w -> w.Length > 3)
    let word = getRandomWordUsingTimeSeed words
    let state = { AllowedNumberOfGuesses = 10; WordBeingGuessed = "biscuit"; LettersGuessed = Set.empty; WordsGuessed = Set.empty }
    printfn $"{play state}"
    0