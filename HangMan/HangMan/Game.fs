module HangMan.Game
open System

type GuessingState = {
                      AllowedNumberOfGuesses: int
                      WordBeingGuessed: string
                      LettersGuessed: char Set
                      WordsGuessed: string Set
                      }

type GameState = | Guessing of GuessingState
                 | Lost
                 | Won
                 

type Guess = | Letter of char
             | Word of string
           
let countRemainingGuesses state =
    let numberOflettersGuessed = state.LettersGuessed |> Set.count
    let numberOfWordsGuessed = state.WordsGuessed |> Set.count
    
    state.AllowedNumberOfGuesses - (numberOflettersGuessed + numberOfWordsGuessed)  

let getWordProgress gameState =
    
    let assembleWordInProgress word letters =
        word
        |> Seq.map (fun l -> match Set.contains l letters with
                             | true -> l 
                             | false -> '_')
        |> String.Concat
        

    assembleWordInProgress gameState.WordBeingGuessed gameState.LettersGuessed


let guessALetter state guess =
    let lowerGuess = guess |> Char.ToLower
    let hasLetterBeenGuessedYet = state.LettersGuessed |> Set.contains lowerGuess
    
    match hasLetterBeenGuessedYet with
    | false -> {state with LettersGuessed = ( state.LettersGuessed |> Set.add lowerGuess)}
    | true -> state
    
let guessAWord state (guess:string) =
    let lowerGuess = guess.ToLower ()
    let hasWordBeenGuessedYet = state.WordsGuessed |> Set.contains lowerGuess
    
    match hasWordBeenGuessedYet with
    | true -> state
    | false -> {state with WordsGuessed = (state.WordsGuessed |> Set.add lowerGuess)}
    

let evaluateGuessingState state =
    match state with
    | g when (g |> getWordProgress) = g.WordBeingGuessed -> Won
    | g when (g.WordsGuessed |> Set.contains g.WordBeingGuessed) -> Won
    | g when countRemainingGuesses g = 0 -> Lost
    | g -> Guessing g
    
let makeAGuess state guess =
    match state with
    | Won | Lost -> state
    | Guessing g -> match guess with
                    | Letter l -> guessALetter g l |> evaluateGuessingState
                    | Word w -> guessAWord g w |> evaluateGuessingState                                    