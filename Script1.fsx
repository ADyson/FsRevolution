#load "Types.fs"
#load "RandomPlayer.fs"
#load "PrinterBot.fs"
#load "Library.fs"

open Players
open RevolutionEngine
open Types
open System.Collections.Generic

let scores = new Dictionary<Color, int>()

scores.Add(Black, 0)
scores.Add(Red, 0)
scores.Add(Blue, 0)
scores.Add(Yellow, 0)
scores.Add(Green, 0)
scores.Add(Grey, 0)

[1..200]
|> List.iter (fun i ->

    //let p1 = new RandomPlayer(13) :> IRevolutionPlayer;
    let p2 = new RandomPlayer(10 * i + 1):> IRevolutionPlayer;
    let p3 = new RandomPlayer(10 * i + 2):> IRevolutionPlayer;
    let p4 = new RandomPlayer(10 * i + 3):> IRevolutionPlayer;
    let p5 = new RandomPlayer(10 * i + 4):> IRevolutionPlayer;
    let p6 = new RandomPlayer(10 * i + 5):> IRevolutionPlayer;
    let p7 = new PrinterBot():> IRevolutionPlayer;

    let results =
        GameHandler.runGame([p2; p3; p4; p5; p6; p7]);

    let winner =
        results
        |> Array.sortByDescending (fun (_ , s) -> s)
        |> Array.head
        |> fst

    //printfn "%A" i

    scores.[winner] <- scores.[winner] + 1)

scores