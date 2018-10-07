namespace Revolution
open Types

module Helpers =
    let logBids bids =
        bids
        |> Seq.iter ( fun (c, b) ->
           printfn "Color %A Bids:" c
           printfn "%A" b)

    let getBidWinner (bids : Bid []) =
        let bidScore = function
        | Force -> 100
        | Blackmail -> 10
        | Gold -> 1
        
        let scoredBids =
            bids
            |> Array.map (fun b -> b.Player, b.Tokens |> List.fold (fun score t -> score + bidScore t) 0)
            |> Array.sortByDescending snd
        
        if scoredBids.Length > 1 && (scoredBids.[1] |> snd) = (scoredBids.[0] |> snd)
        then None 
        else scoredBids.[0] |> fst |> Some

    let logWinner choice winner =
        match winner with 
        | Some c -> printfn "%A won %A" winner choice
        | None ->  ignore()
        ignore

    let getTeam = function 
        | 1 -> Red
        | 2 -> Blue
        | 3 -> Yellow
        | 4 -> Green
        | 5 -> Grey
        | _ -> Black
        
    let locationSize = function
        | Plantation -> 5  
        | Tavern -> 4
        | Cathedral -> 7
        | TownHall -> 6
        | Asylum -> 5
        | Jail -> 5
        | Fortress -> 8
        | Market -> 5
        | Harbor -> 6
        | Garden -> 6

    let getLocation board location =
        match location with
        | Plantation -> board.Plantation  
        | Tavern -> board.Tavern
        | Cathedral -> board.Cathedral
        | TownHall -> board.TownHall
        | Asylum -> board.Asylum
        | Jail -> board.Jail
        | Fortress -> board.Fortress
        | Market -> board.Market
        | Harbor -> board.Harbor
        | Garden -> board.Garden

    let getBids bids choice =
        match choice with
        | General -> bids.General
        | Captain -> bids.Captain
        | InnKeeper -> bids.InnKeeper
        | Magistrate -> bids.Magistrate
        | Priest -> bids.Priest
        | Aristocrat -> bids.Aristocrat
        | Merchant -> bids.Merchant
        | Printer -> bids.Printer
        | Warden -> bids.Warden
        | Spy -> bids.Spy
        | Apothecary -> bids.Apothecary
        | Heretic -> bids.Heretic
        | Governor -> bids.Governor
        | Rogue -> bids.Rogue
        | Mercenary -> bids.Mercenary
        | Anarchist -> bids.Anarchist
    
    let isFull board location =
        getLocation board location
        |> Array.length = locationSize location

    let locationScore = function
        | Plantation -> Majority, 35  
        | Tavern -> Majority, 20
        | Cathedral -> Majority, 35
        | TownHall -> Majority, 45
        | Asylum -> Majority, -30
        | Jail -> Majority, -30
        | Fortress -> Majority, 50
        | Market -> Majority, 25
        | Harbor -> Majority, 40
        | Garden -> Each, 10

    let tokenValue = function
        | Force -> 5
        | Blackmail -> 3
        | Gold -> 1


    type GameStatus (gameState : GameState) =
        member __.RawState =  gameState

        member __.Bids loc =  match getBids gameState.Bids loc with | Some x -> x | None ->  [||]

        member __.Support col =  
            let player = gameState.Players |> Array.find (fun p -> p.Team = col)
            player.Support

        // member __.CurrentScores =
        //     ignore

        member __.Hands col = 
            let player = gameState.Players |> Array.find (fun p -> p.Team = col)
            player.Hand |> Array.ofList

module Player =
    open Helpers

    type IRevolutionPlayer =
        // Todo group onto same type and then only perform first 6.
       abstract member MakeBids: GameStatus -> seq<Token> -> seq<BidAction> 

       abstract member Spy: GameStatus -> Color * BoardLocation
        


