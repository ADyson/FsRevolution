namespace RevolutionEngine

    module Helpers =
        open Types
        let logBids bids =
            ignore
            //bids
            //|> Seq.iter ( fun (c, b) ->
            //    printfn "Color %A Bids:" c
            //    printfn "%A" b)

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
            //match winner with 
            //| Some c -> printfn "%A won %A" winner choice
            //| None ->  ignore()
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

    module TurnResolvers =
        open Types
        open Helpers
        open System.Collections.Generic
        
        let doActions state reducer actions =
            match actions with 
            | Some (team, actions)->
                actions
                |> List.map (fun b -> (team, b)) 
                |> List.fold (fun init bid -> reducer init bid) state
            | _ -> state


        let resolve choice actions state reducer =
            let actions' state = 
                match getBids state.Bids choice with
                | Some bids -> 
                    match getBidWinner bids with
                    | Some winner ->
                        // logWinner choice <| Some winner
                        (winner, actions)
                        |> Some
                    | _ -> None 
                | _ -> None
            doActions state reducer <| actions' state

        let resolveGeneral =
            resolve General [
                Support 1; 
                Influence Fortress; 
                AddTokenToHand Force]

        let resolveCaptain =
            resolve Captain [
                Support 1; 
                Influence Harbor;
                AddTokenToHand Force]

        let resolveInnKeeper =
            resolve InnKeeper [
                Support 3;
                Influence Tavern; 
                AddTokenToHand Blackmail ]

        let resolveMagistrate =
            resolve Magistrate [
                Support 1; 
                Influence TownHall; 
                AddTokenToHand Blackmail ]

        let resolvePriest =
            resolve Priest [Support 6; Influence Cathedral]

        let resolveAristocrat =
            resolve Aristocrat [
                            Support 5; 
                            Influence Plantation; 
                            AddTokenToHand Gold; 
                            AddTokenToHand Gold; 
                            AddTokenToHand Gold ]

        let resolveMerchant =
            resolve Merchant [
                            Support 3; 
                            Influence Market; 
                            AddTokenToHand Gold; 
                            AddTokenToHand Gold; 
                            AddTokenToHand Gold; 
                            AddTokenToHand Gold; 
                            AddTokenToHand Gold ]

        let resolvePrinter =
            resolve Printer [Support 10]

        let resolveRogue =
            resolve Rogue [AddTokenToHand Blackmail; AddTokenToHand Blackmail]

        let resolveMercenary = 
            resolve Mercenary [Support 3; AddTokenToHand Force]

        let resolveSpy (players: IDictionary<Color,#IRevolutionPlayer>) state reducer =
            let actions' state = 
                match getBids state.Bids Spy with
                | Some bids -> 
                    match getBidWinner bids with
                    | Some winner ->
                        // logWinner Spy <| Some winner
                        
                        let p = players.[winner]
                        let c,l = p.Spy(state)
                        
                        (winner, [Spy' (l,c)])
                        |> Some
                    | _ -> None 
                | _ -> None
            doActions state reducer <| actions' state

    module GameHandler =
        open Types
        open Helpers
        open TurnResolvers

        let addSupport players color support =
            players
            |> Array.map (fun p ->
                match p.Team with
                | c when c = color -> { p with Support = p.Support + support}
                | _ -> p )

        let addToken players color token =
            players
            |> Array.map (fun p ->
                match p.Team with
                | c when c = color -> { p with Hand = p.Hand @ [token]}
                | _ -> p )

        let addInfluence board color location =
            if not <| isFull board location
            then
                match location with
                | Plantation -> { board with Plantation = board.Plantation |> Array.append [|color|] }  
                | Tavern -> { board with Tavern = board.Tavern |> Array.append [|color|] } 
                | Cathedral -> { board with Cathedral = board.Cathedral |> Array.append [|color|] } 
                | TownHall -> { board with TownHall = board.TownHall |> Array.append [|color|] } 
                | Asylum -> { board with Asylum = board.Asylum |> Array.append [|color|] } 
                | Jail -> { board with Jail = board.Jail |> Array.append [|color|] } 
                | Fortress -> { board with Fortress = board.Fortress |> Array.append [|color|] } 
                | Market -> { board with Market = board.Market |> Array.append [|color|] } 
                | Harbor -> { board with Harbor = board.Harbor |> Array.append [|color|] } 
                | Garden -> { board with Garden = board.Garden |> Array.append [|color|] } 
            else
                board    

        let addBid bidstate (bid : BidAction) color =
            
            let setBids bids choice newBids =
                match choice with
                | General -> { bids with General = newBids }  
                | Captain -> { bids with Captain = newBids }  
                | InnKeeper -> { bids with InnKeeper = newBids }
                | Magistrate -> { bids with Magistrate = newBids }
                | Priest -> { bids with Priest = newBids }
                | Aristocrat -> { bids with Aristocrat = newBids }
                | Merchant -> { bids with Merchant = newBids }
                | Printer -> { bids with Printer = newBids }
                | _ -> bids

            match bid with
            | Bid (choice, tokens) -> 
                let bids = getBids bidstate choice
                let newBids = 
                    match bids with
                    | Some b ->
                        let curTokens = 
                            b
                            |> Array.tryFind (fun p -> p.Player = color)
                    
                        match curTokens with
                        | Some bid -> 
                            b
                            |> Array.map (fun b -> if b.Player = color then { bid with Tokens = tokens @ bid.Tokens } else b)
                        | None -> b|> Array.append [|{ Player = color; Tokens = tokens }|]

                    | _ -> [|{ Player = color; Tokens = tokens }|]

                setBids bidstate choice <| Some newBids

        let spySwap board color location otherColor = 
            let influence = getLocation board location
            
            let removeIndex =
                influence 
                |> Array.findIndex (fun c -> c = otherColor)

            let swap i x =
                if i = removeIndex then color else x

            match location with
            | Plantation -> { board with Plantation = board.Plantation |> Array.mapi swap }  
            | Tavern -> { board with Tavern = board.Tavern |> Array.mapi swap } 
            | Cathedral -> { board with Cathedral = board.Cathedral |> Array.mapi swap } 
            | TownHall -> { board with TownHall = board.TownHall |> Array.mapi swap } 
            | Asylum -> { board with Asylum = board.Asylum |> Array.mapi swap } 
            | Jail -> { board with Jail = board.Jail |> Array.mapi swap } 
            | Fortress -> { board with Fortress = board.Fortress |> Array.mapi swap } 
            | Market -> { board with Market = board.Market |> Array.mapi swap } 
            | Harbor -> { board with Harbor = board.Harbor |> Array.mapi swap } 
            | Garden -> { board with Garden = board.Garden |> Array.mapi swap }    


        let gameReducer (state : GameState) (action : PlayerAction) =
            let (team, gameAction) = action
            
            match gameAction with
            | AddBid bid -> 
                { state with Bids = addBid state.Bids bid team}
            | Support amt -> 
                { state with Players = addSupport state.Players team amt }
            | AddTokenToHand tkn ->
                { state with Players = addToken state.Players team tkn }
            | Influence loc -> 
                { state with Board = addInfluence state.Board team loc }
            | Spy' (loc, other) -> 
                { state with Board = spySwap state.Board team loc other }
            | _ -> state

        let gameFinished (state : GameState) =
            let getResults state =
                let getLocationWinner location = 
                    let list = 
                        getLocation state.Board location
                    
                    let sorted =
                        list
                        |> Array.groupBy id
                        |> Array.map (fun c -> fst c, snd c |> Array.length)
                        |> Array.sortByDescending snd

                    if sorted |> Array.length = 1 then sorted.[0] |> fst |> Some                 
                    elif (sorted.[0] |> snd) <> (sorted.[1] |> snd) then sorted.[0] |> fst |> Some
                    else None
                
                let results = 
                    state.Players
                    |> Array.map (fun p -> p.Team, p.Support)
                    |> dict

                //printfn "Support: %A" results

                let locations = 
                    [Plantation; Tavern; Cathedral; TownHall; Fortress; Market; Harbor]

                let locationResults =
                    locations
                    |> List.map (fun l -> 
                        let winner = getLocationWinner l
                        //printfn "%A won %A : (%A) " winner l <| locationScore l
                        l, winner)

                let handResults =
                    state.Players
                    |> Array.map (fun p -> p.Team, p.Hand |> List.map tokenValue |> List.fold (fun t s -> t + s) 0)
                    |> dict

                //printfn "Hand: %A" handResults
                
                state.Players
                |> Array.map (fun p -> p.Team)
                |> Array.map (fun c -> c, results.[c] + handResults.[c] + (locationResults |> List.map (fun (l, col) -> match col with Some (team) when c = team -> locationScore l |> snd | _ -> 0) |> List.sum))
            
            // Do these also have to be full
            let asylumFull = 
                state.Board.Asylum.Length = locationSize Asylum
            let jailFull = 
                state.Board.Jail.Length = locationSize Jail
            
            let plantationFull = 
                state.Board.Plantation.Length = locationSize Plantation
            let tavernFull = 
                state.Board.Tavern.Length = locationSize Tavern
            let cathedralFull = 
                state.Board.Cathedral.Length = locationSize Cathedral
            let townHallFull = 
                state.Board.TownHall.Length = locationSize TownHall
            let fortressFull = 
                state.Board.Fortress.Length = locationSize Fortress
            let marketFull = 
                state.Board.Market.Length = locationSize Market
            let harborFull = 
                state.Board.Harbor.Length = locationSize Harbor
            let gardenFull = 
                state.Board.Garden.Length = locationSize Garden
            
            //asylumFull && jailFull && gardenFull 
            if plantationFull && tavernFull && cathedralFull && townHallFull && fortressFull && marketFull && harborFull 
            then Some <| getResults state
            else None

        let initState players = { 
            Board = inititialBoard; 
            Players = players
            Bids = initialBidState 
        }

        let runGame (players : #IRevolutionPlayer []) =
           
            let playerState = 
                players
                |> Array.mapi (fun i p -> { Team = getTeam i; Hand = initialHand; Support = 0 })

            let playerMap =
                players
                |> Array.mapi (fun i p -> (getTeam i, p))
                |> dict

            let getHand state color =
                let player = 
                    state.Players
                    |> Array.find (fun p -> p.Team = color)
                player.Hand

            let rec fillHand tokens = 
                match tokens |> List.length with
                | x when x >= 5 -> tokens
                | _ -> fillHand <| tokens @ [Gold]
            
            /// Resolve the bid winners in order and get player choices
            let resolveTurn state playerMap =
                let resolvers =
                    [   resolveGeneral; 
                        resolveCaptain; 
                        resolveInnKeeper; 
                        resolveMagistrate; 
                        resolvePriest; 
                        resolveAristocrat; 
                        resolveMerchant; 
                        resolvePrinter;
                        // warden
                        resolveSpy playerMap;
                        // apothecary
                        // heretic
                        // governor
                        resolveRogue;
                        resolveMercenary
                        // anarchist 
                        ]
               
                resolvers
                |> List.fold (fun s r -> r s gameReducer) state

            /// Main Game Loop
            let rec loop state counter =
                let playerBids = 
                    playerMap
                    |> Seq.map (fun p -> p.Key, p.Value.MakeBids state (getHand state p.Key) )

                logBids playerBids

                let bidActions =
                    playerBids
                    |> Seq.map (fun (team, bidActions) -> 
                        bidActions 
                        |> Seq.map (fun bidAction -> team, AddBid bidAction))
                    |> Seq.concat
                
                let emptyHandsState =
                    { state with Players = state.Players |> Array.map (fun p -> { p with Hand = [] } )}

                let postBidState = 
                    bidActions
                    |> Seq.fold (fun init bid -> gameReducer init bid) emptyHandsState

                let resolveTurnState =
                    resolveTurn postBidState playerMap

                let fillHandsState =
                    { resolveTurnState 
                        with Players = resolveTurnState.Players 
                            |> Array.map (fun p -> { p with Hand = fillHand p.Hand } ); Bids = initialBidState }

                let endOfRoundState = fillHandsState

                //printfn "Turn %A Completed" counter

                // Check game finished before filling hand with gold.
                match gameFinished resolveTurnState with
                | Some x -> x 
                | _ -> loop endOfRoundState <| counter + 1

            loop <| initState playerState <| 0


