namespace Revolution

open System
open Types
open Revolution.Player

    type RandomPlayer(i) =
        let r = Random(i);
        
        let spendForce tokens =
            let bidWithForce()  =
                let rand = r.Next(0,6);
                match rand with
                | 0 -> InnKeeper
                | 1 -> Magistrate
                | 2 -> Priest
                | 3 -> Aristocrat
                | 4 -> Merchant
                | _ -> Printer

            let bids = 
                tokens
                |> List.filter (fun t -> t = Force)
                |> List.map (fun t -> Bid (bidWithForce(), [t]))

            bids, tokens |> List.filter (fun t -> t <> Force)
        
        let spendBlackmail tokens =
            let bidWithBlackmail() =
                let rand = r.Next(0,6);
                match rand with
                | 0 -> General
                | 1 -> Captain
                | 2 -> Priest
                | 3 -> Aristocrat
                | 4 -> Merchant
                | _ -> Printer

            let bids = 
                tokens
                |> List.filter (fun t -> t = Blackmail)
                |> List.map (fun t -> Bid (bidWithBlackmail(), [t]))

            bids, tokens |> List.filter (fun t -> t <> Blackmail)

        let spendGold tokens =
            let bidWithGold() =
                let rand = r.Next(0,10);
                match rand with
                | 0 -> General
                | 1 -> Captain
                | 2 -> InnKeeper
                | 3 -> Magistrate
                | 4 -> Priest
                | 5 -> Aristocrat
                | 6 -> Merchant
                | 7 -> Printer
                | 8 -> Rogue
                | _ -> Mercenary

            let bids = 
                tokens
                |> List.filter (fun t -> t = Gold) // Should all be gold
                |> List.map (fun t -> Bid (bidWithGold(), [t]))

            bids

        interface IRevolutionPlayer with
            member __.MakeBids _ tokens =
                let tkns = tokens |> Seq.toList

                let (forceBids, remaining) = spendForce tkns
                let (blackmailBids, remaining2) = spendBlackmail remaining
                let (goldBids) = spendGold remaining2

                // Check for more than 6 distinct bids
                forceBids @ blackmailBids @ goldBids
                |> List.toSeq

            member __.Spy state =
                // Need to get the options or board state.
                (Blue, Garden)



            