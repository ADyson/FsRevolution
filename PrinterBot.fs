namespace Revolution

open System
open Types
open Player

    type PrinterBot() =               
         interface IRevolutionPlayer with
            member __.MakeBids _ tokens =
                let bids = 
                    tokens 
                    |> Seq.toList 
                    |> List.map (fun t -> Bid (Printer, [t]))
                bids
                |> List.toSeq

            member __.Spy _ =
                failwith "PrinterBot only prints"