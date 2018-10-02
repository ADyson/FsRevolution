namespace Players

open System
open Types

    type PrinterBot() =               
         interface IRevolutionPlayer with
            member this.MakeBids tokens =
                let bids = 
                    tokens 
                    |> Seq.toList 
                    |> List.map (fun t -> Bid (Printer, [t]))
                bids
                |> List.toSeq

            member this.Spy() =
                failwith "PrinterBot only prints"