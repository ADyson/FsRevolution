module Types

open System

    type Color = Blue | Green | Red | Grey | Yellow | Black

    type Token = Force | Blackmail | Gold
    
    type BidChoice = 
        | General
        | Captain
        | InnKeeper
        | Magistrate
        | Priest
        | Aristocrat
        | Merchant
        | Printer
        | Warden
        | Spy
        | Apothecary
        | Heretic
        | Governor
        | Rogue
        | Mercenary
        | Anarchist

    type BoardLocation =
        | Plantation
        | Tavern
        | Cathedral
        | TownHall
        | Garden
        | Asylum
        | Jail
        | Fortress
        | Market
        | Harbor

    type ScoreType = Each | Majority

    type Player = { 
        Team: Color; 
        Support: int; 
        Hand: Token list 
    }

    type Bid = {
        Player: Color;
        Tokens: Token list;
    }

    type BidState = {
        General: Bid list option;
        Captain: Bid list option;
        InnKeeper: Bid list option;
        Magistrate: Bid list option;
        Priest: Bid list option;
        Aristocrat: Bid list option;
        Merchant: Bid list option;
        Printer: Bid list option;
        Warden: Bid list option;
        Spy: Bid list option;
        Apothecary: Bid list option;
        Heretic: Bid list option;
        Governor: Bid list option;
        Rogue: Bid list option;
        Mercenary: Bid list option;
        Anarchist: Bid list option;
    }

    type BoardState = {
        Plantation: Color list;
        Tavern: Color list; 
        Cathedral: Color list; 
        TownHall: Color list; 
        Garden: Color list; 
        Asylum: Color list;
        Jail: Color list; 
        Fortress: Color list; 
        Market: Color list; 
        Harbor: Color list; 
    }

    type GameState = {
        Board: BoardState;
        Players: Player list;
        Bids : BidState;
    }

    type BidAction = 
        | Bid of BidChoice * Token list

    let initialBidState = {
        General = None
        Captain = None
        InnKeeper= None
        Magistrate= None
        Priest= None
        Aristocrat = None
        Merchant = None
        Printer = None
        Warden = None
        Spy = None
        Apothecary = None
        Heretic = None
        Governor = None
        Rogue = None
        Mercenary = None
        Anarchist = None
    }

    let inititialBoard = {
        Plantation = []
        Tavern = []
        Cathedral = []
        TownHall = []
        Garden = []
        Asylum = []
        Jail = []
        Fortress = []
        Market = []
        Harbor = []
    }

    let initialHand = [ Force; Blackmail; Gold; Gold; Gold; ]

    type GameAction =
        | AddBid of BidAction
        | Influence of BoardLocation
        | AddTokenToHand of Token
        | Support of int
        | Spy' of BoardLocation * Color
        | Warden' of Color * Color option * Token
        | Governor' of BoardLocation option * (Color * BoardLocation) option
        | Heretic' of Color option
        | Apothecary' of Color * BoardLocation * Color * BoardLocation


    type FinalScores = (Color * int) list

    type PlayerAction = Color * GameAction

    type IRevolutionPlayer =
       // abstract method
       abstract member MakeBids: seq<Token> -> seq<BidAction> 

       abstract member Spy: unit -> Color * BoardLocation
        
