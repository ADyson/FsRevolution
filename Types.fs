module Types

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
        General: Bid [] option;
        Captain: Bid [] option;
        InnKeeper: Bid [] option;
        Magistrate: Bid [] option;
        Priest: Bid [] option;
        Aristocrat: Bid [] option;
        Merchant: Bid [] option;
        Printer: Bid [] option;
        Warden: Bid [] option;
        Spy: Bid [] option;
        Apothecary: Bid [] option;
        Heretic: Bid [] option;
        Governor: Bid [] option;
        Rogue: Bid [] option;
        Mercenary: Bid [] option;
        Anarchist: Bid [] option;
    }

    type BoardState = {
        Plantation: Color []
        Tavern: Color []
        Cathedral: Color [] 
        TownHall: Color [] 
        Garden: Color []
        Asylum: Color []
        Jail: Color []
        Fortress: Color [] 
        Market: Color []
        Harbor: Color [] 
    }

    type GameState = {
        Board: BoardState;
        Players: Player[];
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
        Plantation = [||]
        Tavern = [||]
        Cathedral = [||]
        TownHall = [||]
        Garden = [||]
        Asylum = [||]
        Jail = [||]
        Fortress = [||]
        Market = [||]
        Harbor = [||]
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

