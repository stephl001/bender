namespace Bender

type Direction =
    | South
    | East
    | North
    | West
type Directions = Direction list
type DirectionPriority =
    | Normal of Directions
    | Inverted of Directions
type Obstacle = 
    | Breakable 
    | Unbreakable
type MapItem =
    | Start
    | SuicideShack
    | Obstacle of Obstacle
    | PathModifier of Direction
    | Beer
    | CircuitInverter
    | Teleporter
    | Blank
type Map = Map of MapItem [,]
type Outcome = 
    | Direction of Direction
    | Loop
type Position = { X: int; Y: int }

module Priority =
    let directionPriorities = [South;East;North;West]
    let directionPrioritiesInverted = directionPriorities |> List.rev

    let inverseDirectionPriorities = function
    | Normal _ -> Inverted directionPrioritiesInverted
    | _ -> Normal directionPriorities

module MapReader = 
    let toMapItem = function
    | ' ' -> Blank
    | 'T' -> Teleporter
    | 'I' -> CircuitInverter
    | 'B' -> Beer
    | 'E' -> PathModifier East
    | 'W' -> PathModifier West
    | 'N' -> PathModifier North
    | 'S' -> PathModifier South
    | '#' -> Obstacle Unbreakable
    | 'X' -> Obstacle Breakable
    | '@' -> Start
    | '$' -> SuicideShack

    let readLine (line:string) =
        line |> Seq.map toMapItem

    let readMap (lines:string seq) =
        lines |> Seq.map readLine |> array2D |> Map

module BenderSolver =
    ()
