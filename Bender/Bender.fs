namespace Bender

type Direction =
    | South
    | East
    | North
    | West
type DirectionPriority =
    | Default
    | Inverted
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

type Position = { Row: int; Column: int }
type Map = Map of MapItem [,]

type BenderMode = Sober | Breaker
type Outcome = 
    | Directions of Direction list
    | Loop

module Priority =
    let getPossibleDirections currentDir = function
    | Default -> currentDir::[South;East;North;West]
    | Inverted -> currentDir::[West;North;East;South]

    let inverseDirectionPriorities = function
    | Default -> Inverted
    | Inverted -> Default

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
    | _ -> failwith "Invalid map character"

    let readLine (line:string) =
        line |> Seq.map toMapItem

    type InputReader = unit -> string seq

    let readMap (reader:InputReader) =
        reader() |> Seq.map readLine |> array2D |> Map
        
    let toRowColumnItem row column item = 
        ({Row=row;Column=column},item)
    let withItemPosition map =
        map |> Array2D.mapi toRowColumnItem
    let flatten (Map map) =
        let posMap = withItemPosition map
        let rowCount = posMap |> Array2D.length1
        [0..rowCount-1] |> List.collect (fun r -> posMap.[r,*] |> List.ofArray)
    let find map item = 
        map |> flatten |> List.filter (snd >> ((=) item))
    let findFirst map =
        find map >> List.head >> fst
    let getAt (Map map) {Row=r;Column=c} =
        map.[r,c]
    let setAt (Map map) {Row=r;Column=c} value =
        let newMap = Array2D.copy map
        newMap.[r,c] <- value
        newMap

module BenderSolver =
    open Priority
    open MapReader
    open System
    open System.Security.Cryptography
    open System.Security.Cryptography

    type OutputWriter = string list -> unit
        
    type BenderState = {
        CurrentPos: Position
        CurrentDirection: Direction
        DirPriority: DirectionPriority
        Mode: BenderMode
        RecordedMoves: (Direction*MapItem) list
    }

    let initState map = { 
        CurrentPos=findFirst map Start
        CurrentDirection=South
        DirPriority=Default
        Mode=Sober
        RecordedMoves=[]
    }

    let teleporters map = 
        find map Teleporter |> List.map fst

    let getTeleportPosition map pos =
        let allTeleporters = teleporters map
        if pos = allTeleporters.[0] then allTeleporters.[1] else allTeleporters.[0]

    let inverseMode = function
    | Sober -> Breaker
    | Breaker -> Sober

    let getSiblingPos ({Row=r;Column=c} as pos) = function
    | East -> {pos with Column=c+1}
    | North -> {pos with Row=r-1}
    | West -> {pos with Column=c-1}
    | South -> {pos with Row=r+1}

    let isPositionInRange (Map map) {Row=r; Column=c} =
        let (rowCount,colCount) = (map |> Array2D.length1, map |> Array2D.length2)
        r >= 0 && r < rowCount && c >= 0 && c < colCount      

    let isValidTargetPosition map mode pos =
        match getAt map pos with
        | Obstacle Unbreakable -> false
        | Obstacle Breakable when mode=Sober -> false
        | _ -> true

    let move map ({CurrentPos=pos; CurrentDirection=dir;DirPriority=priority;Mode=mode;RecordedMoves=moves} as state)  = 
        let getItem = getAt map
        let nextPositionInfo = 
            getPossibleDirections dir priority 
            |> List.map (fun d -> (getSiblingPos pos d),d)
            |> List.filter (fst>>(isPositionInRange map))
            |> List.filter (fst>>(isValidTargetPosition map mode))
            |> List.head
        let (nextPos,nextDirection) = nextPositionInfo
        {state with CurrentPos=nextPos; CurrentDirection=nextDirection; RecordedMoves=(nextDirection,getItem pos)::moves}

    let rec solve map ({CurrentPos=pos;DirPriority=priority;Mode=mode;RecordedMoves=moves} as state) =
        let setItem = setAt map
        let moveBender = move map
        let solveBender = solve map
        let moveAndSolve = moveBender >> solveBender
        let getTeleportPosition' = getTeleportPosition map
        
        let currentMapItem = getAt map pos
        match currentMapItem with
        | SuicideShack -> moves |> (List.rev >> List.map fst) |> Directions
        | Obstacle Breakable -> 
            let newMap = setItem pos Blank |> Map
            move newMap state |> solve newMap
        | PathModifier newDir ->
            moveAndSolve {state with CurrentDirection=newDir}
        | Beer ->
            moveAndSolve {state with Mode=inverseMode mode}
        | CircuitInverter ->
            moveAndSolve {state with DirPriority=inverseDirectionPriorities priority}
        | Teleporter -> 
            let newPosition = getTeleportPosition' pos
            {state with CurrentPos=newPosition} |> moveAndSolve
        | Obstacle Unbreakable -> failwith "Invalid state"
        | Start | Blank -> moveAndSolve state

    let getMoveList map = initState map |> solve map
    let getMoveListFromReader = readMap >> getMoveList
    let moveListToStringList = function
    | Directions dirs -> dirs |> List.map string
    | Loop -> ["LOOP"]
    let getMoveStringsFromReader = getMoveListFromReader >> moveListToStringList
    let inputReaderToOutputWriter (writer:OutputWriter) = getMoveStringsFromReader >> writer

    let readConsole _ = Console.In.ReadLine()
    let consoleReader() = 
        seq {
            let firstLine = Console.In.ReadLine()
            let lines = firstLine.Split ' ' |> Array.map Int32.Parse |> Array.head
            for i in [1..lines] do yield Console.In.ReadLine()
        }
    let writeToConsole (s:string) = Console.Out.WriteLine(s.ToUpperInvariant())
    let consoleWriter = List.iter writeToConsole

    //consoleReader |> inputReaderToOutputWriter consoleWriter