// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

#load "Bender.fs"
open Bender
open MapReader
open BenderSolver

// Define your library scripting code here
let constReader () =
    [
        "###############"
        "#      IXXXXX #"
        "#  @          #"
        "#E S          #"
        "#             #"
        "#  I          #"
        "#  B          #"
        "#  B   S     W#"
        "#  B   T      #"
        "#             #"
        "#         T   #"
        "#         B   #"
        "#N          W$#"
        "#        XXXX #"
        "###############"
    ] |> Seq.ofList

//let moves = getMoveListFromReader constReader
