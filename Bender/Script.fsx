// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

#load "Bender.fs"
open Bender
open MapReader
open BenderSolver

// Define your library scripting code here
let constReader () =
    [
        "##########"
        "#        #"
        "#  S   W #"
        "#        #"
        "#  $     #"
        "#        #"
        "#@       #"
        "#        #"
        "#E     N #"
        "##########"
    ] |> Seq.ofList

//let moves = getMoveListFromReader constReader
