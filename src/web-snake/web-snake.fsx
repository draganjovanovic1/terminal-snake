#load "../snake-game/game.fsx"
#r "../../packages/Fable.Core/lib/netstandard1.6/Fable.Core.dll"

//open System
open Fable.Core
open Fable.Import

open SnakeGame.Core
open SnakeGame.Game

let maxX, maxY = 300, 200

let gameConfig = 
    let min = { x = 0; y = 2 }
    let max = { x = maxX; y = maxY }
    let gameBounds = min, max
    { bounds = gameBounds
      startPosition = getBoundsCenter (gameBounds)
      startDirection = Right }

let fireAction, actionsStream = 
    let e = Event<_> ()
    e.Trigger, e.Publish

let canvas =  Browser.document.getElementsByTagName_canvas().[0]
canvas.width <- maxX |> float
canvas.height <- maxY |> float
let ctx = canvas.getContext_2d ()
ctx.font = "10px Arial"

let renderer = 
    { redraw = fun s f m l ->
        let text = sprintf "Moving; snake=%A; food=%A; mines=%A; level=%i" s f m l
        ctx.clearRect (0., 0., ctx.canvas.width, ctx.canvas.height)
        ctx.strokeText (text, 5., 30.)
      ended = fun s f m l ->
        let text = sprintf "Game over; snake=%A; food=%A; mines=%A; level=%i" s f m l
        ctx.strokeText (text, 5., 5.) }

let getGameStatus = startGame gameConfig renderer actionsStream