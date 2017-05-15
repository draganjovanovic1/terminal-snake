#load "../snake-game/game.fs"
#r "../../node_modules/fable-core/Fable.Core.dll"

open System
open Fable.Core
open Fable.Import

open SnakeGame.Core
open SnakeGame.Game

[<AutoOpen>]
module Graphics =

    type Color = 
        | DarkGreen
        | DarkRed
        | Magenta
        | DarkMagenta
        | Red
        | White
    with member x.ToRgb () = 
            let toString = sprintf "rgb(%i,%i,%i)"
            match x with 
            | DarkGreen -> toString 0 100 0
            | DarkRed -> toString 139 0 0
            | Magenta -> toString 255 0 255
            | DarkMagenta -> toString 102 0 102
            | Red -> toString 255 0 0 
            | White -> toString 255 255 255 

    let private clearCanvas (ctx: Browser.CanvasRenderingContext2D) = 
        ctx.clearRect (0., 0., ctx.canvas.width, ctx.canvas.height)

    let private getCoordsFromSquare square =
        let { x = xx; y = yy } = square
        xx |> float, yy |> float

    let private drawSquare (ctx: Browser.CanvasRenderingContext2D) squareSize (color: Color) square =
        ctx.fillStyle <- U3.Case1 <| color.ToRgb ()
        let x, y = square |> getCoordsFromSquare
        ctx.fillRect (x * squareSize, y * squareSize, squareSize, squareSize)

    let private drawCircle (ctx: Browser.CanvasRenderingContext2D) squareSize (color: Color) square =
        ctx.fillStyle <- U3.Case1 <| color.ToRgb ()
        ctx.strokeStyle <- U3.Case1 <| White.ToRgb ()
        let x, y = square |> getCoordsFromSquare
        let halfSquareSize = (squareSize / 2.) |> Math.Floor
        ctx.beginPath ()
        ctx.arc (x * squareSize + halfSquareSize, y * squareSize + halfSquareSize, halfSquareSize, 0., 2. * Math.PI)
        ctx.fill ()
        ctx.lineWidth <- 0.
        ctx.stroke ()

    let private drawMany drawFn squareSize color (ctx: Browser.CanvasRenderingContext2D) squares = squares |> List.iter (drawFn ctx squareSize color)

    let makeRenderer maxX maxY squareSize (canvas: Browser.HTMLCanvasElement) =
        canvas.width <- (maxX |> float) * squareSize
        canvas.height <- (maxY |> float) * squareSize
        let ctx = canvas.getContext_2d ()

        let drawScore score level s = 
            let div = Browser.document.getElementById ("game-status")
            div.innerHTML <- sprintf "Score: %05i     Level: %05i" score level
        let drawSnake = drawMany drawSquare squareSize DarkGreen
        let drawDeadSnake = drawMany drawSquare squareSize DarkRed
        let drawFood ctx food = 
            let good, expiring =
                food |> List.partition (fun f -> f.BestBefore > DateTime.Now.AddSeconds (5.))
            drawMany drawCircle squareSize Magenta ctx (good |> List.map (fun f -> f.Position)) 
            drawMany drawCircle squareSize DarkMagenta ctx (expiring |> List.map (fun f -> f.Position))
        let drawMines = drawMany drawSquare squareSize Red

        { redraw = fun s f m l sc ->
            drawScore sc l s
            clearCanvas ctx
            drawSnake ctx s
            drawFood ctx f
            drawMines ctx m
          ended = fun s f m l sc ->
            drawScore sc l s
            clearCanvas ctx
            drawDeadSnake ctx s
            drawFood ctx f
            drawMines ctx m }

let maxX, maxY = 50, 50
let squareSize = 10.
let canvas = Browser.document.getElementsByTagName_canvas().[0]
let renderer = makeRenderer maxX maxY squareSize canvas

let gameConfig = 
    let min = { x = 0; y = 0 }
    let max = { x = maxX; y = maxY }
    let gameBounds = min, max
    { bounds = gameBounds
      startPosition = getBoundsCenter gameBounds
      startLength = 4
      startDirection = Right }

let fireAction, actionsStream = 
    let e = Event<_> ()
    e.Trigger, e.Publish

let checkGameStatus = startGame gameConfig renderer actionsStream

Browser.window.addEventListener_keydown (fun e ->
    match e.keyCode |> int with
    | 38 -> ChangeDirection Up |> fireAction // up arrow
    | 39 -> ChangeDirection Right |> fireAction // right arrow
    | 40 -> ChangeDirection Down |> fireAction // down arrow
    | 37 -> ChangeDirection Left |> fireAction // left arrow 
    | 65 -> AdvanceToNextLevel |> fireAction // a
    | 27 -> EndGame |> fireAction // esc
    | _ -> ()
   :> obj
)