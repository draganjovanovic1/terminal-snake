open System
open System.Threading.Tasks

[<AutoOpen>]
module Core =

    type Square = { x : int; y : int }

    type Direction = Up | Right | Down | Left

    let degToDirection angle = 
        let degToRad = (*) (Math.PI / 180.0)
        let rad = degToRad angle
        (Math.Cos rad, Math.Sin rad)

    let getVector magnitude direction =
        let mag = magnitude |> float
        let vX, vY = direction
        (vX * mag), (vY * mag)

    let moveByAngle angle steps pos =
        let vX, vY = 
            degToDirection angle 
            |> getVector steps
        { x = pos.x + (vX |> round |> int)
          y = pos.y + (vY |> round |> int) }

    let moveToDirection direction steps pos =
        let deg =
            match direction with
            | Up -> 270.0
            | Right -> 0.0
            | Down -> 90.0
            | Left -> 180.0
        moveByAngle deg steps pos

    let ensureInBounds bounds pos =
        let min, max = bounds
        let getValue v min max =
            match v with 
            | x when x < min -> max - 1
            | x when x >= max -> 0
            | x -> x

        let x = getValue pos.x min.x max.x
        let y = getValue pos.y min.y max.y
        { x = x; y = y}

[<AutoOpen>]
module Graphics =
    let drawSquare char square =
        Console.SetCursorPosition (square.x, square.y)
        printf char

    let clearScreen () =
        Console.Clear ()

    let drawMany char squares =
        squares |> List.iter (drawSquare char)

    let drawSnake = drawMany "*"
    let drawFood = drawMany "o"
    let drawMines = drawMany "#"

module SnakeGame =

    let mutable private gameBounds = ({x = 0; y = 0}, { x = 0; y = 0 })
    let mutable private direction = Right
    let mutable private snake = []
    let mutable private mines = []
    let mutable private food = []
    let mutable private level = 1

    let private aliveSnakeColor = ConsoleColor.DarkBlue
    let private deadSnakeColor = ConsoleColor.DarkRed
    let private foodColor = ConsoleColor.DarkGreen
    let private minesColor = ConsoleColor.Red

    let private redraw snakeColor foodColor =
        clearScreen ()
        Console.ForegroundColor <- snakeColor
        drawSnake snake
        Console.ForegroundColor <- foodColor
        drawFood food
        Console.ForegroundColor <- minesColor
        drawMines mines

    let private createSquare bounds occupied = 
        let rnd = new Random ()
        let _, { x = maxX; y = maxY } = bounds
        let rec makeOne () =
            let square = 
                { x = rnd.Next(maxX)
                  y = rnd.Next(maxY) }
            let isOccupied = 
                occupied 
                |> List.exists ((=) square)
            if isOccupied then
                makeOne ()
            else
                square
        makeOne ()

    let private isCrashed head =
        snake@mines |> List.exists ((=) head)

    let private tryEatSomeFood head =
        let eaten = food |> List.exists ((=) head)
        if eaten then
            food <- food |> List.filter ((<>) head)
        eaten

    let private moveSnake snake direction =
        match snake with
        | [] -> true, []
        | head::_ ->
            let head = 
                head 
                |> moveToDirection direction 1
                |> ensureInBounds gameBounds
            let foodEaten = tryEatSomeFood head
            let body =
                if foodEaten then
                    snake
                else
                    snake |> List.take (snake.Length - 1)
            isCrashed head, head::body

    let start bounds startPosition initialDirection =
        gameBounds <- bounds
        snake <- (startPosition |> ensureInBounds gameBounds)::[]
        direction <- initialDirection

        redraw aliveSnakeColor foodColor

        let rec foodLoop () =
            async {
                do! Async.Sleep (5000 / level)
                food <- (createSquare bounds (snake@mines))::food
                return! foodLoop ()
            }

        let rec gameLoop () =
            async {
                do! Async.Sleep (100 / level)
                let isEnd, newSnake = moveSnake snake direction
                snake <- newSnake

                if isEnd then
                    redraw deadSnakeColor foodColor
                    Console.SetCursorPosition(0, 1)
                    printfn "GAME OVER! Your score: %i" snake.Length
                else
                    redraw aliveSnakeColor foodColor
                    if (snake.Length / level) > 10 then 
                        level <- level + 1
                        mines <- (createSquare bounds (snake@food))::mines
                    return! gameLoop ()
            }

        foodLoop () |> Async.StartImmediate
        gameLoop () |> Async.StartImmediate

        while true do
            let key = Console.ReadKey(true).Key
            direction <-
                match key with 
                | ConsoleKey.UpArrow when direction <> Down -> Up
                | ConsoleKey.RightArrow when direction <> Left -> Right
                | ConsoleKey.DownArrow when direction <> Up -> Down
                | ConsoleKey.LeftArrow  when direction <> Right -> Left
                | _ -> direction
            
[<EntryPoint>]
let main _ = 
    Console.Clear ()
    Console.CursorVisible <- false
    Console.BackgroundColor <- ConsoleColor.White

    let maxX, maxY = Console.WindowWidth, Console.WindowHeight
    let gameBounds = { x = 0; y = 0 }, { x = maxX; y = maxY }
    let startPosition = { x = maxX / 2; y = maxY / 2 }
    let startDirection = Right
    
    SnakeGame.start gameBounds startPosition startDirection

    Console.Clear ()
    Console.ResetColor ()
    Console.SetCursorPosition (0, 0)
    0