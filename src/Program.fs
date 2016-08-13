open System
open System.Threading.Tasks

[<AutoOpen>]
module Core =

    type Square = { x : int; y : int }

    type Direction = Up | Right | Down | Left

    type Action =
        | MoveSnake
        | ChangeDirection of Direction 
        | AddFood
        | AddMines of int
        | AdvanceToNextLevel

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
            | x when x >= max -> min
            | x -> x

        let x = getValue pos.x min.x max.x
        let y = getValue pos.y min.y max.y
        { x = x; y = y}

    let getBoundsCenter bounds =
        let min, max = bounds
        { x = (max.x - min.x) / 2
          y = (max.y - min.y) / 2 }

    let ensureValidBounds bounds minValue minDiff =
        let min, max = bounds
        match min.x with
        | x when x < minValue -> failwith "Min X bound must be 0 or greater"
        | x when max.x - x < minDiff -> failwith (sprintf "Difference between max X and min X bound must be at least %i" minDiff)
        | _ -> ()
        match min.y with
        | y when y <= minValue -> failwith "Min Y bound must be 0 or greater"
        | y when max.y - y < 5 -> failwith (sprintf "Difference between max Y and min Y bound must be at least %i" minDiff)
        | _ -> ()

[<AutoOpen>]
module Graphics =
    let drawSquare char color square =
        Console.ForegroundColor <- color
        Console.SetCursorPosition (square.x, square.y)
        printf char

    let clearScreen () =
        Console.Clear ()

    let drawMany char color squares =
        squares |> List.iter (drawSquare char color)

    let drawSnake = drawMany "*"
    let drawFood = drawMany "o"
    let drawMines = drawMany "#"

    let drawGameStatus color bounds level score =
        Console.ForegroundColor <- color
        let { x = _; y = minY }, { x = maxX; y = _ } = bounds
        if minY >= 1 then
            Console.SetCursorPosition (0, 0)
            printf "Score: %05i\tLevel: %05i" score level
            if minY >= 2 then
                [0..maxX]
                |> List.iter (fun x ->
                    Console.SetCursorPosition (x, minY - 1)
                    printf "_"
                )

type SnakeGame (bounds) =

    do
        ensureValidBounds bounds 0 5
        
    let mutable direction = Right
    let mutable snake = []
    let mutable mines = []
    let mutable food = []
    let mutable level = 1
    let mutable gameInProgress = false

    let gameStatusColor = ConsoleColor.DarkYellow
    let aliveSnakeColor = ConsoleColor.DarkGreen
    let deadSnakeColor = ConsoleColor.DarkRed
    let foodColor = ConsoleColor.Magenta
    let minesColor = ConsoleColor.Red

    let redraw snakeColor foodColor =
        clearScreen ()
        drawGameStatus gameStatusColor bounds level (snake.Length * level)
        drawSnake snakeColor snake
        drawFood foodColor food 
        drawMines minesColor mines

    let createSquare occupied = 
        let rnd = new Random ()
        let min, max = bounds
        let rec makeOne () =
            let square = 
                { x = rnd.Next(min.x, max.x)
                  y = rnd.Next(min.y, max.y) }
            let isOccupied = 
                occupied 
                |> List.exists ((=) square)
            if isOccupied then
                makeOne ()
            else
                square
        makeOne ()

    let isCrashed head =
        snake@mines |> List.exists ((=) head)

    let tryEatSomeFood head =
        let eaten = food |> List.exists ((=) head)
        if eaten then
            food <- food |> List.filter ((<>) head)
        eaten

    let moveSnake snake direction =
        match snake with
        | [] -> true, []
        | head::_ ->
            let head = 
                head 
                |> moveToDirection direction 1
                |> ensureInBounds bounds
            let foodEaten = tryEatSomeFood head
            let body =
                if foodEaten then
                    snake
                else
                    snake |> List.take (snake.Length - 1)
            isCrashed head, head::body

    let canAcceptDirection direction =
        match snake with 
        | [] -> false
        | [_] -> true
        | head::(neck::tail)->
            match direction with
            | Up when head.x = neck.x -> false
            | Right when head.y = neck.y -> false
            | Down when head.x = neck.x -> false
            | Left when head.y = neck.y -> false
            | _ -> true

    let endGame () = gameInProgress <- false

    let gameProcessor = MailboxProcessor.Start(fun inbox ->
        let rec loop() = async {
            try
                let! action = inbox.Receive()
                match action with
                | MoveSnake ->
                    let isEnd, newSnake = moveSnake snake direction
                    snake <- newSnake

                    if isEnd then
                        redraw deadSnakeColor foodColor
                        endGame ()
                    else
                        redraw aliveSnakeColor foodColor
                | ChangeDirection d -> 
                    if canAcceptDirection d then
                        direction <- d
                | AddFood -> 
                    food <- (createSquare (snake@food@mines))::food
                | AdvanceToNextLevel ->
                    level <- level + 1
                | AddMines i ->
                    [0..i]
                    |> List.iter (fun _ -> 
                        mines <- (createSquare (snake@food@mines))::mines
                    )
            with
                | ex -> ()
            return! loop()
        }
        loop ()
    )

    let advanceToNextLevel () =
        gameProcessor.Post AdvanceToNextLevel
        AddMines (level - 1) |> gameProcessor.Post

    member x.Start (startPosition, startDirection) =
        snake <- (startPosition |> ensureInBounds bounds)::[]
        direction <- startDirection

        redraw aliveSnakeColor foodColor

        let rec addFoodLoop () =
            async {
                do! Async.Sleep (5000 / level)
                gameProcessor.Post AddFood 
                return! addFoodLoop ()
            }

        let rec gameLoop () =
            async {
                do! Async.Sleep (100 / level)
                if gameInProgress then
                    gameProcessor.Post MoveSnake
                    let shouldAdvanceToNextLevel = snake.Length / level > 9 
                    if shouldAdvanceToNextLevel then
                        advanceToNextLevel ()
                return! gameLoop ()
            }

        addFoodLoop () |> Async.StartImmediate
        gameLoop () |> Async.StartImmediate
        gameInProgress <- true

        while gameInProgress do
            let key = Console.ReadKey(true).Key
            match key with 
            | ConsoleKey.UpArrow -> ChangeDirection Up |> gameProcessor.Post
            | ConsoleKey.RightArrow -> ChangeDirection Right |> gameProcessor.Post
            | ConsoleKey.DownArrow -> ChangeDirection Down |> gameProcessor.Post
            | ConsoleKey.LeftArrow -> ChangeDirection Left |> gameProcessor.Post
            | ConsoleKey.Add -> advanceToNextLevel ()
            | ConsoleKey.Escape -> endGame ()
            | _ -> ()
            
[<EntryPoint>]
let main _ = 
    Console.Clear ()
    Console.CursorVisible <- false
    Console.BackgroundColor <- ConsoleColor.White

    let min = { x = 0; y = 2 }
    let max = { x = Console.WindowWidth; y = Console.WindowHeight }
    let gameBounds = min, max
    let startPosition = getBoundsCenter gameBounds
    let startDirection = Right
    
    SnakeGame(gameBounds).Start (startPosition, startDirection)

    Console.SetCursorPosition (startPosition.x, startPosition.y)
    Console.ForegroundColor <- ConsoleColor.Red
    printf "GAME OVER! Press any key to continue..."

    Console.ReadKey true |> ignore

    Console.SetCursorPosition (0, 0)
    Console.ResetColor ()
    Console.Clear ()
    0