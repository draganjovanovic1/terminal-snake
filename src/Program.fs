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
        | AdvanceToNextLevel
        | EndGame

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

    let clearScreen () = Console.Clear ()

    let drawMany char color squares =
        squares |> List.iter (drawSquare char color)

    let drawSnake = drawMany "*"
    let drawFood = drawMany "o"
    let drawMines = drawMany "#"

    let drawGameStatus color bounds level score =
        Console.ForegroundColor <- color
        let { x = minX; y = minY }, { x = maxX; y = _ } = bounds
        if minY >= 1 then
            Console.SetCursorPosition (0, 0)
            printf "Score: %05i\tLevel: %05i" score level
            if minY >= 2 then
                [minX..maxX-1]
                |> List.iter (fun x ->
                    Console.SetCursorPosition (x, minY - 1)
                    printf "_"
                )

module SnakeGame =
    let private gameStatusColor = ConsoleColor.DarkYellow
    let private aliveSnakeColor = ConsoleColor.DarkGreen
    let private deadSnakeColor = ConsoleColor.DarkRed
    let private foodColor = ConsoleColor.Magenta
    let private minesColor = ConsoleColor.Red

    let private redraw bounds foodColor minesColor snakeColor snake food mines level =
        clearScreen ()
        let score = snake |> List.fold (fun s _ ->  s + 1) 0
        drawGameStatus gameStatusColor bounds level score
        drawSnake snakeColor snake
        drawFood foodColor food 
        drawMines minesColor mines

    let private createSquare bounds occupied = 
        let rnd = Random ()
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

    let private isCrashed snake mines =
        match snake with
        | [] -> true
        | head::body -> body@mines |> List.exists ((=) head)

    let private tryEatSomeFood food head =
        let eaten = food |> List.exists ((=) head)
        
        let food =
            if eaten then
                food |> List.filter ((<>) head)
            else 
                food
        eaten, food

    let private moveSnake bounds snake food mines direction =
        match snake with
        | [] -> snake, food, mines
        | head::_ ->
            let head = 
                head 
                |> moveToDirection direction 1
                |> ensureInBounds bounds
            let foodEaten, food = tryEatSomeFood food head
            let body =
                if foodEaten then
                    snake
                else
                    snake |> List.take (snake.Length - 1)
            let snake = head::body
            snake, food, mines

    let private canAcceptDirection snake direction =
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

    let private shouldAdvanceToNextLevel (snake : 'a list) level = snake.Length / level > 9

    let private addMines bounds snake food mines level =
        let rec addMines mines left =
            match left with
            | 0 -> mines
            | _ -> 
                let mines = (createSquare bounds (snake@food@mines))::mines
                let left = left - 1
                addMines mines left
        let mines = addMines mines level
        mines

    let private createGame bounds snake food mines direction level = 
        let mutable level = level
        let mutable isEnd = false
        let redraw = redraw bounds foodColor minesColor
        let getLevel () = level
        let isGameInProgress () = not isEnd
        let gameAgent = 
            MailboxProcessor.Start(fun inbox ->
                let rec loop snake food mines direction = async {
                    try
                        let! action = inbox.Receive()

                        match action with
                        | MoveSnake ->
                            isEnd <- isCrashed snake mines
                            if isEnd then
                                redraw deadSnakeColor snake food mines level
                            else
                                redraw aliveSnakeColor snake food mines level

                                let snake, food, mines = moveSnake bounds snake food mines direction
                                if shouldAdvanceToNextLevel snake level then
                                    inbox.Post AdvanceToNextLevel                                
                                return! loop snake food mines direction
                        | ChangeDirection d ->
                            let direction =  
                                if canAcceptDirection snake d then
                                    d
                                else
                                    direction
                            return! loop snake food mines direction
                        | AddFood -> 
                            let food = (createSquare bounds (snake@food@mines))::food
                            return! loop snake food mines direction
                        | AdvanceToNextLevel ->
                            level <- level + 1
                            let mines = addMines bounds snake food mines level
                            return! loop snake food mines direction
                        | EndGame -> ()
                    with
                        | ex -> ()
                }
                loop snake food mines direction
            )
        gameAgent, getLevel, isGameInProgress
        
    let startGame bounds startPosition startDirection =
        ensureValidBounds bounds 0 5
        let startSquare = startPosition |> ensureInBounds bounds
        let gameAgent, getLevel, isGameInProgress = createGame bounds [startSquare] [] [] startDirection 1

        let rec addFoodLoop () =
            async {
                do! Async.Sleep 5000
                gameAgent.Post AddFood 
                return! addFoodLoop ()
            }

        let rec gameLoop () =
            async {
                do! Async.Sleep (100 / getLevel ())
                gameAgent.Post MoveSnake
                return! gameLoop ()
            }

        addFoodLoop () |> Async.StartImmediate
        gameLoop () |> Async.StartImmediate

        let rec collectInput () =
            let postAction action = action |> Option.iter gameAgent.Post 
            let checkGameStatusAndContinue () = if isGameInProgress () then collectInput () 
            let postActionAndContinue = postAction >> checkGameStatusAndContinue
            match Console.ReadKey(true).Key with 
            | ConsoleKey.UpArrow -> ChangeDirection Up |> Some |> postActionAndContinue
            | ConsoleKey.RightArrow -> ChangeDirection Right |> Some |> postActionAndContinue
            | ConsoleKey.DownArrow -> ChangeDirection Down |> Some |> postActionAndContinue
            | ConsoleKey.LeftArrow -> ChangeDirection Left |> Some |> postActionAndContinue
            | ConsoleKey.Add -> AdvanceToNextLevel |> Some |> postActionAndContinue
            | ConsoleKey.Escape -> EndGame |> Some |> postAction
            | _ -> None |> postActionAndContinue
        collectInput ()
            
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
    
    SnakeGame.startGame gameBounds startPosition startDirection

    Console.SetCursorPosition (startPosition.x, startPosition.y)
    Console.ForegroundColor <- ConsoleColor.Red
    printf "GAME OVER! Press any key to continue..."

    Console.ReadKey true |> ignore

    Console.SetCursorPosition (0, 0)
    Console.ResetColor ()
    Console.Clear ()
    0