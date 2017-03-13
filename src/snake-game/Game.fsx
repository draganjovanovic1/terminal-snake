namespace SnakeGame

module Core =

    open System

    type Square = { x : int; y : int }
    type Bounds = Square * Square
    type Snake = Square list
    type Food = Square list
    type Mines = Square list
    type Level = int
    type Direction = Up | Right | Down | Left
    type GameStatus = InProgress | GameOver

    type GameConfig = 
        { bounds : Bounds
          startPosition : Square
          startDirection : Direction }

    type Action =
        | MoveSnake
        | ChangeDirection of Direction 
        | AddFood
        | AdvanceToNextLevel
        | EndGame

    type Renderer = 
        { redraw : Snake -> Food -> Mines -> Level -> unit 
          ended : Snake -> Food -> Mines -> Level -> unit  }

    exception BoundLessThanZeroException of string
    exception BoundsTooSmallException of string

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
        | x when x < minValue -> raise (BoundLessThanZeroException "Min X bound must be 0 or greater")
        | x when max.x - x < minDiff -> raise (BoundsTooSmallException <| sprintf "Difference between max X and min X bound must be at least %i" minDiff)
        | _ -> ()
        match min.y with
        | y when y <= minValue -> raise (BoundLessThanZeroException "Min Y bound must be 0 or greater")
        | y when max.y - y < 5 -> raise (BoundsTooSmallException <| sprintf "Difference between max Y and min Y bound must be at least %i" minDiff)
        | _ -> ()

module Game =
    
    open System
    open Core

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

    let private createGame bounds renderer snake food mines direction level = 
        let mutable level = level
        let mutable gameStatus = InProgress
        let getLevel () = level
        let checkGameStatus () = gameStatus
        let gameAgent = 
            MailboxProcessor.Start(fun inbox ->
                let rec loop snake food mines direction = async {
                    try
                        let! action = inbox.Receive()

                        match action with
                        | MoveSnake ->
                            gameStatus <- if isCrashed snake mines then GameOver else InProgress
                            match gameStatus with
                            | GameOver ->
                                renderer.ended snake food mines level
                            | InProgress ->
                                renderer.redraw snake food mines level
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
        gameAgent, getLevel, checkGameStatus
        
    let startGame gameConfig renderer commandsStream =
        let { bounds = bounds; startPosition = startPosition; startDirection = startDirection } = gameConfig

        ensureValidBounds bounds 0 5
        let startSquare = startPosition |> ensureInBounds bounds
        let gameAgent, getLevel, checkGameStatus = 
            createGame bounds renderer [startSquare] [] [] startDirection 1

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

        commandsStream 
        |> Observable.subscribe gameAgent.Post
        |> ignore

        checkGameStatus