namespace SnakeGame

module Core =

    open System

    type Square = { x: int; y: int }
    type Bounds = Square * Square
    type Snake = Square list
    type FoodItem = { Position: Square; BestBefore: DateTime }
    type Food = FoodItem list
    type Mines = Square list
    type Level = int
    type Score = int
    type Direction = Up | Right | Down | Left
    type GameStatus = 
        | InProgress of level: Level * score: Score 
        | GameOver of level: Level * score: Score 

    type GameConfig = 
        { bounds: Bounds
          startPosition: Square
          startLength: int
          startDirection: Direction }

    type Action =
        | MoveSnake
        | ChangeDirection of Direction 
        | AddFood
        | AdvanceToNextLevel
        | EndGame

    type Renderer = 
        { redraw: Snake -> Food -> Mines -> Level -> Score -> unit 
          ended: Snake -> Food -> Mines -> Level -> Score -> unit  }

    exception BoundLessThanZeroException of string
    exception BoundsTooSmallException of string
    exception SnakeTooShortException of string

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
        | y when y < minValue -> raise (BoundLessThanZeroException "Min Y bound must be 0 or greater")
        | y when max.y - y < minDiff -> raise (BoundsTooSmallException <| sprintf "Difference between max Y and min Y bound must be at least %i" minDiff)
        | _ -> ()

    let ensureValidLength startLength = 
        if startLength <= 0 then
            raise (SnakeTooShortException "Start length must be greater than 0")

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
        let eaten = food |> List.exists (fun f -> f.Position = head)
        let food =
            if eaten then
                food |> List.filter (fun f -> f.Position <> head)
            else 
                food
        eaten, food

    let private removeRottenFood = List.filter (fun f -> f.BestBefore > DateTime.Now)

    let private squaresOccupiedByFood = List.map (fun f -> f.Position)

    let private moveSnake bounds snake food mines direction =
        match snake with
        | [] -> snake, food, mines, false
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
            snake, food, mines, foodEaten

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

    let private addMines bounds snake food mines level =
        let rec addMines mines left =
            match left with
            | 0 -> mines
            | _ -> 
                let foodSquares = squaresOccupiedByFood food
                let mines = (createSquare bounds (snake@foodSquares@mines))::mines
                let left = left - 1
                addMines mines left
        let mines = addMines mines level
        mines

    let rec private getTail = function
        | [] -> []
        | [t] -> [t]
        | head::tail -> getTail tail

    let private shouldAdvanceToNextLevel score currentLevel =
        score % 10 = 0 && score / 10 + 1 > currentLevel

    type GameInit = 
        { bounds: Bounds
          renderer: Renderer
          snake: Snake
          food: Food
          mines: Mines
          direction: Direction
          startLength: int
          level: Level }

    let private createGame (init: GameInit) = 
        let setGameStatus, checkGameStatus =
            let mutable status = InProgress (1, 0) 
            (fun s -> status <- s), (fun () -> status)
        let gameAgent = 
            MailboxProcessor.Start(fun inbox ->
                let rec loop snake food mines direction level score = async {
                    try
                        let! action = inbox.Receive ()

                        match action with
                        | MoveSnake ->
                            if isCrashed snake mines then 
                                setGameStatus <| GameOver (level, score)
                            else 
                                setGameStatus <| InProgress (level, score)
                            match checkGameStatus () with
                            | GameOver _ ->
                                init.renderer.ended snake food mines level score
                            | InProgress _ ->
                                init.renderer.redraw snake food mines level score
                                
                                let tail = getTail snake
                                let snake = 
                                    if snake.Length < init.startLength then
                                        snake@tail
                                    else
                                        snake

                                let food = removeRottenFood food
                                let snake, food, mines, foodEaten = moveSnake init.bounds snake food mines direction
                                
                                let score = if foodEaten then score + 1 else score

                                if shouldAdvanceToNextLevel score level then
                                    inbox.Post AdvanceToNextLevel

                                return! loop snake food mines direction level score
                        | ChangeDirection requestedDirection ->
                            let direction =  
                                if canAcceptDirection snake requestedDirection then
                                    requestedDirection
                                else
                                    direction
                            return! loop snake food mines direction level score
                        | AddFood -> 
                            let foodSquares = squaresOccupiedByFood food
                            let newFood = 
                                { Position = createSquare init.bounds (snake@foodSquares@mines)
                                  BestBefore = DateTime.Now.AddSeconds (15.) }
                            let food = newFood::food
                            return! loop snake food mines direction level score
                        | AdvanceToNextLevel ->
                            let level = level + 1
                            let mines = addMines init.bounds snake food mines level
                            return! loop snake food mines direction level score
                        | EndGame -> ()
                    with
                        | ex -> ()
                }
                loop init.snake init.food init.mines init.direction init.level 0
            )
        gameAgent, checkGameStatus
        
    let startGame gameConfig renderer commandsStream =
        let { bounds = bounds
              startPosition = startPosition
              startLength = startLength
              startDirection = startDirection } = gameConfig

        ensureValidBounds bounds 0 5
        ensureValidLength startLength
        let startSquare = startPosition |> ensureInBounds bounds

        let gameInit =
            { bounds = bounds
              renderer = renderer
              snake = [startSquare]
              food = []
              mines = []
              direction = startDirection
              startLength = startLength
              level = 1 }

        let gameAgent, checkGameStatus = createGame gameInit

        let rec addFoodLoop () =
            async {
                do! Async.Sleep 5000
                gameAgent.Post AddFood 
                return! addFoodLoop ()
            }

        let rec gameLoop () =
            async {
                let delay level =
                    async {
                        let divideBy = 1. + Math.Log (float (level))
                        let delay = Math.Floor (100. / divideBy) |> int
                        do! Async.Sleep delay
                        gameAgent.Post MoveSnake
                    }

                match checkGameStatus () with
                | InProgress (l, _) -> 
                    do! delay l
                    return! gameLoop ()
                | GameOver (l, _) -> 
                    do! delay l
            }

        addFoodLoop () |> Async.StartImmediate
        gameLoop () |> Async.StartImmediate

        commandsStream 
        |> Observable.subscribe gameAgent.Post
        |> ignore

        checkGameStatus