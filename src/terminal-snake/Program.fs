namespace TerminalSnake

open System
open SnakeGame
open SnakeGame.Core

[<AutoOpen>]
module ConsoleGraphics =

    let private  drawSquare char color square =
        Console.ForegroundColor <- color
        Console.SetCursorPosition (square.x, square.y)
        printf char

    let private clearScreen () = Console.Clear ()

    let private  drawMany char color squares =
        squares |> List.iter (drawSquare char color)

    let private  drawSnake = drawMany "*"
    let private  drawFood = drawMany "o"
    let private  drawMines = drawMany "#"

    let private  drawGameStatus color bounds level score =
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

    let redraw bounds gameStatus snake food mines level =
        let gameStatusColor = ConsoleColor.DarkYellow
        let aliveSnakeColor = ConsoleColor.DarkGreen
        let deadSnakeColor = ConsoleColor.DarkRed
        let foodColor = ConsoleColor.Magenta
        let minesColor = ConsoleColor.Red

        let snakeColor = 
            match gameStatus with
            | InProgress -> aliveSnakeColor
            | GameOver -> deadSnakeColor

        clearScreen ()
        let score = snake |> List.fold (fun s _ ->  s + 1) 0
        drawGameStatus gameStatusColor bounds level score
        drawSnake snakeColor snake
        drawFood foodColor food 
        drawMines minesColor mines

module Program =

    [<EntryPoint>]
    let main _ = 
        Console.Clear ()
        Console.CursorVisible <- false
        Console.BackgroundColor <- ConsoleColor.White

        let gameConfig = 
            let min = { x = 0; y = 2 }
            let max = { x = Console.WindowWidth; y = Console.WindowHeight }
            let gameBounds = min, max
            { bounds = gameBounds
              startPosition = getBoundsCenter (gameBounds)
              startDirection = Right }

        let fireAction, actionsStream = 
            let e = Event<_> ()
            e.Trigger, e.Publish

        let renderer = 
            { redraw = redraw gameConfig.bounds InProgress
              ended = redraw gameConfig.bounds GameOver }

        let checkGameStatus = Game.startGame gameConfig renderer actionsStream

        let rec collectInput () =
            let checkGameStatusAndContinue () =
                match checkGameStatus () with
                | InProgress -> collectInput () 
                | GameOver -> 
                    Console.SetCursorPosition (gameConfig.startPosition.x, gameConfig.startPosition.y)
                    Console.ForegroundColor <- ConsoleColor.Red
                    printf "GAME OVER! Press any key to continue..."
            let postActionAndContinue = fireAction >> checkGameStatusAndContinue
            match Console.ReadKey(true).Key with 
            | ConsoleKey.UpArrow -> ChangeDirection Up |> postActionAndContinue
            | ConsoleKey.RightArrow -> ChangeDirection Right |> postActionAndContinue
            | ConsoleKey.DownArrow -> ChangeDirection Down |> postActionAndContinue
            | ConsoleKey.LeftArrow -> ChangeDirection Left |> postActionAndContinue
            | ConsoleKey.Add -> AdvanceToNextLevel |> postActionAndContinue
            | ConsoleKey.Escape -> EndGame |> postActionAndContinue
            | _ -> checkGameStatusAndContinue ()
        collectInput ()

        Console.ReadKey true |> ignore
        Console.SetCursorPosition (0, 0)
        Console.ResetColor ()
        Console.Clear ()
        
        0