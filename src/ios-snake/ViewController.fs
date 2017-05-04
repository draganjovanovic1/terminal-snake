namespace IOS.Snake

open System

open Foundation
open UIKit
open SkiaSharp.Views.iOS

open SnakeGame
open SnakeGame.Core
open Gestures

[<Register ("ViewController")>]
type ViewController (handle: IntPtr) =
    inherit UIViewController (handle)

    let makeEvent () =
        let e = Event<_>()
        e.Trigger, e.Publish

    let fireTapped, tappedStream = makeEvent ()
    let fireAction, actionsStream = makeEvent ()

    [<Outlet>]
    member val CanvasView: SKCanvasView = null with get, set

    override x.TouchesBegan (touches, evt) =
        base.TouchesBegan (touches, evt)
        let touch = touches.AnyObject :?> UITouch
        let tapX = touch.LocationInView(x.View).X

        if tapX < UIScreen.MainScreen.Bounds.Width / nfloat 2. then 
            fireTapped LeftHalfTap 
        else 
            fireTapped RightHalfTap

    override x.ViewDidLoad () =
        base.ViewDidLoad ()

        x.CanvasView <- new SKCanvasView (UIScreen.MainScreen.Bounds)
        x.CanvasView.AutoresizingMask <- UIViewAutoresizing.FlexibleDimensions
        x.View.Add (x.CanvasView)

        let squareSize = 10 * int UIScreen.MainScreen.Scale

        let maxX = (int UIScreen.MainScreen.Bounds.Width * int UIScreen.MainScreen.Scale) / squareSize
        let maxY = (int UIScreen.MainScreen.Bounds.Height * int UIScreen.MainScreen.Scale) / squareSize

        let gameConfig = 
            let min = { x = 0; y = 0 }
            let max = { x = maxX; y = maxY }
            let gameBounds = min, max
            { bounds = gameBounds
              startPosition = getBoundsCenter gameBounds
              startLength = 4
              startDirection = Right }

        let tapToDirection =
            let mutable lastDirection = gameConfig.startDirection
            let memoize d = lastDirection <- d; d
            fun tap ->
                match lastDirection, tap with
                | Up, RightHalfTap -> Right
                | Right, RightHalfTap -> Down
                | Down, RightHalfTap -> Left
                | Left, RightHalfTap -> Up
                | Up, LeftHalfTap -> Left
                | Right, LeftHalfTap -> Up
                | Down, LeftHalfTap -> Right
                | Left, LeftHalfTap -> Down
                |> memoize
                |> ChangeDirection
        
        tappedStream
        |> Observable.subscribe (tapToDirection >> fireAction)
        |> ignore

        let renderer = makeRenderer x (float32 squareSize) x.CanvasView

        let checkGameStatus = Game.startGame gameConfig renderer actionsStream
        ()

    override x.ShouldAutorotateToInterfaceOrientation (toInterfaceOrientation) =
        // Return true for supported orientations
        if UIDevice.CurrentDevice.UserInterfaceIdiom = UIUserInterfaceIdiom.Phone then
           toInterfaceOrientation <> UIInterfaceOrientation.PortraitUpsideDown
        else
           true