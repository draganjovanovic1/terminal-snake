namespace Android.Snake

open Android.App
open Android.Content.PM
open Android.Views
open SkiaSharp
open SkiaSharp.Views.Android
type Resources = Android.Snake.Resource

open SnakeGame
open SnakeGame.Core
open Gestures

[<Activity (MainLauncher = true, LaunchMode = LaunchMode.SingleTask, Icon = "@mipmap/icon", NoHistory = true, 
    Theme = "@android:style/Theme.DeviceDefault.NoActionBar", ConfigurationChanges = ConfigChanges.Orientation, ScreenOrientation = ScreenOrientation.Portrait)>]
type MainActivity () =
    inherit Activity ()

    let mutable gestureDetector: GestureDetector = null
    let mutable checkGameStatus = fun () -> GameStatus.GameOver

    let fireAction, actionsStream = 
        let e = Event<_> ()
        e.Trigger, e.Publish

    override __.OnTouchEvent (e) = 
        gestureDetector.OnTouchEvent e |> ignore
        true

    override __.OnBackPressed () =
        match checkGameStatus () with
        | GameOver -> base.OnBackPressed ()
        | InProgress -> fireAction EndGame

    override x.OnCreate (bundle) =
        base.OnCreate (bundle)
        x.SetContentView (Resources.Layout.Main)

        let gestureListener = new Gestures.TapGestureListener (x)
        gestureDetector <- new GestureDetector (x, gestureListener)

        let canvasView = x.FindViewById<SKCanvasView> (Resources.Id.canvasView)

        let bootstrap canvasWidth canvasHeight =
            let squareSize = 10.f |> Utils.dpToPixels x

            let maxX = canvasWidth / squareSize
            let maxY = canvasHeight / squareSize

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
            
            gestureListener.Tapped
            |> Observable.subscribe (tapToDirection >> fireAction)
            |> ignore

            let renderer = makeRenderer x (float32 squareSize) canvasView

            checkGameStatus <- Game.startGame gameConfig renderer actionsStream

        canvasView.ViewTreeObserver.PreDraw
        |> Observable.subscribe (
            let mutable initialized = false
            fun p ->
                let width = canvasView.MeasuredWidth
                let height = canvasView.MeasuredHeight
                if not initialized then
                    bootstrap width height
                    initialized <- true
        )
        |> ignore