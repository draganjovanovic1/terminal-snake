namespace Android.Snake

module Utils =

    open Android.Runtime
    open Android.Content
    open Android.Views
    open Android.Util

    let getScreenMetrics (context: Context) =
        let wm = context.GetSystemService(Context.WindowService).JavaCast<IWindowManager>()
        let metrics = new DisplayMetrics()
        wm.DefaultDisplay.GetMetrics (metrics)
        metrics

    let dpToPixels (context: Context) dp =
        let scale = context.Resources.DisplayMetrics.Density
        let px = dp * scale + 0.5f |> round |> int
        px

module Gestures =

    open Java.Lang
    open Android.Views
    open Android.Content

    type Swipe =
        | NoSwipe
        | LeftSwipe
        | UpSwipe
        | RightSwipe
        | DownSwipe

    type HalfScreenTap =
        | LeftHalfTap
        | RightHalfTap

    type SwipeGestureListener () =
        inherit GestureDetector.SimpleOnGestureListener ()

        let swiped = Event<_> ()

        let (|ToUp|_|) i = 
            if i > 45. && i <= 135. then Some () else None
        let (|ToLeft|_|) i = 
            if i >= 135. && i <= 180. || i < -135. && i > -180. then Some () else None
        let (|ToDown|_|) i = 
            if i < -45. && i >= -135. then Some () else None
        let (|ToRight|_|) i = 
            if i > -45. && i <= 45. then Some () else None

        let getSlope (x1: float32, y1: float32) (x2: float32, y2: float32) =
            let angle = Math.ToDegrees (Math.Atan2 (float(y1 - y2), float(x2 - x1)))
            match angle with
            | ToUp -> UpSwipe
            | ToLeft -> LeftSwipe
            | ToDown -> DownSwipe
            | ToRight -> RightSwipe
            | _ -> NoSwipe

        member __.Swiped = swiped.Publish

        override __.OnFling (e1, e2, _, _) =
            match getSlope (e1.GetX (), e1.GetY ()) (e2.GetX (), e2.GetY ()) with
            | NoSwipe -> false
            | s -> swiped.Trigger s; true

    type TapGestureListener (context: Context) =
        inherit GestureDetector.SimpleOnGestureListener ()

        let tapped = Event<_> ()

        member __.Tapped = tapped.Publish

        override __.OnDown (e) =
            let currentX = e.GetX () |> Math.Round |> int

            let halfWidth =
                let screenMetrics = Utils.getScreenMetrics context
                screenMetrics.WidthPixels / 2

            if currentX < halfWidth then
                tapped.Trigger LeftHalfTap
            else
                tapped.Trigger RightHalfTap

            true

[<AutoOpen>]
module AndroidGraphics = 

    open Android.App
    open SkiaSharp
    open SkiaSharp.Views.Android

    open SnakeGame.Core

    type Color = 
        | DarkGreen
        | DarkRed
        | Magenta
        | Red
        | White
    with member x.ToSkColor () = 
            let toSkColor r g b = SKColor (byte r, byte g, byte b)
            match x with 
            | DarkGreen -> toSkColor 0 100 0
            | DarkRed -> toSkColor 139 0 0
            | Magenta -> toSkColor 255 0 255
            | Red -> toSkColor 255 0 0 
            | White -> toSkColor 255 255 255 

    let private makePaint color =
        let paint = new SKPaint ()
        paint.IsAntialias <- true
        paint.Color <- color
        paint.StrokeWidth <- 0.f
        paint

    let private clearCanvas (canvas: SKCanvas) = canvas.Clear (SKColors.White)

    let private drawScore (canvas: SKCanvas) maxX maxY level score =
        use paint = SKColor (byte 0, byte 0, byte 155, byte 35) |> makePaint
        paint.StrokeCap <- SKStrokeCap.Round
        paint.TextAlign <- SKTextAlign.Center
        paint.TextSize <- 150.f
        canvas.DrawText (sprintf "L%02i S%04i" level score, float32 (maxX / 2), float32 (maxY / 2), paint)

    let private getCoordsFromSquare square =
        let { x = xx; y = yy } = square
        xx |> float32, yy |> float32

    let private drawSquare (canvas: SKCanvas) squareSize (color: Color) square =
        use paint = color.ToSkColor () |> makePaint
        paint.StrokeCap <- SKStrokeCap.Square

        let x, y = square |> getCoordsFromSquare
        let rect = new SKRect (x * squareSize, y * squareSize, x * squareSize + squareSize, y * squareSize + squareSize)
        canvas.DrawRect (rect, paint)

    let private drawCircle (canvas: SKCanvas) squareSize (color: Color) square =
        use paint = color.ToSkColor () |> makePaint
        paint.StrokeCap <- SKStrokeCap.Round

        let x, y = square |> getCoordsFromSquare
        let halfSquareSize = (squareSize / 2.f) |> float |> System.Math.Floor |> float32
        canvas.DrawCircle (x * squareSize + halfSquareSize, y * squareSize + halfSquareSize, halfSquareSize, paint)

    let private drawMany drawFn squareSize color (canvas: SKCanvas) squares = squares |> List.iter (drawFn canvas squareSize color)

    let makeRenderer (activity: Activity) squareSize (canvasView: SKCanvasView) =

        let drawSnake = drawMany drawSquare squareSize DarkGreen
        let drawDeadSnake = drawMany drawSquare squareSize DarkRed
        let drawFood = drawMany drawCircle squareSize Magenta
        let drawMines = drawMany drawSquare squareSize Red

        let mutable drawGame : SKCanvas -> unit = fun _ -> ()

        canvasView.PaintSurface
        |> Observable.subscribe (fun s -> drawGame s.Surface.Canvas)
        |> ignore

        { redraw = fun s f m l ->
            drawGame <- fun canvas ->
                clearCanvas canvas
                let score = s |> List.fold (fun s _ ->  s + 1) 0
                drawScore canvas canvasView.Width canvasView.Height l score
                drawSnake canvas s
                drawFood canvas f
                drawMines canvas m
            activity.RunOnUiThread (fun _ -> canvasView.Invalidate ())
          ended = fun s f m l ->
            drawGame <- fun canvas ->
                clearCanvas canvas
                let score = s |> List.fold (fun s _ ->  s + 1) 0
                drawScore canvas canvasView.Width canvasView.Height l score
                drawDeadSnake canvas s
                drawFood canvas f
                drawMines canvas m
            activity.RunOnUiThread (fun _ -> canvasView.Invalidate ()) }

open Android.App
open Android.Content.PM
open Android.Views
type Resources = Android.Snake.Resource
open SkiaSharp
open SkiaSharp.Views.Android

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
        canvasView.IgnorePixelScaling <- false

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