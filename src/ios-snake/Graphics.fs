namespace IOS.Snake

[<AutoOpen>]
module Graphics = 

    open Foundation
    open UIKit
    open SkiaSharp
    open SkiaSharp.Views.iOS

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
        paint.TextSize <- 80.f
        canvas.DrawText (sprintf "L%02i S%04i" level score, float32 (maxX / 2), float32 (maxY / 2), paint)

    let private getCoordsFromSquare square =
        let { x = xx; y = yy } = square
        xx |> float32, yy |> float32

    let private drawSquare (canvas: SKCanvas) squareSize (color: Color) square =
        use paint = color.ToSkColor () |> makePaint
        paint.StrokeCap <- SKStrokeCap.Square

        let x, y = square |> getCoordsFromSquare
        let rect = SKRect (x * squareSize, y * squareSize, x * squareSize + squareSize, y * squareSize + squareSize)
        canvas.DrawRect (rect, paint)

    let private drawCircle (canvas: SKCanvas) squareSize (color: Color) square =
        use paint = color.ToSkColor () |> makePaint
        paint.StrokeCap <- SKStrokeCap.Round

        let x, y = square |> getCoordsFromSquare
        let halfSquareSize = (squareSize / 2.f) |> float |> System.Math.Floor |> float32
        canvas.DrawCircle (x * squareSize + halfSquareSize, y * squareSize + halfSquareSize, halfSquareSize, paint)

    let private drawMany drawFn squareSize color (canvas: SKCanvas) squares = squares |> List.iter (drawFn canvas squareSize color)

    let makeRenderer (caller: NSObject) squareSize (canvasView: SKCanvasView) =

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
                drawScore canvas (int canvasView.Bounds.Width * int UIScreen.MainScreen.Scale) (int canvasView.Bounds.Height * int UIScreen.MainScreen.Scale) l score
                drawSnake canvas s
                drawFood canvas f
                drawMines canvas m
            caller.InvokeOnMainThread (fun _ -> canvasView.SetNeedsDisplay ())
          ended = fun s f m l ->
            drawGame <- fun canvas ->
                clearCanvas canvas
                let score = s |> List.fold (fun s _ ->  s + 1) 0
                drawScore canvas (int canvasView.Bounds.Width * int UIScreen.MainScreen.Scale) (int canvasView.Bounds.Height * int UIScreen.MainScreen.Scale) l score
                drawDeadSnake canvas s
                drawFood canvas f
                drawMines canvas m
            caller.InvokeOnMainThread (fun _ -> canvasView.SetNeedsDisplay  ()) }