namespace Android.Snake

open System

open Android.App
open Android.Content
open Android.Content.PM
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Android.Util
open SnakeGame
open SnakeGame.Core

type Resources = Android.Snake.Resource

type GameView (context: Context, attrs: IAttributeSet) =
    inherit View (context, attrs)

    override x.OnMeasure (widthMeasureSpec, heightMeasureSpec) =
        base.OnMeasure (widthMeasureSpec, heightMeasureSpec)

module Utils =

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


[<Activity (Label = "Android Snake", MainLauncher = true, Icon = "@mipmap/icon", ScreenOrientation = ScreenOrientation.Portrait)>]
type MainActivity () =
    inherit Activity ()

    override x.OnCreate (bundle) =
        x.RequestWindowFeature (WindowFeatures.NoTitle) |> ignore
        base.OnCreate (bundle)

        x.SetContentView (Resources.Layout.Main)
        let debugTextView = x.FindViewById<TextView>(Resources.Id.debugTextView)

        let writeDebug text = debugTextView.Text <- sprintf "%s\r\n%s" text debugTextView.Text

        let screenMetrics = Utils.getScreenMetrics (x)

        writeDebug (sprintf "Screen width: %i; screen height: %i" screenMetrics.WidthPixels screenMetrics.HeightPixels)

        let squareSize = 10.f |> Utils.dpToPixels x

        writeDebug (sprintf "Square size: %i" squareSize)

        let maxX = screenMetrics.WidthPixels / squareSize
        let maxY = screenMetrics.HeightPixels / squareSize

        writeDebug (sprintf "Max X: %i" maxX)
        writeDebug (sprintf "Max Y: %i" maxY)

        let gameConfig = 
            let min = { x = 0; y = 0 }
            let max = { x = maxX; y = maxY }
            let gameBounds = min, max
            { bounds = gameBounds
              startPosition = getBoundsCenter gameBounds
              startLength = 4
              startDirection = Right }

        let fireAction, actionsStream = 
            let e = Event<_> ()
            e.Trigger, e.Publish

        let renderer = 
            { redraw = fun s f m l -> x.RunOnUiThread (fun _ -> writeDebug (sprintf "Moving; snake=%A; food=%A; mines=%A" s f m))
              ended = fun s f m l -> writeDebug (sprintf "Ended; snake=%A; food=%A; mines=%A" s f m) }

        let checkGameStatus = Game.startGame gameConfig renderer actionsStream

        writeDebug (sprintf "Game started; status=%A" <| checkGameStatus ())