namespace Android.Snake

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