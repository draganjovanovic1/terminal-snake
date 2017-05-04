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