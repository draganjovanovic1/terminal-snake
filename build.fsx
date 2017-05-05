// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.XamarinHelper

// Directories
let buildDir  = "./build/"

// Targets
Target "Clean" <| fun _ ->
    CleanDirs [buildDir]

Target "Restore" <| fun _ ->
    Shell.Exec ("dotnet", "restore terminal-snake.sln") |> ignore

Target "RestoreNpm" <| fun _ ->
    Shell.Exec ("npm", "install") |> ignore

Target "BuildLib" <| fun _ ->
    Shell.Exec ("dotnet", "build ./src/snake-game-lib/snake-game-lib.fsproj -o ../../build/snake-game-lib") |> ignore

Target "BuildTerminal" <| fun _ ->
    Shell.Exec ("dotnet", "build ./src/terminal-snake/terminal-snake.fsproj -o ../../build/terminal-snake") |> ignore

Target "BuildWeb" <| fun _ ->
    Shell.Exec ("./node_modules/.bin/fable", "src/web-snake/") |> ignore
    Copy "./build/web-snake/" ["./src/web-snake/index.html"]

Target "AndroidPackage" <| fun () ->
    trace "Cleaning Android project"
    MSBuildWithDefaults "Clean" ["./src/android-snake/android-snake.fsproj"] |> ignore

    trace "Packaging Android app"
    AndroidPackage (fun defaults ->
        { defaults with
            ProjectPath = "./src/android-snake/android-snake.fsproj"
            Configuration = "Release"
            OutputPath = "./build/android/.temp/x" }
    ) 
    |> AndroidSignAndAlign (fun defaults ->
        { defaults with
            KeystorePath = "/Users/dragan/Library/Developer/Xamarin/Keystore/draganjovanovic1/draganjovanovic1.keystore"
            KeystorePassword = "123123" // TODO: don't store this in the build script for a real app!
            KeystoreAlias = "draganjovanovic1" }
    )
    |> fun file -> MoveFile "./build/android/" file.FullName

    trace "Removing Android temp build"
    FileHelper.DeleteDir "./build/android/.temp"

Target "IosPackage" <| fun () ->
    trace "Cleaning iOS project"
    MSBuildWithDefaults "Clean" ["./src/ios-snake/ios-snake.fsproj"] |> ignore

    trace "Packaging iOS app"
    iOSBuild (fun defaults ->
        { defaults with
            ProjectPath = "./src/ios-snake/ios-snake.fsproj"
            Configuration = "Release|iPhone"
            Target = "Build"
            OutputPath = "./build/ios/.temp/" }
    )

    // let outputFolder = Path.Combine("src", "TipCalc.iOS", "bin", "iPhone", "AppStore")
    // let appPath = Directory.EnumerateDirectories(outputFolder, "*.app").First()
    // let zipFilePath = Path.Combine(outputFolder, "TipCalc.iOS.zip")
    // let zipArgs = String.Format("-r -y '{0}' '{1}'", zipFilePath, appPath)

    // Exec "zip" zipArgs

    // TeamCityHelper.PublishArtifact zipFilePath


// Build order
"Clean"
  ==> "Restore"
  ==> "RestoreNpm"
  ==> "BuildLib"
  ==> "BuildTerminal"
  ==> "BuildWeb"
  ==> "AndroidPackage"
  ==> "IosPackage"

// start build
RunTargetOrDefault "BuildWeb"