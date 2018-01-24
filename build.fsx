#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.XamarinHelper
open Fake.DotNetCli
open Fake.NpmHelper

//sh build.sh AndroidPackage -ev keystorePath=*.keystore -ev keystorePwd=****** -ev keystoreAlias=******

// Directories
let buildDir  = "./build/"

// Targets
Target "Clean" <| fun _ ->
    !! "src/**/bin"
     ++ "src/**/obj"
     ++ buildDir
    |> CleanDirs

Target "Restore" <| fun _ ->
    Restore <| fun p ->
        { p with
            NoCache = false
            AdditionalArgs = ["terminal-snake.sln"] }

Target "RestoreNpm" <| fun _ ->
    Npm <| fun p ->
        { p with
            Command = Install Standard }

Target "BuildLib" <| fun _ ->
    DotNetCli.Build <| fun p ->
        { p with
            Configuration = "Release"
            Project = "./src/snake-game-lib/snake-game-lib.fsproj"
            Output = "../../build/snake-game-lib" }

Target "BuildTerminal" <| fun _ ->
    DotNetCli.Build <| fun p ->
        { p with
            Configuration = "Release"
            Project = "./src/terminal-snake/terminal-snake.fsproj"
            Output = "../../build/terminal-snake" }

Target "BuildWeb" <| fun _ ->
    Shell.Exec ("./node_modules/.bin/fable", "src/web-snake/") |> ignore
    Copy "./build/web-snake/" ["./src/web-snake/index.html"]

Target "AndroidPackage" <| fun () ->
    trace "Cleaning Android project"
    MSBuildWithDefaults "Clean" ["./src/android-snake/android-snake.fsproj"] |> ignore

    let keystorePath = getBuildParam "keystorePath"
    let keystorePwd = getBuildParam "keystorePwd"
    let keystoreAlias = getBuildParam "keystoreAlias"

    trace <| sprintf "Using keystore %s; path=%s; password=%s" keystoreAlias keystorePath keystorePwd

    trace "Packaging Android app"
    AndroidPackage (fun defaults ->
        { defaults with
            ProjectPath = "./src/android-snake/android-snake.fsproj"
            Configuration = "Release"
            OutputPath = "./build/android/.temp/x" }
    )
    |> AndroidSignAndAlign (fun defaults ->
        { defaults with
            KeystorePath = keystorePath
            KeystorePassword = keystorePwd
            KeystoreAlias = keystoreAlias }
    )
    |> fun file -> MoveFile "./build/android/" file.FullName

    trace "Removing Android temp build"
    FileHelper.DeleteDir "./build/android/.temp"

Target "IosPackage" <| fun () ->
    trace "Cleaning iOS project"
    MSBuildWithDefaults "Clean" ["./src/ios-snake/ios-snake.fsproj"] |> ignore

    let buildPath = "./build/ios/"

    trace "Packaging iOS app"
    iOSBuild (fun defaults ->
        { defaults with
            ProjectPath = "./src/ios-snake/ios-snake.fsproj"
            Configuration = "Release|iPhone"
            Target = "Build"
            OutputPath = buildPath + ".temp/" }
    )

    let appPath = "./build/ios/IOS.Snake.app"
    ensureDirectory appPath

    FileSystemHelper.subDirectories (directoryInfo buildPath)
    |> Seq.find (fun d -> d.Name.EndsWith ".app")
    |> fun a -> copyRecursive a (directoryInfo appPath) true |> ignore

    DeleteDirs ["./build/ios/.tempIOS.Snake.app"; "./build/ios/.temp"]
    DeleteFile "./build/ios/.tempmtouch.stamp"

    // let outputFolder = Path.Combine("src", "TipCalc.iOS", "bin", "iPhone", "AppStore")
    // let appPath = Directory.EnumerateDirectories(outputFolder, "*.app").First()
    // let zipFilePath = Path.Combine(outputFolder, "TipCalc.iOS.zip")
    // let zipArgs = String.Format("-r -y '{0}' '{1}'", zipFilePath, appPath)

    // Exec "zip" zipArgs

    // TeamCityHelper.PublishArtifact zipFilePath


// Build order
"Clean"

"Restore"
  ==> "BuildLib"
  ==> "BuildTerminal"

"RestoreNpm"
  ==> "BuildWeb"

// "Restore"
//   ==> "AndroidPackage"

// "Restore"
//   ==> "IosPackage"

// start build
RunTargetOrDefault "BuildTerminal"