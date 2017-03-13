// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake

// Directories
let buildDir  = "./build/"

// Targets
Target "Clean" <| fun _ ->
    CleanDirs [buildDir]

Target "Restore packages" <| fun _ ->
    Shell.Exec ("dotnet", "restore") |> ignore

Target "Build lib" <| fun _ ->
    Shell.Exec ("dotnet", "build src/snake-game-lib/snake-game-lib.fsproj -o ../../build/snake-game-lib") |> ignore

Target "Build JS" <| fun _ ->
    Shell.Exec ("fable", "src/snake-game/Game.fsx -o ./build/snake-game-js/") |> ignore

Target "Build terminal" <| fun _ ->
    Shell.Exec ("dotnet", "build src/terminal-snake/terminal-snake.fsproj -o ../../build/terminal-snake") |> ignore

// Build order
"Clean"
  ==> "Restore packages"
  ==> "Build lib"
  ==> "Build JS"
  ==> "Build terminal"

// start build
RunTargetOrDefault "Build terminal"