// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake

// Directories
let buildDir  = "./build/"

// Targets
Target "Clean" <| fun _ ->
    CleanDirs [buildDir]

Target "Restore" <| fun _ ->
    Shell.Exec ("dotnet", "restore terminal-snake.sln") |> ignore

Target "BuildLib" <| fun _ ->
    Shell.Exec ("dotnet", "build src/snake-game-lib/snake-game-lib.fsproj -o ../../build/snake-game-lib") |> ignore

Target "BuildWeb" <| fun _ ->
    Shell.Exec ("fable", "src/web-snake/") |> ignore
    Copy "./build/web-snake/" ["src/web-snake/index.html"]

Target "BuildTerminal" <| fun _ ->
    Shell.Exec ("dotnet", "build src/terminal-snake/terminal-snake.fsproj -o ../../build/terminal-snake") |> ignore

// Build order
"Clean"
  ==> "Restore"
  ==> "BuildLib"
  ==> "BuildWeb"
  ==> "BuildTerminal"

// start build
RunTargetOrDefault "BuildTerminal"