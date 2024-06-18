## A sysfetch entirely styled from <https://github.com/willeccles/f>
app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.5/1JOFFXrqOrdoINq6C4OJ8k3UK0TJhgITLbcOb-6WMwY.tar.br",
}

import cli.Cmd
import cli.Env
import cli.Stdout
import cli.Task exposing [Task]
import ansi.Core as Ansi

strLen = \text ->
    Str.toUtf8 text
    |> List.len

basenameOfPath = \path ->
    when Str.splitLast path "/" is
        Ok { before: _, after: base } -> base
        _ -> path

getProgramNameFromEnvVar = \varName ->
    Env.var varName
    |> Task.map basenameOfPath
    |> Task.result

getStdoutOfCommand = \name, args ->
    output =
        Cmd.new name
            |> Cmd.args args
            |> Cmd.output
            |> Task.result!

    when output is
        Ok { stdout } ->
            stdout
            |> Str.fromUtf8
            |> Result.map Str.trim
            |> Task.fromResult

        Err (CmdOutputError (_, err)) ->
            commandName = List.concat [name] args |> Str.joinWith " "
            errorMessage = "Failed to run command `$(commandName)`: $(Inspect.toStr err)"

            Task.err (Exit 1 errorMessage)

infoSectionsBlock = \sections ->
    longestSectionName =
        sections
        |> List.map \(name, _info) -> strLen name
        |> List.max
        |> Result.withDefault 0

    renderSection = \(name, info) ->
        nameLen = strLen name
        startBuffer = Str.repeat " " (longestSectionName - nameLen + 1)

        [startBuffer, name |> Ansi.withFg (Standard Cyan), " | ", info]
        |> Str.joinWith ""

    sections
    |> List.map renderSection
    |> Str.joinWith "\n"

colorDemoBlock = \colors ->
    [(" ▄▄", Standard), (" ▀▀", Bright)]
    |> List.map \(textToColor, modifier) ->
        colors
        |> List.map \color -> Ansi.withFg textToColor (modifier color)
        |> Str.joinWith ""
    |> Str.joinWith "\n"

main =
    editor = getProgramNameFromEnvVar! "EDITOR"
    shell = getProgramNameFromEnvVar! "SHELL"

    username = getStdoutOfCommand! "logname" []
    hostname = getStdoutOfCommand! "hostname" []
    osName = getStdoutOfCommand! "uname" []
    osVersion = getStdoutOfCommand! "uname" ["-r"]
    architecture = getStdoutOfCommand! "uname" ["-m"]

    colorsToDemo = [Red, Green, Yellow, Blue, Magenta, Cyan]
    unknownInItalics = Ansi.withStyle "unknown" [Italic On]
    infoSections = [
        ("os", "$(osName) $(osVersion)"),
        ("arch", architecture),
        ("editor", editor |> Result.withDefault unknownInItalics),
        ("shell", shell |> Result.withDefault unknownInItalics),
    ]
    Stdout.line!
        " $(Ansi.withFg username (Standard Blue))@$(Ansi.withFg hostname (Standard Blue))"
    Stdout.line! (infoSectionsBlock infoSections)
    Stdout.line! (colorDemoBlock colorsToDemo)
